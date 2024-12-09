#include <erl_nif.h>
#include <math.h>
#include <string.h>
#include "sc_plug.h"

/* Code from SuperCollider plugin example:
https://github.com/supercollider/example-plugins/blob/master/03-AnalogEcho/AnalogEcho.cpp
translated to Erlang NIF library.
*/

static inline int advance_int_phase(int newphase, int max){
  if(newphase < 0) {
    newphase += max;
  }
  return(newphase < max)? newphase:(newphase - max);
}

static ErlNifResourceType* analog_echo_type;

typedef struct
{
  unsigned int rate;
  unsigned int period_size;
  float maxdelay;  // Max delay in seconds
  int bufsize;     // Size of buffer in samples, always modulo 8
  float* empty_period; // Empty period buffer
  float* buf; // Buffer itself
  int writephase;  // Position of write head
  float s1;   // State of the one-pole lowpass filter
} AnalogEcho;


static ERL_NIF_TERM analog_echo_ctor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  double maxdelay;
  unsigned int rate, period_size;
  if (!enif_get_uint(env, argv[0], &rate)){
    return enif_make_badarg(env);
  }
  if (!enif_get_uint(env, argv[1], &period_size)){
    return enif_make_badarg(env);
  }
  if (!enif_get_double(env, argv[2], &maxdelay)){
    return enif_make_badarg(env);
  }

  AnalogEcho * aep  = enif_alloc_resource(analog_echo_type, sizeof(AnalogEcho));
  aep->rate = rate;
  aep->period_size = period_size;
  aep->maxdelay = (float) maxdelay;

  aep->empty_period = (float *) enif_alloc(period_size * sizeof(float));
  for(unsigned int i = 0; i < period_size; i++){
    aep->empty_period[i] = 0.0;
  }

  aep->bufsize = ((int)(rate * aep->maxdelay) / 8 + 2) * 8;
  aep->buf = (float *) enif_alloc(aep->bufsize * sizeof(float));
  memset(aep->buf, 0, aep->bufsize * sizeof(float));

  aep->writephase = 0;
  aep->s1 = 0.0;
  ERL_NIF_TERM term = enif_make_resource(env, aep);
  enif_release_resource(aep);
  return term;
}

// ErlNifResourceDtor
static void ae_resource_dtor(ErlNifEnv* env, void * obj){
  enif_free(((AnalogEcho*) obj)->buf);
  enif_free(((AnalogEcho*) obj)->empty_period);
}

static ERL_NIF_TERM analog_echo_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  AnalogEcho * aep; // state pointer

  // Audio rate input output
  ErlNifBinary in_bin;
  float * out, * in;
  ERL_NIF_TERM out_term;

  // control(-rate) parameters
  double delay; // delay
  double fb;    // feedback coefficient
  double coeff; // filter coefficient

  if (!enif_get_resource(env, argv[0], analog_echo_type, (void**) &aep)){
    return enif_make_badarg(env);
  }
  if(!enif_inspect_binary(env, argv[1], &in_bin)){
    return enif_make_badarg(env);
  }

  if(!(enif_get_double(env, argv[2], &delay) &&
       enif_get_double(env, argv[3], &fb) &&
       enif_get_double(env, argv[4], &coeff))) {
    return enif_make_badarg(env);
  }

  unsigned int inNumSamples;

  if(in_bin.size == 0) {
    inNumSamples = aep->period_size;
    in = aep->empty_period;
  } else {
    inNumSamples = in_bin.size / sizeof(float);
    in = (float * ) in_bin.data;
  }
  out = (float *) enif_make_new_binary(env, inNumSamples * sizeof(float), &out_term);

  float* buf = aep->buf;
  int writephase = aep->writephase;
  float s1 = aep->s1;
  int bufsize = aep->bufsize;

  if (delay > aep->maxdelay){
    delay = aep->maxdelay;
  }

  float delay_samples = (float) aep->rate * delay;
  int offset = delay_samples;
  float frac = delay_samples - offset;

  float a = 1 - fabsf(coeff);
  for (unsigned int i = 0; i < inNumSamples; i++) {

    // Four integer phases into the buffer
    int phase1 = writephase - offset;
    int phase2 = phase1 - 1;
    int phase3 = phase1 - 2;
    int phase0 = phase1 + 1;
    float d0 = buf[advance_int_phase(phase0, bufsize)];
    float d1 = buf[advance_int_phase(phase1, bufsize)];
    float d2 = buf[advance_int_phase(phase2, bufsize)];
    float d3 = buf[advance_int_phase(phase3, bufsize)];
    // Use cubic interpolation with the fractional part of the delay in samples
    float delayed = cubicinterp(frac, d0, d1, d2, d3);

    // Apply lowpass filter and store the state of the filter.
    float lowpassed = a * delayed + coeff * s1;
    s1 = lowpassed;

    // Multiply by feedback coefficient and add to input signal.
    // zapgremlins gets rid of Bad Things like denormals, explosions, etc.
    out[i] = zapgremlins(in[i] + fb * lowpassed);
    buf[writephase] = out[i];

    writephase = advance_int_phase(writephase + 1, bufsize);
  }

  // Two variables were updated and need to be stored back into the state of the UGen.
  aep->writephase = writephase;
  aep->s1 = s1;

  return out_term;
}

/* ----------------------------------------------------------------------- */

static ErlNifFunc nif_funcs[] = {
  {"analog_echo_ctor", 3, analog_echo_ctor},
  {"analog_echo_next", 5, analog_echo_next}
};

static int open_analog_echo_resource_type(ErlNifEnv* env)
{
  const char* mod = "Elixir.Granulix.Plugin.AnalogEcho";
  const char* resource_type = "analog_echo";
  int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
  analog_echo_type =
    enif_open_resource_type(env, mod, resource_type,
                            ae_resource_dtor, flags, NULL);
  return ((analog_echo_type == NULL) ? -1:0);
}

static int load(ErlNifEnv* caller_env, void** priv_data, ERL_NIF_TERM load_info)
{
  return open_analog_echo_resource_type(caller_env);
}

static int upgrade(ErlNifEnv* caller_env, void** priv_data, void** old_priv_data,
		   ERL_NIF_TERM load_info)
{
  return open_analog_echo_resource_type(caller_env);
}


ERL_NIF_INIT(Elixir.SC.Reverb.AnalogEcho, nif_funcs, load, NULL, upgrade, NULL);
