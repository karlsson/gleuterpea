#include <erl_nif.h>
#include <math.h>
#include <string.h>
#include "gleuterpea_nif.h"

static ErlNifResourceType* osc_type;
static float twopi = 2 * acosf(-1.0);

static float saw(float progress) {
  return (1.0 - progress);
}

static float triangle(float progress) {
  // 0.0 <= progress < 4.0
  return (progress < 2.f)?(progress - 1.f):(3.f - progress);
}

/* ----------------------------------------------------------------------- */
typedef struct
{
  unsigned int rate;
  float phase;
  float max;
  float (*f)(float);
} Osc;

static ERL_NIF_TERM osc_ctor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned int rate;
  char type[12];
  if (!enif_get_uint(env, argv[0], &rate)){
    return enif_make_badarg(env);
  }
  if (!enif_get_atom(env, argv[1], type, 12, ERL_NIF_LATIN1)){
    return enif_make_badarg(env);
  }

  Osc *unit  = enif_alloc_resource(osc_type, sizeof(Osc));
  unit->rate = rate;
  if (strcmp(type, "sin") == 0) {
    unit->f = &sinf;
    unit->max = twopi;
  } else if (strcmp(type, "saw") == 0) {
    unit->f = &saw;
    unit->max = 2.0;
  } else {
    unit->f = &triangle;
    unit->max = 4.0;
  }
  unit->phase = 0.0;
  ERL_NIF_TERM term = enif_make_resource(env, unit);
  enif_release_resource(unit);
  return term;
}

static ERL_NIF_TERM osc_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  Osc* unit;
  unsigned int no_of_frames;
  float phase;
  double freq;
  ERL_NIF_TERM new_binary;

  if (!enif_get_resource(env, argv[0], osc_type, (void**) &unit)){
    return enif_make_badarg(env);
  }
  phase = unit->phase;
  if (!enif_get_double(env, argv[1], &freq)){
    return enif_make_badarg(env);
  }
  if (!enif_get_uint(env, argv[2], &no_of_frames)){
    return enif_make_badarg(env);
  }

  unsigned int bin_size = no_of_frames * FRAME_SIZE;
  FRAME_TYPE * data = (FRAME_TYPE *) enif_make_new_binary(env, bin_size, &new_binary);
  float delta = unit->max * freq / unit->rate;
  for(unsigned int i = 0; i < no_of_frames; i++){
    data[i] = (*unit->f)(phase);
    phase = advance_phase(phase + delta, unit->max);
  }
  unit->phase = phase;
  return new_binary;
}

/* ----------------------------------------------------------------------- */

static ErlNifFunc nif_funcs[] = {
  {"osc_ctor", 2, osc_ctor},
  {"osc_next", 3, osc_next}
};

static int open_osc_resource_type(ErlNifEnv* env)
{
  const char* mod = "Elixir.Granulix.Generator.Oscillator";
  const char* resource_type = "osc";
  int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
  osc_type = enif_open_resource_type(env, mod, resource_type,
                                     NULL, flags, NULL);
  return ((osc_type == NULL) ? -1:0);
}

static int load(ErlNifEnv* caller_env, void** priv_data, ERL_NIF_TERM load_info)
{
  return open_osc_resource_type(caller_env);
}

static int upgrade(ErlNifEnv* caller_env, void** priv_data, void** old_priv_data,
		   ERL_NIF_TERM load_info)
{
  return open_osc_resource_type(caller_env);
}


ERL_NIF_INIT(gleuterpea@generator@osc, nif_funcs, load, NULL, upgrade, NULL);
