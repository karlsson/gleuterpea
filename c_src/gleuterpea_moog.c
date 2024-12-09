#include <erl_nif.h>
#include <math.h>
#include <string.h>

/* Code translated from Elixir - Synthex.Filter.Moog:
https://github.com/bitgamma/synthex/blob/master/lib/synthex/filter/moog.ex
transposed to Erlang NIF library.
*/

static ErlNifResourceType* moog_type;

typedef struct
{
  /* Pass  cutoff and resonance in period call (.._next function) instead
     so that they can be updated every period (control rate)
     double cutoff, resonance;
  */
  float i1, i2, i3, i4, o1, o2, o3, o4;
} Moog;


static ERL_NIF_TERM moog_ctor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  Moog * unit  = enif_alloc_resource(moog_type, sizeof(Moog));
  unit->i1 = unit->i2 = unit->i3 = unit->i4 = 0.0;
  unit->o1 = unit->o2 = unit->o3 = unit->o4 = 0.0;
  ERL_NIF_TERM term = enif_make_resource(env, unit);
  enif_release_resource(unit);
  return term;
}


static ERL_NIF_TERM moog_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  Moog * unit; // state pointer

  // Audio rate input output
  ErlNifBinary in_bin;
  float * out, * in;
  ERL_NIF_TERM out_term;

  float i1, i2, i3, i4, o1, o2, o3, o4;
  float sample, output;
  // control(-rate) parameters
  double cutoff, resonance;

  if (!enif_get_resource(env, argv[0],
                         moog_type,
                         (void**) &unit)){
    return enif_make_badarg(env);
  }

  if(!enif_inspect_binary(env, argv[1], &in_bin)){
    return enif_make_badarg(env);
  }

  if(!(enif_get_double(env, argv[2], &cutoff) &&
       enif_get_double(env, argv[3], &resonance)
       )) {
    return enif_make_badarg(env);
  }

  int no_of_frames = in_bin.size / sizeof(float);
  in = (float * ) in_bin.data;
  out = (float *) enif_make_new_binary(env, in_bin.size, &out_term);

  i1 = unit->i1; i2 = unit->i2; i3 = unit->i3; i4 = unit->i4;
  o1 = unit->o1; o2 = unit->o2; o3 = unit->o3; o4 = unit->o4;

  float f = cutoff * 1.16;
  float f_squared = f * f;
  float fb = resonance * (1.0 - 0.15 * f_squared);
  float f2 = 0.35013 * f_squared * f_squared;

  sample = output =  0.0;

  for (int i = 0; i < no_of_frames; i++) {
    sample = in[i];
    sample = sample - o4 * fb;
    sample = sample * f2;
    o1 = sample + 0.3 * i1 + (1 - f) * o1;
    o2 = o1 + 0.3 * i2 + (1 - f) * o2;
    o3 = o2 + 0.3 * i3 + (1 - f) * o3;
    o4 = o3 + 0.3 * i4 + (1 - f) * o4;
    i1 = sample;
    i2 = o1;
    i3 = o2;
    i4 = o3;
    out[i] = o4;
  }

  // Variables were updated need to be stored back into the state
  unit->i1 = i1; unit->i2 = i2; unit->i3 = i3; unit->i4 = i4;
  unit->o1 = o1; unit->o2 = o2; unit->o3 = o3; unit->o4 = o4;

  return out_term;
}

/* ----------------------------------------------------------------------- */

static ErlNifFunc nif_funcs[] = {
  {"moog_ctor", 0, moog_ctor},
  {"moog_next", 4, moog_next}
};

static int open_moog_resource_type(ErlNifEnv* env)
{
  const char* mod = "Elixir.Granulix.Filter.Moog";
  const char* resource_type = "moog";
  int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
  moog_type =
    enif_open_resource_type(env, mod, resource_type,
                            NULL, flags, NULL);
  return ((moog_type == NULL) ? -1:0);
}

static int load(ErlNifEnv* caller_env, void** priv_data, ERL_NIF_TERM load_info)
{
  return open_moog_resource_type(caller_env);
}

static int upgrade(ErlNifEnv* caller_env, void** priv_data, void** old_priv_data,
		   ERL_NIF_TERM load_info)
{
  return open_moog_resource_type(caller_env);
}


ERL_NIF_INIT(gleuterpea@filter@moog, nif_funcs, load, NULL, upgrade, NULL);
