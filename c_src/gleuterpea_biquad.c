#include <erl_nif.h>
#include <math.h>
#include <string.h>

/* Code translated from Elixir - Synthex.Filter.Biquad:
https://github.com/bitgamma/synthex/blob/master/lib/synthex/filter/biquad.ex
transposed to Erlang NIF library.
*/

static ErlNifResourceType* biquad_type;

typedef struct
{
  /* Pass coefficients in period call (.._next function) instead so that they
     can be updated every period
     float a0, a1, a2, b0, b1, b2;
  */
  double i1, i2, o1, o2;
} Biquad;


static ERL_NIF_TERM biquad_ctor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  Biquad * unit  = enif_alloc_resource(biquad_type, sizeof(Biquad));
  unit->i1 = unit->i2 = unit->o1 = unit->o2 = 0.0;
  ERL_NIF_TERM term = enif_make_resource(env, unit);
  enif_release_resource(unit);
  return term;
}


static ERL_NIF_TERM biquad_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  Biquad * unit; // state pointer

  // Audio rate input output
  ErlNifBinary in_bin;
  float * out, * in;
  ERL_NIF_TERM out_term;

  // control(-rate) parameters
  const ERL_NIF_TERM * cofs; // Coefficients tuple with 6 elements of double
  int arity;
  double a0, a1, a2, b0, b1, b2;

  if (!enif_get_resource(env, argv[0],
                         biquad_type,
                         (void**) &unit)){
    return enif_make_badarg(env);
  }

  if(!enif_inspect_binary(env, argv[1], &in_bin)){
    return enif_make_badarg(env);
  }

  if(!(enif_get_tuple(env, argv[2], &arity, &cofs) &&
       arity == 7 &&
       // Note first element [0] in tuple will be gleam type {bi_quad, ....}
       enif_get_double(env, cofs[1], &a0) &&
       enif_get_double(env, cofs[2], &a1) &&
       enif_get_double(env, cofs[3], &a2) &&
       enif_get_double(env, cofs[4], &b0) &&
       enif_get_double(env, cofs[5], &b1) &&
       enif_get_double(env, cofs[6], &b2)
       )) {
    return enif_make_badarg(env);
  }

  int inNumSamples = in_bin.size / sizeof(float);
  in = (float * ) in_bin.data;
  out = (float *) enif_make_new_binary(env, in_bin.size, &out_term);

  double i1 = unit->i1;
  double i2 = unit->i2;
  double o1 = unit->o1;
  double o2 = unit->o2;

  float sample_m = b0/a0;
  float i1_m  = b1/a0;
  float i2_m = b2/a0;
  float o1_m = a1/a0;
  float o2_m = a2/a0;
  float sample, output = 0.0;

  for (int i = 0; i < inNumSamples; i++) {
    sample = in[i];
    output = ((sample_m * sample) + (i1_m * i1) + (i2_m * i2) - (o1_m * o1) - (o2_m * o2));
    i2 = i1;
    i1 = sample;
    o2 = o1;
    o1 = out[i] = output;
  }

  // Four variables were updated and need to be stored back into the state
  unit->i1 = i1;
  unit->i2 = i2;
  unit->o1 = o1;
  unit->o2 = o2;

  return out_term;
}

/* ----------------------------------------------------------------------- */

static ErlNifFunc nif_funcs[] = {
  {"biquad_ctor", 0, biquad_ctor},
  {"biquad_next", 3, biquad_next}
};

static int open_biquad_resource_type(ErlNifEnv* env)
{
  const char* mod = "Elixir.Granulix.Filter.Biquad";
  const char* resource_type = "biquad";
  int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
  biquad_type =
    enif_open_resource_type(env, mod, resource_type,
                            NULL, flags, NULL);
  return ((biquad_type == NULL) ? -1:0);
}

static int load(ErlNifEnv* caller_env, void** priv_data, ERL_NIF_TERM load_info)
{
  return open_biquad_resource_type(caller_env);
}

static int upgrade(ErlNifEnv* caller_env, void** priv_data, void** old_priv_data,
		   ERL_NIF_TERM load_info)
{
  return open_biquad_resource_type(caller_env);
}


ERL_NIF_INIT(gleuterpea@filter@biquad, nif_funcs, load, NULL, upgrade, NULL);