#include <erl_nif.h>
#include <math.h>
#include <string.h>

/* Code translated from Elixir - Synthex.Filter.Bitcrusher:
https://github.com/bitgamma/synthex/blob/master/lib/synthex/filter/bitcrusher.ex
transposed to Erlang NIF library.
*/

static ErlNifResourceType* bitcrusher_type;

typedef struct
{
  /* Pass  cutoff and resonance in period call (.._next function) instead
     so that they can be updated every period (control rate)
     double bits; double normalized_frequency;
  */
  float last, phaser;
} Bitcrusher;


static ERL_NIF_TERM bitcrusher_ctor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  Bitcrusher * unit  = enif_alloc_resource(bitcrusher_type, sizeof(Bitcrusher));
  unit->last = unit->phaser = 0.0;
  ERL_NIF_TERM term = enif_make_resource(env, unit);
  enif_release_resource(unit);
  return term;
}


static ERL_NIF_TERM bitcrusher_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  Bitcrusher * unit; // state pointer

  // Audio rate input output
  ErlNifBinary in_bin;
  float * out, * in;
  ERL_NIF_TERM out_term;

  float phaser, last;
  float sample, output;
  // control(-rate) parameters
  double normalized_frequency;
  double bits;

  if (!enif_get_resource(env, argv[0],
                         bitcrusher_type,
                         (void**) &unit)){
    return enif_make_badarg(env);
  }

  if(!enif_inspect_binary(env, argv[1], &in_bin)){
    return enif_make_badarg(env);
  }

  if(!(enif_get_double(env, argv[2], &bits) &&
       enif_get_double(env, argv[3], &normalized_frequency)
       )) {
    return enif_make_badarg(env);
  }

  int no_of_frames = in_bin.size / sizeof(float);
  in = (float * ) in_bin.data;
  out = (float *) enif_make_new_binary(env, in_bin.size, &out_term);

  last = unit->last;
  phaser = unit->phaser;
  float step =  powf(0.5, (float) bits);

  sample = output =  0.0;

  for (int i = 0; i < no_of_frames; i++) {
    sample = in[i];
    phaser += normalized_frequency;
    if (phaser >= 1.0) {
      last = step * floor(sample / step + 0.5);
      phaser -= 1.0;
    }  
    out[i] = last;
  }

  // Variables were updated need to be stored back into the state
  unit->last = last;
  unit->phaser = phaser;
  
  return out_term;
}

/* ----------------------------------------------------------------------- */

static ErlNifFunc nif_funcs[] = {
  {"bitcrusher_ctor", 0, bitcrusher_ctor},
  {"bitcrusher_next", 4, bitcrusher_next}
};

static int open_bitcrusher_resource_type(ErlNifEnv* env)
{
  const char* mod = "Elixir.Granulix.Filter.Bitcrusher";
  const char* resource_type = "bitcrusher";
  int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
  bitcrusher_type =
    enif_open_resource_type(env, mod, resource_type,
                            NULL, flags, NULL);
  return ((bitcrusher_type == NULL) ? -1:0);
}

static int load(ErlNifEnv* caller_env, void** priv_data, ERL_NIF_TERM load_info)
{
  return open_bitcrusher_resource_type(caller_env);
}

static int upgrade(ErlNifEnv* caller_env, void** priv_data, void** old_priv_data,
		   ERL_NIF_TERM load_info)
{
  return open_bitcrusher_resource_type(caller_env);
}


ERL_NIF_INIT(gleuterpea@filter@bitcrusher, nif_funcs, load, NULL, upgrade, NULL);
