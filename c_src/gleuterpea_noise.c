#include <erl_nif.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

/* Code translated from Elixir - Synthex.Generator.Noise:
https://github.com/bitgamma/synthex/blob/master/lib/synthex/generator/noise.ex
transposed to Erlang NIF library.
*/

static ErlNifResourceType* noise_type;

typedef enum
  {
    WHITE,
    PINK,
    BROWN
  } NoiseType;

typedef struct
{
  NoiseType type;
  float b0, b1, b2, b3, b4, b5, b6;
} Noise;


static inline float frand() {
  float x = (float) rand()/RAND_MAX;
  return 2.f * x - 1.f;
}

static ERL_NIF_TERM noise_ctor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  char type[12];

  if (!enif_get_atom(env, argv[0], type, 12, ERL_NIF_LATIN1)){
    return enif_make_badarg(env);
  }

  Noise * unit  = enif_alloc_resource(noise_type, sizeof(Noise));

  if (strcmp(type, "white") == 0) {
    unit->type = WHITE;
  } else if (strcmp(type, "pink") == 0) {
    unit->type = PINK;
  } else {
    unit->type = BROWN;
  }

  unit->b0 = unit->b1 = unit->b2 = unit->b3 = unit->b4 = unit->b5 = unit->b6 = 0.0;

  ERL_NIF_TERM term = enif_make_resource(env, unit);
  enif_release_resource(unit);
  return term;
}


static ERL_NIF_TERM noise_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  Noise * unit; // state pointer
  unsigned int no_of_frames;

  // Audio rate input output
  float * out;
  ERL_NIF_TERM out_term;

  if (!enif_get_resource(env, argv[0],
                         noise_type,
                         (void**) &unit)){
    return enif_make_badarg(env);
  }

  if (!enif_get_uint(env, argv[1], &no_of_frames)){
    return enif_make_badarg(env);
  }

  out = (float *) enif_make_new_binary(env, no_of_frames * sizeof(float), &out_term);

  float b0, b1, b2, b3, b4, b5, b6;
  float white = 0.0, pink = 0.0;
  b0 = unit->b0; b1 = unit->b1; b2 = unit->b2; b3 = unit->b3;
  b4 = unit->b4; b5 = unit->b5; b6 = unit->b6;

  for (unsigned int i = 0; i < no_of_frames; i++) {
    white = frand();
    switch(unit->type) {
    case WHITE:
      out[i] = white;
      break;
    case PINK:
      b0 = 0.99886 * b0 + white * 0.0555179;
      b1 = 0.99332 * b1 + white * 0.0750759;
      b2 = 0.96900 * b2 + white * 0.1538520;
      b3 = 0.86650 * b3 + white * 0.3104856;
      b4 = 0.55000 * b4 + white * 0.5329522;
      b5 = -0.7616 * b5 - white * 0.0168980;
      pink = b0 + b1 + b2 + b3 + b4 + b5 + b6 + white * 0.5362;
      b6 = white * 0.115926;
      out[i] = pink * 0.11;
      break;
    case BROWN:
      b0 = (b0 + (0.02 * white)) / 1.02;
      out[i] = b0 * 3.5;
      break;
    }
  }
  unit->b0 = b0; unit->b1 = b1; unit->b2 = b2; unit->b3 = b3;
  unit->b4 = unit->b4; unit->b5 = b5; unit->b6 = b6;
  return out_term;
}

/* ----------------------------------------------------------------------- */

static ErlNifFunc nif_funcs[] = {
  {"noise_ctor", 1, noise_ctor},
  {"noise_next", 2, noise_next}
};

static int open_noise_resource_type(ErlNifEnv* env)
{
  const char* mod = "Elixir.Granulix.Generator.Noise";
  const char* resource_type = "noise";
  int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
  noise_type =
    enif_open_resource_type(env, mod, resource_type,
                            NULL, flags, NULL);
  return ((noise_type == NULL) ? -1:0);
}

static int load(ErlNifEnv* caller_env, void** priv_data, ERL_NIF_TERM load_info)
{
  srand((unsigned)time(NULL));
  return open_noise_resource_type(caller_env);
}

static int upgrade(ErlNifEnv* caller_env, void** priv_data, void** old_priv_data,
		   ERL_NIF_TERM load_info)
{
  return open_noise_resource_type(caller_env);
}


ERL_NIF_INIT(gleuterpea@generator@noise, nif_funcs, load, NULL, upgrade, NULL);
