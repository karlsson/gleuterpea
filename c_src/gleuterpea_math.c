#include <erl_nif.h>
#include <math.h>
#ifdef __AVX__
#include <immintrin.h>
#endif

#define FRAME_TYPE float
#define FRAME_SIZE sizeof(FRAME_TYPE)

static ERL_NIF_TERM mul(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  ErlNifBinary xbin;
  FRAME_TYPE *x, *z;
  ERL_NIF_TERM zterm;
  int is_int = 0;
  double md;
  int mi;

  if(!(enif_inspect_binary(env, argv[0], &xbin) &&
       (enif_get_double(env, argv[1], &md) ||
	(is_int = enif_get_int(env, argv[1], &mi)))
       )){
    return enif_make_badarg(env);
  }
  if(is_int) {
    md = mi;
  }

  unsigned int size = xbin.size / FRAME_SIZE;
  x = (FRAME_TYPE *) xbin.data;
  z = (FRAME_TYPE *) enif_make_new_binary(env, xbin.size, &zterm);
  unsigned int i;
  for(i = 0; i < size; i++){
    *z++ = *x++ * md;
  }

  return zterm;
}

static ERL_NIF_TERM cross(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  ErlNifBinary xbin, ybin;
  ERL_NIF_TERM zterm;
  FRAME_TYPE *x, *y, *z;

  if(!(enif_inspect_binary(env, argv[0], &xbin) &&
       enif_inspect_binary(env, argv[1], &ybin) &&
       xbin.size == ybin.size)) {
      return enif_make_badarg(env);
  }

  z = (FRAME_TYPE *) enif_make_new_binary(env, xbin.size, &zterm);
  unsigned int size = xbin.size / FRAME_SIZE;
  x = (FRAME_TYPE *) xbin.data;
  y = (FRAME_TYPE *) ybin.data;
#ifdef __AVX__
#define AVXSTEP  sizeof(__m256) / FRAME_SIZE // 8
  __m256 mx, my, mz;

  for( unsigned int i = 0; i < size; i += AVXSTEP){
    mx = _mm256_loadu_ps(x);
    my = _mm256_loadu_ps(y);
    mz = _mm256_mul_ps(mx,my);
    _mm256_storeu_ps(z, mz);
    x += AVXSTEP; y += AVXSTEP; z += AVXSTEP;
  }
#else
  for( unsigned int i = 0; i < size; i++){
    *z++ = *x++ * *y++;
  }
#endif
  return zterm;
}

static ERL_NIF_TERM add(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  ErlNifBinary xbin, ybin;
  ERL_NIF_TERM zterm;
  FRAME_TYPE *x, *y, *z;
  double d;
  int is_bin;
  unsigned int i, j;

  if(!(enif_inspect_binary(env, argv[0], &xbin) &&
       ((is_bin = enif_inspect_binary(env, argv[1], &ybin)) ||
	enif_get_double(env, argv[1], &d)))) {
    return enif_make_badarg(env);
  }

  z = (FRAME_TYPE *) enif_make_new_binary(env, xbin.size, &zterm);
  unsigned int xsize = xbin.size / FRAME_SIZE;
  x = (FRAME_TYPE *) xbin.data;
  if(is_bin) {
    y = (FRAME_TYPE *) ybin.data;
    unsigned int ysize = ybin.size / FRAME_SIZE;
    for(i = 0; (i < xsize) && (i < ysize); i++){
      *z++ = *x++ + *y++;
    }

    if(xsize != ysize) {
      if(i == xsize) {
	for(j = i; j < ysize; j++){
	  *z++ = *y++;
	}
      }else{
	for(j = i; j < xsize; j++){
	  *z++ = *x++;
	}
      }
    }

  }else{
    for( unsigned int i = 0; i < xsize; i++){
      *z++ = d + *x++;
    }
  }
  return zterm;
}

static ERL_NIF_TERM subtract(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  ErlNifBinary xbin, ybin;
  ERL_NIF_TERM zterm;
  FRAME_TYPE *x, *y, *z;

  if(!(enif_inspect_binary(env, argv[0], &xbin) &&
       enif_inspect_binary(env, argv[1], &ybin) &&
       xbin.size == ybin.size)) {
      return enif_make_badarg(env);
  }

  z = (FRAME_TYPE *) enif_make_new_binary(env, xbin.size, &zterm);
  unsigned int size = xbin.size / FRAME_SIZE;
  x = (FRAME_TYPE *) xbin.data;
  y = (FRAME_TYPE *) ybin.data;
  for( unsigned int i = 0; i < size; i++){
    *z++ = *x++ - *y++;
  }
  return zterm;
}

static ERL_NIF_TERM float_list_to_binary(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  ERL_NIF_TERM float_list, new_binary, head, tail;
  unsigned int list_length, bin_size;
  double frame;
  FRAME_TYPE * float_data;

  float_list = argv[0];
  if(!enif_get_list_length(env, float_list, &list_length)){
    return enif_make_badarg(env);
  }

  bin_size = list_length * FRAME_SIZE;
  float_data = (FRAME_TYPE *) enif_make_new_binary(env, bin_size, &new_binary);
  while(enif_get_list_cell(env, float_list, &head, &tail)){
    if(!enif_get_double(env, head, &frame)) {
      return enif_make_badarg(env);
    }
    *float_data++ = (FRAME_TYPE) frame;
    float_list = tail;
  }
  return new_binary;
}

static ERL_NIF_TERM binary_to_float_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  ErlNifBinary xbin;
  ERL_NIF_TERM float_list_term;

  if(!(enif_inspect_binary(env, argv[0], &xbin))) {
    return enif_make_badarg(env);
  }
  unsigned int size = xbin.size / FRAME_SIZE;
  FRAME_TYPE * x = (FRAME_TYPE *) xbin.data;
  ERL_NIF_TERM * darray = (ERL_NIF_TERM *) enif_alloc(size * sizeof(ERL_NIF_TERM));

  for(unsigned int i = 0; i < size; i++){
    darray[i] = enif_make_double(env, (double) x[i]);
  }

  float_list_term = enif_make_list_from_array(env, darray, size);
  enif_free(darray);
  return float_list_term;
}

static ErlNifFunc nif_funcs[] = {
  {"mul", 2, mul},
  {"cross", 2, cross},
  {"add", 2, add},
  {"add2", 2, add}, // Yes, the same function "add" for both array and scalar add
  {"subtract2", 2, subtract},
  {"float_list_to_binary", 1, float_list_to_binary},
  {"binary_to_float_list", 1, binary_to_float_list}
};

ERL_NIF_INIT(gleuterpea@math, nif_funcs, NULL, NULL, NULL, NULL);
