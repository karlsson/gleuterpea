/*
  SuperCollider real time audio synthesis system
  Copyright (c) 2002 James McCartney. All rights reserved.
  http://www.audiosynth.com

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
*/


// #include "SC_PlugIn.h"
#include <erl_nif.h>
#include <math.h>
#include <string.h>
#include "sc_plug.h"

static ErlNifResourceType* sc_filter_type;

// NaNs are not equal to any floating point number
static const float uninitializedControl = NAN;

#define PI M_PI


// static InterfaceTable* ft;

static double radians_per_sample(double rate) {
  return 2.0 * PI / rate;
}

////////////////////////////////////////////////////////////////////////////////////

typedef struct {
  double m_level, m_slope;
  int m_counter, first;
  unsigned int rate, period_size;
} Ramp;

static ERL_NIF_TERM ramp_ctor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned int rate, period_size;

  if (!enif_get_uint(env, argv[0], &rate)){
    return enif_make_badarg(env);
  }
  if (!enif_get_uint(env, argv[1], &period_size)){
    return enif_make_badarg(env);
  }

  Ramp * unit = enif_alloc_resource(sc_filter_type, sizeof(Ramp));
  unit->rate = rate;
  unit->period_size = period_size;
  unit->m_counter = 1;
  unit->m_slope = 0.f;
  unit->first  = 1;
  ERL_NIF_TERM term = enif_make_resource(env, unit);
  enif_release_resource(unit);
  return term;
}

static ERL_NIF_TERM ramp_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  Ramp * unit;
  ErlNifBinary in_bin;
  double in_scalar;
  double period; // lagtime

  if (!enif_get_resource(env, argv[0],
                         sc_filter_type,
                         (void**) &unit)){
    return enif_make_badarg(env);
  }

  if(!enif_get_double(env, argv[2], &period)){
    return enif_make_badarg(env);
  }

  if(enif_inspect_binary(env, argv[1], &in_bin)){
    ERL_NIF_TERM out_term;
    int no_of_frames = in_bin.size / sizeof(float);
    float * in = (float * ) in_bin.data;
    float * out = (float *) enif_make_new_binary(env, in_bin.size, &out_term);
    if(unit->first) {
      unit->m_level = *in;
      unit->first = 0;
    }
    double slope = unit->m_slope;
    double level = unit->m_level;
    int counter = unit->m_counter;
    int remain = no_of_frames;
    while (remain) {
      int nsmps = sc_min(remain, counter);
      for(int i = 0; i < nsmps; i++) {
        *out++ = level;
        level += slope;
      }
      in += nsmps;
      counter -= nsmps;
      remain -= nsmps;
      if (counter <= 0) {
        counter = (int)(period * unit->rate);
        counter = sc_max(1, counter);
        slope = (*in - level) / counter;
      }
    }
    unit->m_level = level;
    unit->m_slope = slope;
    unit->m_counter = counter;
    return out_term;
  }else if(enif_get_double(env, argv[1], &in_scalar)){
    if(unit->first) {
      unit->m_level = in_scalar;
      unit->rate = unit->rate / unit->period_size;
      unit->period_size = 1;
      unit->first = 0;
    }
    double out = unit->m_level;
    if (--unit->m_counter <= 0) {
      int counter = (int)(period * unit->rate);
      unit->m_counter = counter = sc_max(1, counter);
      unit->m_slope = (in_scalar - unit->m_level) / counter;
    }
    return(enif_make_double(env, out));
  }else{
    return enif_make_badarg(env);
  }
}

////////////////////////////////////////////////////////////////////////////////////

typedef struct {
  float m_lag;
  double m_b1, m_y1;
  uint rate, period_size;
  int first;
} Lag;

static ERL_NIF_TERM lag_ctor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned int rate, period_size;
  if (!enif_get_uint(env, argv[0], &rate)){
    return enif_make_badarg(env);
  }
  if (!enif_get_uint(env, argv[1], &period_size)){
    return enif_make_badarg(env);
  }
  Lag * unit = enif_alloc_resource(sc_filter_type, sizeof(Lag));
  unit->m_lag = uninitializedControl;
  unit->m_b1 = 0.f;
  unit->rate = rate;
  unit->period_size = period_size;
  unit->first = 1;
  unit->m_y1 = uninitializedControl;
  ERL_NIF_TERM term = enif_make_resource(env, unit);
  enif_release_resource(unit);
  return term;
}

static ERL_NIF_TERM lag_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  Lag * unit;
  ErlNifBinary in_bin;
  float * out, * in;
  int is_bin;
  double in_scalar, out_scalar;
  ERL_NIF_TERM out_term;
  double lag;
  int inNumSamples;

  if (!enif_get_resource(env, argv[0],
                         sc_filter_type,
                         (void**) &unit)){
    return enif_raise_exception(env,
                                enif_make_string(env, "No valid reference", ERL_NIF_LATIN1));
  }

  if(enif_inspect_binary(env, argv[1], &in_bin)){
    inNumSamples = in_bin.size / sizeof(float);
    in = (float * ) in_bin.data;
    out = (float *) enif_make_new_binary(env, in_bin.size, &out_term);
    is_bin = 1;
  }else if(enif_get_double(env, argv[1], &in_scalar)){
    is_bin = 0;
  }else{
    return enif_raise_exception(env,
                                enif_make_string(env, "Not a binary nor a float", ERL_NIF_LATIN1));
  }

  if(!enif_get_double(env, argv[2], &lag)){
    return enif_raise_exception(env,
                                enif_make_string(env, "Lagtime not a float", ERL_NIF_LATIN1));
  }

  if(unit->first){
    unit->m_y1 = is_bin?(*in):in_scalar;
    if(!is_bin) {
      unit->rate = unit->rate / unit->period_size;
      unit->period_size = 1;
    }
    unit->first = 0;
  }

  double y1 = unit->m_y1;
  double b1 = unit->m_b1;
  double y0;

  if(is_bin){
    if (lag == unit->m_lag) {
      for(int i = 0; i < inNumSamples; i++) {
        y0 = *in++;
        *out++ = y1 = y0 + b1 * (y1 - y0);
      }
    } else {
      unit->m_b1 = lag == 0.f ? 0.f : exp(log001 / (lag * unit->rate));
      double b1_slope = (unit->m_b1 - b1) / unit->period_size;
      unit->m_lag = lag;
      for(int i = 0; i < inNumSamples; i++){
        b1 += b1_slope;
        y0 = *in++;
        *out++ = y1 = y0 + b1 * (y1 - y0);
      }
    }
  }else{
    if (lag == unit->m_lag) {
      y0 = in_scalar;
      out_scalar = y1 = y0 + b1 * (y1 - y0);
    } else {
      unit->m_b1 = b1 = lag == 0.f ? 0.f : exp(log001 / (lag * unit->rate));
      unit->m_lag = lag;
      y0 = in_scalar;
      out_scalar = y1 = y0 + b1 * (y1 - y0);
    }
    out_term = enif_make_double(env, out_scalar);
  }
  unit->m_y1 = zapgremlins(y1);
  return out_term;
}

////////////////////////////////////////////////////////////////////////////////////
typedef struct LagUD {
  double m_lagu, m_lagd;
  double m_b1u, m_b1d, m_y1;
  double rate, period_size;
  int first;
  void (*next)(struct LagUD *, float *, float *, double *, int);
} LagUD;

static void LagUD_next(LagUD* unit, float * out, float * in, double * args, int inNumSamples) {
    double lagu = args[0];
    double lagd = args[1];

    double y1 = unit->m_y1;
    double b1u = unit->m_b1u;
    double b1d = unit->m_b1d;

    if ((lagu == unit->m_lagu) && (lagd == unit->m_lagd)) {
      for(int i = 0; i < inNumSamples; i++) {
        double y0 = *in++;
        if (y0 > y1)
          *out++ = y1 = y0 + b1u * (y1 - y0);
        else
          *out++ = y1 = y0 + b1d * (y1 - y0);
      }
    } else {
        unit->m_b1u = lagu == 0. ? 0. : exp(log001 / (lagu * unit->rate));
        double b1u_slope = (unit->m_b1u - b1u) / unit->period_size;
        unit->m_lagu = lagu;
        unit->m_b1d = lagd == 0. ? 0. : exp(log001 / (lagd * unit->rate));
        double b1d_slope = (unit->m_b1d - b1d) / unit->period_size;
        unit->m_lagd = lagd;
        for(int i = 0; i < inNumSamples; i++) {
          b1u += b1u_slope;
          b1d += b1d_slope;
          double y0 = *in++;
          if (y0 > y1)
            *out++ = y1 = y0 + b1u * (y1 - y0);
          else
            *out++ = y1 = y0 + b1d * (y1 - y0);
        }
    }
    unit->m_y1 = zapgremlins(y1);
}

static ERL_NIF_TERM lagud_ctor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned int rate, period_size;
  if (!enif_get_uint(env, argv[0], &rate)){
    return enif_make_badarg(env);
  }
  if (!enif_get_uint(env, argv[1], &period_size)){
    return enif_make_badarg(env);
  }
  LagUD * unit = enif_alloc_resource(sc_filter_type, sizeof(LagUD));
  unit->m_lagu = uninitializedControl;
  unit->m_lagd = uninitializedControl;
  unit->m_b1u = 0.;
  unit->m_b1d = 0.;
  unit->rate = rate;
  unit->period_size = period_size;
  unit->first = 1;
  unit->m_y1 = uninitializedControl;
  unit->next = &LagUD_next;
  ERL_NIF_TERM term = enif_make_resource(env, unit);
  enif_release_resource(unit);
  return term;
}
static ERL_NIF_TERM lagud_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  LagUD * unit;
  ErlNifBinary in_bin;
  double in_scalar;
  double args[2];

  if (!enif_get_resource(env, argv[0],
                         sc_filter_type,
                         (void**) &unit)){
    return enif_raise_exception(env,
                                enif_make_string(env,
                                                 "No valid reference",
                                                 ERL_NIF_LATIN1));
  }

  if(!enif_get_double(env, argv[2], &args[0])){
    return enif_raise_exception(env,
                                enif_make_string(env,
                                                 "Lag UP not a float",
                                                 ERL_NIF_LATIN1));
  }

  if(argc > 3) {
    if(!enif_get_double(env, argv[3], &args[1])){
      return enif_raise_exception(env,
                                  enif_make_string(env,
                                                   "Lag DOWN not a float",
                                                   ERL_NIF_LATIN1));
    }
  }

  if(enif_inspect_binary(env, argv[1], &in_bin)){
    ERL_NIF_TERM out_term;
    int inNumSamples = in_bin.size / sizeof(float);
    float * in = (float *) in_bin.data;
    float * out = (float *) enif_make_new_binary(env, in_bin.size, &out_term);
    if(unit->first) {
      (*unit->next)(unit, out, in, args, 1);
      unit->first = 0;
    }
    (*unit->next)(unit, out, in, args, inNumSamples);
    return out_term;
  }else if(enif_get_double(env, argv[1], &in_scalar)){
    float in[1], out[1];
    in[0] = (float) in_scalar;
    if(unit->first) {
      unit->rate = unit->rate/unit->period_size;
      unit->period_size = 1;
      (*unit->next)(unit, out, in, args, 1);
      unit->first = 0;
    }
    (*unit->next)(unit, out, in, args, 1);
    return(enif_make_double(env, (double) out[0]));
  }else{
    return enif_raise_exception(env,
                                enif_make_string(env,
                                                 "Not a binary",
                                                 ERL_NIF_LATIN1));
  }
}

/* ------------------------------------------------------------ */
typedef struct LHPF {
  double m_freq, m_bw;
  double m_y1, m_y2, m_a0, m_a1, m_b1, m_b2;
  double rate, period_size;
  int first;
  void (*next)(struct LHPF *, float *, float *, double *, int);
  void (*next_1)(struct LHPF *, double *, double, double *);
} LHPF;

static void LPF_next(LHPF* unit, float * out, float * in, double * args, int inNumSamples) {
  double freq = args[0];
  double y0;
  double y1 = unit->m_y1;
  double y2 = unit->m_y2;
  double a0 = unit->m_a0;
  double b1 = unit->m_b1;
  double b2 = unit->m_b2;
  int mFilterLoops, mFilterRemain;
  if(unit->first) {
    mFilterLoops = 0;
    mFilterRemain = 1;
  } else {
    mFilterLoops = inNumSamples / 3;
    mFilterRemain = inNumSamples % 3;
  }
  if (freq != unit->m_freq) {
    double mFilterSlope = (mFilterLoops == 0) ? 0. : 1. / mFilterLoops;
    double pfreq = freq * radians_per_sample(unit->rate) * 0.5;

    double C = 1. / tan(pfreq);
    double C2 = C * C;
    double sqrt2C = C * sqrt2;
    double next_a0 = 1. / (1. + sqrt2C + C2);
    double next_b1 = -2. * (1. - C2) * next_a0;
    double next_b2 = -(1. - sqrt2C + C2) * next_a0;

    double a0_slope = (next_a0 - a0) * mFilterSlope;
    double b1_slope = (next_b1 - b1) * mFilterSlope;
    double b2_slope = (next_b2 - b2) * mFilterSlope;
    for (int i = 0; i < mFilterLoops; i++) {
      y0 = *in++ + b1 * y1 + b2 * y2;
      *out++ = a0 * (y0 + 2. * y1 + y2);

      y2 = *in++ + b1 * y0 + b2 * y1;
      *out++ = a0 * (y2 + 2. * y0 + y1);

      y1 = *in++ + b1 * y2 + b2 * y0;
      *out++ = a0 * (y1 + 2.f * y2 + y0);

      a0 += a0_slope; b1 += b1_slope; b2 += b2_slope;
    }
    for(int i = 0; i < mFilterRemain; i++){
      y0 = *in++ + b1 * y1 + b2 * y2;
      *out++ = a0 * (y0 + 2. * y1 + y2);
      y2 = y1;
      y1 = y0;
    }
    unit->m_freq = freq;
    unit->m_a0 = next_a0;
    unit->m_b1 = next_b1;
    unit->m_b2 = next_b2;
  } else {
    for (int i = 0; i < mFilterLoops; i++) {
      y0 = *in++ + b1 * y1 + b2 * y2;
      *out++ = a0 * (y0 + 2. * y1 + y2);

      y2 = *in++ + b1 * y0 + b2 * y1;
      *out++ = a0 * (y2 + 2. * y0 + y1);

      y1 = *in++ + b1 * y2 + b2 * y0;
      *out++ = a0 * (y1 + 2. * y2 + y0);
    }
    for(int i = 0; i < mFilterRemain; i++){
      y0 = *in++ + b1 * y1 + b2 * y2;
      *out++ = a0 * (y0 + 2. * y1 + y2);
      y2 = y1;
      y1 = y0;
    }
  }
  unit->m_y1 = zapgremlins(y1);
  unit->m_y2 = zapgremlins(y2);
}

static void LPF_next_1(LHPF* unit, double * out, double in, double * args) {
  double freq = args[0];
  double y0;
  double y1 = unit->m_y1;
  double y2 = unit->m_y2;
  double a0 = unit->m_a0;
  double b1 = unit->m_b1;
  double b2 = unit->m_b2;

  if (freq != unit->m_freq) {
    double pfreq = freq * radians_per_sample(unit->rate) * 0.5;

    double C = 1. / tan(pfreq);
    double C2 = C * C;
    double sqrt2C = C * sqrt2;
    a0 = 1. / (1. + sqrt2C + C2);
    b1 = -2. * (1. - C2) * a0;
    b2 = -(1. - sqrt2C + C2) * a0;

    y0 = in + b1 * y1 + b2 * y2;
    *out = a0 * (y0 + 2. * y1 + y2);
    y2 = y1;
    y1 = y0;

    unit->m_freq = freq;
    unit->m_a0 = a0;
    unit->m_b1 = b1;
    unit->m_b2 = b2;
  } else {
    y0 = in + b1 * y1 + b2 * y2;
    *out = a0 * (y0 + 2. * y1 + y2);
    y2 = y1;
    y1 = y0;
  }
  unit->m_y1 = zapgremlins(y1);
  unit->m_y2 = zapgremlins(y2);
}

/* ---------------------------------------------------------- */

static void HPF_next(LHPF* unit, float * out, float * in, double * args, int inNumSamples) {
  double freq = args[0];
  double y0;
  double y1 = unit->m_y1;
  double y2 = unit->m_y2;
  double a0 = unit->m_a0;
  double b1 = unit->m_b1;
  double b2 = unit->m_b2;
  int mFilterLoops, mFilterRemain;
  if(unit->first) {
    mFilterLoops = 0;
    mFilterRemain = 1;
  } else {
    mFilterLoops = inNumSamples / 3;
    mFilterRemain = inNumSamples % 3;
  }

  if (freq != unit->m_freq) {
    double mFilterSlope = (mFilterLoops == 0) ? 0. : 1. / mFilterLoops;
    double pfreq = freq * radians_per_sample(unit->rate) * 0.5;

    double C = tan(pfreq);
    double C2 = C * C;
    double sqrt2C = C * sqrt2;
    double next_a0 = 1. / (1. + sqrt2C + C2);
    double next_b1 = 2. * (1. - C2) * next_a0;
    double next_b2 = -(1. - sqrt2C + C2) * next_a0;
    double a0_slope = (next_a0 - a0) * mFilterSlope;
    double b1_slope = (next_b1 - b1) * mFilterSlope;
    double b2_slope = (next_b2 - b2) * mFilterSlope;
    for (int i = 0; i < mFilterLoops; i++) {
      y0 = *in++ + b1 * y1 + b2 * y2;
      *out++ = a0 * (y0 - 2. * y1 + y2);

      y2 = *in++ + b1 * y0 + b2 * y1;
      *out++ = a0 * (y2 - 2. * y0 + y1);

      y1 = *in++ + b1 * y2 + b2 * y0;
      *out++ = a0 * (y1 - 2. * y2 + y0);

      a0 += a0_slope; b1 += b1_slope; b2 += b2_slope;
    }
    for(int i = 0; i < mFilterRemain; i++){
      y0 = *in++ + b1 * y1 + b2 * y2;
      *out++ = a0 * (y0 - 2. * y1 + y2);
      y2 = y1;
      y1 = y0;
    }

    unit->m_freq = freq;
    unit->m_a0 = next_a0;
    unit->m_b1 = next_b1;
    unit->m_b2 = next_b2;
  } else {
    for (int i = 0; i < mFilterLoops; i++) {
      y0 = *in++ + b1 * y1 + b2 * y2;
      *out++ = a0 * (y0 - 2. * y1 + y2);

      y2 = *in++ + b1 * y0 + b2 * y1;
      *out++ = a0 * (y2 - 2. * y0 + y1);

      y1 = *in++ + b1 * y2 + b2 * y0;
      *out++ = a0 * (y1 - 2. * y2 + y0);
    }
    for(int i = 0; i < mFilterRemain; i++){
      y0 = *in++ + b1 * y1 + b2 * y2;
      *out++ = a0 * (y0 - 2. * y1 + y2);
      y2 = y1; y1 = y0;
    }
  }
  unit->m_y1 = zapgremlins(y1);
  unit->m_y2 = zapgremlins(y2);
}

static void HPF_next_1(LHPF* unit, double * out, double in, double * args) {
  double freq = args[0];
  double y1 = unit->m_y1;
  double y2 = unit->m_y2;
  double a0 = unit->m_a0;
  double b1 = unit->m_b1;
  double b2 = unit->m_b2;

  if (freq != unit->m_freq) {
    double pfreq = freq * radians_per_sample(unit->rate) * 0.5;

    double C = tan(pfreq);
    double C2 = C * C;
    double sqrt2C = C * sqrt2;
    a0 = 1. / (1. + sqrt2C + C2);
    b1 = 2. * (1. - C2) * a0;
    b2 = -(1. - sqrt2C + C2) * a0;

    double y0 = in + b1 * y1 + b2 * y2;
    *out = a0 * (y0 - 2. * y1 + y2);
    y2 = y1;
    y1 = y0;

    unit->m_freq = freq;
    unit->m_a0 = a0;
    unit->m_b1 = b1;
    unit->m_b2 = b2;
  } else {
    double y0 = in + b1 * y1 + b2 * y2;
    *out = a0 * (y0 - 2. * y1 + y2);
    y2 = y1;
    y1 = y0;
  }

  unit->m_y1 = zapgremlins(y1);
  unit->m_y2 = zapgremlins(y2);
}

/* ---------------------------------------------------------- */

static void BPF_next(LHPF* unit, float * out, float * in, double * args, int inNumSamples) {
  double freq = args[0];
  double bw = args[1];

  double y0;
  double y1 = unit->m_y1;
  double y2 = unit->m_y2;
  double a0 = unit->m_a0;
  double b1 = unit->m_b1;
  double b2 = unit->m_b2;
  int mFilterLoops, mFilterRemain;
  if(unit->first) {
    mFilterLoops = 0;
    mFilterRemain = 1;
  } else {
    mFilterLoops = inNumSamples / 3;
    mFilterRemain = inNumSamples % 3;
  }

  if (freq != unit->m_freq || bw != unit->m_bw) {
    double mFilterSlope = (mFilterLoops == 0) ? 0. : 1. / mFilterLoops;
    double pfreq = freq * radians_per_sample(unit->rate);
    double pbw = bw * pfreq * 0.5;

    double C = 1. / tan(pbw);
    double D = 2. * cos(pfreq);

    double next_a0 = 1. / (1. + C);
    double next_b1 = C * D * next_a0;
    double next_b2 = (1. - C) * next_a0;

    double a0_slope = (next_a0 - a0) * mFilterSlope;
    double b1_slope = (next_b1 - b1) * mFilterSlope;
    double b2_slope = (next_b2 - b2) * mFilterSlope;

    for (int i = 0; i < mFilterLoops; i++) {
      y0 = *in++ + b1 * y1 + b2 * y2;
      *out++ = a0 * (y0 - y2);

      y2 = *in++ + b1 * y0 + b2 * y1;
      *out++ = a0 * (y2 - y1);

      y1 = *in++ + b1 * y2 + b2 * y0;
      *out++ = a0 * (y1 - y0);

      a0 += a0_slope; b1 += b1_slope; b2 += b2_slope;
    }
    for(int i = 0; i < mFilterRemain; i++){
      y0 = *in++ + b1 * y1 + b2 * y2;
      *out++ = a0 * (y0 - y2);
      y2 = y1;
      y1 = y0;
    }

    unit->m_freq = freq;
    unit->m_bw = bw;
    unit->m_a0 = next_a0;
    unit->m_b1 = next_b1;
    unit->m_b2 = next_b2;
  } else {
    for (int i = 0; i < mFilterLoops; i++) {
      y0 = *in++ + b1 * y1 + b2 * y2;
      *out++ = a0 * (y0 - y2);

      y2 = *in++ + b1 * y0 + b2 * y1;
      *out++ = a0 * (y2 - y1);

      y1 = *in++ + b1 * y2 + b2 * y0;
      *out++ = a0 * (y1 - y0);
    }
    for(int i = 0; i < mFilterRemain; i++){
      y0 = *in++ + b1 * y1 + b2 * y2;
      *out++ = a0 * (y0 - y2);
      y2 = y1;
      y1 = y0;
    }
  }
  unit->m_y1 = zapgremlins(y1);
  unit->m_y2 = zapgremlins(y2);
}


static void BPF_next_1(LHPF* unit, double * out, double in, double * args) {
  double freq = args[0];
  double bw = args[1];

  double y0;
  double y1 = unit->m_y1;
  double y2 = unit->m_y2;
  double a0 = unit->m_a0;
  double b1 = unit->m_b1;
  double b2 = unit->m_b2;

  if (freq != unit->m_freq || bw != unit->m_bw) {
    double pfreq = freq * radians_per_sample(unit->rate);
    double pbw = bw * pfreq * 0.5;

    double C = 1. / tan(pbw);
    double D = 2. * cos(pfreq);

    double a0 = 1. / (1. + C);
    double b1 = C * D * a0;
    double b2 = (1. - C) * a0;

    y0 = in + b1 * y1 + b2 * y2;
    *out = a0 * (y0 - y2);
    y2 = y1;
    y1 = y0;

    unit->m_freq = freq;
    unit->m_bw = bw;
    unit->m_a0 = a0;
    unit->m_b1 = b1;
    unit->m_b2 = b2;
  } else {
    y0 = in + b1 * y1 + b2 * y2;
    *out = a0 * (y0 - y2);
    y2 = y1;
    y1 = y0;
  }
  unit->m_y1 = zapgremlins(y1);
  unit->m_y2 = zapgremlins(y2);
}

static void BRF_next(LHPF* unit, float * out, float * in, double * args, int inNumSamples) {
  double freq = args[0];
  double bw = args[1];

  double ay;
  double y0;
  double y1 = unit->m_y1;
  double y2 = unit->m_y2;
  double a0 = unit->m_a0;
  double a1 = unit->m_a1;
  double b2 = unit->m_b2;
  int mFilterLoops, mFilterRemain;
  if(unit->first) {
    mFilterLoops = 0;
    mFilterRemain = 1;
  } else {
    mFilterLoops = inNumSamples / 3;
    mFilterRemain = inNumSamples % 3;
  }

  if (freq != unit->m_freq || bw != unit->m_bw) {
    double mFilterSlope = (mFilterLoops == 0) ? 0. : 1. / mFilterLoops;
    double pfreq = freq * radians_per_sample(unit->rate);
    double pbw = bw * pfreq * 0.5;
    double C = tan(pbw);
    double D = 2. * cos(pfreq);

    double next_a0 = 1. / (1. + C);
    double next_a1 = -D * next_a0;
    double next_b2 = (1. - C) * next_a0;

    double a0_slope = (next_a0 - a0) * mFilterSlope;
    double a1_slope = (next_a1 - a1) * mFilterSlope;
    double b2_slope = (next_b2 - b2) * mFilterSlope;
    for (int i = 0; i < mFilterLoops; i++) {
      ay = a1 * y1; y0 = *in++ - ay - b2 * y2;
      *out++ = a0 * (y0 + y2) + ay;

      ay = a1 * y0; y2 = *in++ - ay - b2 * y1;
      *out++ = a0 * (y2 + y1) + ay;

      ay = a1 * y2; y1 = *in++ - ay - b2 * y0;
      *out++ = a0 * (y1 + y0) + ay;

      a0 += a0_slope; a1 += a1_slope; b2 += b2_slope;
    }
    for(int i = 0; i < mFilterRemain; i++){
      ay = a1 * y1; y0 = *in++ - ay - b2 * y2;
      *out++ = a0 * (y0 + y2) + ay;
      y2 = y1;
      y1 = y0;
    }
    unit->m_freq = freq;
    unit->m_bw = bw;
    unit->m_a0 = next_a0;
    unit->m_a1 = next_a1;
    unit->m_b2 = next_b2;
  } else {
    for (int i = 0; i < mFilterLoops; i++) {
      ay = a1 * y1; y0 = *in++ - ay - b2 * y2;
      *out++ = a0 * (y0 + y2) + ay;

      ay = a1 * y0; y2 = *in++ - ay - b2 * y1;
      *out++ = a0 * (y2 + y1) + ay;

      ay = a1 * y2; y1 = *in++ - ay - b2 * y0;
      *out++ = a0 * (y1 + y0) + ay;
    }
    for(int i = 0; i < mFilterRemain; i++){
      ay = a1 * y1; y0 = *in++ - ay - b2 * y2;
      *out++ = a0 * (y0 + y2) + ay;
      y2 = y1;
      y1 = y0;
    }
  }
  unit->m_y1 = zapgremlins(y1);
  unit->m_y2 = zapgremlins(y2);
}

static void BRF_next_1(LHPF* unit, double * out, double in, double * args) {
  double freq = args[0];
  double bw = args[1];

  double ay;
  double y0;
  double y1 = unit->m_y1;
  double y2 = unit->m_y2;
  double a0 = unit->m_a0;
  double a1 = unit->m_a1;
  double b2 = unit->m_b2;

  if (freq != unit->m_freq || bw != unit->m_bw) {
    double pfreq = freq * radians_per_sample(unit->rate);
    double pbw = bw * pfreq * 0.5;
    double C = tan(pbw);
    double D = 2. * cos(pfreq);

    double a0 = 1. / (1. + C);
    double a1 = -D * a0;
    double b2 = (1. - C) * a0;

    ay = a1 * y1;
    y0 = in - ay - b2 * y2;
    *out = a0 * (y0 + y2) + ay;
    y2 = y1;
    y1 = y0;

    unit->m_freq = freq;
    unit->m_bw = bw;
    unit->m_a0 = a0;
    unit->m_a1 = a1;
    unit->m_b2 = b2;
  } else {
    ay = a1 * y1;
    y0 = in - ay - b2 * y2;
    *out = a0 * (y0 + y2) + ay;
    y2 = y1;
    y1 = y0;
  }
  unit->m_y1 = zapgremlins(y1);
  unit->m_y2 = zapgremlins(y2);
}

/* ---------------------------------------------------------- */

static ERL_NIF_TERM lhpf_ctor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned int rate, period_size;
  char type[12];
  if (!enif_get_uint(env, argv[0], &rate)){
    return enif_make_badarg(env);
  }
  if (!enif_get_uint(env, argv[1], &period_size)){
    return enif_make_badarg(env);
  }
  if (!enif_get_atom(env, argv[2], type, 12, ERL_NIF_LATIN1)){
    return enif_make_badarg(env);
  }

  LHPF * unit = enif_alloc_resource(sc_filter_type, sizeof(LHPF));
  unit->rate = (double) rate;
  unit->period_size = (double) period_size;
  unit->first = 1;
  unit->m_a0 = 0.;
  unit->m_a1 = 0.;
  unit->m_b1 = 0.;
  unit->m_b2 = 0.;
  unit->m_y1 = 0.;
  unit->m_y2 = 0.;
  unit->m_freq = uninitializedControl;
  unit->m_bw = uninitializedControl;
  if (strcmp(type, "lpf") == 0) {
    unit->next = &LPF_next;
    unit->next_1 = &LPF_next_1;
  } else if (strcmp(type, "hpf") == 0) {
    unit->next = &HPF_next;
    unit->next_1 = &HPF_next_1;
  } else if (strcmp(type, "bpf") == 0) {
    unit->next = &BPF_next;
    unit->next_1 = &BPF_next_1;
  } else if (strcmp(type, "brf") == 0) {
    unit->next = &BRF_next;
    unit->next_1 = &BRF_next_1;
  } else {
    return enif_make_badarg(env);
  }
  ERL_NIF_TERM term = enif_make_resource(env, unit);
  enif_release_resource(unit);
  return term;
}

static ERL_NIF_TERM lhpf_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  LHPF * unit;
  ErlNifBinary in_bin;
  double in_scalar;
  double args[4];

  if (!enif_get_resource(env, argv[0],
                         sc_filter_type,
                         (void**) &unit)){
    return enif_raise_exception(env,
                                enif_make_string(env,
                                                 "No valid reference",
                                                 ERL_NIF_LATIN1));
  }

  if(!enif_get_double(env, argv[2], &args[0])){
    return enif_raise_exception(env,
                                enif_make_string(env,
                                                 "Frequency not a float",
                                                 ERL_NIF_LATIN1));
  }

  if(argc > 3) {
    if(!enif_get_double(env, argv[3], &args[1])){
      return enif_raise_exception(env,
                                  enif_make_string(env,
                                                   "Bandwidth not a float",
                                                   ERL_NIF_LATIN1));
    }
  }

  if(enif_inspect_binary(env, argv[1], &in_bin)){
    ERL_NIF_TERM out_term;
    int inNumSamples = in_bin.size / sizeof(float);
    float * in = (float *) in_bin.data;
    float * out = (float *) enif_make_new_binary(env, in_bin.size, &out_term);
    if(unit->first) {
      (*unit->next)(unit, out, in, args, inNumSamples);
      unit->first = 0;
    }
    (*unit->next)(unit, out, in, args, inNumSamples);
    return out_term;
  }else if(enif_get_double(env, argv[1], &in_scalar)){
    double out;
    if(unit->first) {
      (*unit->next_1)(unit, &out, in_scalar, args);
      unit->rate = unit->rate / unit->period_size;
      unit->period_size = 1;
      unit->first = 0;
    }
    (*unit->next_1)(unit, &out, in_scalar, args);
    return(enif_make_double(env, out));
  }else{
    return enif_raise_exception(env,
                                enif_make_string(env,
                                                 "Not a binary nor a float",
                                                 ERL_NIF_LATIN1));
  }
}

/* ---------------------------------------------------------- */
static ErlNifFunc nif_funcs[] = {
  {"ramp_ctor_nif", 2, ramp_ctor},
  {"ramp_next_nif", 3, ramp_next},
  {"lag_ctor_nif", 2, lag_ctor},
  {"lag_next_nif", 3, lag_next},
  {"lagud_ctor_nif", 2, lagud_ctor},
  {"lagud_next_nif", 4, lagud_next},
  {"pf_ctor_nif", 3, lhpf_ctor},
  {"lhpf_next_nif", 3, lhpf_next},
  {"bprf_next_nif", 4, lhpf_next}
};

static int open_filter_resource_type(ErlNifEnv* env)
{
  const char* mod = "Elixir.SC.Filter";
  const char* resource_type = "sc_filter";
  int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
  sc_filter_type =
    enif_open_resource_type(env, mod, resource_type,
                            NULL, flags, NULL);
  return ((sc_filter_type == NULL) ? -1:0);
}

static int load(ErlNifEnv* caller_env, void** priv_data, ERL_NIF_TERM load_info)
{
  return open_filter_resource_type(caller_env);
}

static int upgrade(ErlNifEnv* caller_env, void** priv_data, void** old_priv_data,
		   ERL_NIF_TERM load_info)
{
  return open_filter_resource_type(caller_env);
}


ERL_NIF_INIT(gleuterpea@filter@sc_filter, nif_funcs, load, NULL, upgrade, NULL);
