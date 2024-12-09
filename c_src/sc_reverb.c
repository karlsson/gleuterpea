// From ReverbUGens.cpp
// FreeVerb UGens
// faust code generation experiments. blackrain 07/2005
/*  Copyright (c) 2005 blackrain <blackrain.sc@gmail.com>. All rights reserved.
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

#include <erl_nif.h>
#include <math.h>
#include <string.h>
#include "sc_plug.h"

static ErlNifResourceType* sc_reverb_type;

typedef struct FreeVerb {
  int iota0;
  int iota1;
  int iota2;
  int iota3;
  int iota4;
  int iota5;
  int iota6;
  int iota7;
  int iota8;
  int iota9;
  int iota10;
  int iota11;

  float R0_1;
  float R1_1;
  float R2_1;
  float R3_1;
  float R0_0;
  float R1_0;
  float R2_0;
  float R3_0;
  float R4_0;
  float R5_0;
  float R6_0;
  float R7_0;
  float R8_0;
  float R9_0;
  float R10_0;
  float R11_0;
  float R12_0;
  float R13_0;
  float R14_0;
  float R15_0;
  float R16_0;
  float R17_0;
  float R18_0;
  float R19_0;

  float dline0[225];
  float dline1[341];
  float dline2[441];
  float dline3[556];
  float dline4[1617];
  float dline5[1557];
  float dline6[1491];
  float dline7[1422];
  float dline8[1277];
  float dline9[1116];
  float dline10[1188];
  float dline11[1356];

} FreeVerb;

// FreeVerb2
typedef struct FreeVerb2 {
  int iota0;
  int iota1;
  int iota2;
  int iota3;
  int iota4;
  int iota5;
  int iota6;
  int iota7;
  int iota8;
  int iota9;
  int iota10;
  int iota11;
  int iota12;
  int iota13;
  int iota14;
  int iota15;
  int iota16;
  int iota17;
  int iota18;
  int iota19;
  int iota20;
  int iota21;
  int iota22;
  int iota23;
  float R0_1;
  float R1_1;
  float R2_1;
  float R3_1;
  float R0_0;
  float R1_0;
  float R2_0;
  float R3_0;
  float R4_0;
  float R5_0;
  float R6_0;
  float R7_0;
  float R8_0;
  float R9_0;
  float R10_0;
  float R11_0;
  float R12_0;
  float R13_0;
  float R14_0;
  float R15_0;
  float R16_0;
  float R17_0;
  float R18_0;
  float R19_0;
  float R20_0;
  float R21_0;
  float R22_0;
  float R23_0;
  float R24_0;
  float R25_0;
  float R26_0;
  float R27_0;
  float R28_0;
  float R29_0;
  float R30_0;
  float R31_0;
  float R32_0;
  float R33_0;
  float R34_0;
  float R35_0;
  float R36_0;
  float R37_0;
  float R38_0;
  float R39_0;
  float R20_1;
  float R21_1;
  float R22_1;
  float R23_1;
  float dline0[225];
  float dline1[341];
  float dline2[441];
  float dline3[556];
  float dline4[1617];
  float dline5[1557];
  float dline6[1491];
  float dline7[1422];
  float dline8[1277];
  float dline9[1116];
  float dline10[1188];
  float dline11[1356];
  float dline12[248];
  float dline13[364];
  float dline14[464];
  float dline15[579];
  float dline16[1640];
  float dline17[1580];
  float dline18[1514];
  float dline19[1445];
  float dline20[1300];
  float dline21[1139];
  float dline22[1211];
  float dline23[1379];
} FreeVerb2;

/*  GVerb work */
#define FDNORDER 4

typedef struct {
  int size;
  int idx;
  float* buf;
} g_fixeddelay;

typedef struct {
  int size;
  float coef;
  int idx;
  float* buf;
} g_diffuser;

typedef struct {
  float damping;
  float delay;
} g_damper;


typedef struct GVerb {
  float roomsize, revtime, damping, spread, inputbandwidth, drylevel, earlylevel, taillevel;
  float maxroomsize;
  float maxdelay, largestdelay;
  g_damper* inputdamper;
  g_fixeddelay* fdndels[FDNORDER];
  float fdngains[FDNORDER];
  int fdnlens[FDNORDER];
  g_damper* fdndamps[FDNORDER];
  double alpha;
  float u[FDNORDER], f[FDNORDER], d[FDNORDER];
  g_diffuser* ldifs[FDNORDER];
  g_diffuser* rdifs[FDNORDER];
  g_fixeddelay* tapdelay;
  int taps[FDNORDER];
  float tapgains[FDNORDER];
  float earlylevelslope, taillevelslope, drylevelslope;
  float fdngainslopes[FDNORDER], tapgainslopes[FDNORDER];
  // make the CALCSLOPE values part of the struct
  // calculate changes first, store them
  // grab values and use in the sample loop
  float rate;
  float period_size;
} GVerb;

typedef union {
  FreeVerb * fv;
  FreeVerb2 * fv2;
  GVerb * gv;
} SubUnit;

typedef struct Reverb {
  double rate;
  double period_size;
  SubUnit unit;
  void (*first)(struct Reverb *, double *);
  void (*next)(struct Reverb *, float**, float**, double*, int);
  void (*dtor)(struct Reverb *);
} Reverb;


static void FreeVerb_Ctor(Reverb* rev, double * args) {
  FreeVerb * unit = rev->unit.fv;
  unit->iota0 = 0;
  unit->iota1 = 0;
  unit->iota2 = 0;
  unit->iota3 = 0;
  unit->iota4 = 0;
  unit->iota5 = 0;
  unit->iota6 = 0;
  unit->iota7 = 0;
  unit->iota8 = 0;
  unit->iota9 = 0;
  unit->iota10 = 0;
  unit->iota11 = 0;

  unit->R0_0 = 0.0;
  unit->R1_0 = 0.0;
  unit->R2_0 = 0.0;
  unit->R3_0 = 0.0;
  unit->R4_0 = 0.0;
  unit->R5_0 = 0.0;
  unit->R6_0 = 0.0;
  unit->R7_0 = 0.0;
  unit->R8_0 = 0.0;
  unit->R9_0 = 0.0;
  unit->R10_0 = 0.0;
  unit->R11_0 = 0.0;
  unit->R12_0 = 0.0;
  unit->R13_0 = 0.0;
  unit->R14_0 = 0.0;
  unit->R15_0 = 0.0;
  unit->R16_0 = 0.0;
  unit->R17_0 = 0.0;
  unit->R18_0 = 0.0;
  unit->R19_0 = 0.0;

  unit->R0_1 = 0.0;
  unit->R1_1 = 0.0;
  unit->R2_1 = 0.0;
  unit->R3_1 = 0.0;

  for (int i = 0; i < 225; i++)
    unit->dline0[i] = 0.0;
  for (int i = 0; i < 341; i++)
    unit->dline1[i] = 0.0;
  for (int i = 0; i < 441; i++)
    unit->dline2[i] = 0.0;
  for (int i = 0; i < 556; i++)
    unit->dline3[i] = 0.0;
  for (int i = 0; i < 1617; i++)
    unit->dline4[i] = 0.0;
  for (int i = 0; i < 1557; i++)
    unit->dline5[i] = 0.0;
  for (int i = 0; i < 1491; i++)
    unit->dline6[i] = 0.0;
  for (int i = 0; i < 1422; i++)
    unit->dline7[i] = 0.0;
  for (int i = 0; i < 1277; i++)
    unit->dline8[i] = 0.0;
  for (int i = 0; i < 1116; i++)
    unit->dline9[i] = 0.0;
  for (int i = 0; i < 1188; i++)
    unit->dline10[i] = 0.0;
  for (int i = 0; i < 1356; i++)
    unit->dline11[i] = 0.0;

}

static void FreeVerb_next(Reverb * rev, float** output, float** input ,
                          double* args, int inNumSamples) {

  float * output0 = output[0];
  float * input0 = input[0];
  FreeVerb * unit = rev->unit.fv;
  float ftemp0 = (float) args[0]; // mix
  if (ftemp0 > 1.)
    ftemp0 = 1.;
  if (ftemp0 < 0.)
    ftemp0 = 0.;
  float ftemp1 = (1 - ftemp0);

  float room = (float) args[1]; // room
  if (room > 1.)
    room = 1.;
  if (room < 0.)
    room = 0.;
  float ftemp5 = (0.700000f + (0.280000f * room));

  float damp = (float) args[2]; // damp
  if (damp > 1.)
    damp = 1.;
  if (damp < 0.)
    damp = 0.;
  float ftemp6 = (0.400000f * damp);
  float ftemp7 = (1 - ftemp6);

  int iota0 = unit->iota0;
  int iota1 = unit->iota1;
  int iota2 = unit->iota2;
  int iota3 = unit->iota3;
  int iota4 = unit->iota4;
  int iota5 = unit->iota5;
  int iota6 = unit->iota6;
  int iota7 = unit->iota7;
  int iota8 = unit->iota8;
  int iota9 = unit->iota9;
  int iota10 = unit->iota10;
  int iota11 = unit->iota11;

  float R0_1 = unit->R0_1;
  float R1_1 = unit->R1_1;
  float R2_1 = unit->R2_1;
  float R3_1 = unit->R3_1;

  float R0_0 = unit->R0_0;
  float R1_0 = unit->R1_0;
  float R2_0 = unit->R2_0;
  float R3_0 = unit->R3_0;
  float R4_0 = unit->R4_0;
  float R5_0 = unit->R5_0;
  float R6_0 = unit->R6_0;
  float R7_0 = unit->R7_0;
  float R8_0 = unit->R8_0;
  float R9_0 = unit->R9_0;
  float R10_0 = unit->R10_0;
  float R11_0 = unit->R11_0;
  float R12_0 = unit->R12_0;
  float R13_0 = unit->R13_0;
  float R14_0 = unit->R14_0;
  float R15_0 = unit->R15_0;
  float R16_0 = unit->R16_0;
  float R17_0 = unit->R17_0;
  float R18_0 = unit->R18_0;
  float R19_0 = unit->R19_0;

  float* dline0 = unit->dline0;
  float* dline1 = unit->dline1;
  float* dline2 = unit->dline2;
  float* dline3 = unit->dline3;
  float* dline4 = unit->dline4;
  float* dline5 = unit->dline5;
  float* dline6 = unit->dline6;
  float* dline7 = unit->dline7;
  float* dline8 = unit->dline8;
  float* dline9 = unit->dline9;
  float* dline10 = unit->dline10;
  float* dline11 = unit->dline11;

  for (int i = 0; i < inNumSamples; i++) {
    float ftemp2 = input0[i];
    float ftemp4 = (1.500000e-02f * ftemp2);


    if (++iota0 == 225)
      iota0 = 0;
    float T0 = dline0[iota0];

    if (++iota1 == 341)
      iota1 = 0;
    float T1 = dline1[iota1];

    if (++iota2 == 441)
      iota2 = 0;
    float T2 = dline2[iota2];

    if (++iota3 == 556)
      iota3 = 0;
    float T3 = dline3[iota3];


    if (++iota4 == 1617)
      iota4 = 0;
    float T4 = dline4[iota4];
    R5_0 = ((ftemp7 * R4_0) + (ftemp6 * R5_0));
    dline4[iota4] = (ftemp4 + (ftemp5 * R5_0));
    R4_0 = T4;

    if (++iota5 == 1557)
      iota5 = 0;
    float T5 = dline5[iota5];
    R7_0 = ((ftemp7 * R6_0) + (ftemp6 * R7_0));
    dline5[iota5] = (ftemp4 + (ftemp5 * R7_0));
    R6_0 = T5;

    if (++iota6 == 1491)
      iota6 = 0;
    float T6 = dline6[iota6];
    R9_0 = ((ftemp7 * R8_0) + (ftemp6 * R9_0));
    dline6[iota6] = (ftemp4 + (ftemp5 * R9_0));
    R8_0 = T6;

    if (++iota7 == 1422)
      iota7 = 0;
    float T7 = dline7[iota7];
    R11_0 = ((ftemp7 * R10_0) + (ftemp6 * R11_0));
    dline7[iota7] = (ftemp4 + (ftemp5 * R11_0));
    R10_0 = T7;

    if (++iota8 == 1277)
      iota8 = 0;
    float T8 = dline8[iota8];
    R13_0 = ((ftemp7 * R12_0) + (ftemp6 * R13_0));
    dline8[iota8] = (ftemp4 + (ftemp5 * R13_0));
    R12_0 = T8;

    if (++iota9 == 1116)
      iota9 = 0;
    float T9 = dline9[iota9];
    R15_0 = ((ftemp7 * R14_0) + (ftemp6 * R15_0));
    dline9[iota9] = (ftemp4 + (ftemp5 * R15_0));
    R14_0 = T9;

    if (++iota10 == 1188)
      iota10 = 0;
    float T10 = dline10[iota10];
    R17_0 = ((ftemp7 * R16_0) + (ftemp6 * R17_0));
    dline10[iota10] = (ftemp4 + (ftemp5 * R17_0));
    R16_0 = T10;

    if (++iota11 == 1356)
      iota11 = 0;
    float T11 = dline11[iota11];
    R19_0 = ((ftemp7 * R18_0) + (ftemp6 * R19_0));
    dline11[iota11] = (ftemp4 + (ftemp5 * R19_0));
    R18_0 = T11;

    float ftemp8 = (R16_0 + R18_0);

    dline3[iota3] = ((((0.500000f * R3_0) + R4_0) + (R6_0 + R8_0)) + ((R10_0 + R12_0) + (R14_0 + ftemp8)));
    R3_0 = T3;

    R3_1 = (R3_0 - (((R4_0 + R6_0) + (R8_0 + R10_0)) + ((R12_0 + R14_0) + ftemp8)));
    dline2[iota2] = ((0.500000f * R2_0) + R3_1);
    R2_0 = T2;

    R2_1 = (R2_0 - R3_1);
    dline1[iota1] = ((0.500000f * R1_0) + R2_1);
    R1_0 = T1;

    R1_1 = (R1_0 - R2_1);
    dline0[iota0] = ((0.500000f * R0_0) + R1_1);
    R0_0 = T0;

    R0_1 = (R0_0 - R1_1);
    output0[i] = ((ftemp1 * ftemp2) + (ftemp0 * R0_1));
  }

  unit->iota0 = iota0;
  unit->iota1 = iota1;
  unit->iota2 = iota2;
  unit->iota3 = iota3;
  unit->iota4 = iota4;
  unit->iota5 = iota5;
  unit->iota6 = iota6;
  unit->iota7 = iota7;
  unit->iota8 = iota8;
  unit->iota9 = iota9;
  unit->iota10 = iota10;
  unit->iota11 = iota11;

  unit->R0_1 = R0_1;
  unit->R1_1 = R1_1;
  unit->R2_1 = R2_1;
  unit->R3_1 = R3_1;

  unit->R0_0 = R0_0;
  unit->R1_0 = R1_0;
  unit->R2_0 = R2_0;
  unit->R3_0 = R3_0;
  unit->R4_0 = R4_0;
  unit->R5_0 = R5_0;
  unit->R6_0 = R6_0;
  unit->R7_0 = R7_0;
  unit->R8_0 = R8_0;
  unit->R9_0 = R9_0;
  unit->R10_0 = R10_0;
  unit->R11_0 = R11_0;
  unit->R12_0 = R12_0;
  unit->R13_0 = R13_0;
  unit->R14_0 = R14_0;
  unit->R15_0 = R15_0;
  unit->R16_0 = R16_0;
  unit->R17_0 = R17_0;
  unit->R18_0 = R18_0;
  unit->R19_0 = R19_0;
}



static void FreeVerb2_Ctor(Reverb* rev, double * args) {
  FreeVerb2 * unit = rev->unit.fv2;

  unit->iota0 = 0;
  unit->iota1 = 0;
  unit->iota2 = 0;
  unit->iota3 = 0;
  unit->iota4 = 0;
  unit->iota5 = 0;
  unit->iota6 = 0;
  unit->iota7 = 0;
  unit->iota8 = 0;
  unit->iota9 = 0;
  unit->iota10 = 0;
  unit->iota11 = 0;
  unit->iota12 = 0;
  unit->iota13 = 0;
  unit->iota14 = 0;
  unit->iota15 = 0;
  unit->iota16 = 0;
  unit->iota17 = 0;
  unit->iota18 = 0;
  unit->iota19 = 0;
  unit->iota20 = 0;
  unit->iota21 = 0;
  unit->iota22 = 0;
  unit->iota23 = 0;

  unit->R0_0 = 0.0;
  unit->R1_0 = 0.0;
  unit->R2_0 = 0.0;
  unit->R3_0 = 0.0;
  unit->R4_0 = 0.0;
  unit->R5_0 = 0.0;
  unit->R6_0 = 0.0;
  unit->R7_0 = 0.0;
  unit->R8_0 = 0.0;
  unit->R9_0 = 0.0;
  unit->R10_0 = 0.0;
  unit->R11_0 = 0.0;
  unit->R12_0 = 0.0;
  unit->R13_0 = 0.0;
  unit->R14_0 = 0.0;
  unit->R15_0 = 0.0;
  unit->R16_0 = 0.0;
  unit->R17_0 = 0.0;
  unit->R18_0 = 0.0;
  unit->R19_0 = 0.0;
  unit->R20_0 = 0.0;
  unit->R21_0 = 0.0;
  unit->R22_0 = 0.0;
  unit->R23_0 = 0.0;
  unit->R24_0 = 0.0;
  unit->R25_0 = 0.0;
  unit->R26_0 = 0.0;
  unit->R27_0 = 0.0;
  unit->R28_0 = 0.0;
  unit->R29_0 = 0.0;
  unit->R30_0 = 0.0;
  unit->R31_0 = 0.0;
  unit->R32_0 = 0.0;
  unit->R33_0 = 0.0;
  unit->R34_0 = 0.0;
  unit->R35_0 = 0.0;
  unit->R36_0 = 0.0;
  unit->R37_0 = 0.0;
  unit->R38_0 = 0.0;
  unit->R39_0 = 0.0;

  unit->R0_1 = 0.0;
  unit->R1_1 = 0.0;
  unit->R2_1 = 0.0;
  unit->R3_1 = 0.0;

  unit->R23_1 = 0.0;
  unit->R22_1 = 0.0;
  unit->R21_1 = 0.0;
  unit->R20_1 = 0.0;

  for (int i = 0; i < 225; i++)
    unit->dline0[i] = 0.0;
  for (int i = 0; i < 341; i++)
    unit->dline1[i] = 0.0;
  for (int i = 0; i < 441; i++)
    unit->dline2[i] = 0.0;
  for (int i = 0; i < 556; i++)
    unit->dline3[i] = 0.0;
  for (int i = 0; i < 1617; i++)
    unit->dline4[i] = 0.0;
  for (int i = 0; i < 1557; i++)
    unit->dline5[i] = 0.0;
  for (int i = 0; i < 1491; i++)
    unit->dline6[i] = 0.0;
  for (int i = 0; i < 1422; i++)
    unit->dline7[i] = 0.0;
  for (int i = 0; i < 1277; i++)
    unit->dline8[i] = 0.0;
  for (int i = 0; i < 1116; i++)
    unit->dline9[i] = 0.0;
  for (int i = 0; i < 1188; i++)
    unit->dline10[i] = 0.0;
  for (int i = 0; i < 1356; i++)
    unit->dline11[i] = 0.0;
  for (int i = 0; i < 248; i++)
    unit->dline12[i] = 0.0;
  for (int i = 0; i < 364; i++)
    unit->dline13[i] = 0.0;
  for (int i = 0; i < 464; i++)
    unit->dline14[i] = 0.0;
  for (int i = 0; i < 579; i++)
    unit->dline15[i] = 0.0;
  for (int i = 0; i < 1640; i++)
    unit->dline16[i] = 0.0;
  for (int i = 0; i < 1580; i++)
    unit->dline17[i] = 0.0;
  for (int i = 0; i < 1514; i++)
    unit->dline18[i] = 0.0;
  for (int i = 0; i < 1445; i++)
    unit->dline19[i] = 0.0;
  for (int i = 0; i < 1300; i++)
    unit->dline20[i] = 0.0;
  for (int i = 0; i < 1139; i++)
    unit->dline21[i] = 0.0;
  for (int i = 0; i < 1211; i++)
    unit->dline22[i] = 0.0;
  for (int i = 0; i < 1379; i++)
    unit->dline23[i] = 0.0;
}

static void FreeVerb2_next(Reverb* rev, float** output, float** input,
                           double* args, int inNumSamples) {
  FreeVerb2 * unit = rev->unit.fv2;
  float* input0 = input[0];
  float* input1 = input[1];
  float* output0 = output[0];
  float* output1 = output[1];

  float ftemp0 = args[0]; // mix
  if (ftemp0 > 1.)
    ftemp0 = 1.;
  if (ftemp0 < 0.)
    ftemp0 = 0.;
  float ftemp1 = (1 - ftemp0);

  float room = args[1]; // room
  if (room > 1.)
    room = 1.;
  if (room < 0.)
    room = 0.;
  float ftemp5 = (0.700000f + (0.280000f * room));

  float damp = args[2]; // damp
  if (damp > 1.)
    damp = 1.;
  if (damp < 0.)
    damp = 0.;
  float ftemp6 = (0.400000f * damp);
  float ftemp7 = (1 - ftemp6);

  float R0_0 = unit->R0_0;
  float R1_0 = unit->R1_0;
  float R2_0 = unit->R2_0;
  float R3_0 = unit->R3_0;
  float R4_0 = unit->R4_0;
  float R5_0 = unit->R5_0;
  float R6_0 = unit->R6_0;
  float R7_0 = unit->R7_0;
  float R8_0 = unit->R8_0;
  float R9_0 = unit->R9_0;
  float R10_0 = unit->R10_0;
  float R11_0 = unit->R11_0;
  float R12_0 = unit->R12_0;
  float R13_0 = unit->R13_0;
  float R14_0 = unit->R14_0;
  float R15_0 = unit->R15_0;
  float R16_0 = unit->R16_0;
  float R17_0 = unit->R17_0;
  float R18_0 = unit->R18_0;
  float R19_0 = unit->R19_0;
  float R20_0 = unit->R20_0;
  float R21_0 = unit->R21_0;
  float R22_0 = unit->R22_0;
  float R23_0 = unit->R23_0;
  float R24_0 = unit->R24_0;
  float R25_0 = unit->R25_0;
  float R26_0 = unit->R26_0;
  float R27_0 = unit->R27_0;
  float R28_0 = unit->R28_0;
  float R29_0 = unit->R29_0;
  float R30_0 = unit->R30_0;
  float R31_0 = unit->R31_0;
  float R32_0 = unit->R32_0;
  float R33_0 = unit->R33_0;
  float R34_0 = unit->R34_0;
  float R35_0 = unit->R35_0;
  float R36_0 = unit->R36_0;
  float R37_0 = unit->R37_0;
  float R38_0 = unit->R38_0;
  float R39_0 = unit->R39_0;

  float R0_1 = unit->R0_1;
  float R1_1 = unit->R1_1;
  float R2_1 = unit->R2_1;
  float R3_1 = unit->R3_1;

  float R23_1 = unit->R23_1;
  float R22_1 = unit->R22_1;
  float R21_1 = unit->R21_1;
  float R20_1 = unit->R20_1;

  int iota0 = unit->iota0;
  int iota1 = unit->iota1;
  int iota2 = unit->iota2;
  int iota3 = unit->iota3;
  int iota4 = unit->iota4;
  int iota5 = unit->iota5;
  int iota6 = unit->iota6;
  int iota7 = unit->iota7;
  int iota8 = unit->iota8;
  int iota9 = unit->iota9;
  int iota10 = unit->iota10;
  int iota11 = unit->iota11;
  int iota12 = unit->iota12;
  int iota13 = unit->iota13;
  int iota14 = unit->iota14;
  int iota15 = unit->iota15;
  int iota16 = unit->iota16;
  int iota17 = unit->iota17;
  int iota18 = unit->iota18;
  int iota19 = unit->iota19;
  int iota20 = unit->iota20;
  int iota21 = unit->iota21;
  int iota22 = unit->iota22;
  int iota23 = unit->iota23;

  float* dline0 = unit->dline0;
  float* dline1 = unit->dline1;
  float* dline2 = unit->dline2;
  float* dline3 = unit->dline3;
  float* dline4 = unit->dline4;
  float* dline5 = unit->dline5;
  float* dline6 = unit->dline6;
  float* dline7 = unit->dline7;
  float* dline8 = unit->dline8;
  float* dline9 = unit->dline9;
  float* dline10 = unit->dline10;
  float* dline11 = unit->dline11;
  float* dline12 = unit->dline12;
  float* dline13 = unit->dline13;
  float* dline14 = unit->dline14;
  float* dline15 = unit->dline15;
  float* dline16 = unit->dline16;
  float* dline17 = unit->dline17;
  float* dline18 = unit->dline18;
  float* dline19 = unit->dline19;
  float* dline20 = unit->dline20;
  float* dline21 = unit->dline21;
  float* dline22 = unit->dline22;
  float* dline23 = unit->dline23;

  for (int i = 0; i < inNumSamples; i++) {
    float ftemp2 = input0[i];
    if (++iota0 == 225)
      iota0 = 0;
    float T0 = dline0[iota0];
    if (++iota1 == 341)
      iota1 = 0;
    float T1 = dline1[iota1];
    if (++iota2 == 441)
      iota2 = 0;
    float T2 = dline2[iota2];
    if (++iota3 == 556)
      iota3 = 0;
    float T3 = dline3[iota3];
    if (++iota4 == 1617)
      iota4 = 0;
    float T4 = dline4[iota4];
    float ftemp3 = input1[i];
    float ftemp4 = (1.500000e-02f * (ftemp2 + ftemp3));
    R5_0 = ((ftemp7 * R4_0) + (ftemp6 * R5_0));
    dline4[iota4] = (ftemp4 + (ftemp5 * R5_0));
    R4_0 = T4;
    if (++iota5 == 1557)
      iota5 = 0;
    float T5 = dline5[iota5];
    R7_0 = ((ftemp7 * R6_0) + (ftemp6 * R7_0));
    dline5[iota5] = (ftemp4 + (ftemp5 * R7_0));
    R6_0 = T5;
    if (++iota6 == 1491)
      iota6 = 0;
    float T6 = dline6[iota6];
    R9_0 = ((ftemp7 * R8_0) + (ftemp6 * R9_0));
    dline6[iota6] = (ftemp4 + (ftemp5 * R9_0));
    R8_0 = T6;
    if (++iota7 == 1422)
      iota7 = 0;
    float T7 = dline7[iota7];
    R11_0 = ((ftemp7 * R10_0) + (ftemp6 * R11_0));
    dline7[iota7] = (ftemp4 + (ftemp5 * R11_0));
    R10_0 = T7;
    if (++iota8 == 1277)
      iota8 = 0;
    float T8 = dline8[iota8];
    R13_0 = ((ftemp7 * R12_0) + (ftemp6 * R13_0));
    dline8[iota8] = (ftemp4 + (ftemp5 * R13_0));
    R12_0 = T8;
    if (++iota9 == 1116)
      iota9 = 0;
    float T9 = dline9[iota9];
    R15_0 = ((ftemp7 * R14_0) + (ftemp6 * R15_0));
    dline9[iota9] = (ftemp4 + (ftemp5 * R15_0));
    R14_0 = T9;
    if (++iota10 == 1188)
      iota10 = 0;
    float T10 = dline10[iota10];
    R17_0 = ((ftemp7 * R16_0) + (ftemp6 * R17_0));
    dline10[iota10] = (ftemp4 + (ftemp5 * R17_0));
    R16_0 = T10;
    if (++iota11 == 1356)
      iota11 = 0;
    float T11 = dline11[iota11];
    R19_0 = ((ftemp7 * R18_0) + (ftemp6 * R19_0));
    dline11[iota11] = (ftemp4 + (ftemp5 * R19_0));
    R18_0 = T11;
    float ftemp8 = (R16_0 + R18_0);
    dline3[iota3] = ((((0.500000f * R3_0) + R4_0) + (R6_0 + R8_0)) + ((R10_0 + R12_0) + (R14_0 + ftemp8)));
    R3_0 = T3;
    R3_1 = (R3_0 - (((R4_0 + R6_0) + (R8_0 + R10_0)) + ((R12_0 + R14_0) + ftemp8)));
    dline2[iota2] = ((0.500000f * R2_0) + R3_1);
    R2_0 = T2;
    R2_1 = (R2_0 - R3_1);
    dline1[iota1] = ((0.500000f * R1_0) + R2_1);
    R1_0 = T1;
    R1_1 = (R1_0 - R2_1);
    dline0[iota0] = ((0.500000f * R0_0) + R1_1);
    R0_0 = T0;
    R0_1 = (R0_0 - R1_1);
    output0[i] = ((ftemp1 * ftemp2) + (ftemp0 * R0_1));

    // right chn
    if (++iota12 == 248)
      iota12 = 0;
    float T12 = dline12[iota12];
    if (++iota13 == 364)
      iota13 = 0;
    float T13 = dline13[iota13];
    if (++iota14 == 464)
      iota14 = 0;
    float T14 = dline14[iota14];
    if (++iota15 == 579)
      iota15 = 0;
    float T15 = dline15[iota15];
    if (++iota16 == 1640)
      iota16 = 0;
    float T16 = dline16[iota16];
    R25_0 = ((ftemp7 * R24_0) + (ftemp6 * R25_0));
    dline16[iota16] = (ftemp4 + (ftemp5 * R25_0));
    R24_0 = T16;
    if (++iota17 == 1580)
      iota17 = 0;
    float T17 = dline17[iota17];
    R27_0 = ((ftemp7 * R26_0) + (ftemp6 * R27_0));
    dline17[iota17] = (ftemp4 + (ftemp5 * R27_0));
    R26_0 = T17;
    if (++iota18 == 1514)
      iota18 = 0;
    float T18 = dline18[iota18];
    R29_0 = ((ftemp7 * R28_0) + (ftemp6 * R29_0));
    dline18[iota18] = (ftemp4 + (ftemp5 * R29_0));
    R28_0 = T18;
    if (++iota19 == 1445)
      iota19 = 0;
    float T19 = dline19[iota19];
    R31_0 = ((ftemp7 * R30_0) + (ftemp6 * R31_0));
    dline19[iota19] = (ftemp4 + (ftemp5 * R31_0));
    R30_0 = T19;
    if (++iota20 == 1300)
      iota20 = 0;
    float T20 = dline20[iota20];
    R33_0 = ((ftemp7 * R32_0) + (ftemp6 * R33_0));
    dline20[iota20] = (ftemp4 + (ftemp5 * R33_0));
    R32_0 = T20;
    if (++iota21 == 1139)
      iota21 = 0;
    float T21 = dline21[iota21];
    R35_0 = ((ftemp7 * R34_0) + (ftemp6 * R35_0));
    dline21[iota21] = (ftemp4 + (ftemp5 * R35_0));
    R34_0 = T21;
    if (++iota22 == 1211)
      iota22 = 0;
    float T22 = dline22[iota22];
    R37_0 = ((ftemp7 * R36_0) + (ftemp6 * R37_0));
    dline22[iota22] = (ftemp4 + (ftemp5 * R37_0));
    R36_0 = T22;
    if (++iota23 == 1379)
      iota23 = 0;
    float T23 = dline23[iota23];
    R39_0 = ((ftemp7 * R38_0) + (ftemp6 * R39_0));
    dline23[iota23] = (ftemp4 + (ftemp5 * R39_0));
    R38_0 = T23;
    float ftemp9 = (R36_0 + R38_0);
    dline15[iota15] = ((((0.500000f * R23_0) + R24_0) + (R26_0 + R28_0)) + ((R30_0 + R32_0) + (R34_0 + ftemp9)));
    R23_0 = T15;
    R23_1 = (R23_0 - (((R24_0 + R26_0) + (R28_0 + R30_0)) + ((R32_0 + R34_0) + ftemp9)));
    dline14[iota14] = ((0.500000f * R22_0) + R23_1);
    R22_0 = T14;
    R22_1 = (R22_0 - R23_1);
    dline13[iota13] = ((0.500000f * R21_0) + R22_1);
    R21_0 = T13;
    R21_1 = (R21_0 - R22_1);
    dline12[iota12] = ((0.500000f * R20_0) + R21_1);
    R20_0 = T12;
    R20_1 = (R20_0 - R21_1);
    output1[i] = ((ftemp1 * ftemp3) + (ftemp0 * R20_1));
  }

  unit->iota0 = iota0;
  unit->iota1 = iota1;
  unit->iota2 = iota2;
  unit->iota3 = iota3;
  unit->iota4 = iota4;
  unit->iota5 = iota5;
  unit->iota6 = iota6;
  unit->iota7 = iota7;
  unit->iota8 = iota8;
  unit->iota9 = iota9;
  unit->iota10 = iota10;
  unit->iota11 = iota11;
  unit->iota12 = iota12;
  unit->iota13 = iota13;
  unit->iota14 = iota14;
  unit->iota15 = iota15;
  unit->iota16 = iota16;
  unit->iota17 = iota17;
  unit->iota18 = iota18;
  unit->iota19 = iota19;
  unit->iota20 = iota20;
  unit->iota21 = iota21;
  unit->iota22 = iota22;
  unit->iota23 = iota23;

  unit->R0_1 = R0_1;
  unit->R1_1 = R1_1;
  unit->R2_1 = R2_1;
  unit->R3_1 = R3_1;

  unit->R20_1 = R20_1;
  unit->R21_1 = R21_1;
  unit->R22_1 = R22_1;
  unit->R23_1 = R23_1;

  unit->R0_0 = R0_0;
  unit->R1_0 = R1_0;
  unit->R2_0 = R2_0;
  unit->R3_0 = R3_0;
  unit->R4_0 = R4_0;
  unit->R5_0 = R5_0;
  unit->R6_0 = R6_0;
  unit->R7_0 = R7_0;
  unit->R8_0 = R8_0;
  unit->R9_0 = R9_0;
  unit->R10_0 = R10_0;
  unit->R11_0 = R11_0;
  unit->R12_0 = R12_0;
  unit->R13_0 = R13_0;
  unit->R14_0 = R14_0;
  unit->R15_0 = R15_0;
  unit->R16_0 = R16_0;
  unit->R17_0 = R17_0;
  unit->R18_0 = R18_0;
  unit->R19_0 = R19_0;
  unit->R20_0 = R20_0;
  unit->R21_0 = R21_0;
  unit->R22_0 = R22_0;
  unit->R23_0 = R23_0;
  unit->R24_0 = R24_0;
  unit->R25_0 = R25_0;
  unit->R26_0 = R26_0;
  unit->R27_0 = R27_0;
  unit->R28_0 = R28_0;
  unit->R29_0 = R29_0;
  unit->R30_0 = R30_0;
  unit->R31_0 = R31_0;
  unit->R32_0 = R32_0;
  unit->R33_0 = R33_0;
  unit->R34_0 = R34_0;
  unit->R35_0 = R35_0;
  unit->R36_0 = R36_0;
  unit->R37_0 = R37_0;
  unit->R38_0 = R38_0;
  unit->R39_0 = R39_0;
}


#define TRUE 1
#define FALSE 0


typedef union {
  float f;
#ifdef _WIN32
  long int i;
#else
  int32_t i;
#endif
} ls_pcast32;

static inline float flush_to_zero(float f) {
  ls_pcast32 v;
  v.f = f;
  // original: return (v.i & 0x7f800000) == 0 ? 0.0f : f;
  // version from Tim Blechmann
  return (v.i & 0x7f800000) < 0x08000000 ? 0.0f : f;
}

static int isprime(int n) {
  unsigned int i;
  const unsigned int lim = (int)sqrtf((float)n);

  if (n == 2)
    return (TRUE);
  if ((n & 1) == 0)
    return (FALSE);
  for (i = 3; i <= lim; i += 2)
    if ((n % i) == 0)
      return (FALSE);
  return (TRUE);
}

static int nearestprime(int n, float rerror) {
  int bound, k;

  if (isprime(n))
    return (n);
  /* assume n is large enough and n*rerror enough smaller than n */
  bound = (int)(n * rerror);
  for (k = 1; k <= bound; k++) {
    if (isprime(n + k))
      return (n + k);
    if (isprime(n - k))
      return (n - k);
  }
  return (-1);
}

static inline int f_round(float f) {
  ls_pcast32 p;
  p.f = f;
  p.f += (3 << 22);
  return p.i - 0x4b400000;
}

static g_damper* make_damper(GVerb* unit, float damping) {
  g_damper* p;
  p = (g_damper*)enif_alloc(sizeof(g_damper));
  p->damping = damping;
  p->delay = 0.f;
  return (p);
}

static void free_damper(GVerb* unit, g_damper* p) { enif_free(p); };

static g_diffuser* make_diffuser(GVerb* unit, int size, float coef) {
  g_diffuser* p;
  p = (g_diffuser*)enif_alloc(sizeof(g_diffuser));
  p->size = size;
  p->coef = coef;
  p->idx = 0;
  p->buf = (float*)enif_alloc(size * sizeof(float));
  for(int i = 0; i < size; i++) p->buf[i] = 0.f;
  return (p);
}

static void free_diffuser(GVerb* unit, g_diffuser* p) {
  enif_free(p->buf);
  enif_free(p);
}

static g_fixeddelay* make_fixeddelay(GVerb* unit, int size, int maxsize) {
  g_fixeddelay* p;
  p = (g_fixeddelay*)enif_alloc(sizeof(g_fixeddelay));
  p->size = size;
  p->idx = 0;
  p->buf = (float*)enif_alloc(maxsize * sizeof(float));
  for(int i = 0; i < maxsize; i++) p->buf[i] = 0.f;
  return (p);
}

static void free_fixeddelay(GVerb* unit, g_fixeddelay* p) {
  enif_free(p->buf);
  enif_free(p);
}

static inline float diffuser_do(GVerb* unit, g_diffuser* p, float x) {
  float y, w;
  w = x - p->buf[p->idx] * p->coef;
  w = flush_to_zero(w);
  y = p->buf[p->idx] + w * p->coef;
  p->buf[p->idx] = zapgremlins(w);
  p->idx = (p->idx + 1) % p->size;
  return (y);
}

static inline float fixeddelay_read(GVerb* unit, g_fixeddelay* p, int n) {
  int i;
  i = (p->idx - n + p->size) % p->size;
  return (p->buf[i]);
}

static inline void fixeddelay_write(GVerb* unit, g_fixeddelay* p, float x) {
  p->buf[p->idx] = zapgremlins(x);
  p->idx = (p->idx + 1) % p->size;
}

static inline void damper_set(GVerb* unit, g_damper* p, float damping) { p->damping = damping; }

static inline float damper_do(GVerb* unit, g_damper* p, float x) {
  float y;
  y = x * (1.0 - p->damping) + p->delay * p->damping;
  p->delay = zapgremlins(y);
  return (y);
}

static inline void gverb_fdnmatrix(float* a, float* b) {
  const float dl0 = a[0], dl1 = a[1], dl2 = a[2], dl3 = a[3];
  b[0] = 0.5f * (+dl0 + dl1 - dl2 - dl3);
  b[1] = 0.5f * (+dl0 - dl1 - dl2 + dl3);
  b[2] = 0.5f * (-dl0 + dl1 - dl2 + dl3);
  b[3] = 0.5f * (+dl0 + dl1 + dl2 + dl3);
}

static inline void gverb_set_roomsize(GVerb* unit, const float a) {
  unsigned int i;

  if (a <= 1.0 || isnan(a)) {
    unit->roomsize = 1.0;
  } else {
    if (a >= unit->maxroomsize)
      unit->roomsize = unit->maxroomsize - 1.;
    else
      unit->roomsize = a;
  };

  unit->largestdelay = unit->rate * unit->roomsize / 340.0; // * 0.00294f;

  // the line below causes everything to blow up.... why?????
  //  unit->fdnlens[0] = nearestprime((int)(unit->largestdelay), 0.5);
  unit->fdnlens[1] = (int)(0.816490 * unit->largestdelay);
  unit->fdnlens[2] = (int)(0.707100 * unit->largestdelay);
  unit->fdnlens[3] = (int)(0.632450 * unit->largestdelay);

  for (i = 0; i < FDNORDER; i++) {
    float oldfdngain = unit->fdngains[i];
    unit->fdngains[i] = -powf((float)unit->alpha, unit->fdnlens[i]);
    unit->fdngainslopes[i] = (unit->fdngains[i] - oldfdngain) / unit->period_size;
  }

  unit->taps[0] = 5 + (int)(0.410 * unit->largestdelay);
  unit->taps[1] = 5 + (int)(0.300 * unit->largestdelay);
  unit->taps[2] = 5 + (int)(0.155 * unit->largestdelay);
  unit->taps[3] = 5; //+ f_round(0.000 * largestdelay);

  for (i = 0; i < FDNORDER; i++) {
    float oldtapgain = unit->tapgains[i];
    unit->tapgains[i] = pow(unit->alpha, unit->taps[i]);
    unit->tapgainslopes[i] = (unit->tapgains[i] - oldtapgain) / unit->period_size;
  }
}

static inline void gverb_set_revtime(GVerb* unit, float a) {
  float ga;
  double n;
  unsigned int i;

  unit->revtime = a;

  ga = 0.001;
  n = unit->rate * a;
  unit->alpha = (double)powf(ga, (float)(1.f / n));

  for (i = 0; i < FDNORDER; i++) {
    float oldfdngain = unit->fdngains[i];
    unit->fdngains[i] = -powf((float)unit->alpha, unit->fdnlens[i]);
    unit->fdngainslopes[i] = (unit->fdngains[i] - oldfdngain) / unit->period_size;
  }
}

static inline void gverb_set_damping(GVerb* unit, float a) {
  unsigned int i;

  unit->damping = a;
  for (i = 0; i < FDNORDER; i++) {
    damper_set(unit, unit->fdndamps[i], unit->damping);
  }
}

static inline void gverb_set_inputbandwidth(GVerb* unit, float a) {
  unit->inputbandwidth = a;
  damper_set(unit, unit->inputdamper, 1.0 - unit->inputbandwidth);
}

static inline float gverb_set_earlylevel(GVerb* unit, float a) {
  float oldearly = unit->earlylevel;
  unit->earlylevel = a;
  unit->earlylevelslope = (a - oldearly) / unit->period_size;
  return (oldearly);
}

static inline float gverb_set_taillevel(GVerb* unit, float a) {
  float oldtail = unit->taillevel;
  unit->taillevel = a;
  unit->taillevelslope = (a - oldtail) / unit->period_size;
  return (oldtail);
}

static inline float gverb_set_drylevel(GVerb* unit, float a) {
  float olddry = unit->drylevel;
  unit->drylevel = a;
  unit->drylevelslope = (a - olddry) / unit->period_size;
  return (olddry);
}

static void GVerb_Ctor(Reverb * rev, double * args) {
  GVerb * unit = rev->unit.gv;
  unit->rate = (float) rev->rate;
  unit->period_size = (float) rev->period_size;
  float roomsize = unit->roomsize = args[0];
  float revtime = unit->revtime = args[1];
  float damping = unit->damping = args[2];
  float inputbandwidth = unit->inputbandwidth = 0.; // IN0(4);
  float spread = unit->spread = args[4]; // IN0(5);
  unit->drylevel = 0.; // IN0(6);
  unit->earlylevel = 0.; // IN0(7);
  unit->taillevel = 0.; // IN0(8);

  float maxroomsize = unit->maxroomsize = args[8];

  float maxdelay = unit->maxdelay = unit->rate * maxroomsize / 340.f;
  float largestdelay = unit->largestdelay = unit->rate * roomsize / 340.f;

  // make the inputdamper
  unit->inputdamper = make_damper(unit, 1. - inputbandwidth);

  // float ga = powf(10.f, -60.f/20.f);
  float ga = 0.001f;
  float n = unit->rate * revtime;
  double alpha = unit->alpha = pow((double)ga, 1. / (double)n);
  float gbmul[4] = { 1.000, 0.816490, 0.707100, 0.632450 };
  for (int i = 0; i < FDNORDER; ++i) {
    float gb = gbmul[i] * largestdelay;
    if (i == 0) {
      unit->fdnlens[i] = nearestprime((int)gb, 0.5);
    } else {
      unit->fdnlens[i] = f_round(gb);
    }
    unit->fdngains[i] = -powf((float)alpha, unit->fdnlens[i]);
  }
  // make the fixeddelay lines and dampers
  for (int i = 0; i < FDNORDER; i++) {
    unit->fdndels[i] = make_fixeddelay(unit, (int)unit->fdnlens[i], (int)maxdelay + 1000);
    unit->fdndamps[i] = make_damper(unit, damping); // damping is the same as fdndamping in source
  }

  // diffuser section
  float diffscale = (float)unit->fdnlens[3] / (210. + 159. + 562. + 410.);
  float spread1 = spread;
  float spread2 = 3.0 * spread;

  int b = 210;
  float r = 0.125541;
  int a = (int)(spread1 * r);
  int c = 210 + 159 + a;
  int cc = c - b;
  r = 0.854046;
  a = (int)(spread2 * r);
  int d = 210 + 159 + 562 + a;
  int dd = d - c;
  int e = 1341 - d;

  unit->ldifs[0] = make_diffuser(unit, f_round(diffscale * b), 0.75);
  unit->ldifs[1] = make_diffuser(unit, f_round(diffscale * cc), 0.75);
  unit->ldifs[2] = make_diffuser(unit, f_round(diffscale * dd), 0.625);
  unit->ldifs[3] = make_diffuser(unit, f_round(diffscale * e), 0.625);
  b = 210;
  r = -0.568366;
  a = (int)(spread1 * r);
  c = 210 + 159 + a;
  cc = c - b;
  r = -0.126815;
  a = (int)(spread2 * r);
  d = 210 + 159 + 562 + a;
  dd = d - c;
  e = 1341 - d;

  unit->rdifs[0] = make_diffuser(unit, f_round(diffscale * b), 0.75);
  unit->rdifs[1] = make_diffuser(unit, f_round(diffscale * cc), 0.75);
  unit->rdifs[2] = make_diffuser(unit, f_round(diffscale * dd), 0.625);
  unit->rdifs[3] = make_diffuser(unit, f_round(diffscale * e), 0.625);

  unit->taps[0] = 5 + (int)(0.410 * largestdelay);
  unit->taps[1] = 5 + (int)(0.300 * largestdelay);
  unit->taps[2] = 5 + (int)(0.155 * largestdelay);
  unit->taps[3] = 5; //+ f_round(0.000 * largestdelay);

  for (int i = 0; i < FDNORDER; i++) {
    unit->tapgains[i] = pow(alpha, (double)unit->taps[i]);
  }

  unit->tapdelay = make_fixeddelay(unit, 44000, 44000);

  // init the slope values
  unit->earlylevelslope = unit->drylevelslope = unit->taillevelslope = 0.f;
  /* uint32 numOuts = unit->mNumOutputs; */
  /* for (uint32 i = 0; i < numOuts; ++i) { */
  /*     float* out = OUT(i); */
  /*     Clear(inNumSamples, out); */
  /* } */
  /* ClearUnitOutputs(unit, 1); */
}

static void GVerb_Dtor(Reverb* rev) {
  GVerb * unit = rev->unit.gv;
  free_damper(unit, unit->inputdamper);
  free_fixeddelay(unit, unit->tapdelay);

  for (int i = 0; i < FDNORDER; i++) {
    free_fixeddelay(unit, unit->fdndels[i]);
    free_damper(unit, unit->fdndamps[i]);
    free_diffuser(unit, unit->ldifs[i]);
    free_diffuser(unit, unit->rdifs[i]);
  }
}

static void GVerb_next(Reverb* rev, float** out, float** in_array, double* args, int inNumSamples) {
  GVerb * unit = rev->unit.gv;
  float* in = in_array[0];
  float* outl = out[0];
  float* outr = out[1];
  float roomsize = args[0];
  float revtime = args[1];
  float damping = args[2];
  float inputbandwidth = args[3]; //IN0(4);
  // float spread = IN0(5); // spread can only be set at inittime
  float drylevel = args[5];
  float earlylevel = args[6];
  float taillevel = args[7];
  float earlylevelslope, taillevelslope, drylevelslope;
  float* fdngainslopes;
  float* tapgainslopes;
  g_diffuser** ldifs = unit->ldifs;
  g_diffuser** rdifs = unit->rdifs;
  float* u = unit->u;
  float* f = unit->f;
  float* d = unit->d;
  g_damper* inputdamper = unit->inputdamper;
  float* tapgains = unit->tapgains;
  g_fixeddelay* tapdelay = unit->tapdelay;
  int* taps = unit->taps;
  g_damper** fdndamps = unit->fdndamps;
  g_fixeddelay** fdndels = unit->fdndels;
  float* fdngains = unit->fdngains;
  int* fdnlens = unit->fdnlens;

  if ((roomsize != unit->roomsize) || (revtime != unit->revtime) || (damping != unit->damping)
      || (inputbandwidth != unit->inputbandwidth) || (drylevel != unit->drylevel) || (earlylevel != unit->earlylevel)
      || (taillevel != unit->taillevel)) {
    // these should calc slopes for k-rate interpolation
    gverb_set_roomsize(unit, roomsize);
    gverb_set_revtime(unit, revtime);
    gverb_set_damping(unit, damping);
    gverb_set_inputbandwidth(unit, inputbandwidth);
    drylevel = gverb_set_drylevel(unit, drylevel);
    earlylevel = gverb_set_earlylevel(unit, earlylevel);
    taillevel = gverb_set_taillevel(unit, taillevel);
  }

  earlylevelslope = unit->earlylevelslope;
  taillevelslope = unit->taillevelslope;
  drylevelslope = unit->drylevelslope;
  fdngainslopes = unit->fdngainslopes;
  tapgainslopes = unit->tapgainslopes;


  for (int i = 0; i < inNumSamples; i++) {
    float sign, sum, lsum, rsum, x;
    if (isnan(in[i]))
      x = 0.f;
    else
      x = in[i];
    sum = 0.f;
    sign = 1.f;

    float z = damper_do(unit, inputdamper, x);
    z = diffuser_do(unit, ldifs[0], z);

    for (int j = 0; j < FDNORDER; j++) {
      u[j] = tapgains[j] * fixeddelay_read(unit, tapdelay, taps[j]);
    }

    fixeddelay_write(unit, tapdelay, z);

    for (int j = 0; j < FDNORDER; j++) {
      d[j] = damper_do(unit, fdndamps[j], fdngains[j] * fixeddelay_read(unit, fdndels[j], fdnlens[j]));
    }

    for (int j = 0; j < FDNORDER; j++) {
      sum += sign * (taillevel * d[j] + earlylevel * u[j]);
      sign = -sign;
    }

    sum += x * earlylevel;
    lsum = sum;
    rsum = sum;

    gverb_fdnmatrix(d, f);

    for (int j = 0; j < FDNORDER; j++) {
      fixeddelay_write(unit, fdndels[j], u[j] + f[j]);
    }

    lsum = diffuser_do(unit, ldifs[1], lsum);
    lsum = diffuser_do(unit, ldifs[2], lsum);
    lsum = diffuser_do(unit, ldifs[3], lsum);
    rsum = diffuser_do(unit, rdifs[1], rsum);
    rsum = diffuser_do(unit, rdifs[2], rsum);
    rsum = diffuser_do(unit, rdifs[3], rsum);

    x = x * drylevel;
    outl[i] = lsum + x;
    outr[i] = rsum + x;

    drylevel += drylevelslope;
    taillevel += taillevelslope;
    earlylevel += earlylevelslope;
    for (int j = 0; j < FDNORDER; j++) {
      fdngains[j] += fdngainslopes[j];
      tapgains[j] += tapgainslopes[j];
    }
  }

  // store vals back to the struct
  for (int i = 0; i < FDNORDER; i++) {
    unit->ldifs[i] = ldifs[i];
    unit->rdifs[i] = rdifs[i];
    unit->u[i] = u[i];
    unit->f[i] = f[i];
    unit->d[i] = d[i];
    unit->tapgains[i] = tapgains[i];
    unit->taps[i] = taps[i];
    unit->fdndamps[i] = fdndamps[i];
    unit->fdndels[i] = fdndels[i];
    unit->fdngains[i] = fdngains[i];
    unit->fdnlens[i] = fdnlens[i];
    unit->fdngainslopes[i] = 0.f;
    unit->tapgainslopes[i] = 0.f;
  }
  unit->inputdamper = inputdamper;
  unit->tapdelay = tapdelay;
  // clear the slopes
  unit->earlylevelslope = unit->taillevelslope = unit->drylevelslope = 0.f;
}

/* ---------------------------------------------------------- */

// ErlNifResourceDtor
static void reverb_resource_dtor(ErlNifEnv* env, void * obj){
  Reverb * rev = (Reverb*) obj;

  if(rev->dtor) {
    (*rev->dtor)(rev);
  }
  enif_free((void*)rev->unit.fv);
}

static ERL_NIF_TERM reverb_ctor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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

  Reverb * rev = enif_alloc_resource(sc_reverb_type, sizeof(Reverb));
  if (strcmp(type, "freeverb") == 0) {
    rev->unit.fv = enif_alloc(sizeof(FreeVerb));
    rev->first = &FreeVerb_Ctor;
    rev->next = &FreeVerb_next;
    rev->dtor = NULL;
  } else if (strcmp(type, "freeverb2") == 0) {
    rev->unit.fv2 = enif_alloc(sizeof(FreeVerb2));
    rev->first = &FreeVerb2_Ctor;
    rev->next = &FreeVerb2_next;
    rev->dtor = NULL;
  } else if (strcmp(type, "gverb") == 0) {
    rev->unit.gv = enif_alloc(sizeof(GVerb));
    rev->first = &GVerb_Ctor;
    rev->next = &GVerb_next;
    rev->dtor = &GVerb_Dtor;
  } else {
    return enif_make_badarg(env);
  }
  rev->rate = rate;
  rev->period_size = period_size;

  ERL_NIF_TERM term = enif_make_resource(env, rev);
  enif_release_resource(rev);
  return term;
}


static ERL_NIF_TERM reverb_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  Reverb * rev;
  double args[4];

  if (!enif_get_resource(env, argv[0],
                         sc_reverb_type,
                         (void**) &rev)){
    return enif_raise_exception(env,
                                enif_make_string(env,
                                                 "No valid reference",
                                                 ERL_NIF_LATIN1));
  }

  if(!enif_get_double(env, argv[2], &args[0])){
    return enif_raise_exception(env,
                                enif_make_string(env,
                                                 "Mix not a float",
                                                 ERL_NIF_LATIN1));
  }
  if(!enif_get_double(env, argv[3], &args[1])){
    return enif_raise_exception(env,
                                enif_make_string(env,
                                                 "Room not a float",
                                                 ERL_NIF_LATIN1));
  }
  if(!enif_get_double(env, argv[4], &args[2])){
    return enif_raise_exception(env,
                                enif_make_string(env,
                                                 "Damp not a float",
                                                 ERL_NIF_LATIN1));
  }

  unsigned len;
  if(enif_get_list_length(env, argv[1], &len)
     && len < 3) {
    ERL_NIF_TERM out_term[2];
    ERL_NIF_TERM list, head, tail;
    float * out_array[2];
    float * in_array[2];
    ErlNifBinary in_bin;
    int inNumSamples = 0;
    list = argv[1];
    unsigned i = 0;
    while (enif_get_list_cell(env, list, &head, &tail)){
      if(enif_inspect_binary(env, head, &in_bin)){
        inNumSamples = in_bin.size / sizeof(float);
        in_array[i] = (float *) in_bin.data;
        out_array[i] = (float *) enif_make_new_binary(env, in_bin.size, &out_term[i]);
        list = tail;
        i++;
      } else {
        return enif_raise_exception(env,
                                    enif_make_string(env,
                                                     "Input stream not a binary",
                                                     ERL_NIF_LATIN1));
      }
    }
    if(rev->first) {
      (*rev->first)(rev, args);
      (*rev->next)(rev, out_array, in_array, args, 1);
      rev->first = NULL;
    }
    (*rev->next)(rev, out_array, in_array, args, inNumSamples);
    if (len == 1){
      return out_term[0];
    } else {
      return enif_make_list_from_array(env, out_term, len);
    }
  }else{
    return enif_raise_exception(env,
                                enif_make_string(env,
                                                 "Input stream not a list",
                                                 ERL_NIF_LATIN1));
  }
}

/* ---------------------------------------------------------- */
static ErlNifFunc nif_funcs[] = {
  {"reverb_ctor", 3, reverb_ctor},
  {"reverb_next", 5,  reverb_next}
};

static int open_reverb_resource_type(ErlNifEnv* env)
{
  const char* mod = "Elixir.SC.Reverb";
  const char* resource_type = "sc_reverb";
  int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
  sc_reverb_type =
    enif_open_resource_type(env, mod, resource_type,
                            reverb_resource_dtor, flags, NULL);
  return ((sc_reverb_type == NULL) ? -1:0);
}

static int load(ErlNifEnv* caller_env, void** priv_data, ERL_NIF_TERM load_info)
{
  return open_reverb_resource_type(caller_env);
}

static int upgrade(ErlNifEnv* caller_env, void** priv_data, void** old_priv_data,
		   ERL_NIF_TERM load_info)
{
  return open_reverb_resource_type(caller_env);
}


ERL_NIF_INIT(Elixir.SC.Reverb, nif_funcs, load, NULL, upgrade, NULL);
