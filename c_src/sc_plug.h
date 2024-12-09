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

/*  The inline functions below are from the SuperCollider 
    include/plugin_interface header files and hence the GPL above.

    From include/plugin_interface/SC_InlineUnaryOp.h 
*/
static inline float zapgremlins(float x) {
  float absx = fabsf(x);
    // very small numbers fail the first test, eliminating denormalized numbers
    //    (zero also fails the first test, but that is OK since it returns zero.)
    // very large numbers fail the second test, eliminating infinities
    // Not-a-Numbers fail both tests and are eliminated.
    return (absx > (float)1e-15 && absx < (float)1e15) ? x : (float)0.;
}


/*  From include/plugin_interface/SC_SndBuf.h */

static inline float cubicinterp(float x, float y0, float y1, float y2, float y3) {
    // 4-point, 3rd-order Hermite (x-form)
    float c0 = y1;
    float c1 = 0.5f * (y2 - y0);
    float c2 = y0 - 2.5f * y1 + 2.f * y2 - 0.5f * y3;
    float c3 = 0.5f * (y3 - y0) + 1.5f * (y1 - y2);

    return ((c3 * x + c2) * x + c1) * x + c0;
}

#define sc_max(a, b) (((a) > (b)) ? (a) : (b))
#define sc_min(a, b) (((a) < (b)) ? (a) : (b))
const double log001 = log(0.001);
const double sqrt2 = sqrt(2.);
