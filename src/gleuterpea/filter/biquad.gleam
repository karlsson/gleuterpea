////  Code from - Synthex.Filter.Biquad:
////  https://github.com/bitgamma/synthex/blob/master/lib/synthex/filter/biquad.ex
////  transposed to use Erlang NIF library.

import gleam
import gleam/erlang
import gleam/int
import gleam/result
import gleam/yielder.{type Yielder}
import gleam_community/maths/elementary
import gleuterpea.{type Frame}
import gleuterpea/nifs.{type ErlangResult, type NifLoadError}

pub opaque type Coeff {
  // default (1.0, 0.0, 0.0, 1.0, 0.0, 0.0)
  Coeff(Float, Float, Float, Float, Float, Float)
}

pub type ErlCoff =
  #(Float, Float, Float, Float, Float, Float)

pub opaque type BiQuad {
  BiQuad(ref: erlang.Reference, coefficients: Coeff)
}

pub fn load_nif() -> Result(Nil, #(NifLoadError, String)) {
  case load_nif_internal("priv/gleuterpea_biquad", 0) {
    nifs.Ok -> gleam.Ok(Nil)
    nifs.Error(#(nifs.Reload, _)) -> gleam.Ok(Nil)
    nifs.Error(err) -> gleam.Error(err)
  }
}

@external(erlang, "erlang", "load_nif")
fn load_nif_internal(nif: String, load_info: Int) -> ErlangResult

// Helper NIF to be called when creating an Oscillator
fn biquad_ctor() -> erlang.Reference {
  panic as "NIF biquad_ctor/0 not loaded"
}

// Helper NIF to be called when getting the next Frame
fn biquad_next(
  _ref: erlang.Reference,
  _frame: Frame,
  _coefficients: Coeff,
) -> Frame {
  panic as "NIF biquad_next/3 not loaded"
}

fn get_a(db_gain: Float) -> Result(Float, String) {
  elementary.power(10.0, db_gain /. 40.0)
}

fn get_w0(rate: Float, freq: Float) -> #(Float, Float, Float) {
  let w0 = elementary.tau() *. { freq /. rate }
  let cos_w0 = elementary.cos(w0)
  let sin_w0 = elementary.sin(w0)
  #(w0, cos_w0, sin_w0)
}

pub type Alpha {
  Q(Float)
  BandWidth(Float)
  Slope(Float)
}

fn get_alpha(
  atype: Alpha,
  w0: Float,
  sin_w0: Float,
  a: Float,
) -> Result(Float, String) {
  case atype {
    Q(par) -> Ok(sin_w0 /. { 2.0 *. par })
    BandWidth(par) -> {
      use a1 <- result.try(elementary.natural_logarithm(2.0))
      let b = elementary.sinh(a1 /. 2.0 *. par *. { w0 /. sin_w0 })
      Ok(sin_w0 *. b)
    }
    Slope(par) -> {
      use a1 <- result.map(elementary.square_root(
        { a +. 1.0 /. a } *. { 1.0 /. par -. 1.0 } +. 2.0,
      ))
      sin_w0 /. 2.0 *. a1
    }
  }
}

fn get_frate() -> Float {
  int.to_float(gleuterpea.rate())
}

pub fn lowpass_default(freq: Float) -> BiQuad {
  lowpass(freq, 1.0)
}

pub fn lowpass(freq: Float, q: Float) -> BiQuad {
  let #(w0, cos_w0, sin_w0) = get_w0(get_frate(), freq)
  let assert Ok(alpha) = get_alpha(Q(q), w0, sin_w0, 0.0)
  let a0 = 1.0 +. alpha
  let a1 = -2.0 *. cos_w0
  let a2 = 1.0 -. alpha
  let b1 = 1.0 -. cos_w0
  let b0 = b1 /. 2.0
  let b2 = b0
  BiQuad(biquad_ctor(), Coeff(a0, a1, a2, b0, b1, b2))
}

pub fn highpass_default(freq: Float) {
  highpass(freq, 1.0)
}

pub fn highpass(freq: Float, q: Float) -> BiQuad {
  let #(w0, cos_w0, sin_w0) = get_w0(get_frate(), freq)
  let assert Ok(alpha) = get_alpha(Q(q), w0, sin_w0, 0.0)
  let one_plus_cos_w0 = 1.0 +. cos_w0
  let a0 = 1.0 +. alpha
  let a1 = -2.0 *. cos_w0
  let a2 = 1.0 -. alpha
  let b0 = one_plus_cos_w0 /. 2.0
  let b1 = 0.0 -. one_plus_cos_w0
  let b2 = b0
  BiQuad(biquad_ctor(), Coeff(a0, a1, a2, b0, b1, b2))
}

pub fn bandpass_skirt_default(freq: Float) -> BiQuad {
  bandpass_skirt(freq, Q(1.0))
}

pub fn bandpass_skirt(freq: Float, q_or_bw: Alpha) -> BiQuad {
  let #(w0, cos_w0, sin_w0) = get_w0(get_frate(), freq)
  let assert Ok(alpha) = get_alpha(q_or_bw, w0, sin_w0, 0.0)
  let half_sin_w0 = sin_w0 /. 2.0
  let a0 = 1.0 +. alpha
  let a1 = -2.0 *. cos_w0
  let a2 = 1.0 -. alpha
  let b0 = half_sin_w0
  let b1 = 0.0
  let b2 = 0.0 -. half_sin_w0
  BiQuad(biquad_ctor(), Coeff(a0, a1, a2, b0, b1, b2))
}

pub fn bandpass_peak_default(freq: Float) -> BiQuad {
  bandpass_peak(freq, Q(1.0))
}

pub fn bandpass_peak(freq: Float, q_or_bw: Alpha) -> BiQuad {
  let #(w0, cos_w0, sin_w0) = get_w0(get_frate(), freq)
  let assert Ok(alpha) = get_alpha(q_or_bw, w0, sin_w0, 0.0)

  let a0 = 1.0 +. alpha
  let a1 = -2.0 *. cos_w0
  let a2 = 1.0 -. alpha
  let b0 = alpha
  let b1 = 0.0
  let b2 = 0.0 -. alpha
  BiQuad(biquad_ctor(), Coeff(a0, a1, a2, b0, b1, b2))
}

pub fn notch_default(freq: Float) -> BiQuad {
  notch(freq, Q(1.0))
}

pub fn notch(freq: Float, q_or_bw: Alpha) -> BiQuad {
  let #(w0, cos_w0, sin_w0) = get_w0(get_frate(), freq)
  let assert Ok(alpha) = get_alpha(q_or_bw, w0, sin_w0, 0.0)
  let a0 = 1.0 +. alpha
  let a1 = -2.0 *. cos_w0
  let b1 = a1
  let a2 = 1.0 -. alpha
  let b0 = 1.0
  let b2 = 1.0
  BiQuad(biquad_ctor(), Coeff(a0, a1, a2, b0, b1, b2))
}

pub fn allpass_default(freq: Float) -> BiQuad {
  allpass(freq, 1.0)
}

pub fn allpass(freq: Float, q: Float) -> BiQuad {
  let #(w0, cos_w0, sin_w0) = get_w0(get_frate(), freq)
  let assert Ok(alpha) = get_alpha(Q(q), w0, sin_w0, 0.0)
  let a0 = 1.0 +. alpha
  let b2 = a0
  let a1 = -2.0 *. cos_w0
  let b1 = a1
  let a2 = 1.0 -. alpha
  let b0 = a2
  BiQuad(biquad_ctor(), Coeff(a0, a1, a2, b0, b1, b2))
}

pub fn peaking_eq_default(freq: Float, db_gain: Float) -> BiQuad {
  peaking_eq(freq, db_gain, Q(1.0))
}

pub fn peaking_eq(freq: Float, db_gain: Float, q_or_bw: Alpha) -> BiQuad {
  let assert Ok(a) = get_a(db_gain)
  let #(w0, cos_w0, sin_w0) = get_w0(get_frate(), freq)
  let assert Ok(alpha) = get_alpha(q_or_bw, w0, sin_w0, 0.0)
  let alpha_on_a = alpha /. a
  let a_times_alpha = alpha *. a
  let a0 = 1.0 +. alpha_on_a
  let a1 = -2.0 *. cos_w0
  let b1 = a1
  let a2 = 1.0 -. alpha_on_a
  let b0 = 1.0 +. a_times_alpha
  let b2 = 1.0 +. a_times_alpha
  BiQuad(biquad_ctor(), Coeff(a0, a1, a2, b0, b1, b2))
}

pub fn lowshelf_default(freq: Float, db_gain: Float) -> BiQuad {
  lowshelf(freq, db_gain, Q(1.0))
}

pub fn lowshelf(freq: Float, db_gain: Float, q_or_slope: Alpha) -> BiQuad {
  let assert Ok(a) = get_a(db_gain)
  let #(w0, cos_w0, sin_w0) = get_w0(get_frate(), freq)
  let assert Ok(alpha) = get_alpha(q_or_slope, w0, sin_w0, a)
  let ap1 = a +. 1.0
  let am1 = a -. 1.0
  let ap1_cos_w0 = ap1 *. cos_w0
  let am1_cos_w0 = am1 *. cos_w0
  let assert Ok(sqrt_a) = elementary.square_root(a)
  let beta = 2.0 *. sqrt_a *. alpha
  let a0 = ap1 +. am1_cos_w0 +. beta
  let a1 = -2.0 *. { am1 +. ap1_cos_w0 }
  let a2 = ap1 +. am1_cos_w0 -. beta
  let b0 = a *. { ap1 -. am1_cos_w0 +. beta }
  let b1 = 2.0 *. a *. { am1 -. ap1_cos_w0 }
  let b2 = a *. { ap1 -. am1_cos_w0 -. beta }
  BiQuad(biquad_ctor(), Coeff(a0, a1, a2, b0, b1, b2))
}

pub fn highshelf_default(freq: Float, db_gain: Float) -> BiQuad {
  lowshelf(freq, db_gain, Q(1.0))
}

pub fn highshelf(freq: Float, db_gain: Float, q_or_slope: Alpha) -> BiQuad {
  let assert Ok(a) = get_a(db_gain)
  let #(w0, cos_w0, sin_w0) = get_w0(get_frate(), freq)
  let assert Ok(alpha) = get_alpha(q_or_slope, w0, sin_w0, a)
  let ap1 = a +. 1.0
  let am1 = a -. 1.0
  let ap1_cos_w0 = ap1 *. cos_w0
  let am1_cos_w0 = am1 *. cos_w0
  let assert Ok(sqrt_a) = elementary.square_root(a)
  let beta = 2.0 *. sqrt_a *. alpha
  let a0 = ap1 -. am1_cos_w0 +. beta
  let a1 = 2.0 *. { am1 -. ap1_cos_w0 }
  let a2 = ap1 -. am1_cos_w0 -. beta
  let b0 = a *. { ap1 +. am1_cos_w0 +. beta }
  let b1 = -2.0 *. a *. { am1 +. ap1_cos_w0 }
  let b2 = a *. { ap1 +. am1_cos_w0 -. beta }
  BiQuad(biquad_ctor(), Coeff(a0, a1, a2, b0, b1, b2))
}

fn next(bq: BiQuad, f: Frame) -> Frame {
  let BiQuad(ref: ref, coefficients: c) = bq
  biquad_next(ref, f, c)
}

pub fn stream(yf: Yielder(Frame), bq: BiQuad) -> Yielder(Frame) {
  yielder.map(yf, fn(f) { next(bq, f) })
}
