////   Emulates the Moog VCF.
////   cutoff must be between 0 and 1
////   resonance must be between 0 and 4
////   This module is from the [Synthex](https://github.com/bitgamma/synthex) application
////   but rewritten to use NIFs. (`c_src/granulix_moog.c`)

import gleam
import gleam/erlang
import gleam/float
import gleam/yielder.{type Yielder}
import gleuterpea.{type Frame}
import gleuterpea/nifs.{type ErlangResult, type NifLoadError}

//   defstruct [:ref, cutoff: 1, resonance: 0]
pub opaque type Moog {
  Moog(ref: erlang.Reference, cutoff: Float, resonance: Float)
}

pub fn load_nif() -> Result(Nil, #(NifLoadError, String)) {
  case load_nif_internal("priv/gleuterpea_moog", 0) {
    nifs.Ok -> gleam.Ok(Nil)
    nifs.Error(#(nifs.Reload, _)) -> gleam.Ok(Nil)
    nifs.Error(err) -> gleam.Error(err)
  }
}

@external(erlang, "erlang", "load_nif")
fn load_nif_internal(nif: String, load_info: Int) -> ErlangResult

// Helper NIF to be called when creating
fn moog_ctor() -> erlang.Reference {
  panic as "NIF moog_ctor/0 not loaded"
}

// Helper NIF to be called when getting the next Frame
fn moog_next(
  _ref: erlang.Reference,
  _frame: Frame,
  _cutoff: Float,
  _resonance: Float,
) -> Frame {
  panic as "NIF moog_next/4 not loaded"
}

pub fn new(cutoff: Float, resonance: Float) -> Moog {
  let cutoff = float.min(1.0, float.max(0.0, cutoff))
  let resonance = float.min(4.0, float.max(0.0, resonance))
  Moog(ref: moog_ctor(), cutoff: cutoff, resonance: resonance)
}

//   def ns(enum, cutoff, resonance) do
//     stream(new(cutoff, resonance), enum)
//   end

fn next(f: Frame, m: Moog) -> Frame {
  moog_next(m.ref, f, m.cutoff, m.resonance)
}

pub fn stream(yf: Yielder(Frame), m: Moog) -> Yielder(Frame) {
  yielder.map(yf, fn(f) { next(f, m) })
}
