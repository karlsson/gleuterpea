////   Quantizer / Decimator with smooth control.
////   bits parameter must be between 1.0 and 16.0
////   normalized_frequency (frequency / rate) must be between 0 and 1
////   This module is from the [Synthex](https://github.com/bitgamma/synthex) application
////   but rewritten to use NIFs. (`c_src/granulix_bitcrusher.c`)

import gleam
import gleam/erlang
import gleam/float
import gleam/yielder.{type Yielder}
import gleuterpea.{type Frame}
import gleuterpea/nifs.{type ErlangResult, type NifLoadError}

pub type BitCrusher {
  BitCrusher(ref: erlang.Reference, bits: Float, normalized_frequency: Float)
}

pub fn load_nif() -> Result(Nil, #(NifLoadError, String)) {
  case load_nif_internal("priv/gleuterpea_bitcrusher", 0) {
    nifs.Ok -> gleam.Ok(Nil)
    nifs.Error(#(nifs.Reload, _)) -> gleam.Ok(Nil)
    nifs.Error(err) -> gleam.Error(err)
  }
}

@external(erlang, "erlang", "load_nif")
fn load_nif_internal(nif: String, load_info: Int) -> ErlangResult

// Helper NIF to be called when creating an Oscillator
fn bitcrusher_ctor() -> erlang.Reference {
  panic as "NIF bitcrusher_ctor/0 not loaded"
}

// Helper NIF to be called when getting the next Frame
fn bitcrusher_next(
  _ref: erlang.Reference,
  _frame: Frame,
  _bits: Float,
  _normalized_frequency: Float,
) -> Frame {
  panic as "NIF bitcrusher_next/4 not loaded"
}

pub fn new(bits: Float, normalized_frequency: Float) -> BitCrusher {
  let bits = float.min(16.0, float.max(1.0, bits))
  let normalized_frequency =
    float.min(1.0, float.max(0.0, normalized_frequency))
  BitCrusher(
    ref: bitcrusher_ctor(),
    bits: bits,
    normalized_frequency: normalized_frequency,
  )
}

pub fn next(f: Frame, bc: BitCrusher) -> Frame {
  bitcrusher_next(bc.ref, f, bc.bits, bc.normalized_frequency)
}

pub fn stream(yf: Yielder(Frame), bc: BitCrusher) {
  yielder.map(yf, fn(f) { next(f, bc) })
}
