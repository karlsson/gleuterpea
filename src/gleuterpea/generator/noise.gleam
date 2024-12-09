import gleam
import gleam/erlang
import gleam/yielder.{type Yielder}
import gleuterpea.{type Frame}
import gleuterpea/nifs.{type ErlangResult, type NifLoadError}

pub type NoiseType {
  White
  Pink
  Brown
}

pub type Noise {
  Noise(ref: erlang.Reference, period_size: Int)
}

pub fn load_nif() -> Result(Nil, #(NifLoadError, String)) {
  case load_nif_internal("priv/gleuterpea_noise", 0) {
    nifs.Ok -> gleam.Ok(Nil)
    nifs.Error(#(nifs.Reload, _)) -> gleam.Ok(Nil)
    nifs.Error(err) -> gleam.Error(err)
  }
}

@external(erlang, "erlang", "load_nif")
fn load_nif_internal(nif: String, load_info: Int) -> ErlangResult

// Helper NIF to be called when creating an Oscillator
fn noise_ctor(_type: NoiseType) -> erlang.Reference {
  panic as "NIF noise_ctor/1 not loaded"
}

// Helper NIF to be called when getting the next Frame
fn noise_next(_ref: erlang.Reference, _no_of_frames: Int) -> Frame {
  panic as "NIF noise_next/2 not loaded"
}

fn new(ntype: NoiseType) -> Noise {
  let ref = noise_ctor(ntype)
  let period_size = gleuterpea.period_size()
  Noise(ref, period_size)
}

pub fn stream(noise: Noise) -> Yielder(Frame) {
  yielder.repeatedly(fn() { noise_next(noise.ref, noise.period_size) })
}

pub fn white() -> Yielder(Frame) {
  stream(new(White))
}

pub fn pink() -> Yielder(Frame) {
  stream(new(Pink))
}

pub fn brown() -> Yielder(Frame) {
  stream(new(Brown))
}
