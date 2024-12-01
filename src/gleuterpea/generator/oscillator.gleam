import gleam
import gleam/erlang
import gleam/yielder.{type Yielder}
import gleuterpea.{type Frame, type Rate}
import gleuterpea/nifs.{type ErlangResult, type NifLoadError}

/// a varying stream (Yielder).
pub type Frequency {
  Varying(Yielder(Float))
  Fixed(Float)
}

pub type Oscillator {
  Oscillator(frequency: Frequency, ref: erlang.Reference)
}

pub type OscType {
  Sin
  Saw
  Triangle
}

pub fn load_nif() -> Result(Nil, #(NifLoadError, String)) {
  case load_nif_internal("priv/gleuterpea_osc", 0) {
    nifs.Ok -> gleam.Ok(Nil)
    nifs.Error(#(nifs.Reload, _)) -> gleam.Ok(Nil)
    nifs.Error(err) -> gleam.Error(err)
  }
}

@external(erlang, "erlang", "load_nif")
fn load_nif_internal(nif: String, load_info: Int) -> ErlangResult

// Helper NIF to be called when creating an Oscillator
fn osc_ctor(_rate: Rate, _type: OscType) -> erlang.Reference {
  panic as "NIF osc_ctor/2 not loaded"
}

// Helper NIF to be called when getting the next Frame
fn osc_next(_ref: erlang.Reference, _freq: Float, _no_of_frames: Int) -> Frame {
  panic as "NIF osc_next/3 not loaded"
}

// ----------------------------------------------

fn osc(frequency: Frequency, osctype: OscType) -> Oscillator {
  let rate = gleuterpea.rate()
  Oscillator(frequency: frequency, ref: osc_ctor(rate, osctype))
}

fn stream(osc: Oscillator) -> Yielder(Frame) {
  let no_of_frames = gleuterpea.period_size()
  case osc.frequency {
    Fixed(freq) -> {
      yielder.repeatedly(fn() { osc_next(osc.ref, freq, no_of_frames) })
    }
    Varying(yfreq) -> {
      yielder.map(yfreq, fn(freq) { osc_next(osc.ref, freq, no_of_frames) })
    }
  }
}

/// A sinus wave oscillator
pub fn sin(frequency: Frequency) {
  stream(osc(frequency, Sin))
}

/// A saw wave oscillator
pub fn saw(frequency: Frequency) {
  stream(osc(frequency, Saw))
}

/// A triangle wave oscillator
pub fn triangle(frequency: Frequency) {
  stream(osc(frequency, Triangle))
}
