////  Analog Echo plugin
////   The sc_analog_echo module and sc_analog_echo.c file combined are translated from the SC [AnalogEcho](https://github.com/supercollider/example-plugins/blob/master/03-AnalogEcho/AnalogEcho.cpp) example as a comparison.
////   Properties that can be set for AnalogEcho.
////
////   Available options are:
////     * `:maxdelay` - max size of delay buffer in seconds. Default 0.3.
////     * `:delay` - delay for echo in seconds. Default 0.3.
////     * `:fb` - feedback coefficient. Default 0.9.
////     * `:coeff` - filter coefficient. Default 0.95.

import gleam
import gleam/erlang
import gleam/yielder
import gleuterpea.{type Frame}
import gleuterpea/nifs.{type ErlangResult, type NifLoadError}
import gleuterpea/stream.{type FrameStream}

pub type AnalogEcho {
  AnalogEcho(ref: erlang.Reference, delay: Float, fb: Float, coeff: Float)
}

pub fn load_nif() -> Result(Nil, #(NifLoadError, String)) {
  case load_nif_internal("priv/sc_analog_echo", 0) {
    nifs.Ok -> gleam.Ok(Nil)
    nifs.Error(#(nifs.Reload, _)) -> gleam.Ok(Nil)
    nifs.Error(err) -> gleam.Error(err)
  }
}

@external(erlang, "erlang", "load_nif")
fn load_nif_internal(nif: String, load_info: Int) -> ErlangResult

fn analog_echo_ctor(
  _rate: Int,
  _period_size: Int,
  _maxdelay: Float,
) -> erlang.Reference {
  panic as "NIF analog_echo_ctor/3 not loaded"
}

fn analog_echo_next(
  _ref: erlang.Reference,
  _frames: Frame,
  _delay: Float,
  _fb: Float,
  _coeff: Float,
) -> Frame {
  panic as "NIF analog_echo_next/5 not loaded"
}

/// maxdelay - set default 0.3
pub fn new0() -> AnalogEcho {
  new(0.3)
}

pub fn new(maxdelay: Float) -> AnalogEcho {
  let rate = gleuterpea.rate()
  let period_size = gleuterpea.period_size()
  AnalogEcho(
    analog_echo_ctor(rate, period_size, maxdelay),
    delay: maxdelay,
    fb: 0.9,
    coeff: 0.95,
  )
}

/// ns - New Stream
pub fn ns0(yf: FrameStream) -> FrameStream {
  stream(yf, new0())
}

pub fn ns(yf: FrameStream, maxdelay: Float) -> FrameStream {
  stream(yf, new(maxdelay))
}

//   @spec next(t(), frames :: binary()) :: binary()
pub fn next(f: Frame, ae: AnalogEcho) -> Frame {
  let AnalogEcho(ref, delay, fb, coeff) = ae
  analog_echo_next(ref, f, delay, fb, coeff)
}

pub fn stream(yf: FrameStream, ae: AnalogEcho) -> FrameStream {
  // When upstream halted - emit echo for an extra 500 * 6 ms ~ 3 s
  let yf2 =
    yielder.unfold(500, fn(x) {
      case x {
        0 -> yielder.Done
        x -> yielder.Next(<<>>, x - 1)
      }
    })
  yielder.append(yf, yf2) |> yielder.map(fn(f) { next(f, ae) })
}
