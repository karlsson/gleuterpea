import gleam
import gleam/erlang
import gleam/float
import gleam/yielder.{type Yielder}
import gleuterpea.{type Frame}
import gleuterpea/nifs.{type ErlangResult, type NifLoadError}
import gleuterpea/stream.{type FrameStream, type VarOrFixed}

pub fn load_nif() -> Result(Nil, #(NifLoadError, String)) {
  case load_nif_internal("priv/sc_filter", 0) {
    nifs.Ok -> gleam.Ok(Nil)
    nifs.Error(#(nifs.Reload, _)) -> gleam.Ok(Nil)
    nifs.Error(err) -> gleam.Error(err)
  }
}

@external(erlang, "erlang", "load_nif")
fn load_nif_internal(nif: String, load_info: Int) -> ErlangResult

//   defstruct [:ref, cutoff: 1, resonance: 0]

fn ramp_ctor_nif(_rate: Int, _period_size: Int) {
  panic as "NIF ramp_ctor/2 not loaded"
}

fn ramp_next_nif(_ref: erlang.Reference, _frames: Frame, _lagtime: Float) {
  panic as "NIF ramp_next/3 not loaded"
}

fn lag_ctor_nif(_rate: Int, _period_size: Int) {
  panic as "NIF lag_ctor/2 not loaded"
}

fn lag_next_nif(_ref: erlang.Reference, _frames: Frame, _lagtime: Float) {
  panic as "NIF lag_next/3 not loaded"
}

fn lagud_ctor_nif(_rate: Int, _period_size: Int) {
  panic as "NIF lagud_ctor/2 not loaded"
}

fn lagud_next_nif(
  _ref: erlang.Reference,
  _frames: Frame,
  _lagup: Float,
  _lagdown: Float,
) {
  panic as "NIF lagud_next/4 not loaded"
}

type PassFilter {
  Lpf
  Hpf
  Bpf
  Brf
}

fn pf_ctor_nif(_rate: Int, _period_size: Int, _type: PassFilter) {
  panic as "NIF lpf_ctor/3 not loaded"
}

fn lhpf_next_nif(_ref: erlang.Reference, _frames: Frame, _freq: Float) {
  panic as "NIF lpf_next/3 not loaded"
}

fn bprf_next_nif(
  _ref: erlang.Reference,
  _frames: Frame,
  _freq: Float,
  _bw: Float,
) {
  panic as "NIF bprf_next/4 not loaded"
}

// -----
pub type LagTime =
  VarOrFixed(Float)

pub type FilterType {
  Ramp(ref: erlang.Reference, lagtime: LagTime)
  Lag(ref: erlang.Reference, lagtime: LagTime)
  LagUD(ref: erlang.Reference, lagtime_u: LagTime, lagtime_d: LagTime)
}

/// Break a continuous signal into linearly interpolated segments
/// with specific durations.
//    defstruct [:ref, lagTime: 0.1]

pub fn ramp() {
  ramp_new(stream.Fixed(0.1))
}

pub fn ramp_new(lagtime: LagTime) -> FilterType {
  let rate = gleuterpea.rate()
  let period_size = gleuterpea.period_size()
  Ramp(ramp_ctor_nif(rate, period_size), lagtime)
}

pub fn ramp_next(f: Frame, ref: erlang.Reference, lagtime: Float) -> Frame {
  ramp_next_nif(ref, f, lagtime)
}

fn ramp_stream(yf: FrameStream, ref: erlang.Reference, lagtime: LagTime) {
  case lagtime {
    stream.Fixed(l) -> yielder.map(yf, fn(f) { ramp_next(f, ref, l) })
    stream.Varying(yl) ->
      yielder.map2(yf, yl, fn(f, l) { ramp_next(f, ref, l) })
  }
}

// --------------------

pub fn lag() {
  lag_new(stream.Fixed(0.1))
}

pub fn lag_new(lagtime: LagTime) -> FilterType {
  let rate = gleuterpea.rate()
  let period_size = gleuterpea.period_size()
  Lag(lag_ctor_nif(rate, period_size), lagtime)
}

pub fn lag_next(f: Frame, ref: erlang.Reference, lagtime: Float) -> Frame {
  lag_next_nif(ref, f, lagtime)
}

fn lag_stream(yf: FrameStream, ref: erlang.Reference, lagtime: LagTime) {
  case lagtime {
    stream.Fixed(l) -> yielder.map(yf, fn(f) { ramp_next(f, ref, l) })
    stream.Varying(yl) -> yielder.map2(yf, yl, fn(f, l) { lag_next(f, ref, l) })
  }
}

// --------------------
pub fn lagud() {
  lagud_new(stream.Fixed(0.1), stream.Fixed(0.1))
}

pub fn lagud_new(lagtime_u: LagTime, lagtime_d: LagTime) -> FilterType {
  let rate = gleuterpea.rate()
  let period_size = gleuterpea.period_size()
  LagUD(ref: lagud_ctor_nif(rate, period_size), lagtime_u:, lagtime_d:)
}

pub fn lagud_next(
  f: Frame,
  ref: erlang.Reference,
  lagtime_u: Float,
  lagtime_d: Float,
) -> Frame {
  lagud_next_nif(ref, f, lagtime_u, lagtime_d)
}

fn lagud_stream(
  yf: FrameStream,
  ref: erlang.Reference,
  lagtime_u: LagTime,
  lagtime_d: LagTime,
) -> FrameStream {
  let ltu = case lagtime_u {
    stream.Fixed(l) -> yielder.repeat(l)
    stream.Varying(yl) -> yl
  }
  let ltd = case lagtime_d {
    stream.Fixed(l) -> yielder.repeat(l)
    stream.Varying(yl) -> yl
  }

  yielder.map2(yf, yielder.zip(ltu, ltd), fn(f, ud) {
    let #(u, d) = ud
    lagud_next(f, ref, u, d)
  })
}

// --------------------

// --------------------
pub fn stream(yf: FrameStream, ft: FilterType) -> FrameStream {
  case ft {
    Ramp(ref, lagtime) -> ramp_stream(yf, ref, lagtime)
    Lag(ref, lagtime) -> lag_stream(yf, ref, lagtime)
    LagUD(ref, lagtime_u, lagtime_d) ->
      lagud_stream(yf, ref, lagtime_u, lagtime_d)
  }
}
