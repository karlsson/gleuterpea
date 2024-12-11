import gleam
import gleam/erlang
import gleam/yielder.{type Yielder}
import gleuterpea.{type Frame}
import gleuterpea/nifs.{type ErlangResult, type NifLoadError}
import gleuterpea/stream.{type FrameStream, type FramesStream}

pub fn load_nif() -> Result(Nil, #(NifLoadError, String)) {
  case load_nif_internal("priv/sc_reverb", 0) {
    nifs.Ok -> gleam.Ok(Nil)
    nifs.Error(#(nifs.Reload, _)) -> gleam.Ok(Nil)
    nifs.Error(err) -> gleam.Error(err)
  }
}

@external(erlang, "erlang", "load_nif")
fn load_nif_internal(nif: String, load_info: Int) -> ErlangResult

/// One or two channel Reverb streams.
pub type FreeVerbType {
  Freeverb
  Freeverb2
  Gverb
}

pub type Par =
  stream.VarOrFixed(Float)

fn reverb_ctor(
  _rate: Int,
  _period_size: Int,
  _type: FreeVerbType,
) -> erlang.Reference {
  panic as "NIF ramp_ctor/3 not loaded"
}

fn reverb_next(
  _ref: erlang.Reference,
  _fs: List(Frame),
  _mix: Float,
  _room: Float,
  _damp: Float,
) -> List(Frame) {
  panic as "NIF ramp_next/5 not loaded"
}

///    Properties that can be set for FreeVerb.
///
///    Available options are:
///    * `:mix` - dry/wet balance. range 0..1.
///    * `:room` - room size. range 0..1.
///    * `:damp` - Reverb HF damp. range 0..1.
///    Default values:
///    mix: 0.33, room: 0.5, damp: 0.5
pub type Reverb {
  Reverb(ref: erlang.Reference, mix: Par, room: Par, damp: Par)
}

/// "Create one channel FreeVerb filter"
pub fn new0() {
  new(stream.Fixed(0.33), stream.Fixed(0.5), stream.Fixed(0.5))
}

pub fn new(mix: Par, room: Par, damp: Par) -> Reverb {
  let rate = gleuterpea.rate()
  let period_size = gleuterpea.period_size()
  Reverb(ref: reverb_ctor(rate, period_size, Freeverb), mix:, room:, damp:)
}

/// "Create two channel FreeVerb filter (FreeVerb2)"
pub fn new02() {
  new2(stream.Fixed(0.33), stream.Fixed(0.5), stream.Fixed(0.5))
}

pub fn new2(mix: Par, room: Par, damp: Par) -> Reverb {
  let rate = gleuterpea.rate()
  let period_size = gleuterpea.period_size()
  Reverb(ref: reverb_ctor(rate, period_size, Freeverb2), mix:, room:, damp:)
}

pub fn next(
  fs: List(Frame),
  ref: erlang.Reference,
  mix: Float,
  room: Float,
  damp: Float,
) {
  reverb_next(ref, fs, mix, room, damp)
}

fn stream(yf: FrameStream, rv: Reverb) -> FrameStream {
  let Reverb(ref, _mix, _room, _damp) = rv
  let zipped = yielder.zip(yielder_pars(rv), yf)

  yielder.map(zipped, fn(z) {
    let #(#(#(mf, rf), df), fs) = z
    let assert [f] = reverb_next(ref, [fs], mf, rf, df)
    f
  })
}

// @spec stream(freeverb :: t(), enum :: Enumerable.t()) :: Enumerable.t()
fn stream2(yfs: FramesStream, rv: Reverb) -> FramesStream {
  let Reverb(ref, _mix, _room, _damp) = rv
  let zipped = yielder.zip(yielder_pars(rv), yfs)

  yielder.map(zipped, fn(z) {
    let #(#(#(mf, rf), df), fs) = z
    reverb_next(ref, fs, mf, rf, df)
  })
}

fn yielder_pars(rv: Reverb) -> Yielder(#(#(Float, Float), Float)) {
  let Reverb(_ref, mix, room, damp) = rv
  let m = case mix {
    stream.Fixed(f) -> yielder.repeat(f)
    stream.Varying(v) -> v
  }
  let r = case room {
    stream.Fixed(f) -> yielder.repeat(f)
    stream.Varying(v) -> v
  }
  let d = case damp {
    stream.Fixed(f) -> yielder.repeat(f)
    stream.Varying(v) -> v
  }
  yielder.zip(m, r) |> yielder.zip(d)
}

/// Create one channel FreeVerb filter stream with default values
pub fn freeverb0(f: FrameStream) -> FrameStream {
  stream(f, new0())
}

/// Create one channel FreeVerb filter stream
pub fn freeverb(f: FrameStream, mix: Par, room: Par, damp: Par) -> FrameStream {
  stream(f, new(mix, room, damp))
}

/// Create two channel FreeVerb filter stream with default values
pub fn freeverb02(f: FramesStream) -> FramesStream {
  stream2(f, new02())
}

/// Create two channel FreeVerb filter stream
pub fn freeverb2(
  f: FramesStream,
  mix: Par,
  room: Par,
  damp: Par,
) -> FramesStream {
  stream2(f, new2(mix, room, damp))
}
