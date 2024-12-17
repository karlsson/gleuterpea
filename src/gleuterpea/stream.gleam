////  gleuterpea/stream - functions for creating and handling audio streams.
////  A stream (yielder) of 32-bit floats
//// Sends the stream to audio output. Returns the stream

import gleam/list
import gleam/yielder.{type Yielder}
import gleuterpea.{type Frame}
import gleuterpea/math

/// Contains an Audio Stream
pub type FrameStream =
  Yielder(Frame)

/// Contains a Stream of a list of Audio Frames, one for each channel
pub type FramesStream =
  Yielder(List(Frame))

pub type VarOrFixed(a) {
  Varying(Yielder(a))
  Fixed(a)
}

pub fn out(fs: FramesStream) -> FramesStream {
  yielder.transform(fs, True, fn(first, els) {
    case first {
      True -> gleuterpea.Ok
      False -> gleuterpea.wait_ready4more()
    }
    list.fold(els, 1, fn(channel_no, el) {
      gleuterpea.out(gleuterpea.FTP3(el, channel_no, channel_no == 1))
      channel_no + 1
    })
    yielder.Next(els, False)
  })
}

/// Play the yielded synthdefs 
pub fn play(fs: FramesStream) {
  out(fs) |> yielder.run()
}

/// Multiply and add
pub fn ma(yf: FrameStream, m: Float, a: Float) -> FrameStream {
  yielder.map(yf, fn(f) { math.add(math.mul(f, m), a) })
}

/// Multiply and add
/// Multiple channels -> list of binaries (of 32 bit floats)
pub fn ma2(yfs: FramesStream, m: Float, a: Float) -> FramesStream {
  yielder.map(yfs, fn(fs) {
    list.map(fs, fn(f) { math.add(math.mul(f, m), a) })
  })
}

/// Multiply and add
/// "Control rate", cr, single value updated every sample rate/period_size
pub fn macr(yv: Yielder(Float), m: Float, a: Float) -> Yielder(Float) {
  yielder.map(yv, fn(v) { v *. m +. a })
}
