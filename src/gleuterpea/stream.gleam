////  gleuterpea/stream - functions for creating and handling audio streams.
////  A stream (yielder) of 32-bit floats
//// Sends the stream to audio output. Returns the stream

import gleam/list
import gleam/yielder.{type Yielder}
import gleuterpea.{type Frame}

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

pub fn play(fs: FramesStream) {
  out(fs) |> yielder.run()
}
