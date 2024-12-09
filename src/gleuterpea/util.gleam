import gleam
import gleam/bit_array
import gleam/erlang/process.{type Subject}
import gleam/float
import gleam/int
import gleam/list
import gleam/yielder.{type Yielder}
import gleuterpea.{type Frame}
import gleuterpea/math
import gleuterpea/stream.{type FrameStream, type FramesStream}

/// Sum a stream of list of frames into one
pub fn mix(fs: FramesStream) -> FrameStream {
  yielder.map(fs, mixframes)
}

fn mixframes(l: List(Frame)) -> Frame {
  list.fold(l, <<>>, fn(x, acc) { math.add2(x, acc) })
}

///  Make two channels muliplied with pos and 1.0 - pos respectively.
///  pos shall be between 0.0 and 1.0. The returned stream holds a list
///  of two frame arrays.
pub fn pan(fs: FrameStream, pos: Float) -> FramesStream {
  let pos = float.min(1.0, float.max(0.0, pos))
  yielder.map(fs, fn(frame: Frame) -> List(Frame) { panframe(frame, pos) })
}

/// Pan when pos is varying 
pub fn pan2(fs: FrameStream, pos: Yielder(Float)) -> FramesStream {
  yielder.map2(fs, pos, fn(frame, panf) { panframe(frame, panf) })
}

//  Make two channels muliplied with pos and 1.0 - pos respectively.
//  pos shall be between 0.0 and 1.0.
fn panframe(x: Frame, pos: Float) -> List(Frame) {
  let posn = 0.5 *. pos +. 0.5
  let r = math.mul(x, posn)
  let l = math.mul(x, 1.0 -. posn)
  [r, l]
}

///    This function is the one that will halt the stream. It shall be included
///    in your pipeline unless you have some other means of stopping it.
///    The time argument is in seconds.
pub fn dur(enum: FrameStream, time: Float) -> FrameStream {
  // let ctx = ctx.get()
  let no_of_frames = float.round(time *. int.to_float(gleuterpea.rate()))
  yielder.transform(enum, no_of_frames, fn(acc, frame) {
    case acc > 0 {
      True -> yielder.Next(frame, acc - bit_array.byte_size(frame) / 4)
      False -> yielder.Done
    }
  })
}

/// A yielder that produces the same value every time
pub fn value(value: Float) -> Yielder(Float) {
  yielder.unfold(from: value, with: fn(x) {
    yielder.Next(element: x, accumulator: x)
  })
}

/// Use to set a new value to "setter" stream via message passing.
pub type Setter {
  Setter(x: Float)
  Halt
}

/// A yielder with a value that may be set or halt the producer using message
/// passing.
/// The subject should be created by the process owning the setter yield.
pub fn setter(set_subject: Subject(Setter), value: Float) -> Yielder(Float) {
  yielder.unfold(from: value, with: fn(x) {
    let y = process.receive(set_subject, 0)
    case y {
      Ok(z) -> {
        case z {
          Halt -> yielder.Done
          Setter(new_value) ->
            yielder.Next(element: new_value, accumulator: new_value)
        }
      }
      // This is the timeout, just stream out the old value
      Error(gleam.Nil) -> yielder.Next(x, x)
    }
  })
}

/// Set a new value (or halt stream) on a setter yielder.
pub fn set(subject: Subject(Setter), value_or_halt: Setter) {
  process.send(subject, value_or_halt)
}
