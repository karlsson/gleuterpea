////   Granulix envelopes - functions for creating envelopes and multiplying streams with them.
////   Here is a simple example of use with own multiplier (envelope value * 0.4):
////   ```gleam
////
////   fm = lfo.sin(4) |> yielder.map(fn(x) {x *. 10 +. 320})
////
////   sc_plugin.stream(Oscillator.triangle(fm))
////   |> envelope.sin_tuple(2.0)
////   |> yielder.map(fn(frames, mul) { ma.mul(frames, mul *. 0.4)})
////   ```
////   Using the sin/3 instead and excluding the Stream.map/2 function is the
////   same as just using mul * 1.0.

import euterpea/music.{type Dur}
import gleam/erlang/process
import gleam/float
import gleam/int
import gleam/option.{type Option}
import gleam/yielder.{type Yielder}
import gleam_community/maths/elementary as ma
import gleuterpea.{type Frame}
import gleuterpea/math

///  Uses a sine shaped mirrored S-form envelope to limit the frame array.
///  The duration argument is in seconds.
pub fn sin(yf: Yielder(Frame), d: Dur) -> Yielder(Frame) {
  sin_tuple(yf, d)
  |> yielder.map(fn(x) {
    let #(f, mul) = x
    math.mul(f, mul)
  })
}

///  Same as `sin` function but do not touch the frames. Instead
///  it returns the frames and the envelope value in a tuple.
pub fn sin_tuple(yf: Yielder(Frame), d: Dur) -> Yielder(#(Frame, Float)) {
  let no_of_frames = d *. int.to_float(gleuterpea.rate())
  let twopi_by_nof = ma.pi() *. 2.0 /. no_of_frames
  let ps = int.to_float(gleuterpea.period_size())
  yielder.transform(yf, 0.0, fn(progress: Dur, frame: Frame) {
    case progress <. no_of_frames {
      True -> {
        let x = ma.cos(progress *. twopi_by_nof) *. -0.5 +. 0.5
        yielder.Next(#(frame, x), progress +. ps)
      }
      False -> yielder.Done
    }
  })
}

///  Uses a line shaped envelope starting from 1 end ending with 0 to
/// limit the frame array. The duration argument is in seconds.
pub fn saw(yf: Yielder(Frame), d: Dur) -> Yielder(Frame) {
  saw_tuple(yf, d)
  |> yielder.map(fn(x) {
    let #(f, mul) = x
    math.mul(f, mul)
  })
}

/// Same as saw function but do not touch the frames. Instead it returns
/// the frames and the envelope value in a tuple.
pub fn saw_tuple(yf: Yielder(Frame), d: Dur) -> Yielder(#(Frame, Float)) {
  line_tuple(yf, d, 1.0, 0.0)
}

/// Line envelope starting at startv and ending at endv
pub fn line(
  yf: Yielder(Frame),
  d: Dur,
  startv: Float,
  endv: Float,
) -> Yielder(Frame) {
  line_tuple(yf, d, startv, endv)
  |> yielder.map(fn(x) {
    let #(f, mul) = x
    math.mul(f, mul)
  })
}

/// Same as line function but do not touch the frames. Instead it returns
/// the frames and the envelope value in a tuple.
pub fn line_tuple(
  yf: Yielder(Frame),
  d: Dur,
  startv: Float,
  endv: Float,
) -> Yielder(#(Frame, Float)) {
  let no_of_frames = float.round(d *. int.to_float(gleuterpea.rate()))
  let ps = gleuterpea.period_size()
  yielder.transform(yf, 0, fn(progress: Int, f) {
    case progress < no_of_frames {
      True -> {
        let x = linev(startv, endv, progress, 0, no_of_frames)
        yielder.Next(#(f, x), progress + ps)
      }
      False -> yielder.Done
    }
  })
}

fn linev(startv: Float, endv: Float, x: Int, startx: Int, endx: Int) {
  case startx != endx {
    True ->
      startv
      +. { endv -. startv }
      *. int.to_float(x - startx)
      /. int.to_float(endx - startx)
    False -> endv
  }
}

/// Attack, Decay, Sustain, Release
pub type ADSR {
  ADSR(
    attack: Float,
    attack_level: Float,
    decay: Float,
    decay_level: Option(Float),
    sustain: Float,
    sustain_level: Float,
    release: Float,
  )
}

pub fn default_adsr() {
  ADSR(0.0, 1.0, 0.0, option.None, 0.0, 1.0, 1.0)
}

pub type NoteOff {
  NoteOff
}

// pub fn adsr(yf: Yielder(Frame), adsr: ADSR, noff_subject) -> Yielder(Frame) {
//   adsr_tuple(yf, adsr, noff_subject)
//   |> yielder.map(fn(x) -> Frame {
//     let #(f, mul) = x
//     math.mul(f, mul)
//   })
// }

pub fn adsr(
  yf: Yielder(Frame),
  a: ADSR,
  noff_subject: process.Subject(NoteOff),
) -> Yielder(Frame) {
  let rate = gleuterpea.rate()
  let period_size = gleuterpea.period_size()
  let dl = case a.decay_level {
    option.None -> a.sustain_level
    option.Some(l0) -> l0
  }

  let fr = int.to_float(rate)
  let d_start = float.round(a.attack *. fr)
  let s_start = float.round(a.decay *. fr) + d_start
  let r_start = float.round(a.sustain *. fr) + s_start
  let r_end = float.round(a.release *. fr) + r_start

  yielder.transform(yf, #(0, a.sustain_level), fn(p0l, frames) {
    let #(p0, level) = p0l
    let #(progress, rl) = case p0 < r_start {
      True -> {
        // If one receives NoteOff, move to release phase and start
        // from current level
        case process.receive(noff_subject, 0) {
          Ok(NoteOff) -> #(r_start, level)
          // Timeout
          Error(Nil) -> #(p0, level)
        }
      }
      False -> #(p0, level)
    }

    let y0 = case progress {
      _ if progress < d_start ->
        linev(0.0, a.attack_level, progress, 0, d_start)
      _ if progress < s_start ->
        linev(a.attack_level, dl, progress, d_start, s_start)
      _ if progress < r_start ->
        linev(dl, a.sustain_level, progress, s_start, r_start)
      _ if progress < r_end -> linev(rl, 0.0, progress, r_start, r_end)
      _ -> 0.0
    }
    let y = float.min(y0, 1.0)

    let newlevel = case progress < r_start {
      True -> y
      False -> level
    }

    case progress < r_end {
      True ->
        yielder.Next(math.mul(frames, y), #(progress + period_size, newlevel))
      False -> yielder.Done
    }
  })
}
