import gleam/result
import gleeunit
import gleeunit/should
import gleuterpea/envelope
import gleuterpea/filter/biquad
import gleuterpea/filter/bitcrusher
import gleuterpea/filter/moog
import gleuterpea/generator/lfo
import gleuterpea/generator/noise
import gleuterpea/generator/osc
import gleuterpea/math
import gleuterpea/stream
import gleuterpea/util

pub fn main() {
  gleeunit.main()
}

fn load_nifs() {
  use _ <- result.try(math.load_nif())
  use _ <- result.try(biquad.load_nif())
  use _ <- result.try(bitcrusher.load_nif())
  use _ <- result.try(moog.load_nif())
  use _ <- result.try(noise.load_nif())
  osc.load_nif()
}

// gleeunit test functions end in `_test`
pub fn load_nifs_test() {
  let assert Ok(Nil) = load_nifs()
}

pub fn sinus_test() {
  // Varying between 420 <-> 460 Hz at 4 Hz
  let fm = lfo.triangle(4.0) |> lfo.nma(40.0, 420.0)
  // You can have a stream as modulating frequency input for osc
  osc.sin(stream.Varying(fm))
  // Eunit (and gleeunit) default timeout is 5 s, stay under
  |> util.dur(4.0)
  |> util.pan(0.0)
  |> stream.play()
}

// pub fn saw_test() {
//   let fm = lfo.triangle(4.0) |> lfo.nma(40.0, 420.0)
//   // You can have a stream as modulating frequency input for osc
//   osc.saw(osc.Varying(fm))
//   // Eunit (and gleeunit) default timeout is 5 s, stay under
//   |> util.dur(4.0)
//   |> util.pan(0.0)
//   |> stream.play()
// }

pub fn noise_test() {
  noise.pink()
  // Eunit (and gleeunit) default timeout is 5 s, stay under
  |> util.dur(4.0)
  |> util.pan(0.0)
  |> stream.play()
}

pub fn lowpass_test() {
  let fm = lfo.triangle(0.25) |> lfo.nma(400.0, 120.0)
  osc.sin(stream.Varying(fm))
  |> biquad.stream(biquad.lowpass(320.0, 2.0))
  // |> LPF.ns(420.0)
  |> util.dur(4.0)
  |> util.pan(0.0)
  |> stream.play()
}
