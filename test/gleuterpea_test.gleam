import euterpea/music
import gleam/int
import gleam/yielder
import gleeunit
import gleuterpea
import gleuterpea/envelope
import gleuterpea/filter/biquad
import gleuterpea/filter/bitcrusher
import gleuterpea/filter/moog
import gleuterpea/filter/sc_reverb
import gleuterpea/generator/lfo
import gleuterpea/generator/noise
import gleuterpea/generator/osc
import gleuterpea/math
import gleuterpea/stream
import gleuterpea/support
import gleuterpea/util

// import gleuterpea/filter/sc_analog_echo
// import gleuterpea/filter/sc_filter

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`

pub fn hello_test() {
  let a = 1
  a == 1
}

pub fn load_nifs_test() {
  let assert Ok(Nil) = support.load_nifs()
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

pub fn saw_test() {
  lfo.triangle(4.0)
  |> lfo.nma(40.0, 420.0)
  |> stream.Varying
  // You can have a stream as modulating frequency input for osc
  |> osc.saw()
  // Eunit (and gleeunit) default timeout is 5 s, stay under
  |> util.dur(4.0)
  |> util.pan(0.0)
  |> stream.play()
}

pub fn triangle_test() {
  // Between 300 and 320 at 4 Hz
  lfo.sin(4.0)
  |> lfo.nma(20.0, 300.0)
  |> stream.Varying
  |> osc.triangle()
  |> envelope.sin_tuple(2.0)
  |> yielder.map(fn(x: #(gleuterpea.Frame, Float)) { math.mul(x.0, x.1 *. 0.4) })
  |> util.dur(2.0)
  // |> sc_analog_echo.ns(0.3)
  // |> yielder.zip(lfo.sin(1.5))
  // |> yielder.map(fn(fp) {util.pan(fp.0, fp.1) })
  // |> sc_reverb.freeverb0()
  |> util.pan2(lfo.sin(1.5))
  |> sc_reverb.freeverb02()
  |> stream.play()

  log_max_gauges()
}

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
  log_max_gauges()
}

pub fn moog_test() {
  let streamf = fn(freq, dur) {
    lfo.triangle(5.0)
    |> stream.macr(5.0, freq)
    |> stream.Varying
    // You can have a stream as modulating frequency input for osc
    |> osc.sin()
    |> moog.stream(moog.new(0.1, 3.2))
    |> util.dur(dur)
    |> util.pan2(lfo.triangle(1.0) |> stream.macr(0.8, 0.0))
    |> stream.out()
  }
  let a = streamf(freq(music.A, 4), 1.2)
  let c = streamf(freq(music.C, 4), 2.2)
  a |> yielder.run()
  c |> yielder.run()
  a |> yielder.run()
  log_max_gauges()
}

fn freq(pitch: music.PitchClass, oct: music.Octave) -> Float {
  case pitch, oct {
    music.A, 4 -> 440.0
    music.C, 4 -> 261.33
    _, _ -> 330.0
  }
}

pub fn bitcrusher_test() {
  let bc = bitcrusher.new(4.0, 0.7)
  osc.sin(stream.Fixed(440.0))
  // Let the bitcrusher bits go from 8 to 1 during 4 seconds
  |> envelope.saw_tuple(4.0)
  |> yielder.map(fn(fe: #(gleuterpea.Frame, Float)) -> gleuterpea.Frame {
    bitcrusher.next(
      fe.0,
      bitcrusher.BitCrusher(..bc, bits: { 1.0 +. fe.1 *. 7.0 }),
    )
  })
  |> util.dur(4.5)
  |> util.pan(0.0)
  |> stream.play()
}

// ---------------------------------------------------

fn log_max_gauges() {
  let assert [max1] = max_mix_time()
  let assert [max2] = max_map_size()
  logger_info("Max mix time ~p microsec, max map size ~p", [
    max1,
    int.to_float(max2),
  ])
  clear_max_mix_time()
  clear_max_map_size()
}

@external(erlang, "Elixir.Xalsa", "max_mix_time")
fn max_mix_time() -> List(Float)

@external(erlang, "Elixir.Xalsa", "max_map_size")
fn max_map_size() -> List(Int)

pub type DoNotLeak

@external(erlang, "Elixir.Xalsa", "clear_max_mix_time")
fn clear_max_mix_time() -> DoNotLeak

@external(erlang, "Elixir.Xalsa", "clear_max_map_size")
fn clear_max_map_size() -> DoNotLeak

@external(erlang, "logger", "info")
fn logger_info(s: String, pars: List(Float)) -> DoNotLeak
