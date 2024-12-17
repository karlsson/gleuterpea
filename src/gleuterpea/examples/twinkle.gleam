//// Play "Twinkle twinkle little star" using the `Music` definition
//// from euterpea converted to MEvents.
//// 
//// Every note will be spawned as an Erlang (Gleam) process that
//// executes the synth defined in a yield at the
//// MEvent specified time and will last for the duration time.
//// 
//// Start with `gleam run -m gleuterpea/examples/twinkle` 

import euterpea/io/midi/m_event.{type MEvent}
import euterpea/music
import euterpea/play_list
import gleam/erlang/process
import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/yielder
import gleam_community/maths/elementary
import gleuterpea/envelope
import gleuterpea/generator/osc
import gleuterpea/stream
import gleuterpea/support
import gleuterpea/util

// import gleuterpea/filter/moog
// import gleuterpea/filter/sc_filter
// import gleuterpea/filter/sc_reverb
// import gleuterpea/generator/lfo

/// Get the song from Euterpea lib and convert to MEvents.
/// Start one process for each MEvent that will trigger the
/// synth (note) execution at the `e_time` setting. 
pub fn main() {
  let _ = support.load_nifs()
  let #(m_ev, dur) =
    play_list.twinkle()
    // |> m_event.perform()
    |> m_event.to_music1
    |> m_event.perform1_dur
  list.map(m_ev, spawn_note)
  process.sleep(float.round(dur *. 1000.0) + 4000)
}

fn spawn_note(m: MEvent) -> process.Pid {
  process.start(fn() { synth_def1(m) }, False)
}

fn synth_def1(m: MEvent) {
  let m_event.MEvent(
    e_time: wait,
    e_inst: _,
    e_pitch: abs_pitch,
    e_dur: dur,
    e_vol: vol,
    e_params: _,
  ) = m
  let volf = int.to_float(vol) /. 127.0
  let synth =
    osc.sin(stream.Fixed(ap_to_hz(abs_pitch)))
    // |> moog.stream(moog.new(0.1, 3.2))
    // |> util.dur(dur)
    |> envelope.adsr(
      envelope.ADSR(
        0.2,
        volf,
        0.0,
        option.None,
        0.3 *. dur,
        volf /. 2.0,
        float.max(0.7 *. dur -. 0.2, 0.0),
      ),
      process.new_subject(),
    )
    |> util.pan(0.0)
    // |> sc_reverb.freeverb02()
    // |> util.pan2(lfo.triangle(1.0) |> stream.macr(0.8, 0.0))
    |> stream.out()

  process.sleep(float.round(wait *. 1000.0))
  process.start(fn() { synth |> yielder.run }, True)
  process.sleep(float.round(dur +. 0.5))
}

/// Converting an AbsPitch to hertz (cycles per second):
fn ap_to_hz(ap: Int) -> Float {
  let assert Ok(fact) =
    elementary.power(
      2.0,
      int.to_float(ap - music.abs_pitch(music.Pitch(music.A, 4))) /. 12.0,
    )
  440.0 *. fact
}
