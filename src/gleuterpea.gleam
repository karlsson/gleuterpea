//// Generates 3 MIDI files:
//// - t251.mid - a chord sequence
//// - twinkle.mid - Twinkle twinkle little star
//// - child_song6.mid - Child song #6

import euterpea/io/midi/to
import euterpea/music.{
  type Music, type Pitch, type PitchClass, A, Bn, C, D, E, F, G, Hn, Par, Qn,
  Seq, Wn, a, b, c, d, e, f, g, nd,
}
import gleam/io
import gleam/list
import hsom/interlude

pub fn main() {
  io.println("Hello from gleuterpea!")
  let _ = to.write_midi(t251(), "t251.mid")
  let _ = to.write_midi(twinkle(), "twinkle.mid")
  let _ = to.write_midi(interlude.child_song6(), "child_song6.mid")
}

fn t251() -> Music(Pitch) {
  let d_minor = d(4, nd(Wn)) |> Par(f(4, nd(Wn))) |> Par(a(4, nd(Wn)))
  let g_major = g(4, nd(Wn)) |> Par(b(4, nd(Wn))) |> Par(d(5, nd(Wn)))
  let c_major = c(4, nd(Bn)) |> Par(e(4, nd(Bn))) |> Par(g(4, nd(Bn)))
  d_minor |> Seq(g_major) |> Seq(c_major)
}

fn twinkle() -> Music(Pitch) {
  let m11 =
    c(4, nd(Qn))
    |> Seq(c(4, nd(Qn)))
    |> Seq(g(4, nd(Qn)))
    |> Seq(g(4, nd(Qn)))
    |> Seq(a(4, nd(Qn)))
    |> Seq(a(4, nd(Qn)))
    |> Seq(g(4, nd(Hn)))
  // Same as above
  let m12 =
    music.line(list.map([C, C, G, G, A, A], pc_to_qn)) |> Seq(g(4, nd(Hn)))
  let m2 =
    music.line(list.map([F, F, E, E, D, D], pc_to_qn)) |> Seq(c(4, nd(Hn)))
  let m3 =
    music.line(list.map([G, G, F, F, E, E], pc_to_qn)) |> Seq(d(4, nd(Hn)))
  music.line([m11, m2, m3, m3, m12, m2])
}

fn pc_to_qn(pc: PitchClass) -> Music(Pitch) {
  music.note(nd(Qn), music.Pitch(pc, 4))
}
