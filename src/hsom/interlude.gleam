//// Childsong 6 from Haskell School of Music examples "transposed" to Gleam.
//// [Interlude.lhs](https://github.com/Euterpea/HSoM/blob/master/HSoM/Examples/Interlude.lhs)

// https://github.com/Euterpea/HSoM
// > module  HSoM.Examples.Interlude
// >         (  childSong6,  --  :: Music Pitch,
// >            prefix       --  :: [Music a] -> Music a)
// >         )  where
// > import Euterpea
import euterpea/music.{
  type Dur, type Music, type Octave, type Pitch, Dhn, Dqn, En, Hn, Note, Par,
  Prim, Qn, Seq, a, as_, b, c, cs, d, e, es, f, fs, g, gs, line, nd, nr, tempo,
  times,
}
import gleam/list

// > addDur       :: Dur -> [Dur -> Music a] -> Music a
// > addDur d ns  =  let f n = n d
// >                 in line (map f ns)
fn add_dur(d: music.NoteLen, ns: List(#(fn(Octave, Dur) -> Music(Pitch), Int))) {
  let f = fn(n) {
    let #(f1, oct) = n
    f1(oct, nd(d))
  }
  line(list.map(ns, f))
}

// > graceNote :: Int -> Music Pitch -> Music Pitch
// > graceNote n  (Prim (Note d p))  =
// >           note (d/8) (trans n p) :+: note (7*d/8) p
// > graceNote n  _                  = 
// >           error "Can only add a grace note to a note."
fn grace_note(n: Int, mp: Music(Pitch)) -> Result(Music(Pitch), Nil) {
  case n, mp {
    _, Prim(Note(d, p)) ->
      Ok(Seq(
        music.note(d /. 8.0, music.trans(p, n)),
        music.note(7.0 *. d /. 8.0, p),
      ))
    _, _ -> Error(Nil)
  }
}

fn b1() {
  add_dur(music.Dqn, [#(b, 2), #(fs, 3), #(g, 3), #(fs, 3)])
}

fn b2() {
  add_dur(music.Dqn, [#(b, 2), #(es, 3), #(fs, 3), #(es, 3)])
}

fn b3() {
  add_dur(music.Dqn, [#(as_, 2), #(fs, 3), #(g, 3), #(fs, 3)])
}

fn bass_line() {
  times(b1(), 3)
  |> Seq(times(b2(), 2))
  |> Seq(times(b3(), 4))
  |> Seq(times(b1(), 5))
}

fn main_voice() {
  times(Seq(v1(), v2()), 3)
}

//  bars 1-2
fn v1() {
  let assert Ok(gnot) = grace_note(-1, d(4, nd(Qn)))
  v1a() |> Seq(gnot) |> Seq(v1b())
}

fn v1a() {
  add_dur(En, [
    #(a, 4),
    #(e, 4),
    #(d, 4),
    #(fs, 4),
    #(cs, 4),
    #(b, 3),
    #(e, 4),
    #(b, 3),
  ])
}

fn v1b() {
  add_dur(En, [#(cs, 4), #(b, 3)])
}

fn v2() {
  v2a()
  |> Seq(v2b())
  |> Seq(v2c())
  |> Seq(v2d())
  |> Seq(v2e())
  |> Seq(v2f())
  |> Seq(v2g())
}

//  bars 7-11
fn v2a() {
  line([
    cs(4, nd(Dhn) +. nd(Dhn)),
    d(4, nd(Dhn)),
    f(4, nd(Hn)),
    gs(4, nd(Qn)),
    fs(4, nd(Hn) +. nd(En)),
    g(4, nd(En)),
  ])
}

// --  bars 12-13
fn v2b() {
  add_dur(En, [#(fs, 4), #(e, 4), #(cs, 4), #(as_, 3)])
  |> Seq(a(3, nd(Dqn)))
  |> Seq(add_dur(En, [#(as_, 3), #(cs, 4), #(fs, 4), #(e, 4), #(fs, 4)]))
}

// --  bars 14-16
fn v2c() {
  line([
    g(4, nd(En)),
    as_(4, nd(En)),
    cs(5, { nd(Hn) +. nd(En) }),
    d(5, nd(En)),
    cs(5, nd(En)),
  ])
  |> Seq(e(4, nd(En)))
  |> Seq(nr(En))
  |> Seq(
    line([
      as_(4, nd(En)),
      a(4, nd(En)),
      g(4, nd(En)),
      d(4, nd(Qn)),
      c(4, nd(En)),
      cs(4, nd(En)),
    ]),
  )
}

// --  bars 17-18.5
fn v2d() {
  add_dur(En, [
    #(fs, 4),
    #(cs, 4),
    #(e, 4),
    #(cs, 4),
    #(a, 3),
    #(as_, 3),
    #(d, 4),
    #(e, 4),
    #(fs, 4),
  ])
}

// --  bars 18.5-20
fn v2e() {
  let asg = fn(n, mp) {
    let assert Ok(mp2) = grace_note(n, mp)
    mp2
  }
  line([
    asg(2, e(4, nd(Qn))),
    d(4, nd(En)),
    asg(2, d(4, nd(Qn))),
    cs(4, nd(En)),
    asg(1, cs(4, nd(Qn))),
    b(3, nd(En) +. nd(Hn)),
    cs(4, nd(En)),
    b(3, nd(En)),
  ])
}

// --  bars 21-23
fn v2f() {
  let den = nd(En)
  line([
    fs(4, den),
    a(4, den),
    b(4, nd(Hn) +. nd(Qn)),
    a(4, den),
    fs(4, den),
    e(4, nd(Qn)),
    d(4, den),
    fs(4, den),
    e(4, nd(Hn)),
    d(4, nd(Hn)),
    fs(4, nd(Qn)),
  ])
}

//  --  bars 24-28
fn v2g() {
  tempo(
    line([cs(4, nd(En)), d(4, nd(En)), cs(4, nd(En))])
      |> Seq(b(3, 3.0 *. nd(Dhn) +. nd(Hn))),
    3.0 /. 2.0,
  )
}

// > childSong6 :: Music Pitch
// >               in instrument  RhodesPiano 
// >                              (tempo t (bassLine :=: mainVoice))
pub fn child_song6() {
  let t = { nd(Dhn) /. nd(Qn) } *. { 69.0 /. 120.0 }
  let m = tempo(Par(bass_line(), main_voice()), t)
  music.instrument(m, music.RhodesPiano)
}
// > prefixes         :: [a] -> [[a]]
// > prefixes []      =  []
// > prefixes (x:xs)  =  let f pf = x:pf
// >                     in [x] : map f (prefixes xs)
// fn prefixes(xs0: List(a)) -> List(List(a)){
//   case xs0 {
//     [] -> []
//     [x, ..xs] -> {

//     }
//   }
// } 
// > prefix :: [Music a] -> Music a
// > prefix mel =  let  m1  = line (concat (prefixes mel))
// >                    m2  = transpose 12 (line (concat (prefixes (reverse mel))))
// >                    m   = instrument Flute m1 :=: instrument VoiceOohs m2
// >               in m :+: transpose 5 m :+: m

// > mel1 = [c 4 en, e 4 sn, g 4 en, b 4 sn, a 4 en, f 4 sn, d 4 en, b 3 sn, c 4 en]
// > mel2 = [c 4 sn, e 4 sn, g 4 sn, b 4 sn, a 4 sn, f 3 sn, d 4 sn, b 3 sn, c 4 sn]
