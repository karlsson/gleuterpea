import gleam/float
import gleam/list
import non_empty_list.{type NonEmptyList}

pub type AbsPitch =
  Int

pub type Octave =
  Int

/// Fake with Float until a real Rational module is implemented.
/// todo -implement Rational type and functions
pub type Rational =
  Float

pub type Dur =
  Rational

pub type PitchClass {
  Cff
  Cf
  C
  Dff
  Cs
  Df
  Css
  D
  Eff
  Ds
  Ef
  Fff
  Dss
  E
  Ff
  Es
  F
  Gff
  Ess
  Fs
  Gf
  Fss
  G
  Aff
  Gs
  Af
  Gss
  A
  Bff
  As
  Bf
  Ass
  B
  Bs
  Bss
}

pub type Pitch {
  Pitch(pc: PitchClass, oct: Octave)
  AbsPitch(AbsPitch)
}

// pub type Pitch {
//   Pitch(pc: PitchClass, oct: Octave)
// }

pub type Primitive(a) {
  Note(Dur, a)
  Rest(Dur)
}

pub type Music(a) {
  //  primitive value
  Prim(Primitive(a))
  //  sequential composition
  Seq(Music(a), Music(a))
  //  parallel composition
  Par(Music(a), Music(a))
  //  modifier
  Modify(Control, Music(a))
}

pub type Control {
  //  scale the tempo
  Tempo(Rational)
  //  transposition
  Transpose(AbsPitch)
  //  instrument label
  Instrument(InstrumentName)
  //  phrase attributes
  Phrase(List(PhraseAttribute))
  //  key signature and mode
  KeySig(PitchClass, Mode)
  //  for user-specified controls
  Custom(String)
}

pub type Mode {
  Major
  Minor
  Ionian
  Dorian
  Phrygian
  Lydian
  Mixolydian
  Aeolian
  Locrian
  CustomMode(String)
}

pub type InstrumentName {
  AcousticGrandPiano
  BrightAcousticPiano
  ElectricGrandPiano
  HonkyTonkPiano
  RhodesPiano
  ChorusedPiano
  Harpsichord
  Clavinet
  Celesta
  Glockenspiel
  MusicBox
  Vibraphone
  Marimba
  Xylophone
  TubularBells
  Dulcimer
  HammondOrgan
  PercussiveOrgan
  RockOrgan
  ChurchOrgan
  ReedOrgan
  Accordion
  Harmonica
  TangoAccordion
  AcousticGuitarNylon
  AcousticGuitarSteel
  ElectricGuitarJazz
  ElectricGuitarClean
  ElectricGuitarMuted
  OverdrivenGuitar
  DistortionGuitar
  GuitarHarmonics
  AcousticBass
  ElectricBassFingered
  ElectricBassPicked
  FretlessBass
  SlapBass1
  SlapBass2
  SynthBass1
  SynthBass2
  Violin
  Viola
  Cello
  Contrabass
  TremoloStrings
  PizzicatoStrings
  OrchestralHarp
  Timpani
  StringEnsemble1
  StringEnsemble2
  SynthStrings1
  SynthStrings2
  ChoirAahs
  VoiceOohs
  SynthVoice
  OrchestraHit
  Trumpet
  Trombone
  Tuba
  MutedTrumpet
  FrenchHorn
  BrassSection
  SynthBrass1
  SynthBrass2
  SopranoSax
  AltoSax
  TenorSax
  BaritoneSax
  Oboe
  Bassoon
  EnglishHorn
  Clarinet
  Piccolo
  Flute
  Recorder
  PanFlute
  BlownBottle
  Shakuhachi
  Whistle
  Ocarina
  Lead1Square
  Lead2Sawtooth
  Lead3Calliope
  Lead4Chiff
  Lead5Charang
  Lead6Voice
  Lead7Fifths
  Lead8BassLead
  Pad1NewAge
  Pad2Warm
  Pad3Polysynth
  Pad4Choir
  Pad5Bowed
  Pad6Metallic
  Pad7Halo
  Pad8Sweep
  FX1Train
  FX2Soundtrack
  FX3Crystal
  FX4Atmosphere
  FX5Brightness
  FX6Goblins
  FX7Echoes
  FX8SciFi
  Sitar
  Banjo
  Shamisen
  Koto
  Kalimba
  Bagpipe
  Fiddle
  Shanai
  TinkleBell
  Agogo
  SteelDrums
  Woodblock
  TaikoDrum
  MelodicDrum
  SynthDrum
  ReverseCymbal
  GuitarFretNoise
  BreathNoise
  Seashore
  BirdTweet
  TelephoneRing
  Helicopter
  Applause
  Gunshot
  Percussion
  CustomInstrument
  String
}

pub type PhraseAttribute {
  Dyn(Dynamic)
  Tmp(Tempo)
  Art(Articulation)
  Orn(Ornament)
}

pub type Dynamic {
  Accent(Rational)
  Crescendo(Rational)
  Diminuendo(Rational)
  StdLoudness(StdLoudness)
  Loudness(Rational)
}

pub type StdLoudness {
  PPP
  PP
  P
  MP
  SF
  MF
  NF
  FF
  FFF
}

pub type Tempo {
  Ritardando(Rational)
  Accelerando(Rational)
}

pub type Articulation {
  Staccato(Rational)
  Legato(Rational)
  Slurred(Rational)
  Tenuto
  Marcato
  Pedal
  Fermata
  FermataDown
  Breath
  DownBow
  UpBow
  Harmonic
  Pizzicato
  LeftPizz
  BartokPizz
  Swell
  Wedge
  Thumb
  Stopped
}

pub type Ornament {
  Trill
  Mordent
  InvMordent
  DoubleMordent
  Turn
  TrilledTurn
  ShortTrill
  Arpeggio
  ArpeggioUp
  ArpeggioDown
  Instruction(String)
  Head(NoteHead)
  DiatonicTrans(Int)
}

pub type NoteHead {
  DiamondHead
  SquareHead
  XHead
  TriangleHead
  TremoloHead
  SlashHead
  ArtHarmonic
  NoHead
}

pub type Volume0 =
  Int

// addVolume    :: Volume -> Music Pitch -> Music (Pitch,Volume)
pub fn add_volume(m: Music(Pitch), v: Volume0) -> Music(#(Pitch, Volume0)) {
  m_map(m, fn(p) { #(p, v) })
}

// pMap               :: (a -> b) -> Primitive a -> Primitive b
pub fn p_map(p: Primitive(a), f) -> Primitive(b) {
  case p {
    Note(d, x) -> Note(d, f(x))
    Rest(d) -> Rest(d)
  }
}

// mMap                 :: (a -> b) -> Music a -> Music b
pub fn m_map(m: Music(a), f) {
  case m {
    Prim(p) -> Prim(p_map(p, f))
    Seq(m1, m2) -> Seq(m_map(m1, f), m_map(m2, f))
    Par(m1, m2) -> Par(m_map(m1, f), m_map(m2, f))
    Modify(c, m) -> Modify(c, m_map(m, f))
  }
}

// How do you Polymorphism in gleam?
// Or you just don't! ?
// > instance Functor Primitive where
// >     fmap = pMap

// > instance Functor Music where
// >     fmap = mMap

// pub fn fmap(x: a) {
//   case x {
//     Primitive -> p_map
//     Music -> m_map
//   }
// }

// mFold ::  (Primitive a -> b) -> (b->b->b) -> (b->b->b) ->
//           (Control -> b -> b) -> Music a -> b
//  // f (+:) (=:) g m =
pub fn m_fold(m: Music(a), primfn, seqfn, parfn, cfn) {
  let rec = fn(x) { m_fold(x, primfn, seqfn, parfn, cfn) }
  case m {
    Prim(p) -> primfn(p)
    Seq(m1, m2) -> seqfn(rec(m1), rec(m2))
    Par(m1, m2) -> parfn(rec(m1), rec(m2))
    Modify(c, m) -> cfn(c, rec(m))
  }
}

// pitch 0 = (C,-1)
// pitch 60 = (C,4)
// pitch 127 = (G,9)

pub fn abs_pitch(p: Pitch) -> AbsPitch {
  case p {
    Pitch(pc, oct) -> 12 * { oct + 1 } + pc_to_int(pc)
    AbsPitch(i) -> i
  }
}

pub fn pc_to_int(pc: PitchClass) -> Int {
  case pc {
    Cff -> -2
    Cf -> -1
    C | Dff -> 0
    Cs | Df -> 1
    Css | D | Eff -> 2
    Ds | Ef | Fff -> 3
    Dss | E | Ff -> 4
    Es | F | Gff -> 5
    Ess | Fs | Gf -> 6
    Fss | G | Aff -> 7
    Gs | Af -> 8
    Gss | A | Bff -> 9
    As | Bf -> 10
    Ass | B -> 11
    Bs -> 12
    Bss -> 13
  }
}

pub fn pitch(ap: AbsPitch) -> Pitch {
  let oct = ap / 12
  let n = ap % 12
  let assert [pc] =
    list.index_fold(
      [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B],
      [],
      fn(acc, item, index) {
        case index == n {
          True -> [item, ..acc]
          False -> acc
        }
      },
    )
  Pitch(pc, oct - 1)
}

pub fn trans(p: Pitch, i: Int) -> Pitch {
  pitch(abs_pitch(p) + i)
}

pub fn note(d: Dur, p: a) -> Music(a) {
  Prim(Note(d, p))
}

pub fn rest(d: Dur) -> Music(a) {
  Prim(Rest(d))
}

pub fn tempo(m: Music(a), r: Dur) -> Music(a) {
  Modify(Tempo(r), m)
}

pub fn transpose(m: Music(a), ap: AbsPitch) -> Music(a) {
  Modify(Transpose(ap), m)
}

pub fn instrument(m: Music(a), i: InstrumentName) -> Music(a) {
  Modify(Instrument(i), m)
}

pub fn phrase(m: Music(a), pa: List(PhraseAttribute)) -> Music(a) {
  Modify(Phrase(pa), m)
}

pub fn keysig(m: Music(a), pc: PitchClass, mo: Mode) -> Music(a) {
  Modify(KeySig(pc, mo), m)
}

// cff,cf,c,cs,css,dff,df,d,ds,dss,eff,ef,e,es,ess,fff,ff,f,
//   fs,fss,gff,gf,g,gs,gss,aff,af,a,as,ass,bff,bf,b,bs,bss ::
//    Octave -> Dur -> Music Pitch
pub fn cff(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Cff, o))
}

pub fn cf(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Cf, o))
}

pub fn c(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(C, o))
}

pub fn cs(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Cs, o))
}

pub fn css(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Css, o))
}

pub fn dff(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Dff, o))
}

pub fn df(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Dff, o))
}

pub fn d(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(D, o))
}

pub fn ds(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Ds, o))
}

pub fn dss(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Dss, o))
}

pub fn eff(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Eff, o))
}

pub fn ef(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Ef, o))
}

pub fn e(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(E, o))
}

pub fn es(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Es, o))
}

pub fn ess(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Ess, o))
}

pub fn fff(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Fff, o))
}

pub fn ff(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Ff, o))
}

pub fn f(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(F, o))
}

pub fn fs(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Fs, o))
}

pub fn fss(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Fss, o))
}

pub fn gff(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Gff, o))
}

pub fn gf(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Gf, o))
}

pub fn g(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(G, o))
}

pub fn gs(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Gs, o))
}

pub fn gss(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Gss, o))
}

pub fn aff(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Aff, o))
}

pub fn af(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Af, o))
}

pub fn a(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(A, o))
}

pub fn as_(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(As, o))
}

pub fn ass(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Ass, o))
}

pub fn bff(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Bff, o))
}

pub fn bf(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Bf, o))
}

pub fn b(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(B, o))
}

pub fn bs(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Bs, o))
}

pub fn bss(o: Octave, d: Dur) -> Music(Pitch) {
  note(d, Pitch(Bss, o))
}

/// Notes
pub type NoteLen {
  /// Brevis note
  Bn
  /// Whole
  Wn
  /// Half
  Hn
  /// Quarter
  Qn
  /// Eigth
  En
  /// Sixteenth
  Sn
  /// Thirty-second
  Tn
  /// Sixty-fourth
  Sfn
  /// Dotted whole note
  Dwn
  /// Dotted half
  Dhn
  /// Dotted quarter
  Dqn
  /// Dotted eigth
  Den
  /// Dotted sixteenth
  Dsn
  /// Dotted thirty-second
  Dtn
  /// Double-dotted half note
  Ddhn
  /// Double-dotted quarter
  Ddqn
  /// Double-dotted eigth
  Dden
}

/// Note duration
pub fn nd(n: NoteLen) -> Dur {
  case n {
    Bn -> 2.0
    Wn -> 1.0
    Hn -> 1.0 /. 2.0
    Qn -> 1.0 /. 4.0
    En -> 1.0 /. 8.0
    Sn -> 1.0 /. 16.0
    Tn -> 1.0 /. 32.0
    Sfn -> 1.0 /. 64.0
    Dwn -> 3.0 /. 2.0
    Dhn -> 3.0 /. 4.0
    Dqn -> 3.0 /. 8.0
    Den -> 3.0 /. 16.0
    Dsn -> 3.0 /. 32.0
    Dtn -> 3.0 /. 64.0
    Ddhn -> 7.0 /. 8.0
    Ddqn -> 7.0 /. 16.0
    Dden -> 7.0 /. 32.0
  }
}

/// Note rest
pub fn nr(n: NoteLen) -> Music(Pitch) {
  rest(nd(n))
}

pub fn line(ml: List(Music(a))) -> Music(a) {
  list.fold_right(ml, rest(0.0), fn(acc, m) { Seq(m, acc) })
}

pub fn chord(ml: List(Music(a))) -> Music(a) {
  list.fold_right(ml, rest(0.0), Par)
}

pub fn line1(ml: NonEmptyList(Music(a))) -> Music(a) {
  foldr1(ml, Seq)
}

pub fn chord1(ml: NonEmptyList(Music(a))) -> Music(a) {
  foldr1(ml, Par)
}

fn foldr1(
  ml: NonEmptyList(Music(a)),
  f: fn(Music(a), Music(a)) -> Music(a),
) -> Music(a) {
  ml
  |> non_empty_list.reverse
  |> non_empty_list.reduce(f)
}

pub fn offset(m: Music(a), d: Dur) -> Music(a) {
  Seq(rest(d), m)
}

//  times      :: Int -> Music a -> Music a
// > times 0 m  = rest 0
// > times n m  = m :+: times (n-1) m
pub fn times(m: Music(a), n: Int) -> Music(a) {
  case n > 0 {
    True -> Seq(m, times(m, n - 1))
    False -> Seq(m, rest(0.0))
  }
}

// Unless Seq is lazy this call will crash your program
pub fn forever(m: Music(a)) -> Music(a) {
  Seq(m, forever(m))
}

pub fn line_to_list(m: Music(a)) -> Result(List(Music(a)), String) {
  line_to_list1(m, [])
}

fn line_to_list1(l: Music(a), acc) -> Result(List(Music(a)), String) {
  case l {
    Prim(Rest(0.0)) -> Ok(list.reverse(acc))
    Seq(n, ns) -> line_to_list1(ns, [n, ..acc])
    _ -> Error("line_to_list: argument not created by function line")
  }
}

// invertAt :: Pitch -> Music Pitch -> Music Pitch
// invertAt pRef = mMap (\p -> pitch (2 * absPitch pRef - absPitch p))
pub fn invert_at(mp: Music(Pitch), p_ref: Pitch) -> Music(Pitch) {
  m_map(mp, fn(p) { pitch(2 * abs_pitch(p_ref) - abs_pitch(p)) })
}

// invertAt1 :: Pitch -> Music (Pitch, a) -> Music (Pitch, a)
// invertAt1 pRef = mMap (\(p,x) -> (pitch (2 * absPitch pRef - absPitch p),x))
pub fn invert_at1(mp: Music(#(Pitch, a)), p_ref: Pitch) -> Music(#(Pitch, a)) {
  m_map(mp, fn(px) {
    let #(p, x) = px
    #(pitch(2 * abs_pitch(p_ref) - abs_pitch(p)), x)
  })
}

// invert :: Music Pitch -> Music Pitch
// invert m =
//     let pRef = mFold pFun (++) (++) (flip const) m
//     in  if null pRef then m -- no pitches in the structure!
//         else invertAt (head pRef) m
//     where pFun (Note d p) = [p]
//           pFun _ = []
pub fn invert(m: Music(Pitch)) -> Music(Pitch) {
  let p_fun = fn(x) {
    case x {
      Note(_d, p) -> [p]
      _ -> []
    }
  }
  let p_ref = m_fold(m, p_fun, list.append, list.append, fn(_, b) { b })
  case p_ref {
    [] -> m
    [h, ..] -> invert_at(m, h)
  }
}

// invert1 :: Music (Pitch,a) -> Music (Pitch,a)
// invert1 m =
//     let pRef = mFold pFun (++) (++) (flip const) m
//     in  if null pRef then m -- no pitches!
//         else invertAt1 (head pRef) m
//     where pFun (Note d (p,x)) = [p]
//           pFun _ = []
pub fn invert1(m: Music(#(Pitch, a))) -> Music(#(Pitch, a)) {
  let p_fun = fn(x) {
    case x {
      Note(_d, #(p, _)) -> [p]
      _ -> []
    }
  }
  let p_ref = m_fold(m, p_fun, list.append, list.append, fn(_, b) { b })
  case p_ref {
    [] -> m
    [h, ..] -> invert_at1(m, h)
  }
}

// retro               :: Music a -> Music a
// retro n@(Prim _)    = n
// retro (Modify c m)  = Modify c (retro m)
// retro (m1 :+: m2)   = retro m2 :+: retro m1
// retro (m1 :=: m2)   =
//    let  d1 = dur m1
//         d2 = dur m2
//    in if d1>d2  then retro m1 :=: (rest (d1-d2) :+: retro m2)
//                 else (rest (d2-d1) :+: retro m1) :=: retro m2
pub fn retro(m: Music(a)) -> Music(a) {
  case m {
    Prim(_) -> m
    Modify(c, n) -> Modify(c, retro(n))
    Seq(m1, m2) -> Seq(retro(m1), retro(m2))
    Par(m1, m2) -> {
      let d1 = dur(m1)
      let d2 = dur(m2)
      case d1 >. d2 {
        True -> Par(retro(m1), Seq(rest(d1 -. d2), retro(m2)))
        False -> Par(Seq(rest(d2 -. d1), retro(m1)), retro(m2))
      }
    }
  }
}

// retroInvert, invertRetro :: Music Pitch -> Music Pitch
pub fn retro_invert(m: Music(Pitch)) -> Music(Pitch) {
  m |> retro |> invert
}

// invertRetro  = invert . retro
pub fn invert_retro(m: Music(Pitch)) -> Music(Pitch) {
  m |> invert |> retro
}

// > dur                       :: Music a -> Dur
// > dur (Prim (Note d _))     = d
// > dur (Prim (Rest d))       = d
// > dur (m1 :+: m2)           = dur m1   +   dur m2
// > dur (m1 :=: m2)           = dur m1 `max` dur m2
// > dur (Modify (Tempo r) m)  = dur m / r
// > dur (Modify _ m)          = dur m
pub fn dur(m: Music(a)) -> Dur {
  case m {
    Prim(Note(d, _)) -> d
    Prim(Rest(d)) -> d
    Seq(m1, m2) -> dur(m1) +. dur(m2)
    Par(m1, m2) -> float.max(dur(m1), dur(m2))
    Modify(Tempo(r), m) -> dur(m) /. r
    Modify(_, m) -> dur(m)
  }
}

// The Note cases have been re-written to turn zero-duration notes into rests.
// > cut :: Dur -> Music a -> Music a
// > cut d m | d <= 0            = rest 0
// > cut d (Prim (Note oldD p))  =  let d' = max (min oldD d) 0
// >                                in if d'>0 then note d' p else rest 0
// > cut d (Prim (Rest oldD))    = rest (max (min oldD d) 0)
// > cut d (m1 :=: m2)           = cut d m1 :=: cut d m2
// > cut d (m1 :+: m2)           =  let  m'1  = cut d m1
// >                                     m'2  = cut (d - dur m'1) m2
// >                                in   m'1 :+: m'2
// > cut d (Modify (Tempo r) m)  = tempo r (cut (d*r) m)
// > cut d (Modify c m)          = Modify c (cut d m)
pub fn cut(m: Music(a), d: Dur) -> Music(a) {
  case m {
    _ if d <=. 0.0 -> rest(0.0)
    Prim(Note(old_d, p)) -> {
      let dprim = float.max(float.min(old_d, d), 0.0)
      case dprim >. 0.0 {
        True -> note(dprim, p)
        False -> rest(0.0)
      }
    }
    Prim(Rest(old_d)) -> rest(float.max(float.min(old_d, d), 0.0))
    Par(m1, m2) -> Par(cut(m1, d), cut(m2, d))
    Seq(m1, m2) -> {
      let mprim1 = cut(m1, d)
      let mprim2 = cut(m2, d -. dur(mprim1))
      Seq(mprim1, mprim2)
    }
    Modify(Tempo(r), m) -> tempo(cut(m, d *. r), r)
    Modify(c, m) -> Modify(c, cut(m, d))
  }
}

// > remove :: Dur -> Music a -> Music a
// > remove d m | d <= 0            = m
// > remove d (Prim (Note oldD p))  =  let d' = max (oldD-d) 0
// >                                   in  if d'>0 then note d' p else rest 0
// > remove d (Prim (Rest oldD))    = rest (max (oldD-d) 0)
// > remove d (m1 :=: m2)           = remove d m1 :=: remove d m2
// > remove d (m1 :+: m2)           =  let  m'1  = remove d m1
// >                                        m'2  = remove (d - dur m1) m2
// >                                   in   m'1 :+: m'2
// > remove d (Modify (Tempo r) m)  = tempo r (remove (d*r) m)
// > remove d (Modify c m)          = Modify c (remove d m)
pub fn remove(m: Music(a), d: Dur) -> Music(a) {
  case m {
    _ if d <=. 0.0 -> m
    Prim(Note(old_d, p)) -> {
      let dprim = float.max(old_d -. d, 0.0)
      case dprim >. 0.0 {
        True -> note(dprim, p)
        False -> rest(0.0)
      }
    }
    Prim(Rest(old_d)) -> rest(float.max(old_d -. d, 0.0))
    Par(m1, m2) -> Par(remove(m1, d), remove(m2, d))
    Seq(m1, m2) -> {
      let mprim1 = remove(m1, d)
      let mprim2 = remove(m2, d -. dur(m1))
      Seq(mprim1, mprim2)
    }
    Modify(Tempo(r), m) -> tempo(cut(m, d *. r), r)
    Modify(c, m) -> Modify(c, cut(m, d))
  }
}

// > removeZeros :: Music a -> Music a
// > removeZeros (Prim p)      = Prim p
// > removeZeros (m1 :+: m2)   =
// >   let  m'1  = removeZeros m1
// >        m'2  = removeZeros m2
// >   in case (m'1,m'2) of
// >        (Prim (Note 0 p), m)  -> m
// >        (Prim (Rest 0  ), m)  -> m
// >        (m, Prim (Note 0 p))  -> m
// >        (m, Prim (Rest 0  ))  -> m
// >        (m1, m2)              -> m1 :+: m2
// > removeZeros (m1 :=: m2)   =
// >   let  m'1  = removeZeros m1
// >        m'2  = removeZeros m2
// >   in case (m'1,m'2) of
// >        (Prim (Note 0 p), m)  -> m
// >        (Prim (Rest 0  ), m)  -> m
// >        (m, Prim (Note 0 p))  -> m
// >        (m, Prim (Rest 0  ))  -> m
// >        (m1, m2)              -> m1 :=: m2
// > removeZeros (Modify c m)  = Modify c (removeZeros m)
pub fn remove_zeros(m: Music(a)) -> Music(a) {
  case m {
    Prim(_) -> m
    Seq(m1, m2) -> {
      let mprim1 = remove_zeros(m1)
      let mprim2 = remove_zeros(m2)
      case mprim1, mprim2 {
        Prim(Note(0.0, _)), m -> m
        Prim(Rest(0.0)), m -> m
        m, Prim(Note(0.0, _p)) -> m
        m, Prim(Rest(0.0)) -> m
        _, _ -> Seq(m1, m2)
      }
    }
    Par(m1, m2) -> {
      let mprim1 = remove_zeros(m1)
      let mprim2 = remove_zeros(m2)
      case mprim1, mprim2 {
        Prim(Note(0.0, _)), m -> m
        Prim(Rest(0.0)), m -> m
        m, Prim(Note(0.0, _p)) -> m
        m, Prim(Rest(0.0)) -> m
        _, _ -> Par(m1, m2)
      }
    }
    Modify(c, m) -> Modify(c, remove_zeros(m))
  }
}

// > type LazyDur = [Dur]
// > durL :: Music a -> LazyDur
// > durL m@(Prim _)            =  [dur m]
// > durL (m1 :+: m2)           =  let d1 = durL m1
// >                               in d1 ++ map (+(last d1)) (durL m2)
// > durL (m1 :=: m2)           =  mergeLD (durL m1) (durL m2)
// > durL (Modify (Tempo r) m)  =  map (/r) (durL m)
// > durL (Modify _ m)          =  durL m
pub type LazyDur =
  List(Dur)

pub fn dur_l(m: Music(a)) -> LazyDur {
  case m {
    Prim(_) -> [dur(m)]
    Seq(m1, m2) -> {
      let dl1 = dur_l(m1)
      // Music(a) cannot be empty and so cannot the list
      let assert Ok(dl1_last) = list.last(dl1)
      list.append(dl1, list.map(dur_l(m2), fn(x) { dl1_last +. x }))
    }
    Par(m1, m2) -> merge_ld(dur_l(m1), dur_l(m2))
    Modify(Tempo(r), m) -> list.map(dur_l(m), fn(x) { x /. r })
    Modify(_, m) -> dur_l(m)
  }
}

// > mergeLD :: LazyDur -> LazyDur -> LazyDur
// > mergeLD [] ld = ld
// > mergeLD ld [] = ld
// > mergeLD ld1@(d1:ds1) ld2@(d2:ds2) =
// >   if d1<d2  then  d1 : mergeLD ds1 ld2
// >             else  d2 : mergeLD ld1 ds2
fn merge_ld(ld1: LazyDur, ld2: LazyDur) -> LazyDur {
  case ld1, ld2 {
    [], ld -> ld
    ld, [] -> ld
    [d1, ..ds1], [d2, ..ds2] -> {
      case d1 <. d2 {
        True -> merge_ld(ds1, ld2)
        False -> merge_ld(ld1, ds2)
      }
    }
  }
}

// > minL :: LazyDur -> Dur -> Dur
// > minL []      d' = d'
// > minL [d]     d' = min d d'
// > minL (d:ds)  d' = if d < d' then minL ds d' else d'
pub fn min_l(ld: LazyDur, dprim: Dur) -> Dur {
  case ld, dprim {
    [], dprim -> dprim
    [d], dprim -> float.min(d, dprim)
    [d, ..ds], dprim -> {
      case d <. dprim {
        True -> min_l(ds, dprim)
        False -> dprim
      }
    }
  }
}

// > cutL :: LazyDur -> Music a -> Music a
// > cutL [] m                     = rest 0
// > cutL (d:ds) m | d <= 0        = cutL ds m
// > cutL ld (Prim (Note oldD p))  = note (minL ld oldD) p
// > cutL ld (Prim (Rest oldD))    = rest (minL ld oldD)
// > cutL ld (m1 :=: m2)           = cutL ld m1 :=: cutL ld m2
// > cutL ld (m1 :+: m2)           =
// >    let  m'1 = cutL ld m1
// >         m'2 = cutL (map (\d -> d - dur m'1) ld) m2
// >    in m'1 :+: m'2
// > cutL ld (Modify (Tempo r) m)  = tempo r (cutL (map (*r) ld) m)
// > cutL ld (Modify c m)          = Modify c (cutL ld m)
pub fn cut_l(ld: LazyDur, m: Music(a)) -> Music(a) {
  case ld, m {
    [], _ -> rest(0.0)
    [d, ..ds], _ if d <=. 0.0 -> cut_l(ds, m)
    _, Prim(Note(old_d, p)) -> note(min_l(ld, old_d), p)
    _, Prim(Rest(old_d)) -> rest(min_l(ld, old_d))
    _, Par(m1, m2) -> Par(cut_l(ld, m1), cut_l(ld, m2))
    _, Seq(m1, m2) -> {
      let mprim1 = cut_l(ld, m1)
      let dur1 = dur(mprim1)
      let mprim2 = cut_l(list.map(ld, fn(d) { d -. dur1 }), m2)
      Seq(mprim1, mprim2)
    }
    _, Modify(Tempo(r), mprim) ->
      tempo(cut_l(list.map(ld, fn(x) { x *. r }), mprim), r)
    _, Modify(c, mprim) -> Modify(c, cut_l(ld, mprim))
  }
}

// > (/=:)      :: Music a -> Music a -> Music a
// > m1 /=: m2  = cutL (durL m2) m1 :=: cutL (durL m1) m2
pub fn par_cut(m1: Music(a), m2: Music(a)) -> Music(a) {
  Par(cut_l(dur_l(m2), m1), cut_l(dur_l(m1), m2))
}

pub type PercussionSound {
  // MIDI Key 35
  AcousticBassDrum
  // MIDI Key 36 
  BassDrum1
  // ...uzw           
  SideStick
  AcousticSnare
  HandClap
  ElectricSnare
  LowFloorTom
  ClosedHiHat
  HighFloorTom
  PedalHiHat
  LowTom
  OpenHiHat
  LowMidTom
  HiMidTom
  CrashCymbal1
  HighTom
  RideCymbal1
  ChineseCymbal
  RideBell
  Tambourine
  SplashCymbal
  Cowbell
  CrashCymbal2
  Vibraslap
  RideCymbal2
  HiBongo
  LowBongo
  MuteHiConga
  OpenHiConga
  LowConga
  HighTimbale
  LowTimbale
  HighAgogo
  LowAgogo
  Cabasa
  Maracas
  ShortWhistle
  LongWhistle
  ShortGuiro
  LongGuiro
  Claves
  HiWoodBlock
  LowWoodBlock
  MuteCuica
  OpenCuica
  MuteTriangle
  // MIDI Key 82
  OpenTriangle
}

// perc :: PercussionSound -> Dur -> Music Pitch
// perc ps dur = instrument Percussion $ note dur (pitch (fromEnum ps + 35))
pub fn perc(ps: PercussionSound, d: Dur) {
  instrument(note(d, pitch(from_enum(ps))), Percussion)
}

/// Return index from enumerated type
pub fn from_enum(ps: PercussionSound) {
  case ps {
    AcousticBassDrum -> 35
    // MIDI Key 36 
    BassDrum1 -> 36
    // ...uzw           
    SideStick -> 37
    AcousticSnare -> 38
    HandClap -> 39
    ElectricSnare -> 40
    LowFloorTom -> 41
    ClosedHiHat -> 42
    HighFloorTom -> 43
    PedalHiHat -> 44
    LowTom -> 45
    OpenHiHat -> 46
    LowMidTom -> 47
    HiMidTom -> 48
    CrashCymbal1 -> 49
    HighTom -> 50
    RideCymbal1 -> 51
    ChineseCymbal -> 52
    RideBell -> 53
    Tambourine -> 54
    SplashCymbal -> 55
    Cowbell -> 56
    CrashCymbal2 -> 57
    Vibraslap -> 58
    RideCymbal2 -> 59
    HiBongo -> 60
    LowBongo -> 61
    MuteHiConga -> 62
    OpenHiConga -> 63
    LowConga -> 64
    HighTimbale -> 65
    LowTimbale -> 66
    HighAgogo -> 67
    LowAgogo -> 68
    Cabasa -> 69
    Maracas -> 70
    ShortWhistle -> 71
    LongWhistle -> 72
    ShortGuiro -> 73
    LongGuiro -> 74
    Claves -> 75
    HiWoodBlock -> 76
    LowWoodBlock -> 77
    MuteCuica -> 78
    OpenCuica -> 79
    MuteTriangle -> 80
    // MIDI Key 82
    OpenTriangle -> 81
  }
}

// -- =========================================================================================

// Sometimes we may wish to alter the internal structure of a Music value
// rather than wrapping it with Modify. The following functions allow this.

// > shiftPitches :: AbsPitch -> Music Pitch -> Music Pitch
// > shiftPitches k = mMap (trans k)
pub fn shift_pitches(ap: AbsPitch) -> fn(Music(Pitch)) -> Music(Pitch) {
  fn(mp: Music(Pitch)) { m_map(mp, trans(_, ap)) }
}

// > shiftPitches1 :: AbsPitch -> Music (Pitch, b) -> Music (Pitch, b)
// > shiftPitches1 k = mMap (\(p,xs) -> (trans k p, xs))
pub fn shift_pitches1(
  ap: AbsPitch,
) -> fn(Music(#(Pitch, b))) -> Music(#(Pitch, b)) {
  fn(mp: Music(#(Pitch, b))) {
    m_map(mp, fn(x) {
      let #(p, xs) = x
      #(trans(p, ap), xs)
    })
  }
}

// > scaleDurations :: Rational -> Music a -> Music a
// > scaleDurations r (Prim (Note d p)) = note (d/r) p
// > scaleDurations r (Prim (Rest d)) = rest (d/r)
// > scaleDurations r (m1 :+: m2) = scaleDurations r m1 :+: scaleDurations r m2
// > scaleDurations r (m1 :=: m2) = scaleDurations r m1 :=: scaleDurations r m2
// > scaleDurations r (Modify c m) = Modify c (scaleDurations r m)
pub fn scale_durations(m: Music(a), r: Rational) -> Music(a) {
  case m {
    Prim(Note(d, p)) -> note(d /. r, p)
    Prim(Rest(d)) -> rest(d /. r)
    Seq(m1, m2) -> Seq(scale_durations(m1, r), scale_durations(m2, r))
    Par(m1, m2) -> Par(scale_durations(m1, r), scale_durations(m2, r))
    Modify(c, m) -> Modify(c, scale_durations(m, r))
  }
}

// > changeInstrument :: InstrumentName -> Music a -> Music a
// > changeInstrument i m = Modify (Instrument i) $ removeInstruments m
pub fn change_instrument(m: Music(a), i: InstrumentName) -> Music(a) {
  Modify(Instrument(i), remove_instruments(m))
}

// > removeInstruments :: Music a -> Music a
// > removeInstruments (Modify (Instrument i) m) = removeInstruments m
// > removeInstruments (Modify c m) = Modify c $ removeInstruments m
// > removeInstruments (m1 :+: m2) = removeInstruments m1 :+: removeInstruments m2
// > removeInstruments (m1 :=: m2) = removeInstruments m1 :=: removeInstruments m2
// > removeInstruments m = m
pub fn remove_instruments(m: Music(a)) -> Music(a) {
  case m {
    Modify(Instrument(_i), mprim) -> remove_instruments(mprim)
    Modify(c, mprim) -> Modify(c, remove_instruments(mprim))
    Seq(m1, m2) -> Seq(remove_instruments(m1), remove_instruments(m2))
    Par(m1, m2) -> Par(remove_instruments(m1), remove_instruments(m2))
    _ -> m
  }
}
