//// Conversion to MEvent datatype.

import euterpea/music.{
  type AbsPitch, type Dur, type InstrumentName, type Music, type Music1,
  type Note1, type NoteAttribute, type PhraseAttribute, type Pitch,
  type Rational, type Volume0, Note1, Params, Volume, m_map,
}

import gleam/float
import gleam/int
import gleam/list

pub type MEvent {
  MEvent(
    /// onset time
    e_time: PTime,
    /// instrument
    e_inst: InstrumentName,
    /// pitch number,
    e_pitch: AbsPitch,
    /// note duration
    e_dur: DurT,
    /// volume
    e_vol: Volume0,
    /// optional other parameters 
    e_params: List(Float),
  )
}

pub type Performance =
  List(MEvent)

pub type PTime =
  Rational

pub type DurT =
  Rational

pub type MContext {
  MContext(
    mc_time: PTime,
    mc_inst: InstrumentName,
    mc_dur: DurT,
    mc_vol: Volume0,
  )
}

// addVolume    :: Volume -> Music Pitch -> Music (Pitch,Volume)
pub fn add_volume1(m: Music(Pitch), v: Volume0) -> Music1 {
  m_map(m, fn(p) { Note1(p, [Volume(v)]) })
}

// merge :: Performance -> Performance -> Performance
// > merge []          es2         =  es2
// > merge es1         []          =  es1
// > merge a@(e1:es1)  b@(e2:es2)  =  
// >   if eTime e1 < eTime e2  then  e1  : merge es1 b
// >                           else  e2  : merge a es2
pub fn merge(es1: Performance, es2: Performance) -> Performance {
  case es1, es2 {
    [], _ -> es2
    _, [] -> es1
    [e1, ..est1], [e2, ..est2] ->
      case e1.e_time <. e2.e_time {
        True -> [e1, ..merge(est1, es2)]
        False -> [e2, ..merge(es1, est2)]
      }
  }
}

/// A new type class to allow for musical polymorphism that ultimately
/// must be converted to Music1 to be converted to MIDI format through
/// the MEvent framework.
// > class ToMusic1 a where
// >     toMusic1 :: Music a -> Music1

// > instance ToMusic1 Pitch where
// >     toMusic1 = mMap (\p -> (p, []))

// > instance ToMusic1 (Pitch, Volume) where
// >     toMusic1  = mMap (\(p, v) -> (p, [Volume v]))

// > instance ToMusic1 (Note1) where
// >     toMusic1 = id

// > instance ToMusic1 (AbsPitch) where
// >     toMusic1 = mMap (\a -> (pitch a, []))

// > instance ToMusic1 (AbsPitch, Volume) where
// >     toMusic1 = mMap (\(p,v) -> (pitch p, [Volume v]))

pub fn to_music1(m: Music(Pitch)) -> Music1 {
  let map_fn = fn(x) {
    case x {
      music.Pitch(p, oct) -> Note1(music.Pitch(p, oct), [])
      music.AbsPitch(i) -> Note1(music.pitch(i), [])
    }
  }
  m_map(m, map_fn)
}

// > perform :: (ToMusic1 a) => Music a -> Performance
// > perform = perform1 . toMusic1

pub fn perform(m: Music(Pitch)) -> Performance {
  m |> to_music1 |> perform1
}

// > perform1 :: Music1 -> Performance
// > perform1 = fst . perform1Dur
pub fn perform1(m: Music1) -> Performance {
  perform1_dur(m).0
}

// > perform1Dur :: Music1 -> (Performance, DurT)
// > perform1Dur = musicToMEvents defCon . applyControls where
// >     defCon  = MContext {mcTime = 0, mcInst = AcousticGrandPiano, mcDur = metro 120 qn, mcVol=127}
// >     -- timing musicToMEventss
// >     metro :: Int -> Dur -> DurT
// >     metro setting dur  = 60 / (fromIntegral setting * dur)
fn metro(setting: Int, dur: Dur) -> DurT {
  let dv = int.to_float(setting) *. dur
  60.0 /. dv
}

pub fn perform1_dur(m: Music1) -> #(Performance, DurT) {
  let def_con =
    MContext(
      mc_time: 0.0,
      mc_inst: music.AcousticGrandPiano,
      mc_dur: metro(120, music.nd(music.Qn)),
      mc_vol: 127,
    )
  m |> apply_controls |> music_to_mevents(def_con)
}

// > applyControls :: Music1 -> Music1
// > applyControls (Modify (Tempo r) m) = scaleDurations r $ applyControls m
// > applyControls (Modify (Transpose k) m) = shiftPitches1 k $ applyControls m
// > applyControls (Modify x m) = Modify x $ applyControls m
// > applyControls (m1 :+: m2) = applyControls m1 :+: applyControls m2
// > applyControls (m1 :=: m2) = applyControls m1 :=: applyControls m2
// > applyControls x = x
pub fn apply_controls(m: Music1) -> Music1 {
  case m {
    music.Modify(mprim, music.Tempo(r)) ->
      music.scale_durations(apply_controls(mprim), r)
    music.Modify(mprim, music.Transpose(k)) ->
      music.shift_pitches1(k)(apply_controls(mprim))
    music.Modify(mprim, x) -> music.Modify(apply_controls(mprim), x)
    music.Seq(m1, m2) -> music.Seq(apply_controls(m1), apply_controls(m2))
    music.Par(m1, m2) -> music.Par(apply_controls(m1), apply_controls(m2))
    _ -> m
  }
}

// > musicToMEvents :: MContext -> Music1 -> (Performance, DurT)
// > musicToMEvents c@MContext{mcTime=t, mcDur=dt} (Prim (Note d p)) = ([noteToMEvent c d p], d*dt)
// > musicToMEvents c@MContext{mcTime=t, mcDur=dt}  (Prim (Rest d)) = ([], d*dt)
// > musicToMEvents c@MContext{mcTime=t, mcDur=dt} (m1 :+: m2) = 
// >     let (evs1, d1) = musicToMEvents c m1
// >         (evs2, d2) = musicToMEvents c{mcTime = t+d1} m2
// >     in  (evs1 ++ evs2, d1+d2)
// > musicToMEvents c@MContext{mcTime=t, mcDur=dt} (m1 :=: m2) = 
// >     let (evs1, d1) = musicToMEvents c m1
// >         (evs2, d2) = musicToMEvents c m2
// >     in  (merge evs1 evs2, max d1 d2)
// > musicToMEvents c (Modify (Instrument i) m) = musicToMEvents c{mcInst=i} m
// > musicToMEvents c (Modify (Phrase pas) m) = phraseToMEvents c pas m
// > musicToMEvents c (Modify (KeySig x y) m) = musicToMEvents c m -- KeySig causes no change
// > musicToMEvents c (Modify (Custom x) m) = musicToMEvents c m -- Custom cuases no change
// > musicToMEvents c m@(Modify x m') = musicToMEvents c $ applyControls m -- Transpose and Tempo addressed by applyControls

pub fn music_to_mevents(m: Music1, c: MContext) -> #(Performance, DurT) {
  case c, m {
    MContext(mc_dur: dt, ..), music.Prim(music.Note(d, p)) -> {
      #([note_to_mevent(c, d, p)], d *. dt)
    }
    MContext(mc_dur: dt, ..), music.Prim(music.Rest(d)) -> #([], d *. dt)
    MContext(mc_time: t, ..), music.Seq(m1, m2) -> {
      let #(evs1, d1) = music_to_mevents(m1, c)
      let #(evs2, d2) = music_to_mevents(m2, MContext(..c, mc_time: t +. d1))
      #(list.append(evs1, evs2), d1 +. d2)
    }
    _, music.Par(m1, m2) -> {
      let #(evs1, d1) = music_to_mevents(m1, c)
      let #(evs2, d2) = music_to_mevents(m2, c)
      #(merge(evs1, evs2), float.max(d1, d2))
    }
    _, music.Modify(mprim, ctrl) ->
      case ctrl {
        music.Instrument(i) ->
          music_to_mevents(mprim, MContext(..c, mc_inst: i))
        music.Phrase(pas) -> phrase_to_mevents(mprim, c, pas)
        music.KeySig(_, _) -> music_to_mevents(mprim, c)
        music.Custom(_) -> music_to_mevents(mprim, c)
        _ -> music_to_mevents(apply_controls(m), c)
      }
  }
}

// > noteToMEvent :: MContext -> Dur -> (Pitch, [NoteAttribute]) -> MEvent
// > noteToMEvent c@(MContext ct ci cdur cvol) d (p, nas) = 
// >     let e0 = MEvent{eTime=ct, ePitch=absPitch p, eInst=ci, eDur=d*cdur, eVol=cvol, eParams=[]}
// >     in  foldr nasFun e0 nas where
// >     nasFun :: NoteAttribute -> MEvent -> MEvent
// >     nasFun (Volume v) ev = ev {eVol = v}
// >     nasFun (Params pms) ev = ev {eParams = pms}
// >     nasFun _ ev = ev

// Note Attributes
fn nas_fun(ev: MEvent, na: NoteAttribute) -> MEvent {
  case na {
    Volume(v) -> MEvent(..ev, e_vol: v)
    Params(pms) -> MEvent(..ev, e_params: pms)
    _ -> ev
  }
}

pub fn note_to_mevent(c: MContext, d: Dur, pnas: Note1) -> MEvent {
  let MContext(ct, ci, cdur, cvol) = c
  let Note1(p, nas) = pnas
  let e0 =
    MEvent(
      e_time: ct,
      e_pitch: music.abs_pitch(p),
      e_inst: ci,
      e_dur: d *. cdur,
      e_vol: cvol,
      e_params: [],
    )
  list.fold_right(nas, e0, nas_fun)
}

// > phraseToMEvents :: MContext -> [PhraseAttribute] -> Music1 -> (Performance, DurT)
// > phraseToMEvents c [] m = musicToMEvents c m
// > phraseToMEvents c@MContext{mcTime=t, mcInst=i, mcDur=dt} (pa:pas) m =
// >  let  pfd@(pf,dur)  =  phraseToMEvents c pas m
// >       loud x        =  phraseToMEvents c (Dyn (Loudness x) : pas) m
// >       stretch x     =  let  t0 = eTime (head pf);  r  = x/dur
// >                             upd (e@MEvent {eTime = t, eDur = d}) = 
// >                               let  dt  = t-t0
// >                                    t'  = (1+dt*r)*dt + t0
// >                                    d'  = (1+(2*dt+d)*r)*d
// >                               in e {eTime = t', eDur = d'}
// >                        in (map upd pf, (1+x)*dur)
// >       inflate x     =  let  t0  = eTime (head pf);  
// >                             r   = x/dur
// >                             upd (e@MEvent {eTime = t, eVol = v}) = 
// >                                 e {eVol =  round ( (1+(t-t0)*r) * 
// >                                            fromIntegral v)}
// >                        in (map upd pf, dur)
// >  in case pa of
// >    Dyn (Accent x) ->
// >        ( map (\e-> e {eVol = round (x * fromIntegral (eVol e))}) pf, dur)
// >    Dyn (StdLoudness l) -> 
// >        case l of 
// >           PPP  -> loud 40;       PP -> loud 50;   P    -> loud 60
// >           MP   -> loud 70;       SF -> loud 80;   MF   -> loud 90
// >           NF   -> loud 100;      FF -> loud 110;  FFF  -> loud 120
// >    Dyn (Loudness x)     ->  phraseToMEvents c{mcVol = round x} pas m
// >    Dyn (Crescendo x)    ->  inflate   x ; Dyn (Diminuendo x)  -> inflate (-x)
// >    Tmp (Ritardando x)   ->  stretch   x ; Tmp (Accelerando x) -> stretch (-x)
// >    Art (Staccato x)     ->  (map (\e-> e {eDur = x * eDur e}) pf, dur)
// >    Art (Legato x)       ->  (map (\e-> e {eDur = x * eDur e}) pf, dur)
// >    Art (Slurred x)      -> 
// >        let  lastStartTime  = foldr (\e t -> max (eTime e) t) 0 pf
// >             setDur e       =   if eTime e < lastStartTime
// >                                then e {eDur = x * eDur e}
// >                                else e
// >        in (map setDur pf, dur) 
// >    Art _                -> pfd -- not supported
// >    Orn _                -> pfd -- not supported

pub fn phrase_to_mevents(
  m: Music1,
  c: MContext,
  pas: List(PhraseAttribute),
) -> #(Performance, DurT) {
  case c, pas, m {
    _, [], _ -> music_to_mevents(m, c)
    _, [pa, ..pas_prim], _ -> {
      let pfd = phrase_to_mevents(m, c, pas_prim)
      let #(pf, dur) = pfd
      let loud = fn(x: Rational) {
        phrase_to_mevents(m, c, [music.Dyn(music.Loudness(x)), ..pas_prim])
      }
      let stretch = fn(x) {
        let assert Ok(e0) = list.first(pf)
        let t0 = e0.e_time
        let r = x /. dur
        let upd = fn(e: MEvent) {
          let dt = e.e_time -. t0
          let tprim = { 1.0 +. dt } *. dt +. t0
          let dprim = { 1.0 +. { 2.0 *. dt +. e.e_dur } *. r } *. e.e_dur
          MEvent(..e, e_time: tprim, e_dur: dprim)
        }
        #(list.map(pf, upd), { 1.0 +. x } *. dur)
      }
      let inflate = fn(x) {
        let assert Ok(e0) = list.first(pf)
        let t0 = e0.e_time
        let r = x /. dur
        let upd = fn(e: MEvent) {
          MEvent(
            ..e,
            e_vol: float.round(
              { 1.0 +. { e.e_time -. t0 } *. r } *. int.to_float(e.e_vol),
            ),
          )
        }
        #(list.map(pf, upd), dur)
      }
      case pa {
        music.Dyn(music.Accent(x)) -> #(
          list.map(pf, fn(e: MEvent) {
            MEvent(..e, e_vol: float.round(x *. int.to_float(e.e_vol)))
          }),
          dur,
        )
        music.Dyn(music.StdLoudness(l)) ->
          case l {
            music.PPP -> loud(40.0)
            music.PP -> loud(50.0)
            music.P -> loud(60.0)
            music.MP -> loud(70.0)
            music.SF -> loud(80.0)
            music.MF -> loud(90.0)
            music.NF -> loud(100.0)
            music.FF -> loud(110.0)
            music.FFF -> loud(120.0)
          }
        music.Dyn(music.Loudness(x)) ->
          phrase_to_mevents(m, MContext(..c, mc_vol: float.round(x)), pas_prim)
        music.Dyn(music.Crescendo(x)) -> inflate(x)
        music.Dyn(music.Diminuendo(x)) -> inflate(0.0 -. x)
        music.Tmp(music.Ritardando(x)) -> stretch(x)
        music.Tmp(music.Accelerando(x)) -> stretch(0.0 -. x)
        music.Art(music.Staccato(x)) -> #(
          list.map(pf, fn(e: MEvent) { MEvent(..e, e_dur: { x *. e.e_dur }) }),
          dur,
        )
        music.Art(music.Legato(x)) -> #(
          list.map(pf, fn(e: MEvent) { MEvent(..e, e_dur: { x *. e.e_dur }) }),
          dur,
        )
        music.Art(music.Slurred(x)) -> {
          let last_start_time =
            list.fold_right(pf, 0.0, fn(t, e) { float.max(e.e_time, t) })
          let set_dur = fn(e: MEvent) {
            case e.e_time <. last_start_time {
              True -> MEvent(..e, e_dur: x *. e.e_dur)
              False -> e
            }
          }
          #(list.map(pf, set_dur), dur)
        }
        music.Art(_) -> pfd
        music.Orn(_) -> pfd
      }
    }
  }
}
