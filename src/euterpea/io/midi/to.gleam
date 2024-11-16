import euterpea/io/midi/codec.{type Message, type Midi}
import euterpea/io/midi/export_file
import euterpea/io/midi/general
import euterpea/io/midi/m_event.{type MEvent}
import euterpea/music.{type InstrumentName, type Music, Percussion}

import gleam/float
import gleam/int
import gleam/list

// import gleam/result

pub type ProgNum =
  Int

pub type Channel =
  Int

pub type UserPatchMap =
  List(#(InstrumentName, Channel))

// > makeGMMap :: [InstrumentName] -> UserPatchMap
// > makeGMMap ins = mkGMMap 0 ins
// >   where mkGMMap _ []        = []
// >         mkGMMap n _ | n>=15 = 
// >                   error "makeGMMap: too many instruments."
// >         mkGMMap n (Percussion : ins)    = 
// >                   (Percussion, 9) : mkGMMap n ins
// >         mkGMMap n (i : ins) = 
// >                   (i, chanList !! n) : mkGMMap (n+1) ins
// >         chanList = [0..8] ++ [10..15]  --  channel 9 is for percussion

pub fn make_gm_map(ins: List(InstrumentName)) -> Result(UserPatchMap, Nil) {
  case
    list.try_fold(ins, #(0, []), fn(acc, in: InstrumentName) {
      make_gm_map1(acc, in)
    })
  {
    Ok(#(_, upm)) -> Ok(upm)
    Error(Nil) -> Error(Nil)
  }
}

fn make_gm_map1(acc, in) {
  case acc, in {
    #(n, _), _ if n >= 15 -> Error(Nil)
    #(n, ins), Percussion -> Ok(#(n, [#(Percussion, 9), ..ins]))
    #(n, ins), in -> Ok(#(n + 1, [#(in, chan_list(n)), ..ins]))
  }
}

fn chan_list(n) -> Int {
  case n > 8 {
    True -> n + 1
    False -> n
  }
}

// > upmLookup :: UserPatchMap  -> InstrumentName 
// >                            -> (Channel, ProgNum)
// > upmLookup upm iName = (chan, toGM iName)
// >   where chan = maybe  (error (  "instrument " ++ show iName ++ 
// >                                 " not in patch map")  )
// >                       id (lookup iName upm)
pub fn upm_lookup(
  upm: UserPatchMap,
  in: InstrumentName,
) -> Result(#(Channel, ProgNum), Nil) {
  case list.key_find(upm, in) {
    Ok(chan) -> Ok(#(chan, general.to_gm(in)))
    Error(Nil) -> Error(Nil)
  }
}

// > toMidi :: [MEvent] -> Midi
// > toMidi = toMidiUPM defUpm
fn to_midi(mes: List(MEvent)) -> Midi {
  mes |> to_midi_upm(def_upm())
}

const division: Int = 96

// > toMidiUPM :: UserPatchMap -> [MEvent] -> Midi
// > toMidiUPM upm pf =
// >    let split     = splitByInst pf
// >        insts     = map fst split
// >        rightMap  =  if (allValid upm insts) then upm
// >                     else (makeGMMap insts)
// >    in Midi  (if length split == 1  then SingleTrack 
// >                                    else MultiTrack)
// >             (TicksPerBeat division)
// >             (map (fromAbsTime . mevsToMessages rightMap) split)
fn to_midi_upm(pf: List(MEvent), upm: UserPatchMap) -> Midi {
  let split = split_by_inst(pf)
  let insts = list.map(split, fn(x) { x.0 })
  let right_map = case all_valid(upm, insts) {
    True -> upm
    False ->
      case make_gm_map(insts) {
        Ok(upm2) -> upm2
        Error(Nil) -> []
      }
  }
  codec.Midi(
    file_type: case list.length(split) == 1 {
      True -> codec.SingleTrack
      False -> codec.MultiTrack
    },
    time_div: codec.TicksPerBeat(division),
    tracks: list.map(split, fn(x) {
      right_map |> mevs_to_messages(x) |> codec.from_abs_time
    }),
  )
}

// > allValid :: UserPatchMap -> [InstrumentName] -> Bool
// > allValid upm = and . map (lookupB upm)
fn all_valid(upm: UserPatchMap, insts: List(InstrumentName)) -> Bool {
  list.fold(insts, True, fn(acc: Bool, in: InstrumentName) {
    acc && { lookup_b(upm, in) }
  })
}

// > lookupB :: UserPatchMap -> InstrumentName -> Bool
// > lookupB upm x = or (map ((== x) . fst) upm)
fn lookup_b(upm: UserPatchMap, x: InstrumentName) -> Bool {
  list.fold(upm, False, fn(acc: Bool, up) { acc || { up.0 == x } })
}

// > splitByInst :: [MEvent] ->  [(InstrumentName, [MEvent])]
// > splitByInst [] = []
// > splitByInst pf = (i, pf1) : splitByInst pf2
// >        where i          = eInst (head pf)
// >              (pf1, pf2) = partition (\e -> eInst e == i) pf
fn split_by_inst(pf: List(MEvent)) -> List(#(InstrumentName, List(MEvent))) {
  case pf {
    [] -> []
    [h, ..] -> {
      let i = h.e_inst
      let #(pf1, pf2) = list.partition(pf, fn(e) { e.e_inst == i })
      [#(i, pf1), ..split_by_inst(pf2)]
    }
  }
}

pub type MidiEvent =
  #(codec.Ticks, Message)

type InmLMEvent =
  #(InstrumentName, List(MEvent))

const def_st = 500_000

// > mevsToMessages ::  UserPatchMap
// >                   -> (InstrumentName, [MEvent]) 
// >                   -> [MidiEvent]
// > mevsToMessages upm (inm, pf) =
// >   let  (chan,progNum)   = upmLookup upm inm
// >        setupInst        = (0, ProgramChange chan progNum)
// >        setTempo         = (0, TempoChange defST)
// >        loop []      =  []
// >        loop (e:es)  =  let (mev1,mev2) = mkMEvents chan e
// >                        in mev1 : insertMEvent mev2 (loop es)
// >   in setupInst : setTempo : loop pf
fn mevs_to_messages(upm: UserPatchMap, inmpf: InmLMEvent) -> List(MidiEvent) {
  let #(inm, pf) = inmpf
  let assert Ok(#(chan, prog_num)) = upm_lookup(upm, inm)
  let setup_inst = #(0, codec.ProgramChange(chan, prog_num))
  let set_tempo = #(0, codec.TempoChange(def_st))
  [setup_inst, set_tempo, ..loop(pf, chan)]
}

fn loop(es: List(MEvent), chan) -> List(MidiEvent) {
  case es {
    [] -> []
    [e, ..es1] -> {
      let #(mev1, mev2) = mk_mevents(chan, e)
      [mev1, ..insert_mevent(mev2, loop(es1, chan))]
    }
  }
}

// > mkMEvents :: Channel -> MEvent -> (MidiEvent,MidiEvent)
// > mkMEvents  mChan (MEvent {  eTime = t, ePitch = p, 
// >                            eDur = d, eVol = v})
// >                   = (  (toDelta t, NoteOn  mChan p v'),
// >                        (toDelta (t+d), NoteOff mChan p v') )
// >            where v' = max 0 (min 127 (fromIntegral v))
fn mk_mevents(m_chan: Channel, e: MEvent) -> #(MidiEvent, MidiEvent) {
  let vprim = int.max(0, int.min(127, e.e_vol))
  #(#(to_delta(e.e_time), codec.NoteOn(m_chan, e.e_pitch, vprim)), #(
    to_delta(e.e_time +. e.e_dur),
    codec.NoteOff(m_chan, e.e_pitch, vprim),
  ))
}

fn to_delta(t) {
  float.round(t *. 2.0 *. int.to_float(division))
}

// > insertMEvent :: MidiEvent -> [MidiEvent] -> [MidiEvent]
// > insertMEvent mev1  []         = [mev1]
// > insertMEvent mev1@(t1,_) mevs@(mev2@(t2,_):mevs') = 
// >       if t1 <= t2 then mev1 : mevs
// >                   else mev2 : insertMEvent mev1 mevs'
fn insert_mevent(mev1: MidiEvent, mevs: List(MidiEvent)) -> List(MidiEvent) {
  case mev1, mevs {
    _, [] -> [mev1]
    #(t1, _), mevs2 -> {
      let assert [mev2, ..mevs2prim] = mevs2
      let #(t2, _) = mev2
      case t1 <= t2 {
        True -> [mev1, ..mevs2]
        False -> [mev2, ..insert_mevent(mev1, mevs2prim)]
      }
    }
  }
}

fn def_upm() -> UserPatchMap {
  [
    #(music.AcousticGrandPiano, 0),
    #(music.Marimba, 1),
    #(music.Vibraphone, 2),
    #(music.AcousticBass, 3),
    #(music.Flute, 4),
    #(music.TenorSax, 5),
    #(music.AcousticGuitarSteel, 6),
    #(music.Viola, 7),
    #(music.StringEnsemble1, 8),
    //  the GM name for drums is unimportant, only channel 9
    #(music.AcousticGrandPiano, 9),
  ]
}

// > writeMidi :: ToMusic1 a => FilePath -> Music a -> IO ()
// > writeMidi fn m = exportMidiFile fn $ toMidi $ perform m
pub fn write_midi(
  m: Music(music.Pitch),
  fpath: export_file.FilePath,
) -> Result(Nil, export_file.ExportMidiFileError) {
  m_event.perform(m) |> to_midi |> export_file.export_midi_file(fpath)
}
//  play :: ToMusic1 a => Music a -> IO ()
//  play = playM . toMidi . perform

//  playM :: Midi -> IO ()
//  playM midi = do
//    initialize
//    (defaultOutput playMidi) midi 
//    terminate
//    return ()
