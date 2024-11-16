//// This module contains a subset from the Haskell module
//// [Codec.Midi](https://hackage.haskell.org/package/HCodecs-0.5/docs/src/Codec-Midi.html)
//// translated into Gleam.

import gleam/list

pub type Midi {
  Midi(file_type: FileType, time_div: TimeDiv, tracks: List(Track(Ticks)))
}

pub type FileType {
  SingleTrack
  MultiTrack
  MultiPattern
}

pub type Track(a) =
  List(#(a, Message))

pub type TimeDiv {
  TicksPerBeat(Int)
  TicksPerSecond(frames_per_second: Int, ticks_per_frame: Int)
}

/// 0 - 15
pub type Channel =
  Int

// microseconds per beat  1 - (2^24 - 1)
pub type Ticks =
  Int

pub type Key =
  Int

// 0 - 127
pub type Velocity =
  Int

// 0 - 127
pub type Pressure =
  Int

// 0 - 127
pub type Preset =
  Int

// 0 - 127
pub type Bank =
  Int

pub type PitchWheel =
  Int

// 0 - (2^14 - 1)
pub type Tempo =
  Int

// microseconds per beat  1 - (2^24 - 1)

pub type Message {
  // Channel Messages
  NoteOff(channel: Channel, key: Key, velocity: Velocity)
  NoteOn(channel: Channel, key: Key, velocity: Velocity)
  KeyPressure(channel: Channel, key: Key, pressure: Pressure)
  ControlChange(channel: Channel, controller_number: Int, controller_value: Int)
  ProgramChange(channel: Channel, preset: Preset)
  ChannelPressure(channel: Channel, pressure: Pressure)
  PitchWheel(channel: Channel, pitch_wheel: PitchWheel)
  // Meta Messages
  SequenceNumber(Int)
  // 0 - (2^16 - 1)
  Text(String)
  Copyright(String)
  TrackName(String)
  InstrumentName(String)
  Lyrics(String)
  Marker(String)
  CuePoint(String)
  ChannelPrefix(Channel)
  ProgramName(String)
  DeviceName(String)
  TrackEnd
  TempoChange(Tempo)
  // 0-23  0-59  0-59  0-30 0-99
  SMPTEOffset(Int, Int, Int, Int, Int)
  // 0-255  0-255   0-255   1-255
  TimeSignature(Int, Int, Int, Int)
  // -7 - 7  0 - 1
  KeySignature(Int, Int)
  Reserved(Int, BitArray)
  // System Exclusive Messages
  // 0xF0 or 0xF7
  Sysex(Int, BitArray)
}

// fromAbsTime :: (Num a) => Track a -> Track a
pub fn from_abs_time(trk: Track(Ticks)) -> Track(Ticks) {
  let #(ts, ms) = list.unzip(trk)
  let #(_, tsprim) = list.map_fold(ts, 0, fn(acc, t) { #(t, t - acc) })
  list.zip(tsprim, ms)
}
