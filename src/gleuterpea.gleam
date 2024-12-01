////   gleuterpea - sound synthesis in gleam.
//// 
////   *Note:* The application currently uses the Linux adapter Xalsa and
////   so it can only be run under Linux.

import euterpea/play_list
import gleam/erlang/process
import gleam/io

pub type SimpleErlang {
  Ok
}

/// The sample rate is set in the application environment
///  (config file for the application).
// pub type Rate {
//   R44100
//   R48000
//   R96000
//   R192000
// }
pub type Rate =
  Int

/// Message to notify that ALSA driver is ready for more frames.
pub type Ready4more {
  Ready4more
}

/// Opaque binary array of native-float-32 of size period_size
/// <<_:period_size()/native-float-size(32)>>
/// The frames are created by "generator" NIFs in gleuterpea/generator 
// pub opaque type Frame {
//   Frame(BitArray)
// }
pub type Frame =
  BitArray

/// Frames are sent to a Xalsa channel ranging 1..no_of_channels()
pub type ChannelNo =
  Int

/// The sender can request a notification from the cannel when to
/// start adding new frames. A Ready4more message will be sent.
pub type NotifyFlag =
  Bool

//   @type frames_tuple2() :: {frames(), channel_no()}
//   @type frames_tuple3() :: {frames(), channel_no(), notify_flag()}
//   @type frames_tuple4() :: {frames(), channel_no(), notify_flag(), from :: pid()}
//   @type out_type() :: frames() | frames_tuple2() | frames_tuple3() | frames_tuple4()

pub type Out {
  FTP2(f: Frame, c: ChannelNo)
  FTP3(f: Frame, c: ChannelNo, nf: NotifyFlag)
  FTP4(f: Frame, c: ChannelNo, nf: NotifyFlag, p: process.Pid)
}

pub fn main() {
  io.println("Hello from gleuterpea!")
  play_list.play_all()
}

@external(erlang, "Elixir.Xalsa", "no_of_channels")
pub fn no_of_channels() -> Int

/// Return the number of frames that the ALSA driver
/// consumes per callback cycle.
@external(erlang, "Elixir.Xalsa", "period_size")
pub fn period_size() -> Int

@external(erlang, "Elixir.Xalsa", "rate")
pub fn rate() -> Rate

@external(erlang, "Elixir.Xalsa", "send_frames")
fn send_frames(
  frames: BitArray,
  channel: Int,
  notify: Bool,
  from: process.Pid,
) -> SimpleErlang

/// Send frames to output.
/// Sends frames in a binary array of frame:32/float-native.
/// If the `notify` flag is true a :ready4more message will be sent to the
/// process in the `from` argument when the process frames are due to be consumed
/// within 5-10 ms. This so that the process may synthesize/produce more frames
pub fn out(o: Out) -> SimpleErlang {
  let p = process.self()
  case o {
    FTP2(f, c) -> send_frames(f, c, False, p)
    FTP3(f, c, nf) -> send_frames(f, c, nf, p)
    FTP4(f, c, nf, p) -> send_frames(f, c, nf, p)
  }
}

/// Blocking wait for ALSA to be ready for more frames.
@external(erlang, "Elixir.Xalsa", "wait_ready4more")
pub fn wait_ready4more() -> SimpleErlang
//   @doc "Return backend module. Defined in env variable backend_api"
//   @spec api() :: atom()
//   def api() do
//     Application.get_env(:granulix, :backend_api)
//   end
// end
