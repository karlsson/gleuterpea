////   **Low Frequency Oscillator**
////   The lfo module returns a stream of floats() between -1.0 and 1.0.
////   To be used as amplitude/frequency modulating input into audio rate streams.
////   Example, create a 4 Hz frequency modulator between 420 and 460 Hz and use
////   it as input for a sinus oscillator:
//// ```
////       import gleuterpea/util
////       import gleuterpea/generator/lfo
////       import gleuterpea/generator/oscillator.{type Frequency} as osc
////       let fm = lfo.triangle(4) |> lfo.nma(40, 420)
////       # You can have a stream as modulating frequency input for osc
////       sinosc = osc.sin(Varying(fm))
//// ```
////   You can also use the yielder module zip function to insert LFOs,
////   here moving sound between left and right channel every second:
//// ```
////       panmove = lfo.triangle(1.0) |> yielder.map(fn(val) {val *. 0.9})
////       sinosc
////       |> yielder.zip(panmove)
////       |> yelder.map(fn(z) { util.pan(z.0, z.1) } )
//// ```
////   Actually the pan2 function can take a stream directly and so:
////      `sinosc |> util.pan2(panmove)`
////

import gleam/float
import gleam/int
import gleam/option.{type Option}
import gleam/yielder.{type Yielder}
import gleam_community/maths/elementary
import gleuterpea

pub fn sin(frequency: Float) -> Yielder(Float) {
  let ps = int.to_float(gleuterpea.period_size())
  let rate = int.to_float(gleuterpea.rate())
  let twopi = elementary.tau()
  let step = twopi *. frequency *. ps /. rate
  yielder.unfold(0.0, fn(acc) {
    let next = acc +. step
    let next2 = case next >. twopi {
      True -> next -. twopi
      False -> next
    }
    yielder.Next(elementary.sin(acc), next2)
  })
}

pub fn triangle(frequency: Float) -> Yielder(Float) {
  let ps = int.to_float(gleuterpea.period_size())
  let rate = int.to_float(gleuterpea.rate())
  let step = 4.0 *. frequency *. ps /. rate
  yielder.unfold(0.0, fn(acc) {
    let val = case acc <. 2.0 {
      True -> acc -. 1.0
      False -> 3.0 -. acc
    }
    let next1 = acc +. step
    let next2 = case next1 >. 4.0 {
      True -> next1 -. 4.0
      False -> next1
    }
    yielder.Next(val, next2)
  })
}

/// A duty of None will be set to 0.5
pub fn square(frequency: Float, duty: Option(Float)) -> Yielder(Float) {
  let ps = int.to_float(gleuterpea.period_size())
  let rate = int.to_float(gleuterpea.rate())
  let step = frequency *. ps /. rate

  let duty1 = case duty {
    option.Some(d) -> d
    option.None -> 0.5
  }

  let duty = float.min(1.0, float.max(0.0, duty1))
  yielder.unfold(0.0, fn(acc) {
    let val = case acc <. duty {
      True -> 1.0
      False -> -1.0
    }
    let next1 = acc +. step
    let next2 = case next1 >. 1.0 {
      True -> next1 -. 1.0
      False -> next1
    }
    yielder.Next(val, next2)
  })
}

/// Normalize, Multiply, Add
/// Move from -1, 1 range to 0, 1 and then multiply and add offset
pub fn nma(
  yfl: Yielder(Float),
  mul: Float,
  bottom_level: Float,
) -> Yielder(Float) {
  let x = 0.5 *. mul
  yielder.map(yfl, fn(fl) { fl *. x +. bottom_level +. x })
}
