import gleam
import gleuterpea/nifs.{type ErlangResult, type NifLoadError}

pub fn load_nif() -> Result(Nil, #(NifLoadError, String)) {
  case load_nif_internal("priv/gleuterpea_math", 0) {
    nifs.Ok -> gleam.Ok(Nil)
    nifs.Error(#(nifs.Reload, _)) -> gleam.Ok(Nil)
    nifs.Error(err) -> gleam.Error(err)
  }
}

/// https://www.erlang.org/doc/apps/erts/erlang#load_nif/2
/// https://www.erlang.org/doc/apps/erts/erl_nif
@external(erlang, "erlang", "load_nif")
fn load_nif_internal(nif: String, load_info: Int) -> ErlangResult

///   Multiply a binary of 32 bit floats with a scalar value
pub fn mul(_x: BitArray, _y: Float) -> BitArray {
  panic as "NIF mul/2 not loaded"
}

///   Multiply two binaries of 32 bit floats with each other 
pub fn cross(_x: BitArray, _y: BitArray) -> BitArray {
  panic as "NIF cross/2 not loaded"
}

///   Add a binary of 32 bit floats with a scalar value
pub fn add(_x: BitArray, _y: Float) -> BitArray {
  panic as "NIF add/2 not loaded"
}

///   Add a two binaries of 32 bit floats with each other
pub fn add2(_x: BitArray, _y: BitArray) -> BitArray {
  panic as "NIF add/2 not loaded"
}

/// Subtract second binary array of 32 bit floats from the first
pub fn subtract2(_x: BitArray, _y: BitArray) {
  panic as "NIF subtract/2 not loaded"
}

/// Convert a list of (Erlang) floats to a binary of 32 bit (C) floats
pub fn float_list_to_binary(_x: List(Float)) -> BitArray {
  panic as "NIF float_list_to_binary/1 not loaded"
}

///  Convert a binary of 32 bit (C) floats to a list of (Erlang) floats
pub fn binary_to_float_list(_x: BitArray) -> List(Float) {
  panic as "NIF binary_to_float_list/1 not loaded"
}
