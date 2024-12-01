//// Types for handling loading of NIFs.
//// The functions need to be in the module using the nifs.
//// 
//// https://www.erlang.org/doc/apps/erts/erlang#load_nif/2
//// https://www.erlang.org/doc/apps/erts/erl_nif
//// 
//// ```
//// import gleam
//// import gleuterpea/nifs.{type ErlangResult, type NifLoadError}
//// pub fn load_nif() -> Result(Nil, #(NifLoadError, String)) {
////   case load_nif_internal("priv/nifname", 0) {
////     nifs.Ok -> gleam.Ok(Nil)
////     nifs.Error(#(nifs.Reload, _)) -> gleam.Ok(Nil)
////     nifs.Error(err) -> gleam.Error(err)
////   }
//// }
//// @external(erlang, "erlang", "load_nif")
//// fn load_nif_internal(nif: String, load_info: Int) -> ErlangResult
//// ``` 

pub type NifLoadError {
  LoadFailed
  BadLib
  Load
  Reload
  Upgrade
  OldCode
}

pub type ErlangResult {
  Ok
  Error(#(NifLoadError, String))
}
