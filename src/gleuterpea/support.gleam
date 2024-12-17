import gleam/result
import gleuterpea/filter/biquad
import gleuterpea/filter/bitcrusher
import gleuterpea/filter/moog
import gleuterpea/filter/sc_analog_echo
import gleuterpea/filter/sc_filter
import gleuterpea/filter/sc_reverb
import gleuterpea/generator/noise
import gleuterpea/generator/osc
import gleuterpea/math
import gleuterpea/nifs

pub fn load_nifs() -> Result(Nil, #(nifs.NifLoadError, String)) {
  use _ <- result.try(math.load_nif())
  use _ <- result.try(biquad.load_nif())
  use _ <- result.try(bitcrusher.load_nif())
  use _ <- result.try(moog.load_nif())
  use _ <- result.try(noise.load_nif())
  use _ <- result.try(osc.load_nif())
  use _ <- result.try(sc_filter.load_nif())
  use _ <- result.try(sc_analog_echo.load_nif())
  sc_reverb.load_nif()
}
