# gleuterpea

## What this is
In an attempt to learn Gleam I have tried to translate a subset of the Haskell Euterpea
library that accompanies the book ["The Haskell School of Music"](https://www.cambridge.org/core/books/haskell-school-of-music/6B377BCD40386E9D27EB93FC2F3B13FB) by Paul Hudak and Donya Quick

## Development

```sh
gleam run   # Run the main function in the gleuterpea module and create MIDI files.
```
## Observations
I have left some parts of the Haskell original code as a comparison in the comments.

In general the transposing "distance" between Haskell and Gleam is not too far.

Gleam does not support polymorphism as Haskell do with `class` and `instance`, which was used when converting the Music type to Music1 type in `m_event.gleam`. This was (partly) fixed by going back and refine the Music type itself to be more generic, which is a bit interesting as it gives a hint that, if you define/refine your types carefully, you may not need all the "bells and whistles" provided in other languages.

The `BitArray` and the bit syntax provided by Erlang is really a bonus when encoding to Midi format. Here one can see how the Haskell implementation has to struggle converting between bytes and bits in the `export_file.gleam` module.

The "pipes" in Gleam can to some extent replace the function composition (".") and infix operations in Haskell, one may have to change the parameter order in the function calls though.

## Todos
Refine the `Music` or `Pitch` type more to support extra parameters.\
Make the `Rational` type to use the [frac](https://hexdocs.pm/frac/) library instead of just aliasing `Float`.\
Learn how to use "`use`" in Gleam.

## References
- [1] [Euterpea2](https://github.com/Euterpea/Euterpea2) - Haskell library for Euterpea. [License](https://github.com/Euterpea/Euterpea2/blob/master/License)
- [2] [HSoM](https://github.com/Euterpea/HSoM) - Examples to accompany the Haskell School of Music textbook. [License](https://github.com/Euterpea/HSoM/blob/master/License)
- [3] [Codec.Midi](https://hackage.haskell.org/package/HCodecs-0.5/docs/src/Codec-Midi.html) - Haskell library for Midi encoding.