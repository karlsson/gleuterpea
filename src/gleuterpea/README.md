# Gleuterpea
## Sound synthesis part
### Sound synthesis in Gleam

This is an experimental application and a proof of concept attempt to make sound synthesis in Gleam using processes as central part in the architecture and to make it fast enough for real-time play. The code is translated from the Elixir application [`Granulix`](https://github.com/karlsson/granulix).

Gleam makes use of the [Xalsa](https://github.com/karlsson/xalsa) application which expects frames to be in a binary array of 32 bit floats for the C api.
The `gleuterpea/math` module holds a helper function to convert from an Elixir list of floats.
Normally one do not need to create the binary arrays oneself, but instead use some of the gleuterpea/generator/* modules.

*Xalsa only runs under Linux.*

Many modules like the Biquad, Bitcrusher and Moog filters have used the [Synthex](https://github.com/bitgamma/synthex) application as input and been converted to use NIFs.

Gleam uses NIFs for generating and transforming the frames in a similar way as Supercollider (SC) uses UGens. The [gleuterpea_analog_echo](https://github.com/karlsson/gleuterpea/blob/main/src/gleuterpea/filter/sc_analog_echo.gleam) plugin application is translated from the SC [AnalogEcho](https://github.com/supercollider/example-plugins/blob/master/03-AnalogEcho/AnalogEcho.cpp) example as a comparison. The plugin is pulled in in Gleam as a dependency.

NIF resources are created to keep state in the C-code between subsequent calls for frames generation or transformation (filtering etc.). A reference to the resource is passed to the Elixir side for this.

**NOTE:** Since the reference points to a NIF resource that is mutable and holds state, it is not meant to be shared between processes. If doing so it will probably give some interesting sound effects but not the expected ones.

Also, the maximum absolute value that the sound driver accepts before clipping is 1.0 (-1.0 to 1.0).

## Installation

**Checkout from github.**
- git clone https://github.com/karlsson/gleuterpea.git
- gleam build
- cd c_src; make. In order to compile the dependency application xalsa's c code you will need some alsa development libraries to be installed (libasound2-dev).
- gleam docs build. Documentation to get around.

## Configuration

If Xalsa's default card configuration does not work for your sound card(s) you
can add xalsa configuration to the config/config.exs in the Gleam application. Par example:
```elixir
config :xalsa,
    rate: 44100,
    pcms: [{:"hw:PCH,0",
            [channels: 2, period_size: 256, period_buffer_size_ratio: 2]},
           {:"plughw:HDMI,3", [channels: 2]}]
```

## Running

- gleam test. Check the test/gleuterpea_test.gleam script for examples on how to generate sound. The `gleuterpa/examples` directory also has a collection of small synthdefs.

## Acknowledgements

Many thanks to Magnus Johansson at [VEMS](https://vems.nu/vems/) for a gentle
introducton to Electroacoustic music and SuperCollider.
