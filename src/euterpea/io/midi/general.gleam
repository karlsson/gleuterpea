import euterpea/music.{type InstrumentName}

pub fn to_gm(in: InstrumentName) -> Int {
  case in {
    music.AcousticGrandPiano -> 0
    music.BrightAcousticPiano -> 1
    music.ElectricGrandPiano -> 2
    music.HonkyTonkPiano -> 3
    music.RhodesPiano -> 4
    music.ChorusedPiano -> 5
    music.Harpsichord -> 6
    music.Clavinet -> 7
    music.Celesta -> 8
    music.Glockenspiel -> 9
    music.MusicBox -> 10
    music.Vibraphone -> 11
    music.Marimba -> 12
    music.Xylophone -> 13
    music.TubularBells -> 14
    music.Dulcimer -> 15
    music.HammondOrgan -> 16
    music.PercussiveOrgan -> 17
    music.RockOrgan -> 18
    music.ChurchOrgan -> 19
    music.ReedOrgan -> 20
    music.Accordion -> 21
    music.Harmonica -> 22
    music.TangoAccordion -> 23
    music.AcousticGuitarNylon -> 24
    music.AcousticGuitarSteel -> 25
    music.ElectricGuitarJazz -> 26
    music.ElectricGuitarClean -> 27
    music.ElectricGuitarMuted -> 28
    music.OverdrivenGuitar -> 29
    music.DistortionGuitar -> 30
    music.GuitarHarmonics -> 31
    music.AcousticBass -> 32
    music.ElectricBassFingered -> 33
    music.ElectricBassPicked -> 34
    music.FretlessBass -> 35
    music.SlapBass1 -> 36
    music.SlapBass2 -> 37
    music.SynthBass1 -> 38
    music.SynthBass2 -> 39
    music.Violin -> 40
    music.Viola -> 41
    music.Cello -> 42
    music.Contrabass -> 43
    music.TremoloStrings -> 44
    music.PizzicatoStrings -> 45
    music.OrchestralHarp -> 46
    music.Timpani -> 47
    music.StringEnsemble1 -> 48
    music.StringEnsemble2 -> 49
    music.SynthStrings1 -> 50
    music.SynthStrings2 -> 51
    music.ChoirAahs -> 52
    music.VoiceOohs -> 53
    music.SynthVoice -> 54
    music.OrchestraHit -> 55
    music.Trumpet -> 56
    music.Trombone -> 57
    music.Tuba -> 58
    music.MutedTrumpet -> 59
    music.FrenchHorn -> 60
    music.BrassSection -> 61
    music.SynthBrass1 -> 62
    music.SynthBrass2 -> 63
    music.SopranoSax -> 64
    music.AltoSax -> 65
    music.TenorSax -> 66
    music.BaritoneSax -> 67
    music.Oboe -> 68
    music.EnglishHorn -> 69
    music.Bassoon -> 70
    music.Clarinet -> 71
    music.Piccolo -> 72
    music.Flute -> 73
    music.Recorder -> 74
    music.PanFlute -> 75
    music.BlownBottle -> 76
    music.Shakuhachi -> 77
    music.Whistle -> 78
    music.Ocarina -> 79
    music.Lead1Square -> 80
    music.Lead2Sawtooth -> 81
    music.Lead3Calliope -> 82
    music.Lead4Chiff -> 83
    music.Lead5Charang -> 84
    music.Lead6Voice -> 85
    music.Lead7Fifths -> 86
    music.Lead8BassLead -> 87
    music.Pad1NewAge -> 88
    music.Pad2Warm -> 89
    music.Pad3Polysynth -> 90
    music.Pad4Choir -> 91
    music.Pad5Bowed -> 92
    music.Pad6Metallic -> 93
    music.Pad7Halo -> 94
    music.Pad8Sweep -> 95
    music.FX1Train -> 96
    music.FX2Soundtrack -> 97
    music.FX3Crystal -> 98
    music.FX4Atmosphere -> 99
    music.FX5Brightness -> 100
    music.FX6Goblins -> 101
    music.FX7Echoes -> 102
    music.FX8SciFi -> 103
    music.Sitar -> 104
    music.Banjo -> 105
    music.Shamisen -> 106
    music.Koto -> 107
    music.Kalimba -> 108
    music.Bagpipe -> 109
    music.Fiddle -> 110
    music.Shanai -> 111
    music.TinkleBell -> 112
    music.Agogo -> 113
    music.SteelDrums -> 114
    music.Woodblock -> 115
    music.TaikoDrum -> 116
    music.MelodicDrum -> 117
    music.SynthDrum -> 118
    music.ReverseCymbal -> 119
    music.GuitarFretNoise -> 120
    music.BreathNoise -> 121
    music.Seashore -> 122
    music.BirdTweet -> 123
    music.TelephoneRing -> 124
    music.Helicopter -> 125
    music.Applause -> 126
    music.Gunshot -> 127
    _ -> -1
  }
}

pub fn from_gm(i: Int) -> InstrumentName {
  case i {
    0 -> music.AcousticGrandPiano
    1 -> music.BrightAcousticPiano
    2 -> music.ElectricGrandPiano
    3 -> music.HonkyTonkPiano
    4 -> music.RhodesPiano
    5 -> music.ChorusedPiano
    6 -> music.Harpsichord
    7 -> music.Clavinet
    8 -> music.Celesta
    9 -> music.Glockenspiel
    10 -> music.MusicBox
    11 -> music.Vibraphone
    12 -> music.Marimba
    13 -> music.Xylophone
    14 -> music.TubularBells
    15 -> music.Dulcimer
    16 -> music.HammondOrgan
    17 -> music.PercussiveOrgan
    18 -> music.RockOrgan
    19 -> music.ChurchOrgan
    20 -> music.ReedOrgan
    21 -> music.Accordion
    22 -> music.Harmonica
    23 -> music.TangoAccordion
    24 -> music.AcousticGuitarNylon
    25 -> music.AcousticGuitarSteel
    26 -> music.ElectricGuitarJazz
    27 -> music.ElectricGuitarClean
    28 -> music.ElectricGuitarMuted
    29 -> music.OverdrivenGuitar
    30 -> music.DistortionGuitar
    31 -> music.GuitarHarmonics
    32 -> music.AcousticBass
    33 -> music.ElectricBassFingered
    34 -> music.ElectricBassPicked
    35 -> music.FretlessBass
    36 -> music.SlapBass1
    37 -> music.SlapBass2
    38 -> music.SynthBass1
    39 -> music.SynthBass2
    40 -> music.Violin
    41 -> music.Viola
    42 -> music.Cello
    43 -> music.Contrabass
    44 -> music.TremoloStrings
    45 -> music.PizzicatoStrings
    46 -> music.OrchestralHarp
    47 -> music.Timpani
    48 -> music.StringEnsemble1
    49 -> music.StringEnsemble2
    50 -> music.SynthStrings1
    51 -> music.SynthStrings2
    52 -> music.ChoirAahs
    53 -> music.VoiceOohs
    54 -> music.SynthVoice
    55 -> music.OrchestraHit
    56 -> music.Trumpet
    57 -> music.Trombone
    58 -> music.Tuba
    59 -> music.MutedTrumpet
    60 -> music.FrenchHorn
    61 -> music.BrassSection
    62 -> music.SynthBrass1
    63 -> music.SynthBrass2
    64 -> music.SopranoSax
    65 -> music.AltoSax
    66 -> music.TenorSax
    67 -> music.BaritoneSax
    68 -> music.Oboe
    69 -> music.EnglishHorn
    70 -> music.Bassoon
    71 -> music.Clarinet
    72 -> music.Piccolo
    73 -> music.Flute
    74 -> music.Recorder
    75 -> music.PanFlute
    76 -> music.BlownBottle
    77 -> music.Shakuhachi
    78 -> music.Whistle
    79 -> music.Ocarina
    80 -> music.Lead1Square
    81 -> music.Lead2Sawtooth
    82 -> music.Lead3Calliope
    83 -> music.Lead4Chiff
    84 -> music.Lead5Charang
    85 -> music.Lead6Voice
    86 -> music.Lead7Fifths
    87 -> music.Lead8BassLead
    88 -> music.Pad1NewAge
    89 -> music.Pad2Warm
    90 -> music.Pad3Polysynth
    91 -> music.Pad4Choir
    92 -> music.Pad5Bowed
    93 -> music.Pad6Metallic
    94 -> music.Pad7Halo
    95 -> music.Pad8Sweep
    96 -> music.FX1Train
    97 -> music.FX2Soundtrack
    98 -> music.FX3Crystal
    99 -> music.FX4Atmosphere
    100 -> music.FX5Brightness
    101 -> music.FX6Goblins
    102 -> music.FX7Echoes
    103 -> music.FX8SciFi
    104 -> music.Sitar
    105 -> music.Banjo
    106 -> music.Shamisen
    107 -> music.Koto
    108 -> music.Kalimba
    109 -> music.Bagpipe
    110 -> music.Fiddle
    111 -> music.Shanai
    112 -> music.TinkleBell
    113 -> music.Agogo
    114 -> music.SteelDrums
    115 -> music.Woodblock
    116 -> music.TaikoDrum
    117 -> music.MelodicDrum
    118 -> music.SynthDrum
    119 -> music.ReverseCymbal
    120 -> music.GuitarFretNoise
    121 -> music.BreathNoise
    122 -> music.Seashore
    123 -> music.BirdTweet
    124 -> music.TelephoneRing
    125 -> music.Helicopter
    126 -> music.Applause
    127 -> music.Gunshot
    _ -> music.String
  }
}
