//Set server options, run before start of server.

(
Server.local.options.blockSize = 128;
Server.internal.options.blockSize = 128;
Server.local.options.sampleRate = 96000;
Server.internal.options.sampleRate = 96000;
)


s.boot;
s.plotTree;
s.meter;
s.freqscope;

{WhiteNoise.ar(0.1)}.play

{BrownNoise.ar(0.1)}.play

{Dust.ar(50)}.play

{ Sieve1.ar(LocalBuf.newFrom([10]++({0.75.rand}!10)),MouseX.kr(2,100))*0.2}.play



