(
f = { |msg, time, replyAddr, recvPort|
    if(msg[0] != '/status.reply') {
        "At time %s received message % \n".postf( time, msg)
    }
};
thisProcess.addOSCRecvFunc(f);
);

OSCFunc.trace(false);

// Set up the groups
(
Group.head(0); //1000
Group.head(1000); //1001
Group.head(1001); //1002


Group.tail(0); //1003
Group.head(1003); //1004
Group.head(1004); //1005

Group.tail(0); //1006
Group.head(1006); //1007
Group.head(1007); //1008
)

// First comes the instruments
(
	SynthDef(\reverb, {
		arg outbus = 0, mix = 0.25, room = 0.15, damp = 0.5, inbus = 0;
		var inSignal, outSignal;
		inSignal = In.ar(inbus, 2);
		outSignal = FreeVerb2.ar(inSignal[0],inSignal[1],mix,room,damp);
		ReplaceOut.ar(outbus, outSignal);
	}).add;

    SynthDef(\reverb2, {
		arg outbus = 0, mix = 0.25, room = 0.15, damp = 0.5, inbus;
		var inSignal, outSignal;
		inSignal = In.ar(inbus, 2);
		outSignal = FreeVerb2.ar(inSignal[0],inSignal[1],mix,room,damp);
		//ReplaceOut.ar(out, outSignal);
		Out.ar(outbus, outSignal);
	}).add;

	SynthDef(\spektrum4, {
		arg dur = 2, startFreqs = #[0,0,0,0], endFreqs = #[0,0,0,0], amp = 0.1, outbus = 0,
		startBws = #[0.000001,0.000001,0.000001,0.000001],
		endBws = #[0.000001,0.000001,0.000001,0.000001],
		attackTime = 0.5, attackCurve = 0, decayCurve = 0, startPan = 0, endPan = 0;
		var env, noise, filt;
		env = EnvGen.kr(Env([0,amp,0],[(attackTime * dur),
			((1 - attackTime) * dur)], [attackCurve, decayCurve]), doneAction:2);
		noise = WhiteNoise.ar(env);
		filt = noise;
		startFreqs.do({
			arg startFreq, i;
			var bwenv =
			EnvGen.kr(Env([startBws[i] * startFreq, endBws[i] * endFreqs[i]], [dur])),
			freqenv = EnvGen.kr(Env([startFreq, endFreqs[i]], [dur]));
			filt = BPF.ar(filt, freqenv, bwenv);
		});
		filt = Balance.ar(filt, noise);
		Out.ar(outbus, Pan2.ar(filt, EnvGen.kr(Env([startPan, endPan], [dur]))));
	}).add;

    SynthDef(\spektrum42, {
		arg dur = 2, startFreqs = #[0,0,0,0], endFreqs = #[0,0,0,0], amp = 0.1, outbus,
		startBws = #[0.000001,0.000001,0.000001,0.000001],
		endBws = #[0.000001,0.000001,0.000001,0.000001],
		attackTime = 0.5, attackCurve = 0, decayCurve = 0, startPan = 0, endPan = 0;
		var env, noise, filt;
		env = EnvGen.kr(Env([0,amp,0],[(attackTime * dur),
			((1 - attackTime) * dur)], [attackCurve, decayCurve]), doneAction:2);
		noise = WhiteNoise.ar(env);
		filt = noise;
		startFreqs.do({
			arg startFreq, i;
			var bwenv =
			EnvGen.kr(Env([startBws[i] * startFreq, endBws[i] * endFreqs[i]], [dur])),
			freqenv = EnvGen.kr(Env([startFreq, endFreqs[i]], [dur]));
			filt = BPF.ar(filt, freqenv, bwenv);
		});
		filt = Balance.ar(filt, noise);
		Out.ar(outbus, Pan2.ar(filt, EnvGen.kr(Env([startPan, endPan], [dur]))));
	}).add;

	SynthDef(\invertedSpektrum4, {
		arg dur = 2, startFreqs = #[0,0,0,0], endFreqs = #[0,0,0,0], amp = 0.1, outbus = 0,
		startBws = #[0.000001,0.000001,0.000001,0.000001],
		endBws = #[0.000001,0.000001,0.000001,0.000001],
		attackTime = 0.5, attackCurve = 0, decayCurve = 0, startPan = 0, endPan = 0;
		var env, noise, filt;
		env = EnvGen.kr(Env([0,amp,0],[(attackTime * dur),
			((1 - attackTime) * dur)], [attackCurve, decayCurve]), doneAction:2);
		noise = WhiteNoise.ar(env);
		filt = noise;
		startFreqs.do({
			arg startFreq, i;
			var bwenv =
			EnvGen.kr(Env([startBws[i] * startFreq, endBws[i] * endFreqs[i]], [dur])),
			freqenv = EnvGen.kr(Env([startFreq, endFreqs[i]], [dur]));
			filt = BRF.ar(filt, freqenv, bwenv);
		});
		filt = Balance.ar(filt, noise);
		Out.ar(outbus, Pan2.ar(filt, EnvGen.kr(Env([startPan, endPan], [dur]))));
	}).add;

    SynthDef(\invertedSpektrum42, {
		arg dur = 2, startFreqs = #[0,0,0,0], endFreqs = #[0,0,0,0], amp = 0.1, outbus,
		startBws = #[0.000001,0.000001,0.000001,0.000001],
		endBws = #[0.000001,0.000001,0.000001,0.000001],
		attackTime = 0.5, attackCurve = 0, decayCurve = 0, startPan = 0, endPan = 0;
		var env, noise, filt;
		env = EnvGen.kr(Env([0,amp,0],[(attackTime * dur),
			((1 - attackTime) * dur)], [attackCurve, decayCurve]), doneAction:2);
		noise = WhiteNoise.ar(env);
		filt = noise;
		startFreqs.do({
			arg startFreq, i;
			var bwenv =
			EnvGen.kr(Env([startBws[i] * startFreq, endBws[i] * endFreqs[i]], [dur])),
			freqenv = EnvGen.kr(Env([startFreq, endFreqs[i]], [dur]));
			filt = BRF.ar(filt, freqenv, bwenv);
		});
		filt = Balance.ar(filt, noise);
		Out.ar(outbus, Pan2.ar(filt, EnvGen.kr(Env([startPan, endPan], [dur]))));
	}).add;


/*
		~theList[0].postln;
(~theList.size - 2).do({
	arg val;
    ~theList[(val + 1)].postln;
});
~theList[~theList.size - 1].postln;
	*/

    SynthDef(\invertedSpektrum63, {
		arg dur = 2, startFreqs = #[0,0,0,0,0,0], endFreqs = #[0,0,0,0,0,0], amp = 0.1, outbus,
		startBws = #[0.000001,0.000001,0.000001,0.000001,0.000001,0.000001],
		endBws = #[0.000001,0.000001,0.000001,0.000001,0.000001,0.000001],
		attackTime = 0.5, attackCurve = 0, decayCurve = 0, startPan = 0, endPan = 0;
		var env, noise, filt, freqenv;
		env = EnvGen.kr(Env([0,amp,0],[(attackTime * dur),
			((1 - attackTime) * dur)], [attackCurve, decayCurve]), doneAction:2);
		noise = WhiteNoise.ar(env);
		filt = noise;

		freqenv = EnvGen.kr(Env([startFreqs[0], endFreqs[0]], [dur]));
		filt = HPF.ar(filt, freqenv);

		(startFreqs.size - 2).do({
			arg val;
			var pos = val + 1;
			var bwenv =
			EnvGen.kr(Env([startBws[pos] * startFreqs[pos], endBws[pos] * endFreqs[pos]], [dur])),
			freqenv = EnvGen.kr(Env([startFreqs[pos], endFreqs[pos]], [dur]));
			filt = BRF.ar(filt, freqenv, bwenv);
		});
		freqenv = EnvGen.kr(Env([startFreqs[startFreqs.size - 1],
			endFreqs[startFreqs.size - 1]], [dur]));
		filt = LPF.ar(filt, freqenv);

		filt = Balance.ar(filt, noise);
		Out.ar(outbus, Pan2.ar(filt, EnvGen.kr(Env([startPan, endPan], [dur]))));
	}).add;



SynthDef(\noiseGrain, {
	arg dur = 1, amp = 0.1, attackTime = 0.5, attackCurve = 0, decayCurve = 0,
	inbus, outbus;
	var env, inSignal;
	inSignal = In.ar(inbus, 2);
	env = EnvGen.kr(Env([0,amp,0],[(attackTime * dur),
			((1 - attackTime) * dur)], [attackCurve, decayCurve]), doneAction:2);
	Out.ar(outbus, inSignal * env);
}).add;

SynthDef(\noiseGrain2, {
	arg dur = 1, amp = 0.1, attackTime = 0.5, attackType = \linear,
	inbus, outbus;
	var env, inSignal;
	inSignal = In.ar(inbus, 2);
	env = EnvGen.kr(Env([0.00001,amp,0.00001],[(attackTime * dur),
			((1 - attackTime) * dur)], attackType), doneAction:2);
	Out.ar(outbus, inSignal * env);
}).add;
)


















