// This is the movement 2
// First we want a noise background using subtractive synthesis

(

SynthDef(\noiseGrain, {
	arg dur = 1, amp = 0.1, attackTime = 0.5, attackCurve = 0, decayCurve = 0,
	inbus, outbus;
	var env, inSignal;
	inSignal = In.ar(inbus, 2);
	env = EnvGen.kr(Env([0,amp,0],[(attackTime * dur),
			((1 - attackTime) * dur)], [attackCurve, decayCurve]), doneAction:2);
	Out.ar(outbus, inSignal * env);
}).add;
)


// Test of the long notes
(
~bws = [0.01,0.001,0.0001,0.00001,0.00000001,0.000000001];

~noiseNotes = (
	noise1: (
		instrument: \invertedSpektrum42,
		startFreqs: [~harmspek[2][[1,7,15,18]]],
		endFreqs: [~harmspek[2][[1,6,12,19]]],
		startBws: [~bws[[2,2,2,2]]],
		endBws: [~bws[[0,0,0,0]]],
		attackCurve: 0,
		decayCurve: 0
	),

	noise2: (
		instrument: \invertedSpektrum63,
		startFreqs: [~harmspek[3][[0,1,2,3,4,5]]],
		endFreqs: [~harmspek[3][[0,1,2,3,4,5]]],
		startBws: [~bws[[0,0,0,0,0,0]]],
		endBws: [~bws[[0,0,0,0,0,0]]],
		attackCurve: 0,
		decayCurve: 0
	),
	noise3: (
		instrument: \invertedSpektrum63,
		startFreqs: [~harmspek[7][[0,1,2,3,4,5]]],
		endFreqs: [~harmspek[7][[0,1,2,3,4,5]]],
		startBws: [~bws[[2,2,2,2,2,2]]],
		endBws: [~bws[[2,2,2,2,2,2]]],
		attackCurve: 0,
		decayCurve: 0
	),

	noise4: (
		instrument: \invertedSpektrum63,
		startFreqs: [~harmspek[0][[0,6,8,10,12,19]]],
		endFreqs: [~harmspek[0][[0,6,8,10,12,19]]],
		startBws: [~bws[[5,5,5,5,5,5]]],
		endBws: [~bws[[5,5,5,5,5,5]]],
		attackCurve: 0,
		decayCurve: 0
	),
	noise5: (
		instrument: \invertedSpektrum63,
		startFreqs: [~harmspek[6][[0,2,4,6,8,5]]],
		endFreqs: [~harmspek[6][[0,2,4,6,8,5]]],
		startBws: [~bws[[2,2,2,2,2,2]]],
		endBws: [~bws[[2,2,2,2,2,2]]],
		attackCurve: 0,
		decayCurve: 0
	),
	harm1: (
		instrument: \spektrum42,
		startFreqs: [~harmspek[2][[1,7,15,18]]],
		endFreqs: [~harmspek[2][[1,6,12,19]]],
		startBws: [~bws[[1,1,1,1]]],
		endBws: [~bws[[3,3,3,3]]],
		attackCurve: 0,
		decayCurve: 0
	)
);

~source = Group.head(s);
~effects = Group.tail(s);
~bus1 = Bus.audio(s, 2);

Synth.tail(~effects, "reverb2", [\mix, 0.08, \room, 0.5, \damp, 0.5, \inbus, ~bus1]);



Ppar([

	Pchain(
		Pdict(~noiseNotes,
			Pseq(#[noise5, noise1, noise2, noise3, noise4])),
		Pbind(
			\group, ~source,
			\outbus, ~bus1,
			\amp, Pseq([0.3], inf),
			#[startPan, endPan], Pseq([[-0.2, 0]], inf),
			#[delta, dur, attackTime], Pseq([[3, 3, 1 - (8/13)], [3, 3, 1 - (8/13)]], 2)
		)
	),
	/*
	Pchain(
		Pdict(~noiseNotes,
			Pseq(#[harm1, harm1])),
		Pbind(
			\group, ~source,
			\outbus, ~bus1,
			\amp, Pseq([0.006], inf),
			#[startPan, endPan], Pseq([[-0.2, 0]], inf),
			#[delta, dur, attackTime],  Pseq([[5, 5, 1 - (8/13)], [5, 5, 1 - (8/13)]], 1)
		)
	)
	*/

]).trace.play;
)








(
~bws = [0.0001,0.00001,0.00000001,0.000000001];
~noiseNotes = (
	noise1: (
		instrument: \invertedSpektrum42,
		startFreqs: [~harmspek[2][[1,7,15,18]]],
		endFreqs: [~harmspek[2][[1,6,12,19]]],
		startBws: [~bws[[2,2,2,2]]],
		endBws: [~bws[[0,0,0,0]]],
		attackCurve: 0,
		decayCurve: 0
	),
	noise2: (
		instrument: \invertedSpektrum63,
		startFreqs: [~harmspek[3][[0,1,2,3,4,5]]],
		endFreqs: [~harmspek[3][[0,1,2,3,4,5]]],
		startBws: [~bws[[0,0,0,0,0,0]]],
		endBws: [~bws[[0,0,0,0,0,0]]],
		attackCurve: 0,
		decayCurve: 0
	),
	noise3: (
		instrument: \invertedSpektrum63,
		startFreqs: [~harmspek[7][[0,1,2,3,4,5]]],
		endFreqs: [~harmspek[7][[0,1,2,3,4,5]]],
		startBws: [~bws[[2,2,2,2,2,2]]],
		endBws: [~bws[[2,2,2,2,2,2]]],
		attackCurve: 0,
		decayCurve: 0
	),
	noise4: (
		instrument: \invertedSpektrum63,
		startFreqs: [~harmspek[0][[0,6,8,10,12,19]]],
		endFreqs: [~harmspek[0][[0,6,8,10,12,19]]],
		startBws: [~bws[[5,5,5,5,5,5]]],
		endBws: [~bws[[5,5,5,5,5,5]]],
		attackCurve: 0,
		decayCurve: 0
	),
	noise5: (
		instrument: \invertedSpektrum63,
		startFreqs: [~harmspek[6][[0,2,4,6,8,5]]],
		endFreqs: [~harmspek[6][[0,2,4,6,8,5]]],
		startBws: [~bws[[2,2,2,2,2,2]]],
		endBws: [~bws[[2,2,2,2,2,2]]],
		attackCurve: 0,
		decayCurve: 0
	),

	harm1: (
		instrument: \spektrum42,
		startFreqs: [~harmspek[2][[1,7,15,18]]],
		endFreqs: [~harmspek[2][[1,6,12,19]]],
		startBws: [~bws[[1,1,1,1]]],
		endBws: [~bws[[3,3,3,3]]],
		attackCurve: 0,
		decayCurve: 0
	),
	grain1: (
		instrument: \noiseGrain,
		attackCurve: 0,
		decayCurve: 0
	),
	grain2: (
		instrument: \noiseGrain,
		attackCurve: 10,
		decayCurve: -10
	),
	grain3: (
		instrument: \noiseGrain,
		attackCurve: -10,
		decayCurve: 10
	)
);

~source = Group.head(s);
~grains = Group.tail(s);
~effects = Group.tail(~grains);
~bus1 = Bus.audio(s, 2);
~bus2 = Bus.audio(s, 2);

Synth.tail(~effects, "reverb2", [\mix, 0.08, \room, 0.5, \damp, 0.5, \inbus, ~bus2]);
//Synth.tail(~effects, "reverb2", [\mix, 0.6, \room, 0.4, \damp, 0.5, \inbus, ~bus2]);


~scaleList = {
	arg len, parts;
	var theSum = parts.sum;
	parts.collect({
		arg item;
		len * (item / theSum);
	});
};

~scaleList2 = {
	arg len, parts;
	var theSum = parts.sum { |i| i[0]};
	parts.collect({
		arg item;
		var tmpLen;
		tmpLen = len * (item[0] / theSum);
		[tmpLen, tmpLen *item[1], item[2]];
	});
};

//1.5, 0.5, 0.2, 0.1, 0.004


~grainPatterns = (
	pattern11: [[2, 1.5, 0.1], [2, 1.5, 0.2], [3, 0.5, 0.3], [21, 0.1, 0.2], [13, 0.004, 0.4]],
	pattern12: [[2, 0.2, 0.3], [2, 0.2, 0.2], [3, 0.1, 0.4], [21, 0.2, 0.3], [13, 0.5, 0.2]],
	pattern21: [[3, 0.5, 0.3], [3, 0.2, 0.2], [13, 0.1, 0.1], [8, 0.004, 0.2]]
);


~makeGrains2 = {
	arg len, parts, patterns;
	var scaledParts = ~scaleList.value(len, parts);
	scaledParts.collect({
		arg item, pos;
		~scaleList2.value(item, ~grainPatterns[patterns[pos]]);
	}).flatten(1);
};


Ppar(
[
	Pchain(
		Pdict(~noiseNotes,
			Pseq(#[noise3, noise5])),
		Pbind(
			\group, ~source,
			\outbus, ~bus1,
			\amp, Pseq([0.3, 0.2], 1),
			#[startPan, endPan], Pseq([[0.9, -0.9], [-0.9, 0.1]], 1),
			#[delta, dur, attackTime], Pseq([[21, 21, 8/13], [13, 13, 1 - (8/13)]], 1)
		)
	),
	Pchain(
		Pdict(~noiseNotes,
			Pseq(#[grain2], inf)),
		Pbind(
			\group, ~grains,
			\inbus, ~bus1,
			\outbus, ~bus2,
			#[delta, dur, amp],
			    Pseq(~makeGrains2.value(21, [5, 8, 1], [\pattern11, \pattern21, \pattern12]) ++
					 ~makeGrains2.value(13, [8, 2, 13], [\pattern21, \pattern12, \pattern11]), 1),
			\attackTime, 8/13,
		)
	)
]).play;
)

// Here we test several long notes at the same time.
(
~bws = [0.01,0.001,0.0001,0.00001,0.00000001,0.000000001];

~noiseNotes = (
	noise1: (
		instrument: \invertedSpektrum42,
		startFreqs: [~harmspek[2][[1,7,15,18]]],
		endFreqs: [~harmspek[2][[1,6,12,19]]],
		startBws: [~bws[[2,2,2,2]]],
		endBws: [~bws[[0,0,0,0]]],
		attackCurve: 0,
		decayCurve: 0
	),

	noise2: (
		instrument: \invertedSpektrum63,
		startFreqs: [~harmspek[3][[0,1,2,3,4,5]]],
		endFreqs: [~harmspek[3][[0,1,2,3,4,5]]],
		startBws: [~bws[[0,0,0,0,0,0]]],
		endBws: [~bws[[0,0,0,0,0,0]]],
		attackCurve: 0,
		decayCurve: 0
	),
	noise3: (
		instrument: \invertedSpektrum63,
		startFreqs: [~harmspek[7][[0,1,2,3,4,5]]],
		endFreqs: [~harmspek[7][[0,1,2,3,4,5]]],
		startBws: [~bws[[2,2,2,2,2,2]]],
		endBws: [~bws[[2,2,2,2,2,2]]],
		attackCurve: 0,
		decayCurve: 0
	),

	noise4: (
		instrument: \invertedSpektrum63,
		startFreqs: [~harmspek[0][[0,6,8,10,12,19]]],
		endFreqs: [~harmspek[0][[0,6,8,10,12,19]]],
		startBws: [~bws[[5,5,5,5,5,5]]],
		endBws: [~bws[[5,5,5,5,5,5]]],
		attackCurve: 0,
		decayCurve: 0
	),
	noise5: (
		instrument: \invertedSpektrum63,
		startFreqs: [~harmspek[6][[0,2,4,6,8,5]]],
		endFreqs: [~harmspek[6][[0,2,4,6,8,5]]],
		startBws: [~bws[[2,2,2,2,2,2]]],
		endBws: [~bws[[2,2,2,2,2,2]]],
		attackCurve: 0,
		decayCurve: 0
	),
	harm1: (
		instrument: \spektrum42,
		startFreqs: [~harmspek[2][[1,7,15,18]]],
		endFreqs: [~harmspek[2][[1,6,12,19]]],
		startBws: [~bws[[1,1,1,1]]],
		endBws: [~bws[[3,3,3,3]]],
		attackCurve: 0,
		decayCurve: 0
	),
	grain1: (
		instrument: \noiseGrain,
		attackCurve: 0,
		decayCurve: 0
	),
	grain2: (
		instrument: \noiseGrain,
		attackCurve: 10,
		decayCurve: -10
	),
	grain3: (
		instrument: \noiseGrain,
		attackCurve: -10,
		decayCurve: 10
	)
);

~scaleList = {
	arg len, parts;
	var theSum = parts.sum;
	parts.collect({
		arg item;
		len * (item / theSum);
	});
};

~scaleList2 = {
	arg len, parts;
	var theSum = parts.sum { |i| i[0]};
	parts.collect({
		arg item;
		var tmpLen;
		tmpLen = len * (item[0] / theSum);
		[tmpLen, tmpLen *item[1], item[2]];
	});
};

//1.5, 0.5, 0.2, 0.1, 0.004

//delta, dur, amp
~grainPatterns = (
	pattern11: [[2, 1.5, 0.1], [2, 1.5, 0.2], [3, 0.5, 0.3], [21, 0.1, 0.2], [13, 0.004, 0.4]],
	pattern12: [[2, 0.2, 0.3], [2, 0.2, 0.2], [3, 0.1, 0.4], [21, 0.2, 0.3], [13, 0.5, 0.2]],
	pattern21: [[3, 0.5, 0.3], [3, 0.2, 0.2], [13, 0.1, 0.1], [8, 0.004, 0.2]],
	pattern31: [[13, 8, 0.3], [8, 0.01, 0.2]],
	pattern41: [[8, 0.01, 0.2], [13, 8, 0.3]]
);


~makeGrains2 = {
	arg len, parts, patterns;
	var scaledParts = ~scaleList.value(len, parts);
	scaledParts.collect({
		arg item, pos;
		~scaleList2.value(item, ~grainPatterns[patterns[pos]]);
	}).flatten(1);
};

~source = Group.head(s);
~grains = Group.tail(s);
~effects = Group.tail(~grains1);
~bus1 = Bus.audio(s, 2);
~bus2 = Bus.audio(s, 2);
~bus3 = Bus.audio(s, 2);
~bus4 = Bus.audio(s, 2);


Synth.tail(~effects, "reverb2", [\mix, 0.08, \room, 0.5, \damp, 0.5, \inbus, ~bus2]);
Synth.tail(~effects, "reverb2", [\mix, 0.2, \room, 0.4, \damp, 0.5, \inbus, ~bus4]);


Ppar([

	Pchain(
		Pdict(~noiseNotes,
			Pseq(#[noise1, noise1])),
		Pbind(
			\group, ~source,
			\outbus, ~bus1,
			\amp, Pseq([0.3, 0.3], inf),
			#[startPan, endPan], Pseq([[1, -5], [-5, -1]], inf),
			#[delta, dur, attackTime], Pseq([[13, 13, 1 - (8/13)], [13, 13, 1 - (8/13)]], 1)
		)
	),

	Pchain(
		Pdict(~noiseNotes,
			Pseq(#[grain2], inf)),
		Pbind(
			\group, ~grains,
			\inbus, ~bus1,
			\outbus, ~bus2,
			#[delta, dur, amp],
			    Pseq(~makeGrains2.value(3, [5, 8, 1], [\pattern11, \pattern21, \pattern12]) ++
				~makeGrains2.value(13, [13], [\pattern31]), 1),
			\attackTime, 8/13,
		)
	),

	Pchain(
		Pdict(~noiseNotes,
			Pseq(#[noise2, noise2])),
		Pbind(
			\group, ~source,
			\outbus, ~bus3,
			\amp, Pseq([0.1, 0.1], inf),
			#[startPan, endPan], Pseq([[-1, 5], [5, -1]], inf),
			#[delta, dur, attackTime],  Pseq([[13, 13, 8/13], [13, 13, 8/13]], 1)
		)
	),

	Pchain(
		Pdict(~noiseNotes,
			Pseq(#[grain2], inf)),
		Pbind(
			\group, ~grains,
			\inbus, ~bus3,
			\outbus, ~bus4,
			#[delta, dur, amp],
			    Pseq(~makeGrains2.value(3, [1, 5, 8], [\pattern12, \pattern11, \pattern21]) ++
				~makeGrains2.value(13, [13], [\pattern41]), 1),
			\attackTime, 8/13,
		)
	),

]).trace.play;
)
