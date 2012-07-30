//First define the notes
(
~bws = [0.0001,0.00001,0.00000001,0.000000001];
~shortNotes = (
	shortMetal: ( // duration 0.1
		instrument: \spektrum42,
		startFreqs: [~harmspek[4][[5,10,15,19]]],
		endFreqs: [~harmspek[4][[5,10,15,19]]],
		startBws: [#[0.000001,0.000001,0.000001,0.000001]], 
		endBws: [#[0.000001,0.000001,0.000001,0.000001]],
		attackCurve: 10,
		decayCurve: -10
	),
	shortMetal2: ( // duration 0.1
		instrument: \spektrum42,
		startFreqs: [~harmspek[3][[5,10,15,19]]],
		endFreqs: [~harmspek[3][[5,10,15,19]]],
		startBws: [#[0.000001,0.000001,0.000001,0.000001]], 
		endBws: [#[0.000001,0.000001,0.000001,0.000001]],
		attackCurve: 10,
		decayCurve: -10
	),
	shortMetal3: ( // duration 0.1
		//attackTime: 0.1,
		instrument: \spektrum42,
		startFreqs: [~harmspek[5][[5,10,15,19]]],
		endFreqs: [~harmspek[5][[5,10,15,19]]],
		startBws: [#[0.000001,0.000001,0.000001,0.000001]], 
		endBws: [#[0.000001,0.000001,0.000001,0.000001]],
		attackCurve: 10,
		decayCurve: -10
	),
	shortHighSnap: ( // duration 0.017
		instrument: \spektrum42,
		startFreqs: [~phispek[2][[10,13,14,15]]],
		endFreqs: [~phispek[2][[10,13,14,15]]],
		startBws: [#[0.000001,0.000001,0.000001,0.000001]], 
		endBws: [#[0.000001,0.000001,0.000001,0.000001]],
		attackCurve: 0,
		decayCurve: 0
	),
	shortHighSnap2: ( // duration 0.017
		instrument: \spektrum42,
		startFreqs: [~phispek[1][[3,13,14,19]]],
		endFreqs: [~phispek[1][[3,13,14,19]]],
		startBws: [#[0.000001,0.000001,0.000001,0.000001]], 
		endBws: [#[0.000001,0.000001,0.000001,0.000001]],
		attackCurve: 0,
		decayCurve: 0
	),
	shortHighSnap3: ( // duration 0.017
		//attackTime: 0.6,
		instrument: \spektrum42,
		startFreqs: [~phispek[3][[2,5,6,7]]],
		endFreqs: [~phispek[3][[2,5,6,7]]],
		startBws: [#[0.000001,0.000001,0.000001,0.000001]], 
		endBws: [#[0.000001,0.000001,0.000001,0.000001]],
		attackCurve: 0,
		decayCurve: 0
	),
	shortBirdHack: ( // duration 0.01
		instrument: \spektrum42,
		startFreqs: [~harmspek[4][[5,10,15,19]]],
		endFreqs: [~harmspek[1][[5,10,15,19]]],
		startBws: [#[0.000001,0.000001,0.0001,0.0001]], 
		endBws: [#[0.0001,0.0001,0.000001,0.000001]],
		attackCurve: 10,
		decayCurve: -10 
	),
	shortBirdHack2: ( // duration 0.01
		instrument: \spektrum42,
		startFreqs: [~harmspek[3][[5,10,15,19]]],
		endFreqs: [~harmspek[1][[5,10,15,19]]],
		startBws: [#[0.000001,0.000001,0.0001,0.0001]], 
		endBws: [#[0.0001,0.0001,0.000001,0.000001]],
		attackCurve: 10,
		decayCurve: -10 
	),
	shortBirdHack3: ( // duration 0.01
		//attackTime: 0.1,
		instrument: \spektrum42,
		startFreqs: [~harmspek[7][[5,10,15,19]]],
		endFreqs: [~harmspek[4][[5,10,15,19]]],
		startBws: [#[0.000001,0.000001,0.0001,0.0001]], 
		endBws: [#[0.0001,0.0001,0.000001,0.000001]],
		attackCurve: 10,
		decayCurve: -10 
	),
	shortLowSnap: ( // duration 0.01
		instrument: \spektrum42,
		startFreqs: [~harmspek[4][[5,10,15,19]]],
		endFreqs: [~harmspek[1][[5,10,15,19]]],
		startBws: [#[0.0001,0.0001,0.000001,0.000001]], 
		endBws: [#[0.000001,0.000001,0.0001,0.0001]],
		attackCurve: 10,
		decayCurve: -10
	),
	shortLowSnap2: ( // duration 0.01
		instrument: \spektrum42,
		startFreqs: [~harmspek[3][[5,10,15,19]]],
		endFreqs: [~harmspek[1][[5,10,15,19]]],
		startBws: [#[0.0001,0.0001,0.000001,0.000001]], 
		endBws: [#[0.000001,0.000001,0.0001,0.0001]],
		attackCurve: 10,
		decayCurve: -10
	),
	shortLowSnap3: ( // duration 0.01
		//attackTime: 0.1,
		instrument: \spektrum42,
		startFreqs: [~harmspek[7][[5,10,15,19]]],
		endFreqs: [~harmspek[4][[5,10,15,19]]],
		startBws: [#[0.0001,0.0001,0.000001,0.000001]], 
		endBws: [#[0.000001,0.000001,0.0001,0.0001]],
		attackCurve: 10,
		decayCurve: -10
	),
	shortRazzel: ( // duration 0.1
		instrument: \spektrum42,
		startFreqs: [~phispek[2][[7,8,14,17]]],
		endFreqs: [~phispek[2][[10,12,13,16]]],
		startBws: [#[0.001,0.001,0.001,0.001]], 
		endBws: [#[0.000001,0.000001,0.000001,0.000001]],
		attackCurve: 10,
		decayCurve: -10
	),
	shortHighNoise: ( // duration 0.1
		instrument: \spektrum42,
		startFreqs: [~phispek[2][[10,13,14,15]]],
		endFreqs: [~phispek[2][[10,13,14,15]]],
		startBws: [#[0.000001,0.000001,0.0001,0.0001]], 
		endBws: [#[0.0001,0.0001,0.000001,0.000001]],
		attackCurve: 0,
		decayCurve: 0
	),
	shortHighNoise2: ( // duration 0.1
		instrument: \spektrum42,
		startFreqs: [~phispek[1][[10,13,14,15]]],
		endFreqs: [~phispek[1][[10,13,14,15]]],
		startBws: [#[0.000001,0.000001,0.0001,0.0001]], 
		endBws: [#[0.0001,0.0001,0.000001,0.000001]],
		attackCurve: 0,
		decayCurve: 0
	),
	shortHighNoise3: ( // duration 0.1
		//attackTime: 0.1,
		instrument: \spektrum42,
		startFreqs: [~phispek[3][[10,13,14,15]]],
		endFreqs: [~phispek[3][[2,5,6,7]]],
		startBws: [#[0.000001,0.000001,0.0001,0.0001]], 
		endBws: [#[0.0001,0.0001,0.000001,0.000001]],
		attackCurve: 0,
		decayCurve: 0
	),
	shortLowNoise: ( // duration 0.1
		instrument: \spektrum42,
		startFreqs: [~harmspek[4][[5,10,15,19]]],
		endFreqs: [~harmspek[4][[5,10,15,19]]],
		startBws: [#[0.0001,0.0001,0.000001,0.000001]], 
		endBws: [#[0.000001,0.000001,0.0001,0.0001]],
		attackCurve: 10,
		decayCurve: -10
	),
	shortLowNoise2: ( // duration 0.1
		instrument: \spektrum42,
		startFreqs: [~harmspek[3][[5,10,15,19]]],
		endFreqs: [~harmspek[3][[5,10,15,19]]],
		startBws: [#[0.0001,0.0001,0.000001,0.000001]], 
		endBws: [#[0.000001,0.000001,0.0001,0.0001]],
		attackCurve: 10,
		decayCurve: -10
	),
	shortLowNoise3: ( // duration 0.1
		//attackTime: 0.1,
		instrument: \spektrum42,
		startFreqs: [~harmspek[5][[5,10,15,19]]],
		endFreqs: [~harmspek[5][[5,10,15,19]]],
		startBws: [#[0.0001,0.0001,0.000001,0.000001]], 
		endBws: [#[0.000001,0.000001,0.0001,0.0001]],
		attackCurve: 10,
		decayCurve: -10
	),
	long1: (		
		instrument: \spektrum42,
		startFreqs: [~harmspek[4][[5,10,15,19]]],
		endFreqs: [~harmspek[4][[5,10,15,19]]],
		startBws: [~bws[[1,1,1,1]]], 
		endBws: [~bws[[0,0,0,0]]],
		attackCurve: 0,
		decayCurve: 0
	),
	long12: (		
		instrument: \spektrum42,
		startFreqs: [~harmspek[3][[5,10,15,19]]],
		endFreqs: [~harmspek[3][[5,10,15,19]]],
		startBws: [~bws[[1,1,1,1]]], 
		endBws: [~bws[[0,0,0,0]]],
		attackCurve: 0,
		decayCurve: 0
	),
	long13: (		
		instrument: \spektrum42,
		startFreqs: [~harmspek[5][[5,10,15,19]]],
		endFreqs: [~harmspek[5][[5,10,15,19]]],
		startBws: [~bws[[1,1,1,1]]], 
		endBws: [~bws[[0,0,0,0]]],
		attackCurve: 0,
		decayCurve: 0
	),
	long2: (		
		instrument: \spektrum42,
		startFreqs: [~harmspek[4][[10,13,14,15]]],
		endFreqs: [~harmspek[4][[10,13,14,15]]],
		startBws: [~bws[[0,0,0,0]]], 
		endBws: [~bws[[3,3,3,3]]],
		attackCurve: 0,
		decayCurve: 0
	),
	long22: (		
		instrument: \spektrum42,
		startFreqs: [~harmspek[3][[10,13,14,15]]],
		endFreqs: [~harmspek[3][[10,13,14,15]]],
		startBws: [~bws[[0,0,0,0]]], 
		endBws: [~bws[[3,3,3,3]]],
		attackCurve: 0,
		decayCurve: 0
	),
	long23: (		
		instrument: \spektrum42,
		startFreqs: [~harmspek[5][[10,13,14,15]]],
		endFreqs: [~harmspek[5][[10,13,14,15]]],
		startBws: [~bws[[0,0,0,0]]], 
		endBws: [~bws[[3,3,3,3]]],
		attackCurve: 0,
		decayCurve: 0
	),
	long3: (		
		instrument: \spektrum42,
		startFreqs: [~harmspek[4][[6,9,16,17]]],
		endFreqs: [~harmspek[4][[6,9,16,17]]],
		startBws: [~bws[[1,1,1,1]]], 
		endBws: [~bws[[0,0,0,0]]],
		attackCurve: 0,
		decayCurve: 0
	),
	long32: (		
		instrument: \spektrum42,
		startFreqs: [~harmspek[3][[6,9,16,17]]],
		endFreqs: [~harmspek[3][[6,9,16,17]]],
		startBws: [~bws[[1,1,1,1]]], 
		endBws: [~bws[[0,0,0,0]]],
		attackCurve: 0,
		decayCurve: 0
	),
	long33: (		
		instrument: \spektrum42,
		startFreqs: [~harmspek[5][[6,9,16,17]]],
		endFreqs: [~harmspek[5][[6,9,16,17]]],
		startBws: [~bws[[1,1,1,1]]], 
		endBws: [~bws[[0,0,0,0]]],
		attackCurve: 0,
		decayCurve: 0
	),
	pause: (
		type: \rest
	)
);

~source = Group.head(s);
~effects = Group.tail(s);
~bus1 = Bus.audio(s, 2);
~bus2 = Bus.audio(s, 2);
~bus3 = Bus.audio(s, 2);
~bus4 = Bus.audio(s, 2);
~bus5 = Bus.audio(s, 2);
~bus6 = Bus.audio(s, 2);

Synth.head(~effects, "reverb2", [\mix, 0.05, \room, 0.25, \damp, 0.5, \inbus, ~bus1]);
Synth.tail(~effects, "reverb2", [\mix, 0.09, \room, 0.9, \damp, 0.9, \inbus, ~bus2]);
Synth.tail(~effects, "reverb2", [\mix, 0.08, \room, 0.5, \damp, 0.5, \inbus, ~bus3]);
Synth.tail(~effects, "reverb2", [\mix, 0.3, \room, 0.3, \damp, 0.9, \inbus, ~bus4]);
Synth.head(~effects, "reverb2", [\mix, 0.05, \room, 0.25, \damp, 0.5, \inbus, ~bus5]);
Synth.tail(~effects, "reverb2", [\mix, 0.09, \room, 0.9, \damp, 0.9, \inbus, ~bus6]);

//1, 1, 2, 3, 5, 8,  13
//1, 3, 4, 7, 11,18, 29
//3,8, 2, 8, 3, 2, 13, 8, 5, 13, 13
//7,18,4,18,7,4,29,18,11,29,29

~rythmGesture1 = [[3,[0,2],[0,1]], [0, 13, 5], [8,[0,2],[0,1]], [2,[0,2],[0,1]], [8,1,[0,1]],
	[3,[0,5],[0,1]], [2, 13, 8], [13,[0,2],[0,1]],
	[8,[0,1],[0,1]], [5,13,5], [13,[0,2],[0,1]], [13,[0,2],[0,1]]];
/*
~rythmGesture2 = [[7,[0,4],[0,3]], [0, 29, 11], [18,[0,4],[0,3]], [4,[0,4],[0,3]], [18,1,[0,3]],
	[7,[0,11],[0,3]], [4, 29, 18], [29,[0,4],[0,3]],
	[18,[0,3],[0,3]], [11,29,11], [29,[0,4],[0,3]], [29,[0,4],[0,3]]];
	*/
~rythmGesture2 = [[7,[0,4],[0,3]], [0, 29, 11], [18,[0,11],[0,7]], [4,[0,4],[0,3]], [18,1,[0,3]],
	[7,[0,11],[0,3]], [4, 29, 18], [29,[0,4],[0,3]],
	[18,[0,13],[0,8]], [11,29,11], [29,[0,4],[0,3]], [29,[0,4],[0,3]]];

~shortRythms1 = 
    ~makeRythm.value(~fastGrains, 
		~rythmGesture1 ++ ~rythmGesture1 ++ ~rythmGesture1 ++ ~rythmGesture1, 
		0);	

~shortRythms2 = ~makeRythm.value(~fastGrains, 
	[[108, 2, 1]] ++ ~rythmGesture1 ++ ~rythmGesture1 ++ ~rythmGesture1 ++ ~rythmGesture1,
	0);

~shortRythms3 = ~makeRythm.value(~fastGrains, 
	[[312, 2, 1]] ++ ~rythmGesture2 ++ ~rythmGesture2 ++ ~rythmGesture2,
	0);

~shortRythms4 = ~makeRythm.value(~fastGrains, 
	[[420, 2, 1]] ++ ~rythmGesture2 ++ ~rythmGesture2 ++ ~rythmGesture2,
	0);

~rythmGesture3 = [[9,[0,5],[0,4]], [0, 38, 14], [23,[0,5],[0,4]], [5,[0,5],[0,4]], [23,1,[0,4]],
	[9,[1,1],[0,4]], [5, 38, 23], [38,[0,5],[0,4]],
	[23,[0,4],[0,4]], [14,38,14], [38,[0,5],[0,4]], [38,[0,5],[0,4]]];


~makeCloud = {
	arg length, size, pattern;
	var delta, gridSize;
	delta = 0;
	gridSize = length / size;
	#[0] ++
	pattern.collect({
		arg gridSteps;
		delta = (gridSteps * gridSize);
		delta;
	});
};

~sumRythmlistDurations = {
	arg rythmList, start = 0;
	var tmpTime;
	tmpTime = start;
	rythmList.collect({
		arg rythmStep;
		var result = [tmpTime, rythmStep[1], rythmStep[2]];
		tmpTime = tmpTime + rythmStep[0];
		result;
	});
};

~shortRythms5 = 
    ~makeRythm.value(~fastGrains, 
		[[798, 2, 1]] ++ ~rythmGesture3 ++ ~rythmGesture3, 
		0);

~summedRythmlist = ~sumRythmlistDurations.value(~shortRythms5, 0);
~summedRythmlist.size;

~shortRythms6 = 
    ~makeRythm.value(~fastGrains, 
		[[972, 2, 1]] ++ ~rythmGesture3 ++ ~rythmGesture3, 
		0);

~summedRythmlist2 = ~sumRythmlistDurations.value(~shortRythms6, 0);
~summedRythmlist2.size;

~noteGesture1 = #[shortMetal, long1, shortHighSnap, shortBirdHack, shortLowSnap,
			 shortLowSnap, long2, shortLowNoise,
			 shortHighNoise, long3, shortBirdHack, shortLowNoise];

~noteGesture2 = #[shortMetal2, long12, shortHighSnap2, shortBirdHack2, shortLowSnap2,
			 shortLowSnap2, long22, shortLowNoise2,
			 shortHighNoise2, long32, shortBirdHack2, shortLowNoise2];

~noteGesture3 = #[shortMetal3, long13, shortHighSnap3, shortBirdHack3, shortLowSnap3,
			 shortLowSnap3, long23, shortLowNoise3,
			 shortHighNoise3, long33, shortBirdHack3, shortLowNoise3];

~ampGesture1 = 
    [0.4, 0.007, 0.1, 0.4, 0.3,
		0.3, 0.005, 0.2,
		0.2, 0.005, 0.3, 0.4];
/*
~ampGesture2 = 
    [0.4, 0.007, 0.1, 0.4, 0.3,
		0.3, 0.005, 0.2,
		0.2, 0.005, 0.3, 0.4];
	*/
~ampGesture2 = 
    [0.4, 0.007, 0.1, 0.4, 0.3,
		0.3, 0.005, 0.3,
		0.1, 0.005, 0.3, 0.4];

~panGesture2 =
    [[0.3, 0.2], [-0.1, -0.3], [0.2,0], [-0.3, -0.1], [0.1,0.1],
			 [-0.3, 0.1], [0.1, 0.4], [0.3, 0.4],
			 [-0.3, 0.1], [0.1, 0.4], [-0.3, 0.1], [0.3, 0.4]];

~ampGesture3 = 
    [0.4, 0.007, 0.1, 0.4, 0.3,
		0.3, 0.005, 0.2,
		0.2, 0.005, 0.3, 0.4];

~panGesture1 =
    [[0.3, 0.2], [-0.1, -0.3], [0.2,0], [-0.3, -0.1], [0.1,0.1],
			 [-0.3, 0.1], [0.1, 0.4], [0.3, 0.4],
			 [-0.3, 0.1], [0.1, 0.4], [-0.3, 0.1], [0.3, 0.4]];

~panGesture2 =
    [[0.3, 0.2], [-0.1, -0.3], [0.2,0], [-0.3, -0.1], [0.1,0.1],
			 [-0.3, 0.1], [0.1, 0.4], [0.3, 0.4],
			 [-0.3, 0.1], [0.1, 0.4], [-0.3, 0.1], [0.3, 0.4]];

~panGesture3 =
    [[0.3, 0.2], [-0.1, -0.3], [0.2,0], [-0.3, -0.1], [0.1,0.1],
			 [-0.3, 0.1], [0.1, 0.4], [0.3, 0.4],
			 [-0.3, 0.1], [0.1, 0.4], [-0.3, 0.1], [0.3, 0.4]];

~makeClusterPattern = {
	arg pos, size, pattern, deltaVal, pans, bus;
	
	Pchain(
		Pdict(~shortNotes, Pseq([~noteGesture3[pos]], inf)),
		Pbind(
			\group, ~source,
			\outbus, bus,
			\amp, Pseq([~ampGesture3[pos]], inf),
			#[startPan, endPan], 
			Pseq([pans[pos]], inf),
			\delta, 
			Pseq(~makeCloud.value(deltaVal, 
				size, 
				pattern), 1),
			\dur, 0.0001,
			\attackTime, 0.1
		)
	);
};

~makeClusterGesture = {
	arg start, rythmList, pans, bus;
	var startTime = rythmList[start][0];
	Ptpar(
		[
			0.0,
			~makeClusterPattern.value(0, 13, #[3, 8, 2, 8, 3, 2, 13, 8, 5, 13, 13], 
				rythmList[start][1], pans, bus),
			rythmList[start + 1][0] - startTime,
			Pchain(
				Pdict(~shortNotes, Pseq([~noteGesture3[1]], 1)),
				Pbind(
					\group, ~source,
					\outbus, bus,
					\amp, Pseq([~ampGesture3[1]], 1),
					#[startPan, endPan], 
					Pseq([pans[1]], 1),
					\delta, 0,
					\dur, rythmList[start + 1][1],
					\attackTime, rythmList[start + 1][2]
				)
			),
			rythmList[start + 2][0] - startTime,
			~makeClusterPattern.value(2, 13, #[3, 8, 2, 8, 3, 2, 13, 8, 5, 13, 13], 
				rythmList[start + 2][1], pans, bus),
			rythmList[start + 3][0] - startTime,
			~makeClusterPattern.value(3, 13, #[3, 8, 2, 8, 3, 2, 13, 8, 5, 13, 13], 
				rythmList[start + 3][1], pans, bus),
			rythmList[start + 4][0] - startTime,
			~makeClusterPattern.value(4, 13, #[3, 8, 2, 8, 3, 2, 13, 8, 5, 13, 13], 
				rythmList[start + 4][1], pans, bus),
			
			rythmList[5][0] - startTime,
			~makeClusterPattern.value(5, 13, #[3, 8, 2, 8, 3, 2, 13, 8, 5, 13, 13], 
				rythmList[start + 5][1], pans, bus),
			rythmList[start + 6][0] - startTime,
			Pchain(
				Pdict(~shortNotes, Pseq([~noteGesture3[6]], 1)),
				Pbind(
					\group, ~source,
					\outbus, bus,
					\amp, Pseq([~ampGesture3[6]], 1),
					#[startPan, endPan], 
				Pseq([pans[6]], 1),
					\delta, 0,
					\dur, rythmList[start + 6][1],
					\attackTime, rythmList[start + 6][2]
				)
			),
			rythmList[start + 7][0] - startTime,
			~makeClusterPattern.value(7, 13, #[3, 8, 2, 8, 3, 2, 13, 8, 5, 13, 13], 
				rythmList[start + 7][1], pans, bus),
			
			rythmList[start + 8][0] - startTime,
			~makeClusterPattern.value(8, 13, #[3, 8, 2, 8, 3, 2, 13, 8, 5, 13, 13], 
				rythmList[start + 8][1], pans, bus),
			rythmList[start + 9][0] - startTime,
			Pchain(
				Pdict(~shortNotes, Pseq([~noteGesture3[9]], 1)),
				Pbind(
					\group, ~source,
					\outbus, bus,
					\amp, Pseq([~ampGesture3[9]], 1),
					#[startPan, endPan], 
					Pseq([pans[9]], 1),
					\delta, 0,
					\dur, rythmList[start + 9][1],
				\attackTime, rythmList[start + 9][2]
				)
			),
			rythmList[start + 10][0] - startTime,
			~makeClusterPattern.value(10, 13, #[3, 8, 2, 8, 3, 2, 13, 8, 5, 13, 13], 
				rythmList[start + 10][1], pans, bus),
			rythmList[start + 11][0] - startTime,
			~makeClusterPattern.value(11, 13, #[3, 8, 2, 8, 3, 2, 13, 8, 5, 13, 13], 
				rythmList[start + 11][1], pans, bus),



			//Refactor
			rythmList[start + 12][0] - startTime,
			~makeClusterPattern.value(0, 13, #[3, 8, 2, 8, 3, 2, 13, 8, 5, 13, 13], 
				rythmList[start + 12][1], pans, bus),
			rythmList[start + 13][0] - startTime,
			Pchain(
				Pdict(~shortNotes, Pseq([~noteGesture3[1]], 1)),
				Pbind(
					\group, ~source,
					\outbus, bus,
					\amp, Pseq([~ampGesture3[1]], 1),
					#[startPan, endPan], 
					Pseq([pans[1]], 1),
					\delta, 0,
					\dur, rythmList[start + 13][1],
					\attackTime, rythmList[start + 13][2]
				)
			),
			rythmList[start + 14][0] - startTime,
			~makeClusterPattern.value(2, 13, #[3, 8, 2, 8, 3, 2, 13, 8, 5, 13, 13], 
				rythmList[start + 14][1], pans, bus),
			rythmList[start + 15][0] - startTime,
			~makeClusterPattern.value(3, 13, #[3, 8, 2, 8, 3, 2, 13, 8, 5, 13, 13], 
				rythmList[start + 15][1], pans, bus),
			rythmList[start + 16][0] - startTime,
			~makeClusterPattern.value(4, 13, #[3, 8, 2, 8, 3, 2, 13, 8, 5, 13, 13], 
				rythmList[start + 16][1], pans, bus),
			
			rythmList[17][0] - startTime,
			~makeClusterPattern.value(5, 13, #[3, 8, 2, 8, 3, 2, 13, 8, 5, 13, 13], 
				rythmList[start + 17][1], pans, bus),
			rythmList[start + 18][0] - startTime,
			Pchain(
				Pdict(~shortNotes, Pseq([~noteGesture3[6]], 1)),
				Pbind(
					\group, ~source,
					\outbus, bus,
					\amp, Pseq([~ampGesture3[6]], 1),
					#[startPan, endPan], 
				Pseq([pans[6]], 1),
					\delta, 0,
					\dur, rythmList[start + 18][1],
					\attackTime, rythmList[start + 18][2]
				)
			),
			rythmList[start + 19][0] - startTime,
			~makeClusterPattern.value(7, 13, #[3, 8, 2, 8, 3, 2, 13, 8, 5, 13, 13], 
				rythmList[start + 19][1], pans, bus),
			
			rythmList[start + 20][0] - startTime,
			~makeClusterPattern.value(8, 13, #[3, 8, 2, 8, 3, 2, 13, 8, 5, 13, 13], 
				rythmList[start + 20][1], pans, bus),
			rythmList[start + 21][0] - startTime,
			Pchain(
				Pdict(~shortNotes, Pseq([~noteGesture3[9]], 1)),
				Pbind(
					\group, ~source,
					\outbus, bus,
					\amp, Pseq([~ampGesture3[9]], 1),
					#[startPan, endPan], 
					Pseq([pans[9]], 1),
					\delta, 0,
					\dur, rythmList[start + 21][1],
					\attackTime, rythmList[start + 21][2]
				)
			),
			rythmList[start + 22][0] - startTime,
			~makeClusterPattern.value(10, 13, #[3, 8, 2, 8, 3, 2, 13, 8, 5, 13, 13], 
				rythmList[start + 22][1], pans, bus),
			rythmList[start + 23][0] - startTime,
			~makeClusterPattern.value(11, 13, #[3, 8, 2, 8, 3, 2, 13, 8, 5, 13, 13], 
				rythmList[start + 23][1], pans, bus)
		]);
};











Ppar(
[	
 Pchain(
	 Pdict(~shortNotes,
		 Pseq(~noteGesture1 ++ ~noteGesture1 ++ ~noteGesture1 ++ ~noteGesture1)),
	 Pbind(
		 \group, ~source,
		 \outbus, ~bus1,
		 \amp, 
		     Pseq(~ampGesture1 ++ ~ampGesture1 ++ ~ampGesture1 ++ ~ampGesture1, 
				 1),
		 #[startPan, endPan], 
		     Pseq(~panGesture1 ++ ~panGesture1 ++ ~panGesture1 ++ ~panGesture1, 1)
		   + Pseq([[0.5,0.5]], inf),
		 #[delta, dur, attackTime], Pseq(~shortRythms1, 1)
	 )
	),
	
	
	Pchain(
		Pdict(~shortNotes,
			Pseq(#[pause] ++ ~noteGesture1 ++ ~noteGesture1 ++ ~noteGesture1 ++ ~noteGesture1)),
		Pbind(
			\group, ~source,
			\outbus, ~bus2,
			\amp,
			    Pseq([0.3] ++ ~ampGesture1 ++ ~ampGesture1 ++ ~ampGesture1 ++ ~ampGesture1
					, 1),
			#[startPan, endPan], 
			    Pseq([[0.0, 0.0]] ++ ~panGesture1 ++ ~panGesture1 ++ ~panGesture1, 1) 
			  - Pseq([[0.5, 0.5]], inf),
		 #[delta, dur, attackTime], Pseq(~shortRythms2, 1)
		)
	),


	Pchain(
		Pdict(~shortNotes,
			Pseq(#[pause] ++ ~noteGesture2 ++ ~noteGesture2 ++ ~noteGesture2)),
		Pbind(
			\group, ~source,
			\outbus, ~bus3,
			\amp, 
			Pseq([0.3] ++ ~ampGesture2 ++ ~ampGesture2 ++ ~ampGesture2, 
				1),
			#[startPan, endPan], 
			Pseq([[0.0, 0.0]] ++ ~panGesture2 ++ ~panGesture2 ++ ~panGesture2, 
				1)
			+ Pseq([[0.5,0.5]], inf),
			#[delta, dur, attackTime], Pseq(~shortRythms3, 1)
		)
	),
	
	
	Pchain(
		Pdict(~shortNotes,
			Pseq(#[pause] ++ ~noteGesture2 ++ ~noteGesture2 ++ ~noteGesture2)),
		Pbind(
			\group, ~source,
			\outbus, ~bus4,
			\amp,
			    Pseq([0.3] ++ ~ampGesture2 ++ ~ampGesture2 ++ ~ampGesture2
					, 1),
			#[startPan, endPan], 
			Pseq([[0.0, 0.0]] ++ ~panGesture2 ++ ~panGesture2 ++ ~panGesture2
				,1) 
			- Pseq([[0.5, 0.5]], inf),
			#[delta, dur, attackTime], Pseq(~shortRythms4, 1)
		)
	),
	
	Ptpar(
	[
		~summedRythmlist[1][0],
		~makeClusterGesture.value(1, ~summedRythmlist, ~panGesture3 + [0.5,0.5], ~bus5),
		~summedRythmlist2[1][0],
		~makeClusterGesture.value(1, ~summedRythmlist2, ~panGesture3 - [0.5,0.5], ~bus6)
	])
	
]).play;
b
)

//s.prepareForRecord;
//s.record;
//s.stopRecording;

