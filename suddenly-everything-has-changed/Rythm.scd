// Rythm
(
~invHarmRythm = ~makeInvStepSerie.value(1, 50); 
~harmRythm = ~makeStepSerie.value(1, 50); 	
~phiRythm = ~makeSerie.value(1, ~phi, 50); 
~invPhiRythm = ~makeSerie.value(1, ~invPhi, 50); 

~invHarmGrains = 
  ~makeGrain.value([
	  [~invHarmRythm[3],5,21], 
	  [~invHarmRythm[5],8,13], 
	  [~invHarmRythm[4],5,13], 
	  [~invHarmRythm[6],8,13], 
	  [~invHarmRythm[7],5,13], 
	  [~invHarmRythm[5],5,21],
	  [~invHarmRythm[7],5,13], 
	  [~invHarmRythm[6],8,13], 
	  [~invHarmRythm[4],5,13], 
	  [~invHarmRythm[6],8,13], 
	  [~invHarmRythm[4],5,13], 
	  [~invHarmRythm[5],8,13],  
	]);
	

~invPhiGrains = 
  ~makeGrain.value([
	  [~invPhiRythm[3],8,21],
	  [~invPhiRythm[1],13,21], 
	  [~invPhiRythm[2],21,13], 
	  [~invPhiRythm[1],13,21], 
	  [~invPhiRythm[2],21,13], 
	  [~invPhiRythm[3],8,21], 
	  [~invPhiRythm[2],21,13],
	  [~invPhiRythm[1],13,21],     
	]);
	
~grains = 
  ~invHarmGrains ++ ~invHarmGrains ++ ~invPhiGrains ++ 
  ~invPhiGrains ++ ~invHarmGrains ++ ~invPhiGrains ++ ~invHarmGrains;

~fastGrains = 
  ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains ++ ~invHarmGrains;

~slowGrains = 
  ~invPhiGrains ++ ~invPhiGrains ++ ~invPhiGrains ++ ~invPhiGrains ++ ~invPhiGrains;	
)
/*
(
p = Pbind(
	\dur, Pseq((~slowGrains).collect({ arg item; item[0] }), 1)
).play;
)
*/