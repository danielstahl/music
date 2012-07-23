 
//Common code
(
	var makeSubgrain;

	~phi = (1 + sqrt(5)) / 2;
	~invPhi = 1 / ~phi;
	
	~makeSerie = {
		arg start, fakt, size;
		var result = Array(size), temp = start;
		result.add(start);
		(size - 1).do {
			temp = temp * fakt;
			result.add(temp);
		};
		result;
	};

// Harmony and spektrum	
	~spekt = {
		arg base, fact, size;
		var result = Array(size);
		size.do {
			arg val, i;
			var mult = fact * ((i + 1) - 1)+1;
			result.add(base * mult);
		};
		result
	};

// Rythm
	~makeStepSerie = {
		arg start, size;
		var result = Array(size);
		size.do {
			arg val;
			result.add(val + start);
		};
		result;
	};

	~makeInvStepSerie = {
		arg start, size;
		var result = Array(size);
		size.do {
			arg val;
			result.add(1 / (val + start));
		};
		result;
	};

	makeSubgrain = {
		arg length, size;
		size.collect({
			length / size;	
		});
	};


	~makeGrain = {
		arg grains;
		grains.collect({
			arg item;
			var firstLevel = 
			makeSubgrain.value(item[0] * item[1], item[1]);			
			firstLevel.collect({
				arg subItem;
				[subItem,makeSubgrain.value(subItem, item[2])];
			});
		}).flatten(1);	
	};

	~makeRythm = {
		arg grains, steps, start = 0;
		var grainPos = start;
		steps.collect({
			arg step;
			var delta = 0, dur = 0, attackTime = 0, 
			tmpDurPos = grainPos, tmpSubDurPos = 0, 
			tmpAttacktimePos = grainPos, tmpSubAttackTimePos = 0;
			step[0].do {
				delta = delta + grains.at(grainPos)[0];
				grainPos = grainPos + 1;	
			};
			if(step[1].isNumber, {
				step[1].do {
					dur = dur + grains.at(tmpDurPos)[0];
					tmpDurPos = tmpDurPos + 1;	
				}}, {
					tmpSubDurPos = 0;
					step[1][0].do {
						dur = dur + grains.at(tmpDurPos)[0];
						tmpDurPos = tmpDurPos + 1;	
					};
					step[1][1].do {
						dur = dur + grains.at(tmpDurPos)[1][tmpSubDurPos];					
						tmpSubDurPos = tmpSubDurPos + 1;
					};			
				}); 
			
			if(step[2].isNumber, {
				step[2].do {
					attackTime = attackTime + grains.at(tmpAttacktimePos)[0];
					tmpAttacktimePos = tmpAttacktimePos + 1;
				}}, {
					tmpSubAttackTimePos = 0;
					step[2][0].do {
						attackTime = attackTime + grains.at(tmpAttacktimePos)[0];
						tmpAttacktimePos = tmpAttacktimePos + 1;	
					};
					step[2][1].do {
						attackTime = 
						attackTime + grains.at(tmpAttacktimePos)[1][tmpSubAttackTimePos];
						tmpSubAttackTimePos = tmpSubAttackTimePos + 1;
					};
				});		
			[delta, dur, attackTime / dur];
		});		
	};	
)











