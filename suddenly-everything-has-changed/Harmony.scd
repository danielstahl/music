// Harmony and spektrum
(
    var harmoctave, invphioctave, phioctave; 
	harmoctave = ~makeSerie.value(27.5, 2, 10);
	invphioctave = ~makeSerie.value(27.5, (2 * ~invPhi), 10);
	phioctave = ~makeSerie.value(27.5, (2 * ~phi), 10);

	~phispek = phioctave.collect {
		arg val;
		~spekt.value(val, ~phi, 20);
	};

	~invphispek = invphioctave.collect {
		arg val;
		~spekt.value(val, ~invPhi, 20);
	};


	~harmspek = harmoctave.collect {
		arg val;
		~spekt.value(val, 1, 20);
	};	
)











