(
var projectDir = "/Users/danielstahl/Documents/Projects/music/microsound/src/main/sc/";
["Options", "Instruments", "Common", "Harmony", "Rythm"].do({
	arg item;
	this.executeFile(projectDir ++ item ++ ".scd");
});
)