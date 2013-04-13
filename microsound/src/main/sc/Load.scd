(
var projectDir = "/Users/daniel_stahl/Documents/Projects/music/microsound/";
["Options", "Instruments", "Common", "Harmony", "Rythm"].do({
	arg item;
	this.executeFile(projectDir ++ item ++ ".scd");
});
)