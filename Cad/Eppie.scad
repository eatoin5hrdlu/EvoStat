// Eppie Model
// Cathal Garvey
// CC License, Attribution, Sharealike
// http://creativecommons.org/licenses/by-sa/2.0/

// ===== Parameters =====
// Change $fn to higher numbers to increase the detail on curved surfaces.
$fn = 25;
TaperLength = 16;
BottomSphereRad = 2.5;
MidwayRad = 5.25;
TopLength = 20;
TopRad = 5.5;
LipRad = 6.5;
LipLength = 1;
WallThickness = 0.5;

// ======= Modules ======
// Tapered tube:
module TaperedTube(LipRad,LipLength,TopRad,TopLength,MidRad,TaperLength,EndRad){
	union(){
		translate([0,0,EndRad+TaperLength+TopLength])
			cylinder(LipLength,LipRad,LipRad);
		translate([0,0,EndRad+TaperLength])
			cylinder(TopLength,MidRad,TopRad);
		translate([0,0,EndRad-0.1])
			cylinder(TaperLength+0.11,EndRad,MidRad);
		translate([0,0,EndRad])
			sphere(EndRad);
	}
}

module HollowTube(){
	difference(){
	TaperedTube(LipRad,LipLength,TopRad,TopLength,MidwayRad,TaperLength,BottomSphereRad);
	translate([0,0,WallThickness]) TaperedTube(TopRad-WallThickness,LipLength,TopRad-WallThickness,TopLength,MidwayRad-WallThickness,TaperLength,BottomSphereRad-WallThickness);
	}
}

// ==== Construction ====
//HollowTube();

