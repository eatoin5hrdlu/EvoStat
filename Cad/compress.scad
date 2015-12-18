//
// Shapes to subtract
// 

// height in design space (thickness Z in printspace) reduced 12 -> 9
// wire hole reduced from r=1.5 to 1.3mm

nsp = 1.5; // pilot hole for #6 screw
nsh = 4;  // connection screw head recess
hradius = 1.3;

module hole(rad, hg) {
		rotate([0,0,0])
			cylinder(r=rad, h=hg,center=true);
}

module cavity(len,wid,hgt,t,gauge) {
//corner
	translate([len/3,1+wid/2,-1]) cube([len/2,2*wid/3,hgt+4],true);
//slot
	translate([len/3,wid/5,0]) cube([len/2,2+wid/3,2+hgt/3],true);
//tube hole
	  translate([(len/2)-10,t/2-1,-hgt])
      rotate([0,0,90]) 
        cylinder(r=t/2,h=2*hgt,$fn=12);
//wire hole
      translate([-70,1,0])
       rotate([0,90,0]) 
        cylinder(r=gauge/2, h=3*len,$fn=6);
// connection screw recessed head
	  translate([36-len/2,t/2-1.5,(2*hgt)/5])
      rotate([0,0,90]) 
        cylinder(r=nsh,h=3+hgt,$fn=12);
// connection screw pilot hole
	  translate([36-len/2,t/2-1.5,0])
      rotate([0,0,90]) 
        cylinder(r=nsp,h=3+hgt,$fn=12);
// electrical wire cavity
		  translate([36-len/2,t/2+8,1+hgt/3])
      rotate([0,0,90]) 
       cube([wid+2,3,3],true);
}
	// MOUNTING Holes
	module mholes(ht) {
	 translate([114.26,0,-3])
	   hole(nsp, ht);
 	 translate([112,23,-3])
 	   hole(nsp, ht);

//   translate([98.61, 0, -3])
//	   hole( hradius, thickplus);
//   translate([96.62, 19.23, -3])
//	   hole( nsp, ht);

//   translate([80.7, 15.85, -3])
//	   hole( nsp, ht);
   }


module compress(len,wid,hgt,t,gauge) {


   difference() {
		translate([12,11,0]) cube([len-(len/3),wid,hgt],true);
		cavity(len,wid,hgt,t,gauge);
	}
	  translate([(len/2)-10,-0.5,-hgt/5])
      rotate([0,0,90]) 
        cylinder(r=gauge/2,h=hgt/3,$fn=8);

}
module piece() {
alpha = 7;
difference() {
translate([0,-4,0])
  rotate([0,0,alpha])
    compress(100,32,9,5.2,3);

//    translate([100,0,-1])
    translate([105,10,-1])
     rotate([0,0,180-alpha/2])
       mholes(6);
}
}
rotate([90,7,0]) 
    piece();


