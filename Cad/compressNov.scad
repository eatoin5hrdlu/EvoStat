//
// Shapes to subtract
// 

// height in design space (thickness Z in printspace) reduced 12 -> 9
// wire hole reduced from r=1.5 to 1.3mm

nsp = 1.7; // pilot hole for #6 screw
nsh = 4;  // connection screw head recess
hradius = 1.3;

module hole(rad, hg) {
		rotate([0,0,0])
			cylinder(r=rad, h=hg,center=true);
}

module cavity(len,wid,hgt,t,gauge) {
//corner
	translate([len/2,1+wid/2,-1]) cube([8+len/2,2*wid/3,hgt+4],true);
//slot
	translate([len/3,3+wid/4,-2.5]) cube([len/2-2,6+wid/2,5+hgt/3],true);
//tube hole
	  translate([(len/2)-18.2,t/2-1,-hgt])
      rotate([0,0,90]) 
   cylinder(r=t/2,h=2*hgt,$fn=12);
//wire hole
      translate([-70,2,1])
       rotate([0,90,0]) 
        cylinder(r=gauge/2, h=3*len,$fn=8);
//      translate([-70,1.4,0])
//       rotate([0,90,0]) 
//        cylinder(r=(gauge/2)-0.1, h=3*len,$fn=6);

// connection screw recessed head
	//  translate([36-len/2,t/2-1.5,((2*hgt)/5)-1])
    //  rotate([0,0,90]) 
    //    cylinder(r=nsh,h=1+hgt,$fn=12);
// connection screw pilot hole
	  translate([36-len/2,t/2-1.5,0])
      rotate([0,0,90]) 
        cylinder(r=nsp,h=3+hgt,$fn=12);
// electrical wire cavity
		  translate([36-len/2,t/2+26,-3+hgt/3])
      rotate([90,0,0]) 
       cylinder(r=1.5,h=wid+2,$fn=6);
//cube([wid+2,3,3],true);
}
	// MOUNTING Holes
//	module mholes(ht) {
//	 translate([114.26,0,-3])
//	   hole(nsp, ht);
	module mholes(ht) {
	 translate([81.2,0,-3])
	   hole(nsp, ht);
// 	 translate([112,23,-3])
// 	   hole(nsp, ht);

//   translate([98.61, 0, -3])
//	   hole( hradius, thickplus);
//   translate([96.62, 19.23, -3])
//	   hole( nsp, ht);

//   translate([80.7, 15.85, -3])
//	   hole( nsp, ht);
   }


module compress(len,wid,hgt,t,gauge) {

   difference() {
		translate([13,11,0]) cube([len/2-4,wid,hgt],true);
		cavity(len,wid,hgt,t,gauge);
     translate([-1,17,0]) hole(nsp, 16);
     translate([-7,26,0])
        rotate([0,0,45])
            cube([24,10,hgt+2],center=true);
     translate([15,24,0])
        rotate([0,0,-45])
            cube([34,16,hgt+2],center=true);
     translate([0,30,0])
            cube([24,12,hgt+2],center=true);
	}

 	translate([(len/2)-18.2,-0.7,hgt/3-5.4])
      rotate([0,0,90]) 
        scale([1.5,1,1]) sphere(r=1.7,$fn=12); 
	translate([(len/2)-18.2,-0.7,hgt/3-5.5])
      rotate([0,0,90]) 
        scale([1.5,1,1]) cylinder(r=1.7,h=hgt/4,$fn=12);

	translate([(len/2)-18.2,-0.7,hgt/3-2])
      rotate([0,0,90]) 
        scale([1.5,1,1]) sphere(r=1.7,$fn=12); 
	translate([(len/2)-18.2,-2.45,hgt/3-6.65])
        cube([5,5,hgt-4],center=true); 
}

module piece() {
alpha = 7;    
   translate([0,-2.5,-7])
			cylinder(r=2.25,h=6,center=true,$fn=8);

    compress(100,32,14,5.6,2.1);	
// gauge was 2.15
}

rotate([180,0,0]) 
    piece();


