nsp = 1.7; // pilot hole for #6 screw
nsh = 4;  // connection screw head recess
hradius = 1.3;

module cavity(len,wid,hgt,t,gauge) {
//corner
	translate([len/2,1+wid/2,-1]) cube([8+len/2,2*wid/3,hgt+4],true);
//slot
	translate([len/3,3+wid/4,-2.5]) cube([len/2-2,6+wid/2,5+hgt/3],true);
//tube hole
	  translate([(len/2)-18.2,t/2-1,-hgt])
      rotate([0,0,90]) 
   cylinder(r=0.3+t/2,h=2*hgt,$fn=12);
// Rounded Tube Slot
   difference() {
	  translate([(len/2)-14.5,2,4.5]) 
                        cube([2.3,4,6],center=true);
	  translate([(len/2)-14.6,-0.1,4.5]) 
                        cylinder(r=1.1,h=6,$fn=12,center=true);
	  translate([(len/2)-14.8,4.2,4.5]) 
                        cylinder(r=1.1,h=6,$fn=12,center=true);
  }                       
  cube([3,2.4,6],center=true);

//wire hole
      translate([-70,2,1])
       rotate([0,90,0]) 
        cylinder(r=gauge/2, h=3*len,$fn=8);

// connection screw pilot hole
	  translate([36-len/2,t/2-1.5,0])
      rotate([0,0,90]) 
        cylinder(r=nsp,h=3+hgt,$fn=12);
	  translate([0,2,1])
      rotate([0,0,90]) 
        cylinder(r=nsp,h=3+hgt,$fn=12);

// electrical wire cavity
		  translate([36-len/2,t/2+26,-3+hgt/3])
      rotate([90,0,0]) 
       cylinder(r=1.5,h=wid+2,$fn=6);
}

module compress(len,wid,hgt,t,gauge) {

   difference() {
		translate([13,11,0]) cube([len/2-4,wid,hgt],true);
		cavity(len,wid,hgt,t,gauge);
     translate([-1,17,0])// hole(nsp, 16);
			cylinder(r=nsp, h=16,center=true);
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
	translate([(len/2)-18.2,-2.48,hgt/3-6.65])
        cube([6,5,hgt-4],center=true); 
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


