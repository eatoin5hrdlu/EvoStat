height = 55;
dia = 28;
ch = 46;

module plug(d) 
{
rad = d/2;
q = d/3;
gz = -height/2;
gh = height;

	difference() 
	{
    union() {
	  cylinder(r=rad, h=height, center=true, $fn=32);

     translate([0,0,-30])
     difference() {
	    cylinder(r=rad, h=6, center=true, $fn=32);
	    cylinder(r=rad/2, h=6, center=true, $fn=32);
      }
     }
	  translate([0,rad/2-4,rad/2-height/4])
		 cube([14,22,ch],center=true);
// LED Hole
     translate([10,0,-4])
     rotate([0,90,0])
          cylinder(r=2.4,h=20,center=true,$fn=24);
// Phototransitor Holes
     translate([-8,0,-4])
     rotate([0,90,0])
          cylinder(r=2.5,h=4,center=true,$fn=24);
     translate([-8,1,-4])
     rotate([0,90,0])
          cylinder(r=0.6,h=16,center=true,$fn=12);
     translate([-8,-1,-4])
     rotate([0,90,0])
          cylinder(r=0.6,h=16,center=true,$fn=12);

// Wire Grooves
     translate([rad-1,-3.4,gz])
          cube([1.8,1.3,2*gh],center=true);
     translate([rad-1,3.4,gz])
          cube([1.8,1.3,2*gh],center=true);

     translate([2-d,0,0]) {
        translate([rad-1,-3,gz])
           cube([1.8,1.3,2*gh],center=true);
        translate([rad-1,3,gz])
           cube([1.8,1.3,2*gh],center=true);
	  }
    translate([0,3,height/2-12])
      rotate([90,0,0])
        cylinder(r=6.8,h=22,center=true);
  translate([0,12,-(3+height/2)])
      cube([4,12,6],center=true);
 translate([0,7,-27]) cav();
  } // End of difference()

}

module cav() {
    difference() {
     union() {
       translate([0,-3,7]) sphere(12,center=true,$fn=64);
       translate([0,-6,1]) cube([20,22,10],center=true);
     }
     translate([0,-20,0]) cube([30,15,30],center=true);
     translate([0,0,-10]) cube([30,30,15],center=true);
  }
}


plug(dia);
//translate([0,0,-7]) cube([4,4,45],center=true);
