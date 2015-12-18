module body0() {

   multmatrix(m = [ [1, 0,    0,   0],
                    [0,  1, -1.8,   0],
                    [0,  0,    1, 0],
                    [0,  0,    0,   1] ])
    resize([20,40,8])
       sphere(r=10,center=true,$fn=24);
}
module body() {
difference() {
	body0();

   translate([0,0,-4])
		cube([60,60,8],center=true);
   translate([0,0,7.3])
		cube([60,60,8],center=true);
   translate([-7,0,0])
      rotate([0,0,-10])
        cube([14,60,10],center=true);
  }
}

module top() {
   multmatrix(m = [ [1, 0,    0,   0],
                    [0,  1, -2,   0],
                    [0,  0,    1, 0],
                    [0,  0,    0,   1] ])
    resize([16,28,10.5])
       sphere(r=10,center=true,$fn=24);
}

module scaleblock() {
   rotate([0,0,-10]) translate([0,-8,3]) top();
   translate([-2.5,-15,1.5])
	   cylinder(r=7,h=3,center=true);
   body();
   translate([0.4,0,0]) mirror([1,0,0] ) rotate([0,0,20]) body();
}

difference() {
	scaleblock();
// Lower large cutouts
	translate([-7,0,10])
      rotate([90,0,-18])
         cylinder(r=8,h=60,center=true);
	translate([7,0,10])
      rotate([90,0,2])
         cylinder(r=8,h=60,center=true);
//Upper large cutouts
	translate([-8,0,13])
      rotate([90,0,-10])
         cylinder(r=9.5,h=60,center=true);
	translate([7.5,0,13])
      rotate([90,0,-5])
         cylinder(r=9.5,h=60,center=true);

   translate([0,0,-4]) cube([50,50,8],center=true);
   translate([0,-24.5,4])
     rotate([0,90,-10]) 
       cylinder(r=4,h=40,center=true);
  }