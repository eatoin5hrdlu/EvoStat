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
   translate([0,0,7])
		cube([60,60,8],center=true);
  }
}

body();
