module bodyX() {
   multmatrix(m = [ [1, 0, 0, 0],
                 [0, 1, -1.1, 0],
                 [0, 0, 1, 0],
                 [0, 0, 0, 1] ])
    resize([20,30,10])
       cylinder(r1=34,r2=1,h=10,center=true,$fn=24);
}
module body0() {
   multmatrix(m = [ [1, 0,    0,   0],
                    [0,  1, -1.3,   0],
                    [0,  0,    1, 0],
                    [0,  0,    0,   1] ])
    resize([20,30,20])
       sphere(r=10,center=true,$fn=24);
}
module body() {
difference() {
	body0();
   translate([0,0,-6])
		cube([60,60,8],center=true);
  }
}
module scale() {
   
difference() {
	body();
   difference() {
	translate([0,-24,2])
     rotate([0,90,0])
       cylinder(r=11,h=20,center=true,$fn=48);
      translate([0,0,-5.2]) cube([100,100,2],center=true);
   }
   difference() {
	translate([18,0,16.4])
     rotate([90,0,22])
       cylinder(r=17,h=80,center=true,$fn=60);
      translate([0,0,-5.2]) cube([100,100,2],center=true);
   }
   difference() {
	 translate([-18,0,16.4])
     rotate([90,0,-22])
       cylinder(r=17,h=80,center=true,$fn=60);
      translate([0,0,-5.2]) cube([100,100,2],center=true);
   }
}
}

//translate([20,0,0])
//   rotate([-35,0,0])
      scale();
//translate([0,20,0]) rotate([0,45,0])   scale();
//translate([0,0,15]) rotate([0,-60,45]) scale();



