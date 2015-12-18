module body() {
   multmatrix(m = [ [1, 0, 0, 0],
                 [0, 1, -1.3, 0],
                 [0, 0, 1, 0],
                 [0, 0, 0, 1] ])
    resize([20,30,10])
       cylinder(r1=40,r2=1,h=10,center=true,$fn=24);
}

module scale() {
   
difference() {
	body();
   difference() {
	translate([0,-14.4,1.8])
     rotate([0,90,0])
       cylinder(r=9,h=20,center=true,$fn=48);
      translate([0,0,-5.2]) cube([100,100,2],center=true);
   }
   difference() {
	translate([16.1,0,12.4])
     rotate([90,0,22])
       cylinder(r=17,h=80,center=true,$fn=60);
      translate([0,0,-5.2]) cube([100,100,2],center=true);
   }
   difference() {
	 translate([-16.1,0,12.4])
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



