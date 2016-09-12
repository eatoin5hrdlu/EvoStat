diameter=10;
height=4;
number=3;
distance=5;
baseline=36; 


module maghole(diameter,height,number,back) {
		translate([0,height*(number-2)/2,0])
        rotate([90,0,0])
         cylinder(r=diameter/2,h=height*(2*number-1),center=true);
    if(back == 1) {
		translate([0,-height*2*(number+1)/2,0])
        rotate([90,0,0])
         cylinder(r=diameter/6,h=height*number,center=true);
    }
}

module smaghole(diameter,height,number,back) {
// Final coordinates for transform are Z, Y, X
		translate([0,(height*(number-2)/2)-1.5,0])
        rotate([90,0,0])
         cylinder(r=0.3+diameter/2,h=height*4,center=true,$fs = 0.01);

    // Square Nut Hole
   translate([-9,-13,-5]) cube([4.3+diameter,3.2,diameter+0.8]);  
   // Pitch SetScrew Hole
    translate([0,(height*(number-2)/2),0]) 
        rotate([90,0,0])
             cylinder(r=1.5,h=40,center=true,$fn=16);  
}

module mirror(diameter,height,number,distance,baseline) {
width=diameter+4;
depth=distance+height*number+4;
length=baseline+diameter+8;
mirrorthick = 2.7;
   difference() {
     translate([0,-height,0])
	   cube([width,  // Small dimension
            depth, // Optical path dim
		      length],center=true);
      translate([0,0,2+baseline/2])
             maghole(diameter,height,number,0);
     translate([0,0,-(2+baseline/2)])
             maghole(diameter,height,number,0);
     translate([0,depth/4,0])
          cube([width+2,depth+2,length-2*width],center=true);
    rotate([7,0,0])
     translate([0,0.6-depth/3,0.75])
          cube([width+2,mirrorthick+0.6,length-(2*diameter+4)],center=true);
     }
}

module sensor(diameter,height,number,distance,baseline) {
width=diameter+4;
depth=distance+height*number+4;
length=baseline+diameter+8;
mirrorthick = 2.6;
//extra=2*width+4;
extra=2*width+8;
laser2015=64; // Degrees
lasera=74;
phtrans2015=116;
ptransa=106;
   difference() {
//     translate([2+width/2,-height,0])
     translate([4+width/2,-height,0])
	   cube([extra,  // Small dimension
            depth, // Optical path dim
		      length],center=true);
      translate([0,0,2+baseline/2])
             smaghole(diameter,height,number,1);
     translate([0,0,-(2+baseline/2)])
             smaghole(diameter,height,number,1);
//LASER
     translate([0,0,6])
        rotate([lasera,0,0])
             cylinder(r=3.7,h=40,center=true,$fn=32);
     translate([-11,0,6])
        rotate([lasera,0,0]) cube([10,2,40],center=true);
//PhotoTransistor
     translate([0,-12,-10])
        rotate([ptransa,0,0])
             cylinder(r=3.2,h=10,center=true,$fn=24);
     translate([0,0,-6.5])
        rotate([ptransa,0,0])
             cylinder(r=2,h=40,center=true,$fn=24);
     translate([-11,0,-6])
        rotate([ptransa,0,0]) cube([10,2,40],center=true);
//TEMP SENSOR
     translate([2+width,0,0])
        rotate([90,0,20])
             cylinder(r=4.9,h=40,center=true,$fn=64);
//SET SCREW
     translate([12+width,0,0])
        rotate([90,0,20])
             cylinder(r=1.5,h=40,center=true,$fn=64);
     }
//Through holes for ptrans
//translate([0,-5,-5.3])
//rotate([25,0,0])
 //    difference() {
  //   translate([0,-4,-2.5])cube([5,6,5],center=true);
  //   translate([0,-8,0])
   //     cylinder(r=3.2,h=10,center=true,$fn=12);
   //  translate([0,0,-1.0])
   //     rotate([90,0,0])
   //          cylinder(r=0.7,h=40,center=true,$fn=6);
   //  translate([0,0,-3])
    //    rotate([90,0,0])
     //        cylinder(r=0.7,h=40,center=true,$fn=6);
   //  }
}

module jarwall() {
  difference() {
     cube([40,40,120],center=true);
	  translate([40,0,0])
	     cylinder(r=50,h=120,center=true,$fn=128);
	   }
}

module jar() {
	  translate([-8,110,-10])
	          cylinder(r=78,h=120,center=true,$fn=128);
}

module powerhole(dim) {
offset = dim+2;
  //  cylinder(r=dim,h=2*dim,center=true,$fn=10);
  //  translate([dim-1.6,0,0])
   //   cylinder(r=dim,h=dim,center=true,$fn=10);
    translate([dim-3,0,0]) cube([2*dim+2,2*dim-1,2*dim-1],center=true);
    translate([-offset,0,0])
      cylinder(r=1.1,h=2*dim,center=true,$fn=10);
    translate([offset+2,0,0])
      cylinder(r=1.1,h=2*dim,center=true,$fn=10);
}

module sensordevice() {
  wall = 32;
  side = 44;
  deep = 50;
  difference() { 
   sensor(diameter,height,number,distance,baseline);
   translate([3,-30,3]) jar();
  }
//  translate([-7,-(wall+30),21]) cube([wall,side+12,3]);

//     translate([-7,-(wall+30),-24])
//         cube([wall,side+12,3]);

//  difference() {
//  translate([23.5,-(wall+7),0])
//    rotate([0,90,0])
//    cube([side,deep,3],center=true);
//     translate([24,-54,12])
//			rotate([0,90,0]) 
//           powerhole(4.5);
//  }
}
module mirrordevice() {
  difference() { 
   mirror(diameter,height,number,distance,baseline);
   translate([0,-3.3,0]) rotate([0,0,-90]) jarwall();
  }
}
  // mirrordevice();

module minimum() {
 difference() {
    rotate([0,90,180]) 
           sensordevice();
   translate([34,0,-30])
     rotate([0,35,0])
      cube([42,42,42],center=true);
   translate([-33,0,-30])
     rotate([0,-37,0])
      cube([42,42,42],center=true);
  }
}
//jarwall();
rotate([0,180,0]) minimum();



     