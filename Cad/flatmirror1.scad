diameter=10;
height=4;
number=3;
distance=5;
baseline=30; 


module maghole(diameter,height,number,back) {
		translate([0,height*(number-2)/2,0])
        rotate([90,0,0])
         cylinder(r=diameter/2,h=height*(2*number-1),center=true);
    if(back == 1) {
		translate([0,-height*2*(number+1)/2,0])
        rotate([90,0,0])
         cylinder(r=diameter/2,h=height*number,center=true);
    }
}

module smaghole(diameter,height,number,back) {
		translate([0,.15+height*(number-2)/2,0])
        rotate([90,0,0])
         cylinder(r=0.3+diameter/2,h=height*(2*number-1),center=true);
    if(back == 1) {
		translate([0,-(0.15+height*2*(number+1)/2),0])
        rotate([90,0,0])
         cylinder(r=0.3+diameter/2,h=height*number,center=true);
    }
}
module mirror(diameter,height,number,distance,baseline) {
width=diameter+4;
depth=distance+height*number+4;
length=baseline+diameter+8;
mirrorthick = 2.6;
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
          cube([width+2,mirrorthick+0.5,length-(2*diameter+5)],center=true);
     }
}

module sensor(diameter,height,number,distance,baseline) {
width=diameter+4;
depth=distance+height*number+4;
length=baseline+diameter+8;
mirrorthick = 2.6;
extra=2*width+4;

   difference() {
     translate([2+width/2,-height,0])
	   cube([extra,  // Small dimension
            depth, // Optical path dim
		      length],center=true);
      translate([0,0,2+baseline/2])
             smaghole(diameter,height,number,1);
     translate([0,0,-(2+baseline/2)])
             smaghole(diameter,height,number,1);
//LASER
     translate([0,0,4])
        rotate([90,0,0])
             cylinder(r=3.1,h=40,center=true,$fn=24);
//PhotoTransistor
     translate([0,0,-2])
        rotate([90,0,0])
             cylinder(r=2,h=40,center=true,$fn=12);
// TEMP SENSOR
     translate([2+width,0,0])
        rotate([90,0,0])
             cylinder(r=4.6,h=40,center=true);

     }

     difference() {
     translate([0,-1,-2.5])cube([5,6,5],center=true);
     translate([0,-4,0])
        cylinder(r=3,h=10,center=true,$fn=12);
     translate([0,0,-1.2])
        rotate([90,0,0])
             cylinder(r=0.4,h=40,center=true,$fn=6);
     translate([0,0,-2.8])
        rotate([90,0,0])
             cylinder(r=0.4,h=40,center=true,$fn=6);
     }
}

rotate([0,90,0])
  // mirror(diameter,height,number,distance,baseline);
     sensor(diameter,height,number,distance,baseline);