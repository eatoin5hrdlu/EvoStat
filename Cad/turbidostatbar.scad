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
mirrorthick = 1.8;
   difference() {
     translate([0,-height,0])
	   cube([width,  // Small dimension
            depth, // Optical path dim
		      length],center=true);
      translate([0,0,2+baseline/2])
             maghole(diameter,height,number,0);
     translate([0,0,-(2+baseline/2)])
             maghole(diameter,height,number,0);
     rotate([-10,0,0])
     translate([-4,0.5+depth/4,-4])
          cube([width+2,depth+2,((length-2*width)/2)-1.5],center=true);
     rotate([10,0,0])
     translate([-4,depth/4,+5])
          cube([width+2,depth+2,((length-2*width)/2)-1.5],center=true);
    rotate([8,0,0])
     translate([0,-depth/3,0.95])
          cube([width+2,mirrorthick+0.6,length-(2*diameter+5)],center=true);
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
             cylinder(r=3.2,h=40,center=true,$fn=24);
//PhotoTransistor
     translate([0,0,-2])
        rotate([90,0,0])
             cylinder(r=2,h=40,center=true,$fn=12);
// TEMP SENSOR
     translate([2+width,0,0])
        rotate([90,0,0])
             cylinder(r=4.7,h=40,center=true);

     }

     difference() {
     translate([0,-4,-2.5])cube([5,6,5],center=true);
     translate([0,-8,0])
        cylinder(r=3.2,h=10,center=true,$fn=12);
     translate([0,0,-1.2])
        rotate([90,0,0])
             cylinder(r=0.6,h=40,center=true,$fn=6);
     translate([0,0,-2.8])
        rotate([90,0,0])
             cylinder(r=0.6,h=40,center=true,$fn=6);
     }
}

module jarwall() {
  difference() {
     cube([40,40,120],center=true);
	  translate([40,0,0])
	     cylinder(r=50,h=120,center=true,$fn=64);
	   }
}

module jar() {
	  translate([-8,110,-10])
	          cylinder(r=50,h=120,center=true,$fn=64);
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
   translate([6,-60,3]) jar();
  }
  translate([-7,-(wall+30),21]) cube([wall,side+12,3]);

     translate([-7,-(wall+30),-24])
         cube([wall,side+12,3]);

  difference() {
  translate([23.5,-(wall+7),0])
    rotate([0,90,0])
    cube([side,deep,3],center=true);
     translate([24,-54,12])
			rotate([0,90,0]) 
           powerhole(4.5);
  }
}
module mirrordevice() {
  difference() { 
   mirror(diameter,height,number,distance,baseline);
   translate([0,-3.3,0]) rotate([0,0,-90]) jarwall();
  }
}

rotate([0,90,180]) 
  // sensordevice();
   mirrordevice();



     