include <innatrix_logo.scad>
insideradius = 9.3;
outsideradius = 16.1;

// Loose fitting
//insideradius = 9;
//outsideradius = 16.5;
height = 12;
inletradius = 1.5;
filterradius= 2.8;
inducerradius = 1.2;
sampleradius = 1.5;
c0 = 14;
t0 = 10;  // tab spacing
tt = 3;   // tab thickness
i0 = 4;  //inlet spacing

module cross() {
   cube([4+insideradius,2,height-3],center=true);
   cube([2,4+insideradius,height-3],center=true);
}

module base(size) {

    difference() {
     cylinder(r=outsideradius+(outsideradius/5),
            h=size,center=true);
     translate([0,0,size/2])
        cylinder(r=outsideradius,h=size,center=true);
    } 
}
module tabs(size) {
    difference() {
     cylinder(r=insideradius,h=size,center=true);
     translate([0,0,size/2])
        cylinder(r=insideradius-tt,h=size,center=true);
     translate([t0,t0,0])
		    cube([c0,c0,height+2],center=true);
     translate([-t0,-t0,0])
		    cube([c0,c0,height+2],center=true);
     translate([-t0,t0,0])
		    cube([c0,c0,height+2],center=true);
     translate([t0,-t0,0])
		    cube([c0,c0,height+2],center=true);
    } 
}
module cap() {
	base(height);
 // rotate([0,0,45])
    tabs(height);
}
module septa() {
     difference() {
        cap();
        translate([i0,i0,0])
			      cylinder(r=inletradius,h=30,center=true,$fn=10);
        translate([-i0,-i0,0])
			      cylinder(r=filterradius,h=30,center=true);
        translate([-i0,i0,0])
			      cylinder(r=inducerradius,h=30,center=true,$fn=8);
        translate([i0,-i0,0])
			      cylinder(r=sampleradius,h=30,center=true,$fn=10);

     }
}
module ring(rd,th,ht) {
  difference() {
     cylinder(r=rd+th/2,h=ht,center=true);
     cylinder(r=rd-th/2,h=ht,center=true);
    }
}

module dualring(rd,th,ht) {
	difference() {
     cylinder(r=rd,h=ht,center=true);
     ring(rd-7,th,ht);
   }
}
module curvedlogo() {
  difference() {
   translate([0,-14,0])
     rotate([90,180,0])
      scale([0.15,0.15,1.0]) innatrix_logo(8);
   translate([0,0,0]) dualring(26.2,1.3,14);
  }
}

//difference() {
   union() {
      septa();
      translate([0,0,2]) cross();
    }
    curvedlogo();

//}
//difference() {
//translate([0,-17,0])
  //  rotate([90,180,0])
  //    scale([0.16,0.16,1.0]) innatrix_logo(8);
//  difference() {
 //    cylinder(r=10+outsideradius+(outsideradius/5),
  //          h=13,center=true);

  //   cylinder(r=1+outsideradius+(outsideradius/5),h=13,center=true);
//    }
 // }



