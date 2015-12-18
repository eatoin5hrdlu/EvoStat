// 2L
insideradius = 20;
outsideradius = 27.5;

// 250mL
//insideradius = 11;
//outsideradius = 18.3;

// 100mL
//insideradius = 9.3;
//outsideradius = 16.1;

// Loose fitting
//insideradius = 9;
//outsideradius = 16.5;

// Small
//height = 12;
//inletradius = 1.5;
//filterradius= 2.8;
//inducerradius = 1.2;
//sampleradius = 1.5;
height = 12;
inletradius = 1.6;
filterradius= 3.1;
inducerradius = 1.5;
sampleradius = 1.8;

// small bottles
//c0 = 14;
//cA = c0;
//cB = c0;
//large bottle
c0 = 18;
cA = c0;
cB = c0;

// small 
// t0 = 10;  // tab spacing
// cx = 7;
//Large
t0 = 14;  // tab spacing

tt = 3;   // tab thickness
//i0 = 4;  //inlet spacing small
i0 = 8;   // inlet spacing large
cx = 15;
module cross() {
   cube([cx+insideradius,2,height-3],center=true);
   cube([2,cx+insideradius,height-3],center=true);
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
		    cube([cA,cB,height+2],center=true);
     translate([-t0,-t0,0])
		    cube([cA,cB,height+2],center=true);
     translate([-t0,t0,0])
		    cube([cA,cB,height+2],center=true);
     translate([t0,-t0,0])
		    cube([cA,cB,height+2],center=true);
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
septa();
translate([0,0,2]) cross();
