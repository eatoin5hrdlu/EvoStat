//
// Optical sensor to receive glass tube.
//
// dimensions subject to change, see variables
// at the bottom of this file
// 

module sensor()
{

  // Optical hole


	difference() {
	// BODY (to be subtracted from)
   cube([length,width,thick],center=true);

	// Glass Tube Conduit
	   rotate([0,90,0])
        cylinder(r=dia/2, h=lengthplus, center=true, $fn=smooth);
  // Slot
     translate([0,0.5+3*dia/2,0])
        cube([lengthplus,3*dia,dia+1],center=true);

  // Optical hole
     cylinder(r=lighthole, h=thickplus/2, center=true, $fn=smooth);

  // LED Holder
    translate([0,0,thick/2])
     cylinder(r=heatsinkradius, h=4, center=true, $fn=smooth);

    translate([0,0,thick/3])
     cylinder(r=ledradius, h=thick/4, center=true, $fn=smooth);

  // Phototransistor holder
    translate([0,0,2-thick/3])
     cylinder(r=phototransradius, h=1+thick/4, center=true, $fn=smooth);

// Phototansistor leads
     translate([1,0,-thick/2]) 
       cylinder(r=0.5, h=thickplus/2, center=false, $fn=smooth);
     translate([-1,0,-thick/2]) 
       cylinder(r=0.5, h=thickplus/2, center=false, $fn=smooth);

  } // end difference

  smooth = 8;  // Number of facets for round features
	length = 80; // mm   (76mm = 3") Main Block
	width  = 30; // mm
	thick = 20;  // mm
   dia  = 6.4;  // 1/4 inch in mm
  heatsinkradius = 25.4*(7/16);
  ledradius = 3;
  phototransradius = 2.1;
	thickplus = thick + 2;  // plus margin
  lengthplus = length + 2;

}

rotate([90,0,0]) sensor();

