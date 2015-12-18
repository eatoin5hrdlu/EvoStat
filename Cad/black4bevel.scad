//
// Optical sensor to receive glass tube.
//
// dimensions subject to change, see variables
// at the bottom of this file
// 

  smooth = 12;  // Number of facets for round features
  hex = 6;
	length = 80; // mm   (76mm = 3") Main Block
	width  = 30; // mm
	thick = 20;  // mm
   dia  =  7.6; // 7.14 = (9*25.4)/32; // 6.4;  // 1/4 inch in mm
  sixscrewrad = 2; // 3.6mm = .146 = #6 screw hole
  heatsinkradius = 25.4*(7/16);
  ledradius = 3;
  phototransradius = 2.1;
  lighthole = 1.4;
  
  thickplus = thick + 2;  // plus margin
  lengthplus = length + 2;
  widthplus = width + 1;

module photohalf()
{
difference() {
	translate([0,width/2-1.5,0])
      cube([length,3,4],center=true);
	translate([0,width/2-3.2,2])
    rotate([-45,0,0])
      cube([length,3,4],center=true);
}

difference() {
	translate([0,1.5-width/2,0])
      cube([length,3,4],center=true);

	translate([0,3.2-width/2,2])
     rotate([45,0,0])
      cube([length,3,4],center=true);
}

difference() {

	sensor();
   translate([0,0,thick/2])
     cube([length+1,widthplus,thick],center=true);

   };
}

module ledhalf()
{

   difference() {
	  sensor();
     translate([0,0,-thick/2])
      cube([length+1,widthplus,thick],center=true);
	translate([0,2-width/2,0])
    rotate([45,0,0]) 
      cube([lengthplus,4,6],center=true);
	translate([0,-width/2,0.5])
      cube([lengthplus,4,6],center=true);
	translate([0,(width/2)-2,0])
    rotate([-45,0,0]) 
      cube([lengthplus,4,6],center=true);
	translate([0,width/2,0.5])
      cube([lengthplus,4,6],center=true);

    translate([2-length/2,0,0])
     rotate([-45,0,90])
	   cube([widthplus,4,6],center=true);
    translate([length/2-2,0,0])
     rotate([45,0,90])
	   cube([widthplus,4,6],center=true);
 translate([length/2,0,0.5])
  rotate([90,0,0])
   cube([4,6,widthplus],center=true);
 translate([-length/2,0,0.5])
  rotate([90,0,0])
   cube([4,6,widthplus],center=true);
}
}

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
  //   translate([0,0.5+3*dia/2,0])
  //      cube([lengthplus,3*dia,dia+1],center=true);

  // Screw holes
    translate([-length/4,width/3,0])
     rotate([0,0,90])
       cylinder(r=sixscrewrad, h=thickplus, center=true, $fn=smooth);

     translate([length/4,-width/3,0])
     rotate([0,0,90])
       cylinder(r=sixscrewrad, h=thickplus, center=true, $fn=smooth);
  // Optical hole
     cylinder(r=lighthole, h=thickplus/2, center=true, $fn=smooth);

  // LED Holder
    translate([0,0,thick/2])
     cylinder(r=heatsinkradius, h=4, center=true, $fn=hex);

    translate([0,0,thick/3])
     cylinder(r=ledradius, h=thick/4, center=true, $fn=smooth);

  // Phototransistor holder
    translate([0,0,2-thick/3])
     cylinder(r=phototransradius, h=1+thick/4, center=true, $fn=smooth);

// Phototansistor leads
     translate([1,0,-thick/2]) 
       cylinder(r=0.7, h=thickplus/2, center=false, $fn=smooth);
     translate([-1,0,-thick/2]) 
       cylinder(r=0.7, h=thickplus/2, center=false, $fn=smooth);

  } // end difference


}

ledhalf();

translate([0,40,thick/2])
  rotate([0,0,0])
    photohalf();

difference(){
 translate([length/2,40,thick/2])
  rotate([90,45,0])
   cube([4,4,width],center=true);
 translate([2+length/2,40,thick/2])
  rotate([90,0,0])
   cube([4,6,widthplus],center=true);
 translate([length/2-2,40,5+thick/2])
  rotate([90,0,0])
   cube([4,6,widthplus],center=true);
 translate([0,40,thick/2])
  rotate([0,90,0])
     cylinder(r=dia/2, h=8+lengthplus, center=true, $fn=smooth);

}

difference() {
 translate([-length/2,40,thick/2])
  rotate([90,-45,0])
   cube([4,4,width],center=true);
 translate([-(2+length/2),40,thick/2])
  rotate([90,0,0])
   cube([4,6,widthplus],center=true);
 translate([2-length/2,40,5+thick/2])
  rotate([90,0,0])
   cube([4,6,widthplus],center=true);
 translate([0,40,thick/2])
  rotate([0,90,0])
     cylinder(r=dia/2, h=8+lengthplus, center=true, $fn=smooth);


}
