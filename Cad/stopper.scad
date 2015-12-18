
up = 1;
down = 0;

function optimal_facets(rad) = max(6,2*round(round(3+2*rad)/2));

module stopper(outer, inner, thickness, angle, trad) {
  off = 6;
  difference() {
    union() {
      difference() {
        cylinder(r=outer,h=thickness,center=true); // CAP
        translate([0,0,-1])
         cylinder(r2=inner+2.2,r1=inner+2.5, h=thickness/2,center=true);
        translate([0,0,-5.5])
         cylinder(r2=inner+1.8,r1=inner+1.8, h=thickness/4,center=true);
      }
      cylinder(r2=inner-3.5,r1=inner-4,h=thickness,center=true); // Stopper
    }
    translate([0,off,-0.5])
        rotate([0,angle,0])
        cone(trad,0.5,down,thickness+10);
    translate([0,-off,-0.5])
        rotate([0,-angle,0])
         cone(trad,0.5,down,thickness+10);
    translate([off,0,-0.5])
        rotate([angle,0,0])
         cone(trad,0.5,down,thickness+10);
    translate([-off,0,-0.5])
        rotate([-angle,0,0])
         cone(trad,0.5,down,thickness+10);
     cone(2.5,0.5,down,thickness+10);

  } // Subtract tubes from cap+stopper
//
}

module cone(radius,delta,updown,len) {
   rad1 = radius;
   rad2 = radius + delta;
   facets = optimal_facets(radius);
   if (updown == 1)
	  cylinder(r1=rad2,r2=rad1,h=len,center=true,$fn=facets);
   else
     cylinder(r2=rad2,r1=rad1,h=len,center=true,$fn=facets);
}

translate([0,0,7])
  rotate([180,0,0])
    stopper(17,12.5,14,30,0.5);