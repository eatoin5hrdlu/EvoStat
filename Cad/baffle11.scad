length = 14;
width = 14;
thickness = 2;

module baffle(slot) {
 difference() {
  union() {
     cube([length,width,thickness],center=true);
     translate([1+length/3,0,0])
     cube([length/3,width,5*thickness],center=true);
  }
  translate([1+length/3,0,5*thickness/2])
    cube([2,slot,20],center=true);

    translate([3.2+length/3,0,0])
  difference() {
      cylinder(r=6.35,h=20,center=true,$fn=64);
      translate([7,0,0])
         cube([20,20,20],center=true);
  }
 }
}
module jarwall() {
  difference() {
      cube([30,30,30],center=true);
      translate([25,0,0])
         cylinder(r=30,h=40,center=true,$fn=64);
  }
}

rotate([0,90,0])
  difference() {
    baffle(11);
    jarwall();
  }

