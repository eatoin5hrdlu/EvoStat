
module cap() {
difference() {
  union() {
   cylinder(r=10.2,h=6,center=true,$fn=64);
   translate([0,0,7])
    cylinder(r=18,h=10,center=true,$fn=128);

  }
  cylinder(r=8.2,h=8,center=true,$fn=64);
  translate([0,0,7])
      cylinder(r=14.5,h=8,center=true,$fn=128);
   translate([0,0,10])
      cylinder(r=15.25,h=8,center=true,$fn=128);
}
}

rotate([180,0,0]) 
   cap();