ventradius = 2.7;
fillradius = 2.7;

module base(size) {

   translate([0,0,0.5]) {
    difference() {
     cylinder(r=3*size/2,h=2,center=true);
     cylinder(r=(3*size/2)-size/6,h=3,center=true);
    } 
   }

   translate([0,0,2.5])
    cylinder(r=3*size/2,h=3,center=true);
	cylinder(r1=size-0.5, r2=size+0.25, h=8, center=true);
	translate([0,0,2])
            cylinder(r2=size+0.5,r1=size+0.25, h=3, center=true);
	translate([0,0,-3])
            cylinder(r2=size+0.25,r1=size, h=3, center=true);
}

module tube(outer,inner,length) {
  difference() {
   union() {
	cylinder(r1=outer,r2=outer+1,h=length,center=true);
  translate([0,0,4-length/2])
     cylinder(r=outer+1,h=2,center=true);
   }
	cylinder(r=inner,h=length+2,center=true);
  }

}
module septa() {
translate([6,-8,-12])tube(3.5,fillradius,20);
translate([6,8,-12]) tube(3.5,fillradius,20);

difference() {
	base(16.5);
	translate([6,-8,0])
           cylinder(r=fillradius,h=20,center=true);
	translate([6,8,0])
           cylinder(r=fillradius,h=20,center=true);
	translate([-9,0,0])
	    rotate([0,-28,0])
           cylinder(r=ventradius,h=20,center=true);
}
}

rotate([180,0,0]) septa();

