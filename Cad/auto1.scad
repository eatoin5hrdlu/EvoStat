
function r_from_dia(d) = d / 2;


module tubes(num, diameter, spacing, size)
{
  length = 3*size;
  holelength = 38;
  for(y =[spacing:spacing:spacing*num])
	{
	 translate([2-size/5,y-length,-holelength/2])
     rotate([0,0,0])
		cylinder(r=r_from_dia(diameter),h=holelength,center=true,$fn=24);
	}
}
module bevelhole(diameter)
{
   rad = r_from_dia(diameter);
  difference() {
	cube([10,10,14],center=true);
   translate([10,0,-14])
     rotate([0,26,0])
      cube([30,10,34],center=true);
   cylinder(r=rad,h=26,center=true,$fn=24);
  }
}

module cores(num, diameter, spacing, size, os)
{
  length = 3*size;
  holelength = 6;
  rad = r_from_dia(diameter);
  for(y =[spacing:spacing:spacing*num])
	{
	 translate([-(size/5),y-length,0])
		cylinder(r=rad,h=holelength,center=true,$fn=18);
 	 translate([-(size/5),y-length, holelength/2])
		sphere(r=rad,center=true,$fn=18);  
 	 translate([-(size/5),y-length,-holelength/2])
		sphere(r=rad,center=true,$fn=18);

 	 translate([os-(1+rad+size/3),6+y-length,-holelength/2])
      cube([3,8,size],center=true);
 	 translate([os-(1+rad+size/3),y-length-6,-holelength/2])
      cube([3,8,size],center=true);
	 translate([2+os-(size/3),y-length,6])
        bevelhole(4);
	 translate([2+os-(size/3),y-length,-6])
      rotate([180,0,0])
        bevelhole(4);
	}
}
module bearing(inner,flip)
{
   size = 2*inner;
	thickness = size/4;
   length = 3*size;
   rotate([0,0,flip])
	difference() {
	 cube([size,thickness,size],center=true);
    translate([0,-3,0])
     rotate([90,0,0])
      cylinder(r=r_from_dia(inner),h=thickness,center=true);
     rotate([90,0,0])
      cylinder(r=r_from_dia(inner)/2,h=thickness+2,center=true,$fn=128);
	}
}

module frame(inner)
{
   size = 2*inner;
   length = 3*size;
	thickness = size/4;

   translate([0,2-length/2,-size/2])
      cube([size,length+4,thickness],center=true);
	bearing(inner,0);
//Step for Optical Interruptor
	translate([1+size/3,-thickness,1-size/3])
		 cube([size/4,2*thickness,size/3],center=true);

   translate([0,thickness/2-length,0])
		bearing(inner,180);

// Wall
   translate([-size/2,4-length,-size/2])
     cube([6,length-4,size]);
}

module autosampler()
{
 difference() {
   frame(15);
   tubes(4, 4, 18, 30);
   rotate([0,90,90])
     cylinder(r=11,h=40,center=true);
 }
   cores(4, 2, 18, 30, 4);
}

module cradle()
{
 difference() {
   translate([0,20,-3])
    cube([30,30,18],center=true);
   translate([0,10,7])
    rotate([0,90,90])
     cylinder(r=11,h=60,center=true);
 }
}
autosampler();
translate([0,-4,-6.75])
   cradle();



