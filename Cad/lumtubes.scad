default_radius = 20;
default_height = 8;
default_depth = 16;
$fn = 128;

module base(radius, height, depth) 
{
  size = 2*radius+4;
  difference()
	 {
      union()
		{
   //      translate([0,-(1+(3*radius/2)),depth/3])
   //        cube([10,6+radius,depth-2.67],center=true);
			cube([size,2*size,depth/2],center=true);
		   translate([0,0,depth/2])
 				cylinder(r=radius,h=height,center=true);
		}
      translate([0,0,1])
	      cube([25,2*size,depth/6],center=true);
		translate([0,0,1+depth/2])
			cylinder(r=radius-2,h=height+10,center=true);
      
      for(p=[0:5:20])
		 translate([p-10,0,1])
        rotate([90,0,0])
			cylinder(r=2.5,h=5*radius,center=true);
	}
}

base(default_radius,default_height,default_depth);