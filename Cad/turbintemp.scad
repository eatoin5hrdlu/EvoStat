height = 50;

module plug(d) 
{
rad = d/2;
q = d/3;

	difference() 
	{
	  cylinder(r=rad, h=height, center=true, $fn=24);
	  translate([0,rad/2,rad/2-height/2])
		 cube([d-6,d+2,height],center=true);
     translate([0,d-4,height/2])
       cube([2*d,2*d,d+6],center=true);
     translate([0,0,2+height/3])
        rotate([90,0,0])
          cylinder(r=4,h=20,center=true,$fn=24);

     translate([0,0,-rad])
     rotate([0,90,0])
          cylinder(r=2,h=20,center=true,$fn=24);

   for ( x = [4 : -3 : -6])
     translate([x,-q,-rad])
            cube([1.2,1.5,height-8],center=true);

translate([0,d-13,height/2-q])   
rotate([90,0,0])
   for ( x = [4 : -3 : -6])
     translate([x,-q,rad])
            cube([1.2,1.5,8],center=true);
   translate([rad-1,-3,3-height/2]) cube([1.2,1.2,height/2],center=true);
translate([rad-1,3,3-height/2]) cube([1.2,1.2,height/2],center=true);

   translate([2-d,0,0]) {
   translate([rad-1,-3,3-height/2]) cube([1.2,1.2,height/2],center=true);
translate([rad-1,3,3-height/2]) cube([1.2,1.2,height/2],center=true);
	}
//     translate([0,0,0])
//            cube([2,2,height]);
//	  rotate([0,0,90])
//	   translate([0,0,-q])
//		cube([rad,q,q],center=true);
  } // End of difference()
 // sphere(rad-2.5,center=true);
}

plug(20);