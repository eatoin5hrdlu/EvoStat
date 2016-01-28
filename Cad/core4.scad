
// Core tube holders for Autosampler

module bevelhole(rad)
{
  difference() {
	cube([10,10,14],center=true);
   translate([10,0,-14])
     rotate([0,45,0])
      cube([30,10,34],center=true);
   cylinder(r=rad,h=26,center=true,$fn=24);
  }
}

module capsule(rad,len)
{
	 cylinder(r=rad,h=len,center=true,$fn=18);
 	 translate([0,0, len/2])
		sphere(r=rad,center=true,$fn=18);  
 	 translate([0,0,-len/2])
		sphere(r=rad,center=true,$fn=18); 
}

module cores(num, rad, spacing, size, os)
{
  length = 3*size;
  holelength = 6;
  xpos = os-(1+rad+size/3);
  xpos2 = 2+os-(size/3);

  for(y =[spacing:spacing:spacing*num])
	{
	 translate([0.3-(size/5),y-length,0])
      capsule(rad,holelength);

// Eliminate overhanging end panels
    if (y < spacing*num) {
 	   translate([xpos,6+(y-length),3-holelength/2])
        cube([3,6,size],center=true);
    }
    if (y > spacing) {
 	   translate([xpos,(y-length)-6,3-holelength/2])
        cube([3,6,size],center=true);
    }
	 translate([xpos2,(y-length),0])
    {
	    translate([0,0,6])
         bevelhole(2);
	    translate([0,0,-6])
        rotate([180,0,0])
         bevelhole(2);
    }
	} // end of for(spacing...)
}

//   cores(4, 1.5, 18, 30, 4);

module modcores(A,B,C,D,E)
{
  difference() {
       cores(A,B,C,D,E);
       translate([0,0,0])
        cube([6,200,60],center=true);
   }
}
// modcores(4, 1.5, 18, 30, 4);

