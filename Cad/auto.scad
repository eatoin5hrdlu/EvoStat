include <core4.scad>

length = 120;
width = 32;
channels = 4;
tube_radius = 1.5;
spacing = 18;
offset = 4;

module tubes(num, radius, spacing)
{
  holelength = 38;
  for(y =[spacing:spacing:spacing*num])
  {
    translate([0,y,0])
      cylinder(r=radius,h=12,center=true,$fn=24);
 }
}

module frame(width,length,height,mount_length)
{
   offset = width/4;

	difference()
	{
      union() {
        cube([width,length,height],center=true); //BLOCK
        translate([0,length/2,-height/3])
        cube([width,2*width,height/3],center=true); // MOTOR PLATFORM
      }

        // MAIN CAVITY
        translate([offset,-offset,offset])
		    cube([width,length-mount_length,height],center=true);

        // MOTOR MOUNTING HOLE
        translate([0,length/2,0])
         rotate([90,0,0])
          cylinder(r=0.5+(width-width/4)/2,h=8+length/2,center=true);
        // BEARING (through) HOLE
        translate([0,-length/2,0])
          rotate([90,0,0])
            cylinder(r=3,h=length,center=true,$fn=18);
        // BEARING HOLDER
        translate([0,5-length/2,0])
          rotate([90,0,0])
            cylinder(r=7.8,h=6,center=true,$fn=36);
        translate([-4,-(3+spacing*3),-12])
             tubes(4,1.9,spacing);
        // Photointerrupter Slot
        translate([-12,28.5,8])
			  cube([14,17,20],center=true);
        translate([-15,28.5,8])
			  cube([2,24,38],center=true);

// Mounting screw for Interrupter board
        translate([-15,28.5,-8])
          rotate([0,90,0])
                     cylinder(r=0.7,h=8,center=true,$fn=12);

//Curtain rod slots
        translate([-2,0,15.2])
             rotate([90,0,0])
                 cylinder(r=1.2,h=200,center=true,$fn=12);
         //       cube([3,130,6],center=true);

	}
}

module slots(num, spacing)
{
  for(y =[spacing:spacing:spacing*num])
        {
     translate([-3,4+y,-20])
     {
        cube([13,2,40]);
        translate([10,1,6])
            cylinder(r=3,h=14,center=true,$fn=18);
     }
   }
}

module autosampler(width, length)
{
   mount_length = length/4;
   frame(width,length,width,mount_length);
}

difference() {
   union() {
      autosampler(width,length);
      translate([0,33,0])
        modcores(channels, tube_radius, spacing, 30, offset);
   }
   translate([0,-(2+length/2),0])
      slots(4,18);
}

