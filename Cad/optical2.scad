
xsize = 40;
ysize = 60;
width = 14;
dia = 3.5;
module shole() {
	 translate([-xsize/8,ysize/4,1])
	  cylinder(r=2*width/3,h=2*width/3,center=true);
	 translate([-xsize/4,-ysize/4,0])
     rotate([90,0,0])
	   cylinder(r=dia, h = ysize, center=true);

	 translate([xsize/8,-ysize/4,1])
	  cylinder(r=2*width/3,h=2*width/3,center=true);
	 translate([xsize/4,ysize/4,0])
     rotate([90,0,0])
	   cylinder(r=dia, h = ysize, center=true);
    rotate([90,0,0])
	  cylinder(r=dia,h=2*ysize/3,center=true);


//upper right
	translate([xsize/2 - 6, 6-ysize/2, 0])
		cylinder(r=1.5,h=width+2,center=true,$fn=8);
//lower left
	translate([6-xsize/2, ysize/2 - 6, 0])
		cylinder(r=1.5,h=width+2,center=true,$fn=8);
}

module top() {

	difference() {
		cube([xsize,ysize,10],center=true);
	   shole();
	   translate([0,0,width/2])
	     cube([xsize-4,ysize-4,width],center=true);
   translate([10-xsize/2,-ysize/2,4]) cube([6.5,7,7],center=true);
   translate([xsize/2-10, ysize/2,4]) cube([6.5,7,7],center=true);
	//optical hole
		cylinder(r=dia/2,h=width+2,center=true,$fn=8);
	}
}


module bottom() {
	difference() {
		cube([xsize-4,ysize-4,width],center=true);
	   shole();
	   translate([0,0,-width/2])
	     cube([xsize,ysize,width],center=true);

	}
}

top();
translate([xsize,0,2])
 rotate([180,0,0])
  bottom();
