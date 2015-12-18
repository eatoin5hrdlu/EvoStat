//
// Block to hold tubing and modulate liquid flow
// 

module rotary()
{
	module hole(r, h) {
		rotate([0,0,0])
			cylinder(r=r, h=h,center=true);
	}
	// MOUNTING POSTS
	module posts() {
	 translate([114.26,0,-3])
	   hole(hradius, thickplus);
//	 translate([112,22.28,-3])
//	   hole(hradius, thickplus);

   translate([98.61, 0, -3])
	   hole( hradius, thickplus);
   translate([96.62, 19.23, -3])
	   hole( hradius, thickplus);

   translate([80.7, 15.85, -3])
	   hole( hradius, thickplus);
   }

    translate([-70,0,0]) rotate([0,0,alpha/2]) posts();
	difference() {
	// BODY (to be subtracted from)
	 translate([long,width/2,0])
	  rotate([0,0,alpha])
	   cube([length,width,thick],center=true);

	// WIRE TROUGH
	 translate([long+25,2+width/3,thickplus/2-dia])
	   rotate([0,0,90+1.2*alpha])
            cube([2*length,dia, dia+0.3],center=true);

	 translate([long+4,2+width/3,thickplus/2-dia])
	   rotate([0,0,1.2*alpha])
            cube([2*length,dia, dia+0.3],center=true);

    // WIRE CUT-OUT
	 translate([long+width-9.5,width-12,0])
	  rotate([0,0,45+alpha])
	   cube([20,32,thickplus],center=true);

	// CORNER CUT-OUTS
// lower left
	 translate([long-6-width/2,0,0])
	  rotate([0,0,alpha])
	   cube([length-10,width/1.9,thickplus],center=true);
// upper right
	 translate([long+20,width-2,0])
	  rotate([0,0,alpha])
	   cube([length/2,2+width/3,thickplus],center=true);

// Upper left
	 translate([medium-20,width-9,0])
	  rotate([0,0,alpha])
	   cube([length/3,2+width/4,thickplus],center=true);
}

	alpha  = 12; // Angle between radials in degrees

	length = 62; // mm   Main Block
	width  = 35; // mm
	thick = 8;  // 8mm

	thickplus = thick + 2;  // plus margin

	long   = 24; // mm  // Distances from center
	medium = 10; // mm
//	long   = 154; // mm  // Distances from center
//	medium = 133; // mm
	short  = 120; // mm

	hradius = 2.5;    // mounting hole radius mm
	dia     = 2.0;  // wire trough mm
	offset  = 5.6;  // distance from edge to mounting hole
}

rotate([180,0,0]) rotary();

