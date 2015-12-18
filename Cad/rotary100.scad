//
// Block to hold tubing and modulate liquid flow
// 

module rotary()
{
	module hole(r, h) {
		rotate([0,0,0])
			cylinder(r=r, h=h,center=true);
	}

	difference() {
	// BODY (to be subtracted from)
	 translate([long,width/2,0])
	  rotate([0,0,alpha])
	   cube([length,width,thick],center=true);

	// MOUNTING HOLES
	 translate([long+width/2,2*offset,0])
	   hole(hradius, thickplus);

   translate([medium+width/2, width-offset-1, 0])
	   hole( hradius, thickplus);

	// WIRE TROUGH
	 translate([long+4,2+width/3,thickplus/2-dia])
	   rotate([0,0,1.3*alpha])
            cube([2*length,dia, dia+0.3],center=true);

	// CORNER CUT-OUT
	 translate([long-width/5,3,0])
	  rotate([0,0,alpha])
	   cube([length/1.5,width/1.9,thickplus],center=true);

	 translate([long+14,width-2,0])
	  rotate([0,0,alpha])
	   cube([length/2,width/3,thickplus],center=true);

	 translate([medium+3,width-8,0])
	  rotate([0,0,alpha])
	   cube([1+length/4,2+width/4,thickplus],center=true);
}

	alpha  = 12; // Angle between radials in degrees

	length = 42; // mm   Main Block
	width  = 35; // mm
	thick = 8;  // 8mm

	thickplus = thick + 2;  // plus margin

	long   = 24; // mm  // Distances from center
	medium = 3; // mm
//	long   = 154; // mm  // Distances from center
//	medium = 133; // mm
	short  = 120; // mm

	hradius = 3;    // mounting hole radius mm
	dia     = 1.5;  // wire trough mm
	offset  = 5.6;  // distance from edge to mounting hole
}

rotary();

