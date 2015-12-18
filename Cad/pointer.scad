

module rotary()
{

difference() {
	// BODY (to be subtracted from)
	cube([length,width,thick],center=true);

   // MOTOR SHAFT HOLE 
	translate([4*hradius-length/2, 0, 0])
	   cylinder(r=hradius, h=thickplus, center=true);
	// WIRES
	 translate([0,3.2,0])
	   rotate([0,90,0])
            cylinder(r=dia,h=length+width,center=true);

	 translate([0,-3.8,0])
	   rotate([0,90,0])
            cylinder(r=dia,h=length+width,center=true);

	translate([23,14,0])
		rotate([0,0,67])
			cube([width,width,thickplus],center=true);

	translate([23,-14,0])
		rotate([0,0,22])
			cube([width,width,thickplus],center=true);

	}
}

	alpha  = 12; // Angle between radials in degrees

	length = 40; // mm   Main Block
	width  = 18; // mm
	thick = 10;  // 8mm
    thickplus = 2*thick;

	hradius = 3.1;    // 6.3mm = .25"
	dia     = 1.3;  // 1/16" wire trough
	yoffset = 5;  // ydistance from edge to mounting hole
    zoffset = 5;

rotary();

