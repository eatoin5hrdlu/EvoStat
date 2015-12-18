
module rotary()
{
	function r_from_dia(d) = d / 2;

	module hole(r, h) {
		rotate([0,0,0])
			cylinder(r=r, h=h,center=true);
	}

	difference() {
      translate([long,width/2,0])
		 rotate([0,0,alpha])
          cube([length,width,thick],center=true);
	  translate([long+width/2,2*offset,0])
		    hole(hradius, thickplus);
      translate([medium+width/2, width-offset, 0])
		    hole( hradius, thickplus);
      translate([long,width/3,thickplus/2-dia])
		  rotate([0,0,2*alpha])
          cube([2*length,dia, dia+0.3],center=true);
      translate([long-width/4,0,0])
        rotate([0,0,2*alpha])
		   cube([length/2,width/2,thickplus],center=true);
	}

   alpha = 12;
	length = 60;
	width = 50;
   thick = 10;
   thickplus = thick + 2;
   long = 220;
   medium = 190;
   short  = 120;
   hradius = 3;
   dia   = 1.6;
   offset = 8;
}

rotary();

