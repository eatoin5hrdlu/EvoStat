
module clip(lgt,wid,hgt,rad) {
   difference() {
     cube([lgt,wid,hgt],center=true);
     translate([0,-0.25,-2])
             cube([lgt-2,(4*wid/7)-1,hgt],center=true);
     translate([0,-wid/2,-3])
             cube([lgt-6,wid,hgt],center=true);

      translate([-lgt/2,wid/2,0])
         rotate([0,0,45])
           cube([lgt,wid/4,hgt+2],            center=true);
        translate([-lgt/2,-wid/2,0])
         rotate([0,0,-45])
           cube([lgt,wid/4,hgt+2],            center=true);
        translate([lgt/2,wid/2,0])
         rotate([0,0,-45])
           cube([lgt,wid/4,hgt+2],            center=true);
        translate([lgt/2,-wid/2,0])
         rotate([0,0,45])
           cube([lgt,wid/4,hgt+2],            center=true);
       
        translate([0,0,3.5])
          rotate([90,0,0])
            cylinder(r=rad,h=10,center=true, $fn=24);
	     translate([0,-2.6,5])
              cylinder(r=3.6,h=10,center=true,$fn=24);
   }

}

rotate([180,0,0]) clip(16,7,20,2);
