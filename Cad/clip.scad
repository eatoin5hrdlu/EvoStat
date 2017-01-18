
module clip(lgt,wid,hgt,rad) {
   difference() {
     cube([lgt,wid,hgt],center=true);
     translate([0,0,-2])
             cube([lgt-2,4*wid/7,hgt],center=true);
     translate([0,-wid/2,-3])
             cube([lgt-4,wid,hgt],center=true);

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
       rotate([90,0,0])
            cylinder(r=rad,h=10,center=true, $fn=24);
   }

}

rotate([180,0,0]) clip(15,6,17,1.4);
