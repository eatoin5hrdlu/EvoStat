module clip(lgt,wid,hgt,rad) 
{
    difference() {
        cube([lgt,wid,hgt],center=true);
        translate([0,0,-2])
           cube([lgt-2,4*wid/7,hgt],center=true);
                translate([0,2,-2])
           cube([lgt-4,wid,hgt-1],center=true);
        rotate([90,0,0])
           cylinder(r=rad,h=2*wid,center=true,$fn=24);
   
    rotate([0,0,-45])
       translate([lgt/4,4*wid/3,0])
       cube([lgt/4,wid/3,hgt+2],center=true);
        rotate([0,0,45])
       translate([lgt/4,-4*wid/3,0])
       cube([lgt/4,wid/3,hgt+2],center=true);
        rotate([0,0,-45])
       translate([-lgt/4,-4*wid/3,0])
       cube([lgt/4,wid/3,hgt+2],center=true);
            rotate([0,0,45])
       translate([-lgt/4,4*wid/3,0])
       cube([lgt/4,wid/3,hgt+2],center=true);
    }
}
clip(15,6,17,1.4);
