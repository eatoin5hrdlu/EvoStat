length = 14;
width = 14;
thickness = 2;


difference() {
cube([length,width,thickness],center=true);
translate([length/4-1,0,0])
    cube([5,6,thickness+1],center=true);
translate([length/3,0,0])
    cube([2,11,thickness+1],center=true);
}

