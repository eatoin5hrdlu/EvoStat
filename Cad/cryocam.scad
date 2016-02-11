module alum(side) {

 color([0.5,0.5,0.5])
   difference() {
    cube([side,side,side],center=true);
    cylinder(r=side/3, h=104, center=true, $fn=128);
 }
}

module heatsink(h) {
  translate([0,0,-h/4])
       cube([h,h,4],center=true);
  for(x=[-14:4:14]) {
     for (y=[-14:4:14]) {
           translate([x,y,0])
              cube([2,2,h/2],center=true);
     }
  }

}


module peltier(side,depth) {
    color([0,0,1]) cube([depth,side,side],center=true);
    color([1,0,0]) 
       translate([depth,0,0])
        cube([depth,side,side],center=true);
    color([0.5,0.5,0.5])
    translate([side/2-depth,0,0])
         rotate([0,90,0])
           heatsink(side);
}

module assem(side) {
   dist = (side + side/5)/2;
   alum(side);
   rotate([90,0,0])
     translate([(side+(side/5))/2,0,0])
       peltier(side,2);
   rotate([-90,0,0])
     translate([-dist,0,0])
      rotate([0,0,180])
       peltier(side,2); 

  translate([0,dist,0])
   rotate([0,0,90])
       peltier(side,2);

  translate([0,-dist,0])
   rotate([0,0,-90])
       peltier(side,2);
}


 //rotate([45,0,0])
    assem(30);
//peltier(30,2);
//heatsink(50);
