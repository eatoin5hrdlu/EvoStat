
module blockr(len, wid, hgt, rad,slot, pipoff) {
	difference() {
      translate([4,0,0])
        cube([len,wid,hgt],center=true);
		//translate([1-len/3,0])
      //   cube([2+len/2,wid/2,hgt],center=true);
	   translate([3-rad,0,0])
		  cylinder(r=rad,h=hgt*2,center=true,$fn=32);
      translate([-4,0,0])
		   rotate([90,0,0])
            cylinder(r=2+hgt/2,h=hgt*2,center=true,$fn=32);
      translate([0,wid/2-2,hgt/2-2])
		   rotate([0,90,0])
            cylinder(r=1,h=len*3,center=true,$fn=32);
      translate([0,-(wid/2-2),hgt/2-2])
		   rotate([0,90,0])
            cylinder(r=1,h=len*3,center=true,$fn=32);
      translate([0,wid/2-2,-(hgt/2-2)])
		   rotate([0,90,0])
            cylinder(r=1,h=len*3,center=true,$fn=32);
      translate([0,-(wid/2-2),-(hgt/2-2)])
		   rotate([0,90,0])
            cylinder(r=1,h=len*3,center=true,$fn=32);
   }
   translate([pipoff,0,2])
      pip(slot,wid,hgt,rad,1, pipoff, 0);
			
}

module pip(slot,wid,ht, tuber,n, xoff, yoff) {
       translate([xoff,yoff,-3.5])
         sphere(tuber/3,center=true,$fn=12);
   	translate([xoff,yoff,-2])
         cylinder(r=tuber/3,h=0.5+ht/4,center=true,$fn=12);
       translate([xoff,yoff,-0.5])
         sphere(tuber/3,center=true,$fn=12);
}

rotate([0,90,0]) {
translate([0,-7,0])
   blockr(6,12,10,  3,  3,   1.5);
translate([0,7,0])
    blockr(6,12,10,  1.5, 1.8,   1.4);
}



