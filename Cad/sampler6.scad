module blockr(len, wid, ht, slot, n) {
   tuber = 3;
   stuber = 2.4;
   yoff = 11;


	difference() { 
      translate([len/3+4,0,8])
         cube([len,wid,ht+8],center=true);
   translate([15.5,0,18]) cube([21,14,16], center=true);
      translate([3*slot/2+4,0,7])
         cube([20,14,18],center=true);
  
		ssection(slot,wid,ht,stuber,n,yoff);
      translate([3*slot,0,0])
        mirror([1,0,0])
		    section(slot,wid,ht,tuber,n,-yoff);

   }
   translate([0,4,0])

translate([0,0,2])
   pip(slot,wid,ht,3,n, 0.5+slot/2, yoff-4);
translate([0,0,2])
   pip(slot,wid,ht,3,n, -(slot/2+3*tuber/2), -yoff);
			
}

module section(slot,wid,ht,tuber, n,yoff) {
		translate([2*n*slot-(7*slot)/8,yoff,5])
			cube([slot,4*wid,ht/2], center=true);
   	translate([2*n*slot,yoff,ht/3])
         cylinder(r=tuber,h=3*ht,center=true,$fn=10);
      translate([2*n*slot-1.75,yoff,ht/2+2])
         rotate([0,60,0])
           cube([9,12,6], center=true);
      translate([2*n*slot-6,yoff,ht/2+8])
           cube([10,12,16], center=true);
}
module ssection(slot,wid,ht,tuber, n,yoff) {
		translate([2*n*slot-(7*slot)/8,yoff,5])
			cube([slot,4*wid,ht/2], center=true);
   	translate([2*n*slot+0.5,yoff,ht/3])
         cylinder(r=tuber,h=3*ht,center=true,$fn=10);
      translate([2*n*slot-1.75,yoff,ht/2+2])
         rotate([0,60,0])
           cube([9,12,6], center=true);
      translate([2*n*slot-6,yoff,ht/2+8])
           cube([10,12,16], center=true);
}


module pip(slot,wid,ht,tuber,n, xoff, yoff) {
       translate([xoff+2*n*slot-tuber/2-0.5,yoff,1.8])
         sphere(tuber/3,center=true,$fn=8);

   	translate([xoff+2*n*slot-tuber/2-0.5,yoff,-1])
         cylinder(r=tuber/3,h=1+ht/5,center=true,$fn=8);


       translate([xoff+2*n*slot-tuber/2-0.5,yoff,-4])
         sphere(tuber/3,center=true,$fn=8);
}
difference() {
   blockr(36,32,20,8,1 );
   translate([20,0,10+3.5])
        rotate([0,90,0]) cylinder(r=3.4,h=70,center=true,$fn=18);
   translate([35,-17,0]) cube([8,34,48], center=true);
   translate([-3,17,0])  cube([8,34,48], center=true);
   translate([26,8,-4]) cylinder(r=1.5,h=8,center=true,$fn=8);
   translate([2, -6,-4]) cylinder(r=1.5,h=8,center=true,$fn=8);
}



