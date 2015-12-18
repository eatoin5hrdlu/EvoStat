module blocka(len,wid,ht,slot,n) {
      translate([-4,-8,4]) cube([40,wid/2,12]);
      translate([len/5,0,0])
		   cube([len/2,wid,ht],center=true);
}

module blockr(len, wid, ht, slot, n) {
   tuber = 3;
   yoff = 8;
	difference() {
      blocka(len,wid,ht,slot,n);    
		section(slot,wid,ht,tuber,n,yoff);
      translate([3*slot,0,0])
        mirror([1,0,0])
		    section(slot,wid,ht,tuber,n,-yoff);
   }
translate([0,0,2])
   pip(slot,wid,ht,3,n, 0.5+slot/2, yoff);
translate([0,0,2])
   pip(slot,wid,ht,3,n, -(slot/2+3*tuber/2), -yoff);
			
}

module section(slot,wid,ht,tuber, n,yoff) {
		translate([2*n*slot-(7*slot)/8,yoff,4+2*ht/3])
			cube([slot,4*wid,2+2*ht], center=true);
   	translate([2*n*slot,yoff,ht/3])
         cylinder(r=tuber,h=3*ht,center=true);
}

module pip(slot,wid,ht,tuber,n, xoff, yoff) {
       translate([xoff+2*n*slot-tuber/2-0.5,yoff,-2.5])
         sphere(tuber/3,center=true,$fn=8);
   	translate([xoff+2*n*slot-tuber/2-0.5,yoff,-1])
         cylinder(r=tuber/3,h=1+ht/5,center=true,$fn=8);
       translate([xoff+2*n*slot-tuber/2-0.5,yoff,0.5])
         sphere(tuber/3,center=true,$fn=8);
}
difference() {
   blockr(80,32,10,8,1 );
   translate([20,0,10])
        rotate([0,90,0]) cylinder(r=3.5,h=70,center=true);
}



