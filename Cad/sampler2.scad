module blocka(len,wid,ht,slot,n) {
      translate([-4,-8,4]) cube([40,wid/2,12]);
      translate([len/5,0,0])
		   cube([len/2,wid,ht],center=true);
}

module blockr(len, wid, ht, slot, n) {
   tuber = 3;
   yoff = 10;
	difference() {
      blocka(len,wid,ht,slot,n); 
      translate([3*slot/2,0,7])
         cube([16,14,18],center=true);   
		section(slot,wid,ht,tuber,n,yoff);
      translate([3*slot,0,0])
        mirror([1,0,0])
		    section(slot,wid,ht,tuber,n,-yoff);
   }
   translate([0,4,0])
   wedge(slot,wid,ht,tuber,n,yoff-1.5);
   translate([3*slot,0,0])
     mirror([1,0,0])
        wedge(slot,wid,ht,tuber,n,-yoff-1.5);
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

module wedge(slot,wid,ht,tuber,n,yoff) {

  difference() {
		translate([2*n*slot+4,yoff,0])
          cube([10,9,10],center=true);
		translate([2*n*slot-slot+1,yoff,3])
          rotate([0,45,0]) cube([18,10,12],center=true);
		translate([2*n*slot,yoff+1.5,0])
          cylinder(r=tuber,h=3*ht,center=true);
		translate([2*n*slot,yoff+6,3])
          cube([2*tuber,2*tuber,2*tuber],center=true);
  }
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
        rotate([0,90,0]) cylinder(r=3.15,h=70,center=true,$fn=18);
}



