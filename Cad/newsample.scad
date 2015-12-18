module switch(len,wid,hgt) {
rad = 0.8;
con = 4.5;
hld = 3.25;
wld = 4;
	difference() {
        cube([len,wid,hgt],center=true);
	     for (i = [ -1 : 1 ]) {
		     translate([i*con,0,0])
		        cylinder(r=rad,h=hgt*4,center=true,$fn=12);
        }
	     for (j = [ -1, 1 ]) {
          for (k = [-1, 1]) {
		     translate([j*hld,k*wld,0])
		        cylinder(r=rad,h=hgt*4,center=true,$fn=12);
          }
        }
  }
}

module blockr(len, wid, hgt, rad,slot, pipoff) {

difference() {
   union() {
    translate([-1,0,-10])
        cube([16,wid,4],center=true);
	 difference() {
      translate([4,0,0])
        cube([len,wid,hgt],center=true);
      translate([-7,0,0])
		   rotate([90,0,0])
            cylinder(r=2+hgt/2,h=hgt*4,center=true,$fn=32);
    }
    }
		//translate([1-len/3,0])
      //   cube([2+len/2,wid/2,hgt],center=true);
	   translate([3-rad,0,0])
		  cylinder(r=rad,h=hgt*4,center=true,$fn=32);
   }
   translate([pipoff,0,2])
      pip(slot,wid,hgt,rad,1, pipoff, 0);
			
}

module pip(slot,wid,ht, tuber,n, xoff, yoff) {
       translate([xoff,yoff,-4])
         sphere(tuber/3,center=true,$fn=12);
   	translate([xoff,yoff,-2])
         cylinder(r=tuber/3,h=0.5+ht/4,center=true,$fn=12);
       translate([xoff,yoff,0])
         sphere(tuber/3,center=true,$fn=12);
}
module phaseswitch() {
  difference() {
   union() {
    cube([36,14,4],center=true);

     translate([16,0,12])
      rotate([0,90,0])switch(20,14,4);
     translate([-16,0,12])
      rotate([0,90,0]) switch(20,14,4);
   }  
  translate([0,0,2])
    cylinder(r1=1.16, r2=3,h=2.8,center=true,$fn=12);
  cylinder(r=1.16,h=10,center=true,$fn=12);
  }
}

module tubepair() {
  difference() {
  union() {
  translate([7,0,10])
   blockr(6,12,16,  3,  3,   1.5);
  translate([-6,0,10])
   rotate([0,0,180])
   blockr(6,12,16,  1.5, 1.8,   1.4);
  }  translate([0,0,2])
    cylinder(r1=1.16, r2=3,h=2.8,center=true,$fn=12);
  cylinder(r=1.16,h=10,center=true,$fn=12);
  }
}

module xreticule() {
  for (i = [-10:10]) {
    translate([i,0,8])
     cube([0.2,0.2,0.2],center=true);
   }
}

module zreticule() {
  for (i = [-14:14]) {
    translate([0,0,i])
     cube([0.2,0.2,0.2],center=true);
   }
}

module test() {
  translate([0,0,2]) xreticule();
  zreticule();
  tubepair();
  translate([0,18,0]) {
    xreticule();
   translate([0,0,2])
     phaseswitch();
  }
}

test();
//phaseswitch();
//tubepair();

