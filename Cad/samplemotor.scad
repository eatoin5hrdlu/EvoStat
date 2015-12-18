motor_radius=11.2;
bearing_radius=7.1;

module motor() {
   r = motor_radius;
   s = 3*motor_radius;
   translate([0,0,2+r/2])
	difference() {
	   cube([s,2*s,s],center=true);
 	   rotate([90,0,0])
          cylinder(r=r,h=4*s,center=true,$fn=64);
   }
}
 
module bearing() {
          r = bearing_radius;
          s = 3*motor_radius;
      delta = r;
    translate([0,0,2+motor_radius/2]) 
      difference() {  
	      cube([s,2*delta,s],center=true);
         translate([0,delta-1,0])
 	       rotate([90,0,0])
           cylinder(r=r,h=delta+6,center=true,$fn=64);
 	      translate([0,1-delta,0])
          rotate([90,0,0])
           cylinder(r=r-2,h=delta+6,center=true,$fn=64);
   }
}


module switch(len,wid,hgt) {
rad = 0.9;
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
    translate([2,0,-10])
        cube([20,wid,4],center=true);
	 difference() {
      translate([4,0,0])
        cube([len,wid,hgt],center=true);
      translate([-8,0,0])
		   rotate([90,0,0])
            cylinder(r=3+hgt/2,h=hgt*4,center=true,$fn=32);
    }
    }
		//translate([1-len/3,0])
      //   cube([2+len/2,wid/2,hgt],center=true);
	   translate([3-rad,0,0])
		  cylinder(r=rad,h=hgt*4,center=true,$fn=32);
   }
   translate([pipoff,0,2])
      pip(slot,wid,hgt,rad+1,1, pipoff, 0);
			
}

module pip(slot,wid,ht, tuber,n, xoff, yoff) {
       translate([xoff,yoff,-4])
         sphere(tuber/2,center=true,$fn=12);
   	translate([xoff,yoff,-2])
         cylinder(r=tuber/2,h=0.5+ht/4,center=true,$fn=12);
       translate([xoff,yoff,0])
         sphere(tuber/2,center=true,$fn=12);
}
module phaseswitch() {
  difference() {
   union() {
    cube([36,14,4],center=true);

     translate([16,0,13])
      rotate([0,90,0])switch(22,14,4);
     translate([-16,0,13])
      rotate([0,90,0]) switch(22,14,4);
   }  
  translate([0,0,2])
    cylinder(r1=1.16, r2=3.8,h=2.8,center=true,$fn=12);
  cylinder(r=1.5,h=10,center=true,$fn=12);
  }
}

module tubepair() {
  difference() {
  union() {
  translate([6,0,8])
   blockr(10,18,16,  3,  3,   1.5);
  translate([-5,0,8])
   rotate([0,0,180])
   blockr(10,18,16,  2, 1.8,   1.4);
  }  translate([0,0,0])
    cylinder(r1=1.5, r2=3.8,h=2.8,center=true,$fn=12);
  cylinder(r=1.5,h=10,center=true,$fn=12);
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
 // translate([0,0,2]) xreticule();
 // zreticule();

//  translate([0,18,0]) {
//    xreticule();

  translate([0,0,-2])
        phaseswitch();
  for(i = [16:18:70]) {
   translate([0,i,0])
       tubepair();
  }
  translate([0,-40,0])
      motor();
  translate([0,86,0])
      bearing();
}

difference() {
test();
translate([0,0,-9])
  cube([40,240,10],center=true);
 }
//phaseswitch();
//tubepair();

