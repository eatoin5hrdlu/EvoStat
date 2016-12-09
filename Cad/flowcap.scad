module cap(d,t) {
  difference() {
      cube([d+t,d+t,t],center=true);
      translate([0,0,2])
       cube([d+2,d+2,t+2],center=true);
  }
  cube([d,d,t],center=true);
}

module luer(off) {// luer connection
 $fn=32;
 Rin=0.75;
 Rout=1.25;
 

 difference(){
	union(){
		translate([0,0,-off]) cylinder(r1=5.6/2,r2=5.4/2,h=6);
		translate([0,0,-off]) cylinder(r=7.5/2,h=0.8);
      translate([0,0,-off+0.65]) cylinder(r1=7.5/2,r2=5.6/2,h=0.7);
	}
	//inside
	translate([0,0,-off-0.1]) cylinder(r=4.0/2,h=7);
	translate([0,0,-5.1]) cylinder(r1=4.0/2,r2=Rin,h=5.2);
	//notches
	translate([5.6/2,-7.5/2,-off-0.1]) cube([1,7.5,1.8]);
	translate([-5.6/2-1,-7.5/2,-off-0.1]) cube([1,7.5,1.8]);
 }
}


module flow(d,t,rad) {
    difference() {
          cap(d,t);
          cylinder(r=rad,h=2*t,center=true,$fn=32);
    }
    luer(7);
}

//cap(10,4);
flow(10,4,2);
