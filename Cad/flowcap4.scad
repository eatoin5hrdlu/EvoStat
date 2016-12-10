

module cap(d,t) {
  difference() {
      cube([d+t+2,d+t+2,t],center=true);
      translate([0,0,2])
       cube([d+4.1,d+4.1,t+2],center=true);
  }
  cube([d-0.5,d-0.5,t],center=true);
}

module luer(off) {// luer connection
 $fn=32;
 Rin=1.5;
 Rout=2.1;
 

 difference(){
	union(){
		translate([0,0,-off]) cylinder(r1=6.6/2,r2=6.4/2,h=6);
		translate([0,0,-off]) cylinder(r=8.2/2,h=0.6);
      translate([0,0,-off+0.5]) cylinder(r1=8.2/2,r2=6.6/2,h=0.6);
	}
	//inside
	translate([0,0,-off-0.1]) cylinder(r=2.7,h=7);
	translate([0,0,-5.1]) cylinder(r1=2.7,r2=Rin,h=5.2);
	//notches
	translate([6.0/2,-8.5/2,-off-0.1]) cube([1.3,8.5,1.8]);
	translate([-6.0/2-1.3,-8.5/2,-off-0.1]) cube([1.3,8.5,1.8]);
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
rotate([180,0,0]) flow(10,4,2.7);
