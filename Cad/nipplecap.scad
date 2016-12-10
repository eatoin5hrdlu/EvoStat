module cap(d,t) {
  difference() {
      cube([d+t,d+t,t],center=true);
      translate([0,0,2])
       cube([d+2,d+2,t+2],center=true);
  }
  cube([d,d,t],center=true);
}

module tube(bore, off) {// luer connection
 $fn=32;
 Rin=0.75;
 Rout=1.25;
 big = 2.9;
 lil = 2.5;

 difference(){
	union(){
		translate([0,0,-off]) cylinder(r=lil,h=6);
		translate([0,0,-off]) cylinder(r1=lil,r2=big,h=0.8);
            translate([0,0,-off+0.8]) cylinder(r1=big,r2=lil,h=0.7);
	}
	//inside
	translate([0,0,-off-0.1]) cylinder(r=bore,h=7);
	translate([0,0,-5.1]) cylinder(r1=bore,r2=Rin,h=5.2);

 }
}


module flow(d,t,rad) {
    difference() {
          cap(d,t);
          cylinder(r=rad,h=2*t,center=true,$fn=32);
    }
   tube(rad,7);
}

//cap(10,4);
rotate([180,0,0]) flow(10,4,1.5);
