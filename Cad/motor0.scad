module mount(outer, length) {
	difference() {
     union() {
		cylinder(r=outer, h=length, center=true);
	    translate([0,0,-length/3])
		cylinder(r=outer+6, h=length/4, center=true);
      }
		cylinder(r=outer-3, h=length+2, center=true);
	    translate([outer-3,0,0])  // X
			cube([outer/3,outer/3,length+2], center=true);

	    translate([-(outer-3),0,0])  // -X
			cube([outer/3,outer/3,length+2], center=true);

	    translate([0,outer-3,0])     // Y
			cube([outer/3,outer/3,length+2], center=true);

	    translate([0,-(outer-3),0])  // -Y
			cube([outer/3,outer/3,length+2], center=true);

    }
}

module hole(x,y,radius) {
	translate([x,y,0])
	cylinder(r=radius,h=24,center=true);
}

module base(outer, inner, height) {
	difference() {
	  cylinder(r=outer, h=height, center=true);
	  cylinder(r=inner, h=height, center=true);
    }
}



motor_radius = 18.5;
long = 25;
shaft = 6;
motor_offset = 7;
offset = 18.5;

difference() {
 union() {
  base(1.7*motor_radius, shaft, 5);
  translate([0,motor_offset,long/2])
	  mount(motor_radius, long);
  }
	hole(offset,0,1.7);
	//  hole(0, offset, 1);
	hole(-offset,0,1.7);
	hole(0,-offset,1.7);
 }