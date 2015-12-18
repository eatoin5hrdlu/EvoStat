module mount(inner, length) {
side = (inner+6)/3;

	difference() {
     union() {
		cylinder(r=inner+4, h=length, center=true);
	    translate([0,0,-length/3])
		cylinder(r=inner+8, h=length/4, center=true);
      }
		cylinder(r=inner, h=length+2, center=true);

	    translate([inner,-3,6.2])  // X
			cube([side,2*side,length-2], center=true);

	    translate([-inner,-3,6.2])  // -X
			cube([side,2*side,length-2], center=true);

	    translate([0,inner,6.2])     // Y
			cube([side,2*side,length-2], center=true);

	    translate([0,-inner,6.2])  // -Y
			cube([side,2*side,length-2], center=true);

    }
}

module hole(x,y,radius) {
	translate([x,y,0])
	cylinder(r=radius,h=48,center=true);
}

module base(outer, inner, height) {
	difference() {
	  cylinder(r=outer, h=height, center=true);
	  cylinder(r=inner, h=height+2, center=true);
    }
}



// motor_radius = 18.5;
motor_radius = 18.5;
long = 25;
shaft = 6.5;
motor_offset = 7;
offset = 18.5;

difference() {
 union() {
  base(1.8*motor_radius, shaft, 6);
  translate([0,motor_offset,long/2])
	  mount(motor_radius, long);

  }
  translate([0,motor_offset,0])
	  cylinder(r=13.4,h=10,center=true);
	hole(offset,0,2.7);
	//  hole(0, offset, 1);
	hole(-offset,0,2.7);
	hole(0,-offset,2.7);
 }