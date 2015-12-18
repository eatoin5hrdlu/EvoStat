module capacitor(width) {
w = 10;
                         cube([10,10,0.2],center=true);
  translate([0,0,-w/2])  
  // rotate([90,0,0])
    cylinder(r=1,h=w,center=true,$fn=12);
  translate([0,0,2])  	 cube([10,10,0.2],center=true);
  translate([0,0,2+w/2])
  // rotate([90,0,0])
    cylinder(r=1,h=w,center=true,$fn=12);
	
}
module resistor(width) {
w = 4;
    translate([0,0,-w])
      cylinder(r=1,h=w,center=true,$fn=12);

    cylinder(r=width,h=w,center=true,$fn=12);

    translate([0,0,w])
      cylinder(r=1,h=w,center=true,$fn=12);
}

capacitor(10);
translate([10,0,0]) resistor(0.1);
translate([0,10,0]) resistor(0.3);
//
// Mendel90
//
// GNU GPL v2
// nop.head@gmail.com
// hydraraptor.blogspot.com
//
// Springs
//

extruder_spring = [ 7,    1, 10, 5];
hob_spring      = [12, 0.75, 10, 6];

function spring_od(type)     = type[0];
function spring_gauge(type)  = type[1];
function spring_length(type) = type[2];
function spring_coils(type)  = type[3];

// taken from openscad example 20
module coil(r1 = 100, r2 = 10, h = 100, twists)
{
    hr = h / (twists * 2);
    stepsize = 1/16;
    module segment(i1, i2) {
        alpha1 = i1 * 360*r2/hr;
        alpha2 = i2 * 360*r2/hr;
        len1 = sin(acos(i1*2-1))*r2;
        len2 = sin(acos(i2*2-1))*r2;
        if (len1 < 0.01)
            polygon([
                [ cos(alpha1)*r1, sin(alpha1)*r1 ],
                [ cos(alpha2)*(r1-len2), sin(alpha2)*(r1-len2) ],
                [ cos(alpha2)*(r1+len2), sin(alpha2)*(r1+len2) ]
            ]);
        if (len2 < 0.01)
            polygon([
                [ cos(alpha1)*(r1+len1), sin(alpha1)*(r1+len1) ],
                [ cos(alpha1)*(r1-len1), sin(alpha1)*(r1-len1) ],
                [ cos(alpha2)*r1, sin(alpha2)*r1 ],
            ]);
        if (len1 >= 0.01 && len2 >= 0.01)
            polygon([
                [ cos(alpha1)*(r1+len1), sin(alpha1)*(r1+len1) ],
                [ cos(alpha1)*(r1-len1), sin(alpha1)*(r1-len1) ],
                [ cos(alpha2)*(r1-len2), sin(alpha2)*(r1-len2) ],
                [ cos(alpha2)*(r1+len2), sin(alpha2)*(r1+len2) ]
            ]);
    }
    linear_extrude(height = h, twist = 180*h/hr,
            $fn = (hr/r2)/stepsize, convexity = 5) {
        for (i = [ stepsize : stepsize : 1+stepsize/2 ])
            segment(i-stepsize, min(i, 1));
    }
}


module comp_spring(type, l = 0) {
    l = (l == 0) ? spring_length(type) : l;

    vitamin(str("SPR", spring_od(type), spring_gauge(type) * 100, type[2],
                ": Spring ", spring_od(type), "mm OD, ", spring_gauge(type), "mm gauge x ", spring_length(type), "mm long" ));

    color(spring_color) render()
        coil(r1 = (spring_od(type) - spring_gauge(type)) / 2, r2 = spring_gauge(type) / 2, h = l, twists = spring_coils(type));

    if($children)
        translate([0, 0, l])
            child();
}

module inductor() {
   w = 5;
   comp_spring(extruder_spring);
   translate([3.2,0,12.5])
      cylinder(r=0.5,h=w,center=true,$fn=12);
   translate([-3.2,0,-w/2])
      cylinder(r=0.5,h=w,center=true,$fn=12);
}

 
translate([0,-12,-5]) inductor();