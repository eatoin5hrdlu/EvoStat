// 2/4/2011 18:30

blade_radius1=12;
blade_radius2=9;
blade_thickness=1.5;
blade_length=20;
chord_dist=sqrt(pow(blade_radius1,2)-pow(blade_length/2,2));
blade_width=blade_radius1-chord_dist;
blade_length2=2*sqrt(pow(blade_radius2,2)-pow(chord_dist,2));
top_radius=blade_length-(blade_length-blade_length2)/2;
blade_height=8;
num_blades=8;
wheel_base_height=1.05;
hub_radius1=4;
hub_radius2=4;
hub_height=9.1;
axle_radius=1.5;
housing_space=0.5;
wall_thickness=1.5;
inlet_radius=blade_height/2;
inlet_length=25;
outlet_height=0;
outlet_radius=8;
axle_space=0.25;

slope=(blade_length-top_radius)/blade_height;

echo(str("Blade Width  is ",blade_width));
echo(str("Blade Length 2 is ",blade_length2));
echo(str("Radius of top of wheel is ",top_radius));


//wheel();
//housing_top();
housing_bottom();


module housing_top() {
		
		cavity_radius1=blade_length+housing_space;
		cavity_radius2=top_radius+housing_space;
		
		housing_radius1=blade_length+wall_thickness+housing_space+slope*(wall_thickness*2+housing_space*2);
		housing_radius2=housing_radius1-slope*(blade_height+wheel_base_height+housing_space*2+wall_thickness*2);
		echo(str("Slope is ",slope));

		difference() {
			union() {
				translate([0,0,-(wall_thickness+housing_space)]) cylinder(r1=housing_radius1,r2=housing_radius2,h=blade_height+wheel_base_height+housing_space*2+wall_thickness*2);
				inlet();
				cylinder(r=outlet_radius+wall_thickness,h=outlet_height+blade_height+wheel_base_height+housing_space+wall_thickness);
			}
			 translate([(cavity_radius1+cavity_radius2)/2-inlet_radius-wall_thickness,0,inlet_radius+wall_thickness]) rotate([90,0,0]) cylinder(r=inlet_radius,h=inlet_length);			
			difference() {
				translate([0,0,wheel_base_height]) cylinder(r1=cavity_radius1, r2=cavity_radius2,h=blade_height+housing_space);
				translate([0,0,wheel_base_height+blade_height]) cylinder(r=axle_radius+wall_thickness+axle_space,h=housing_space, $fn=10);
			}
			translate([0,0,-(wall_thickness+housing_space)]) cylinder(r=cavity_radius1, h=wall_thickness+housing_space*2+wheel_base_height);
			cylinder(r=axle_radius+axle_space,h=outlet_height+blade_height+wheel_base_height+housing_space+wall_thickness,$fn=10);
			outlet();
		}
}

module inlet() {
	housing_height=blade_height+wheel_base_height+housing_space*2+wall_thickness*2;
	translate([-(inlet_radius*2+wall_thickness-top_radius),0,-(housing_space+wall_thickness)]) rotate([90,0,0]) linear_extrude(height=inlet_length)
		polygon(points=[[0,0],[0,housing_height],[inlet_radius*2+wall_thickness*2,housing_height],[inlet_radius*2+wall_thickness*2+slope*housing_height,0]], paths=[[0,1,2,3,0]]);
}

module outlet() {
	height=outlet_height+blade_height+wheel_base_height+housing_space+wall_thickness;
	difference() {
		cylinder(r=outlet_radius,h=height);
		for (c=[1:3]) rotate([0,0,c*120]) translate([0,-wall_thickness/2,0]) cube([outlet_radius,wall_thickness,height]);
		cylinder(r=axle_radius+wall_thickness+axle_space,h=height);
		
	}
}


module housing_bottom() {
	difference() {
		union() {
			translate([0,0,-(wall_thickness*2+housing_space)]) cylinder(r=(blade_length+housing_space+wall_thickness),h=wall_thickness);
			translate([0,0,-(wall_thickness+housing_space)]) cylinder(r=(blade_length+housing_space),h=wall_thickness);
			translate([0,0,-housing_space]) cylinder(r=axle_radius+wall_thickness+axle_space,h=housing_space, $fn=10);
		}
		translate([0,0,-(wall_thickness*2+housing_space)]) cylinder(r=axle_radius+axle_space,h=wall_thickness*2+housing_space,$fn=10);
	}

}

module wheel() {
	difference() {
		union() {
			translate([0,0,wheel_base_height]) for (a=[1:num_blades]) rotate([0,0,a*(360/num_blades)]) translate([0,0,0]) blade();
			cylinder(r=blade_length,h=wheel_base_height);
			cylinder(r1=hub_radius1, r2=hub_radius2, h=hub_height);
		}
		cylinder(r=axle_radius,h=max(hub_height,blade_height+wheel_base_height), $fn=10);
	}
}

module blade() {
	angle=asin((blade_radius1-blade_width)/blade_radius1);
	translate([0,blade_radius1,0]) rotate([0,0,angle]) difference() {
		cylinder(r1=blade_radius1,r2=blade_radius2, h=blade_height);
		cylinder(r1=blade_radius1-blade_thickness,r2=blade_radius2-blade_thickness,h=blade_height);
		translate([blade_width-blade_radius1,-blade_radius1,0]) cube([blade_radius1*2,blade_radius1*2,blade_height]);
	}

}