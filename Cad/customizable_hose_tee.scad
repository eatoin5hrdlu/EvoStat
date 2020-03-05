// Customizable Hose Tee
total_t_length=62;       // Height of Tee
main_t_diameter=15;      // Diameter of main part of Tee
main_t_barb_height=2;    // Height of barbs on Tee
main_t_wall_thickness=3; // Wall thickness of main part of Tee

secondary_diameter=15;  //Diameter of Tee portion of Tee
secondary_barb_height=1.75; //Height of barbs on Tee portion of Tee
secondary_wall_thickness=3; //Wall thickness of Tee portion of Tee

module tee() {
 difference() {
  union() {
    translate([0,0,total_t_length/2]){
      rotate(a=[0,180,0]){
        hose_barb(total_t_length/2, main_t_diameter, main_t_barb_height, main_t_wall_thickness);
      }
    } // end translate

    translate([0,0,-total_t_length/2]){
      rotate(a=[0,0,0]){
        hose_barb(total_t_length/2, main_t_diameter, main_t_barb_height, main_t_wall_thickness);
      }
    } // end translate
    translate([-(total_t_length/2 + main_t_diameter/2 - main_t_wall_thickness),0,0]){
      rotate(a=[0,90,0]){
        hose_barb(total_t_length/2, secondary_diameter, secondary_barb_height, secondary_wall_thickness);
      }
    } // end translate
  } // end union
  rotate(a=[0,90,0]){
    translate([0,0,-total_t_length]){
      cylinder(h=total_t_length, d=secondary_diameter- (secondary_wall_thickness*2), $fn=48);
    }
  } // end rotate
 } // end difference
} // end tee



module hose_barb(height, diameter, barb_height, wall_thickness)
{ 
  end_barb_height = height - (height * 0.45);
  difference(){
    union(){
      cylinder(h=height,d=diameter,$fn=48);//pipe
      cylinder(h=end_barb_height/2,d1=diameter,d2=diameter+barb_height,$fn=48);//barb
      translate([0,0,end_barb_height/2]){
        cylinder(h=end_barb_height/2,d1=diameter,d2=diameter+barb_height,$fn=48);//barb
      }
    }
    cylinder(h=height+2,d=diameter - (wall_thickness*2),$fn=48);//hole
  }
}

module tubing(length) {
color("blue",0.4)
 difference() {
  cylinder(h=length,d=30, $fn=24);
  cylinder(h=length+2,d=15, $fn=24);
 }
}

module polyvent() {
	cylinder(h=8, d=100, $fn=24);
	cylinder(h=14, d=70, $fn=24);
   translate([0,0,-30])
       hose_barb(total_t_length/2, main_t_diameter, main_t_barb_height, main_t_wall_thickness);
   translate([0,0,40])
      rotate([180,0,0])
         hose_barb(total_t_length/2, main_t_diameter, main_t_barb_height, main_t_wall_thickness);
translate([0,0,1])
  linear_extrude(3)
    text("PolyVent",size=1.5);
}

module filter(offset) {
 
  translate([-(offset+20),0,0]) rotate([0,90,0]) polyvent();
   translate([-offset,0,0])    rotate([0,90,0]) tubing(offset-20);

}

module assembly() {
  tee();
  translate([0,0,400]) tee();
  translate([0,0,520]) tee();
  translate([0,0,10])tubing(380);
  translate([0,0,410])tubing(100);
  translate([0,0,0]) filter(80);
  translate([0,0,400]) filter(80);
  translate([0,0,520]) filter(80);
  translate([0,0,-94])tubing(80);
}

translate([0,0,400])
 rotate([180,0,0])
    assembly();