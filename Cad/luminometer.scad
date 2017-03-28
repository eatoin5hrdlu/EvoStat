width = 17.5;
depth = 20.0;
height = 8.0;
margin = 4.0;
lensrad = 4;
module lens(lr) { sphere(lr,center=true,$fn=32);}
module case(cw,ch,cd)
{
crad = 2;
cx = cw/2-3*crad/4;
cy = cd/2-3*crad/4;
    difference() {
		cube([cw+margin,cd+margin,ch+margin/2],center=true);
      translate([0,0,height/2])
      {
      cube([cw,cd,ch],center=true);
      translate([cx,cy,0]){
           cylinder(r=crad,h=ch,center=true,$fn=16);
           cylinder(r=crad/2,h=3*ch,center=true,$fn=16);
      }
      translate([-cx,cy,0])
           cylinder(r=crad,h=ch,center=true,$fn=16);
      translate([cx,-cy,0]){
           cylinder(r=crad,h=ch,center=true,$fn=16);
           cylinder(r=crad/2,h=3*ch,center=true,$fn=16);
      }
      translate([-cx,-cy,0])
           cylinder(r=crad,h=ch,center=true,$fn=16);
      }
      translate([2,0,0])
       cylinder(r=crad,h=3*ch,center=true,$fn=32);
      translate([2,0,-5]) lens(lensrad);
    }
}
module lid(lw,lh,ld) 
{
crad = 2;
cx = lw/2-3*crad/4;
cy = ld/2-3*crad/4;
      difference() {
         cube([lw+margin,ld+margin,lh],center=true);
         translate([0,0,-lh/2])
           cube([lw,ld,lh],center=true);
         translate([-lw/3,0,0])
           cube([2,ld-2,2*lh],center=true);
      translate([cx,cy,0])
           cylinder(r=crad/2,h=3*lh,center=true,$fn=16);
      translate([cx,-cy,0])
           cylinder(r=crad/2,h=3*lh,center=true,$fn=16);
      }
}
module display() {
    case(width,height,depth);
    translate([0,0,20]) lid(width,4,depth);
}

case(width,height,depth);
//rotate([180,0,0]) lid(width,4,depth);



