arch = 6;
bolt = 4;

module ssbolt(len,diam)
{
  color([0.5,0.6,0.65]) {
        difference() {
            union() {
      //Shaft
        cylinder(r=diam/2, h=len, center=true,$fn=18);
      // Head
        translate([0,0,-len/2])
          cylinder(r=diam-1,h=diam,center=true,$fn=18);
          }
     // Hex slot
          translate([0,0,-(len/2 + 1)])
            cylinder(r=diam/2,h=diam,center=true,$fn=6);
    }// end difference
   } // end color
}
// Two bolts on opposite sides of circle
module anglebolt(a,ch) {
      rotate([a,90,0]) {
          translate([ch/4,0,-arch*5])  
           ssbolt(arch*3,bolt); 
          mirror([0,0,1])
           translate([ch/4,0,-arch*5])  
           ssbolt(arch*3,bolt); 
      }
  }
    
module ecoliseum(cd,ch) 
{
  anglebolt(0,ch);
  anglebolt(6*22.5,ch);

  difference() {
   cylinder(r=cd/2, h=ch, center=true,$fn=64);
   cylinder(r=cd/2-3, h=ch+1, center=true,$fn=64);
   rotate([0,90,0]) {
    for( i = [0:1:7] ) {
       deg = i*22.5;
       if (i !=0 &&  i != 6) {
        rotate([deg,0,0])
         union() {
          translate([ch/4,0,0])
           cube([arch+1,arch,2*cd],center=true); 
          cylinder(r=arch/2,h=2*cd,center=true,$fn=18); 
        } //end union
       } else {
        translate([ch/4,0,0])
         rotate([deg,0,0])
          cylinder(r=bolt/2,h=2*cd,center=true,$fn=18);
       } // end else
   } // end for   
  } // end rotate
 } // end difference  
} // end ecoliseum

ecoliseum(7*arch,2*arch);