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

module threaded(a,ch,len,diam)
{
 //color([0.5,0.6,0.65])

  color([0,0,1])
    rotate([a,90,0])
        translate([ch/4,0,-arch*4])
   		cylinder(r=diam/2, h=len, center=true,$fn=18);
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
  color([0,0,1]) anglebolt(0,ch);
  color([0,0,1]) anglebolt(6*22.5,ch);
  deg = 0;
  difference() {
   cylinder(r=cd/2, h=ch, center=true,$fn=64);
   cylinder(r=cd/2-3, h=ch+1, center=true,$fn=64);
   rotate([0,90,0]) {
    for( i = [0:1:7] ) {
       if (i !=0 &&  i != 6) {
        rotate([i*22.5,0,0])
         union() {
          translate([ch/4,0,0])
           cube([arch+1,arch,2*cd],center=true); 
          cylinder(r=arch/2,h=2*cd,center=true,$fn=18); 
        } //end union
       } else {
        translate([ch/4,0,0])
         rotate([i*22.5,0,0])
          cylinder(r=bolt/2,h=2*cd,center=true,$fn=18);
       } // end else
   } // end for   
  } // end rotate
 } // end difference  
} // end ecoliseum

module element(cd,ch) 
{
  color([0,0,1]) threaded(0,ch,arch, bolt);
  color([0,0,1]) threaded(90,ch,arch, bolt);
  color([0,0,1]) threaded(180,ch,arch, bolt);
  color([0,0,1]) threaded(270,ch,arch, bolt);
  deg = 0;
  difference() {
   cylinder(r=cd/2, h=ch, center=true,$fn=64);
   cylinder(r=cd/2-3, h=ch+1, center=true,$fn=64);
   rotate([0,90,0]) {
    for( i = [0:1:7] ) {
       if (i != 0 &&  i != 4) {
        rotate([i*22.5,0,0])
         union() {
          translate([ch/4,0,0])
           cube([arch+1,arch,2*cd],center=true); 
          cylinder(r=arch/2,h=2*cd,center=true,$fn=18); 
        } //end union
       } else {
        translate([ch/4,0,0])
         rotate([i*22.5,0,0])
          cylinder(r=bolt/2,h=2*cd,center=true,$fn=18);
       } // end else
   } // end for   
  } // end rotate
 } // end difference  
} // end ecoliseum

module ec_array(n,m)
{
   for( i = [0:1:n] ) {
     for( j = [0:1:m] ) {
       translate([i*50,j*50,0])
         element(7*arch,2*arch);
     }
   }
}


ec_array(1,1);

// ecoliseum(7*arch,2*arch);

