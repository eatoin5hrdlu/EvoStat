function[newy]=rv(lx,x,y,index,lsl,pm)
  disp( x(lx,index) );
  disp( y(lx,index) );
  center = ( x(lx,index) + y(lx,index) )/2;
  disp(center);
  if (lsl == 0)
    mult = 10**pm;
    newy = center*mult;
  else
    offset = 0.1*pm;
    newy = center + center*offset;
  endif
endfunction
