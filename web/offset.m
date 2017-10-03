function[newval]=offset(val,lsl,otherval)
  if (lsl == 0)
    if (otherval > val)
      newval = val/10
    else
      newval = val*10
    endif
  else
    if (otherval > val)
      newval = val - val*0.1
    else
      newval = val + val*0.1
    endif
  endif
endfunction
