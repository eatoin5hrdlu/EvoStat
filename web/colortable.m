function colortable()
  co = get(gca,'ColorOrder');
  fd = fopen('ctable.m','wt');
  fprintf(fd,'mycolors = [\n');
  fprintf(fd,'   %f,%f,%f;\n',co);
  fprintf(fd,'];\n');
  fclose(fd);
  endfunction
