/* usbreset -- send a USB port reset to a USB device */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <string.h>

#include <linux/usbdevice_fs.h>

int main(int argc, char **argv)
{
  FILE *pp;
  char buf[1000];
  char *line;
  const char *filename;
  int fd;
  int rc;
  char cmdbuf[1000] = "lsusb | grep -i quickcam | cut --fields=2,4 --delimiter=' ' | sed 's:^:/dev/bus/usb/:' | sed 's: :/:' | sed 's/://'";
  if (argc > 1)
    sprintf(cmdbuf,"lsusb | grep -i %s | cut --fields=2,4 --delimiter=' ' | sed 's:^:/dev/bus/usb/:' | sed 's: :/:' | sed 's/://'", argv[1]);

  /* Command above turns this (from lsusb):
   *
   * Bus 008 Device 002: ID 17ef:4815 Lenovo Integrated Webcam [R5U877]
   * Bus 008 Device 001: ID 1d6b:0002 Linux Foundation 2.0 root hub
   * Bus 007 Device 001: ID 1d6b:0001 Linux Foundation 1.1 root hub
   * Bus 006 Device 001: ID 1d6b:0001 Linux Foundation 1.1 root hub
   * Bus 004 Device 001: ID 1d6b:0001 Linux Foundation 1.1 root hub
   * Bus 005 Device 019: ID 046d:09a4 Logitech, Inc. QuickCam E 3500
   * 
   * into these USB camera device paths:
   *
   *      /dev/bus/usb/008/002
   *      /dev/bus/usb/005/019
   */

  
    pp = popen(cmdbuf, "r");
    while(line = fgets(buf,sizeof buf, pp))
    {
      buf[strlen(buf)-1] = (char)0;  // overwrite newline character
      fd = open(line, O_WRONLY);
      if (fd < 0)
        perror("Error opening output file");
      else if ( ioctl(fd, USBDEVFS_RESET, 0) < 0 )
	perror("Error in ioctl");
      close(fd);
    }
}
