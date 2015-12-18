aristotle eth0  00:26:9e:fd:94:06  
aristotle wlan0 00:26:c7:5c:64:62  

buffon    eth0  04:7d:7b:aa:34:74  
buffon    wlan0 60:d8:19:d4:59:fc  

cuvier    eth0  60:eb:69:03:7b:64  
cuvier    wlan0 00:26:c7:53:31:e4  


Prerequisites:

Windows or Linux system with github:eatoin5hrdlu/{plblue,PACE}.git

If you want to be a contributor (e.g. change the shared code):
 1) Create a GitHub account
 2) Send Peter Reintjes (peter.reintjes@ncmls.org):
    a) your CV/resume
    b)  <hostname> of your computer
    c) public RSA key
    d) An example of your improvments the code/bug fix.

mkdir src
cd src
git clone https:github.com/eatoin5hrdlu/plblue.git
git clone https:github.com/eatoin5hrdlu/PACE.git

  plblue is the Bluetooth support library (creates loadable shared object)

  PACE is the Main EvoStat control software and Arduino sketches

 A high quality (HD) camera such as LifeCam (USB or IP)

 In PACE directory, copy template.pl to <hostname>.pl
 Edit <hostname>.pl to reflect your configuration
  

DEBIAN8:

An Evostat under Debian 8 requires the following packages to be installed:

(Generating a similar manifest for other Linux distributions would be helpful)


(Lenovo/ThinkPad wifi) iwlwifi-firmware firmware-realtek
ssh (including sshd)
git
build-essentials
emacs (for Peter R)
cheese (for camera support)
bluez bluetooth libbluetooth-dev python-bluez blueman
arduino
swi-prolog     ( assuming the repository version of swi-prolog is ok)
python-opencv  (                       "             OpenCV         )

AUTOLOGIN MODE:
Edit line 117 in /etc/lightdm/lightdm.conf to say

autologin-user=<user>

AUTOLAUNCH EvoStat Application:
NB: putting a .desktop file into ~/.config/autostart didn't work

/etc/xdg/lxsession/LXDE/autostart  file must look like:
-----------------------------------------------
@lxpanel --profile LXDE
@pcmanfm --desktop --profile LXDE
@xscreensaver -no-splash
@/home/peter/src/PACE/evostat
-------------------------------------end of autostart file

NETWORKING:

Edit /etc/network/interfaces to have something like:

auto eth0
iface eth0 inet dhcp
allow-hotplug eth0

auto wlan0
iface wlan0 inet dhcp
wpa-driver wext
wpa-ap-scan 1
wpa-proto WPA
wpa-key-mgmt WPA-PSK

# SplatSpace
#wpa-ssid splatspace
#wpa-psk <hex-encoded-passphrase>

Run "wpa_passphrase <ssid> <passphrase>" to get the encoded passphrase

SSH

Edit /etc/ssh/sshd_config:

To have the line:  PermitRootLogin yes
Instead of:        PermitRootLogin without-password

"without-password' turns off password authentication for root, hence no login
"without-password" is a poor choice of words since it implies the opposite.
Basically, this string is either "yes" or "<anything that isn't 'yes'>"


RSAAuthentication yes
PubkeyAuthentication yes
#AuthorizedKeysFile	%h/.ssh/authorized_keys

# Don't read the user's ~/.rhosts and ~/.shosts files
IgnoreRhosts yes
# For this to work you will also need host keys in /etc/ssh_known_hosts
RhostsRSAAuthentication no
# similar for protocol version 2
HostbasedAuthentication no
# Uncomment if you don't trust ~/.ssh/known_hosts for RhostsRSAAuthentication
#IgnoreUserKnownHosts yes

# To enable empty passwords, change to yes (NOT RECOMMENDED)
PermitEmptyPasswords no

# Change to yes to enable challenge-response passwords (beware issues with
# some PAM modules and threads)
ChallengeResponseAuthentication no

# Change to no to disable tunnelled clear text passwords
#PasswordAuthentication yes

# Kerberos options
#KerberosAuthentication no
#KerberosGetAFSToken no
#KerberosOrLocalPasswd yes
#KerberosTicketCleanup yes

# GSSAPI options
#GSSAPIAuthentication no
#GSSAPICleanupCredentials yes

X11Forwarding yes
X11DisplayOffset 10
PrintMotd no
PrintLastLog yes
TCPKeepAlive yes
#UseLogin no

#MaxStartups 10:30:60
#Banner /etc/issue.net

# Allow client to pass locale environment variables
AcceptEnv LANG LC_*

Subsystem sftp /usr/lib/openssh/sftp-server

# Set this to 'yes' to enable PAM authentication, account processing,
# and session processing. If this is enabled, PAM authentication will
# be allowed through the ChallengeResponseAuthentication and
# PasswordAuthentication.  Depending on your PAM configuration,
# PAM authentication via ChallengeResponseAuthentication may bypass
# the setting of "PermitRootLogin without-password".
# If you just want the PAM account and session checks to run without
# PAM authentication, then enable this but set PasswordAuthentication
# and ChallengeResponseAuthentication to 'no'.
UsePAM yes



BUILDING OPENCV FROM SOURCE:

# CMAKE OPTIONS for OPENCV

BUILD_EXAMPLES=ON
BUILD_JASPER:=OFF
BUILD_JPEG:BOOL=ON
BUILD_PACKAGE:BOOL=ON
BUILD_PERF_TESTS:BOOL=ON
BUILD_PNG:BOOL=OFF
BUILD_SHARED_LIBS:BOOL=ON
BUILD_TBB:BOOL=OFF
BUILD_TESTS:BOOL=ON
BUILD_WITH_DEBUG_INFO:BOOL=ON
BUILD_ZLIB:BOOL=ON
BUILD_opencv_apps=ON
BUILD_opencv_calib3d=ON
BUILD_opencv_contrib=ON
BUILD_opencv_core=ON
BUILD_opencv_features2d=ON
BUILD_opencv_flann=ON
BUILD_opencv_gpu=ON
BUILD_opencv_highgui=ON
BUILD_opencv_imgproc=ON
BUILD_opencv_legacy=ON
BUILD_opencv_ml=ON
BUILD_opencv_nonfree=ON
BUILD_opencv_objdetect=ON
BUILD_opencv_ocl=ON
BUILD_opencv_photo=ON
BUILD_opencv_python=ON
BUILD_opencv_stitchingON
BUILD_opencv_superres=ON
BUILD_opencv_ts=ON
BUILD_opencv_video=ON
BUILD_opencv_videostab=ON
BUILD_opencv_viz=ON
BUILD_opencv_world=OFF
CMAKE_AR=/usr/bin/ar
CMAKE_BUILD_TYPE=RELEASE
CMAKE_COLOR_MAKEFILE=ON
CMAKE_CONFIGURATION_TYPES=Debug;Release
CMAKE_CXX_COMPILER=/usr/bin/g++
CMAKE_C_COMPILER=/usr/bin/gcc
CMAKE_C_FLAGS_DEBUG=-g
CMAKE_C_FLAGS_MINSIZEREL=-Os -DNDEBUG
CMAKE_C_FLAGS_RELEASE=-O3 -DNDEBUG
CMAKE_C_FLAGS_RELWITHDEBINFO=-O2 -g -DNDEBUG
CMAKE_INSTALL_PREFIX=/usr/local
CMAKE_LINKER=/usr/bin/ld
CMAKE_MAKE_PROGRAM=/usr/bin/make
CMAKE_NM=/usr/bin/nm
CMAKE_OBJCOPY=/usr/bin/objcopy
CMAKE_OBJDUMP=/usr/bin/objdump
CMAKE_PROJECT_NAME=OpenCV
CMAKE_RANLIB=/usr/bin/ranlib
INSTALL_C_EXAMPLES=ON
INSTALL_PYTHON_EXAMPLES=ON
INSTALL_TESTS=OFF
IlmImf_LIB_DEPENDS=general;zlib;
JASPER_INCLUDE_DIR=/usr/include
JASPER_LIBRARY_RELEASE=/usr/lib/i386-linux-gnu/libjasper.so
JPEG_LIBRARY=/usr/lib/i386-linux-gnu/libjpeg.so
MAKEINDEX_COMPILER=/usr/bin/makeindex
OPENCV_CONFIG_FILE_INCLUDE_DIR=/home/peter/src/OpenCV/opencv-2.4.10/build
OPENCV_WARNINGS_ARE_ERRORS=OFF
OPENGL_INCLUDE_DIR=/usr/include
OPENGL_gl_LIBRARY=/usr/lib/i386-linux-gnu/libGL.so
OPENGL_glu_LIBRARY=/usr/lib/i386-linux-gnu/libGLU.so
VTK_DIR=/usr/lib/vtk-5.8
WITH_1394=ON
WITH_CUBLAS=OFF
WITH_CUDA=ON
WITH_CUFFT=ON
WITH_GSTREAMER=ON
WITH_GSTREAMER_0_10=OFF
WITH_GTK=ON
WITH_IPP=OFF
WITH_JASPER=ON
WITH_JPEG=ON
WITH_LIBV4L=ON
WITH_NVCUVID=OFF
WITH_OPENCL=ON
WITH_OPENGL=ON
WITH_OPENMP=OFF
WITH_PNG=ON
WITH_PVAPI=ON
WITH_QT=OFF
WITH_TBB=OFF
WITH_TIFF=OFF
WITH_UNICAP=OFF
WITH_V4L=ON
WITH_VTK=ON
WITH_XIMEA=OFF
WITH_XINE=OFF
ZLIB_LIBRARY=/usr/lib/i386-linux-gnu/libz.so


EMACS SETUP (as a favor to Peter Reintjes)
Add file ~/.emacs containing:
------------------------------------------------
(when window-system (set-frame-size (selected-frame) 120 20))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(when window-system (set-frame-size (selected-frame) 120 32))

(setq initial-frame-alist '((left . 100) (top . 50)))
----------------------------------------------------------end of ~/.emacs
