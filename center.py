#!/usr/bin/python -u
#!C:/cygwin/Python27/python -u
#!C:/Python27/python -u
# Regions lead with y  (y1,x1,y2,x2) where (y1,x1) is (uppermost,leftmost)
# OpenCV blob,contour algorithms return (X,Y,Width,Height) not(x,y,x2,y2)
#

from __future__ import print_function
from contrast import makeTkSliders

import sys, os, time, socket, subprocess, re, traceback
import datetime
from os  import popen
import glob
import base64, urllib2
from suppress_stdout_stderr import suppress_stdout_stderr
import numpy as np
import cv2
import cv2.cv as cv
import evocv2
from util import *

ip = '192.168.2.130'
userpwd = '&user=scrapsec&pwd=lakewould'
picCmd = '/snapshot.cgi?resolution=32&user=admin&pwd=lakewould'
updownrightleft = ['0','2','4','6']

def cmdToCamera(cmd) :
    cmd = "http://" + ip + "/command=" + updownrightleft[c]
    try:
        urllib2.urlopen(urllib2.Request(cmd))
    except urllib2.URLError, msg :
        print("camera "+msg+"-"+ip)

def grabFrame() :
    snapshot = "http://"+ip+picCmd+userpwd
    req = urllib2.Request(snapshot)
    try :
        img1 = urllib2.urlopen(req).read()
        if (img1 == None) :
            print("camera "+ip)
            exit(0)
        img1 = bytearray(img1)
        if (img1 == None) :
            print("bytearray(fail).")
            exit(0)
        img1 = np.asarray(img1, dtype=np.uint8)
        if (img1 == None) :
            print("numpy_conversion(fail).")
            exit(0)
    except urllib2.URLError, msg :
        print("camera "+msg+"-"+ip)
        exit(0)
            
    if (img1 == None) :
        debug = debug + "No image returned in IPcamera.grab()"
        return None
    return(cv2.imdecode(img1, 1))



if __name__ == "__main__" :
    cv2.namedWindow("camera", cv2.CV_WINDOW_AUTOSIZE)
    while(1) :
        img = grabFrame()
        cv2.imshow("camera", img)
        if cv.WaitKey(5000) == 27:
            exit(0)
            
