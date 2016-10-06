#!C:/cygwin/Python27/python -u
#!/usr/bin/python -u
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

ip      = '172.16.3.234'
userpwd = '&user=scrapsec&pwd=lakewould'
picCmd  = '/snapshot.cgi?resolution=32&user=admin&pwd=lakewould'
brightCmd   = '/camera_control.cgi?param=1&value=1' # Range 1-9
contrastCmd = '/camera_control.cgi?param=2&value=6' # Range 1-6

kernal = np.ones((2,2),np.uint8)
alpha   = np.array([  1.2 ])
beta    = np.array([ -60 ])
theta = 1
phi = 1

logging = False

def plog(s) :
    if (logging):
        print(s)
    
def lightLevel() :
    cmd = "http://" + ip + brightCmd + userpwd
    try:
        urllib2.urlopen(urllib2.Request(cmd))
    except urllib2.URLError, msg :
        plog("camera "+str(msg)+"-"+ip)
    cmd = "http://" + ip + contrastCmd + userpwd
    try:
        urllib2.urlopen(urllib2.Request(cmd))
    except urllib2.URLError, msg :
        plog("camera "+str(msg)+"-"+ip)

def ledMode(n) :
    cmd = "http://" + ip + "/set_misc.cgi?led_mode=" + str(n) + userpwd
    try:
        urllib2.urlopen(urllib2.Request(cmd))
    except urllib2.URLError, msg :
        plog("camera "+str(msg)+"-"+ip)

def moveCamera(cnum,dist) :
    cmd = "http://" + ip + "/decoder_control.cgi?command=" + str(cnum) + userpwd # "&onestep=5"
    plog(cmd)
    try:
        urllib2.urlopen(urllib2.Request(cmd))
    except urllib2.URLError, msg :
        plog("camera "+str(msg)+"-"+ip)
    paws = abs(dist)/200.0
    plog("Moving "+str(dist)+ " or " + str(paws)+ " seconds")
    time.sleep(paws)
    cnum = cnum + 1
    cmd = "http://" + ip + "/decoder_control.cgi?command=" + str(cnum) + userpwd
    plog(cmd)
    try:
        urllib2.urlopen(urllib2.Request(cmd))
    except urllib2.URLError, msg :
        plog("camera "+str(msg)+"-"+ip)
    time.sleep(0.2)

def grabFrame() :
    snapshot = "http://"+ip+picCmd+userpwd
    req = urllib2.Request(snapshot)
    try :
        img1 = urllib2.urlopen(req).read()
        if (img1 == None) :
            plog("camera "+ip)
            exit(0)
        img1 = bytearray(img1)
        if (img1 == None) :
            plog("bytearray(fail).")
            exit(0)
        img1 = np.asarray(img1, dtype=np.uint8)
        if (img1 == None) :
            plog("numpy_conversion(fail).")
            exit(0)
    except urllib2.URLError, msg :
        plog("camera "+str(msg)+"-"+ip)
        exit(0)
    if (img1 == None) :
        debug = debug + "No image returned in IPcamera.grab()"
        return None
    return(cv2.imdecode(img1, 1))

#                   UP          DOWN        LEFT         RIGHT
#
keycmds  = { 65362 : 0, 65364 : 2, 65363: 4, 65361 : 6 }
textcmds = { 'up' : 0, 'down': 2, 'left': 4, 'right' : 6 }

def showUser(image, time=10) :
    if (logging) :
        time = 1000
    cv2.imshow("camera", image)
    key = cv.WaitKey(time)
    while (key in keycmds.keys()) :
        moveCamera(keycmds[key],100)
        img = grabFrame()
        cv2.imshow("camera", img)
        key = cv.WaitKey(1000)
    if key == 27:
        exit(0)
    
def nearest_error(bbs, y, x) :
    min_x_plus_y = 200000
    minbb = bbs[0]
    for r in bbs:
        distance = abs(r[0]-y)*abs(r[0]-y) + abs(r[1]-x)*abs(r[1]-x)
        if distance < min_x_plus_y :
            minbb = r
            min_x_plus_y = distance
    plog("Nearest blob is " + str(minbb))
    return(y-minbb[0], x-minbb[1])
    
# Return blobs in monochrome image with width and height between min and max
def monoblobs(monochr, con=(1,1.6,-60), minDim=10, maxDim=30) :
    """IP cameras like 2X(Erode->Dilate->Dilate) erodeDilate(img,2,1,2)
    USB camera likes single erode->dilate cycle erodeDilate(img,1,1,1)
    TODO: Automate variation of these parameters to get a good reading"""
    (it, sc, off) = con
    con = contrast(monochr, iter=it, scale=sc, offset=off)
    gray = erodeDilate(con, 1, 1, 1)
    gray2 = cv2.adaptiveThreshold(gray,255,cv2.ADAPTIVE_THRESH_GAUSSIAN_C,cv2.THRESH_BINARY,11,2)
    contours, _ = cv2.findContours(gray2, cv2.RETR_LIST, cv2.CHAIN_APPROX_SIMPLE)
    toosmall = 0
    toolarge = 0
    bbs = []
    for c in contours:
        rect = cv2.boundingRect(c)
        if (rect[1] < 3) :
            continue
        if rect[2] < minDim or rect[3] < minDim:  
            toosmall += 1
            continue
        elif    rect[2]>maxDim or rect[3]>maxDim :
            toolarge += 1
            continue
        else :
            bbs.append(rect)
    plog(str(toosmall)+" too small "+str(toolarge)+" too large. Returning "+str(len(bbs)) + " blobs")
    return bbs

def erodeDilate(img,iter=1,erode=1,dilate=1) :
    for i in range(iter):
        img = cv2.erode(img,kernal,iterations=erode)
        img = cv2.dilate(img,kernal,iterations=dilate)
    return img

def contrast(image, iter=1, scale=1.4, offset=-70) :
    if (image == None) :
        plog("contrast called with null Image")
    for i in range(iter) :
        plog("Try contrast "+str((iter,scale,offset)))
        if (image == None) :
            plog("contrast loop: Image is None")
        else :
            showUser(image)
            image = cv2.add(cv2.multiply(image,scale),offset)
            if (image == None) :
                plog( "image(None) after add/mulitply in contrast!")
        image = erodeDilate(image, 1, 1, 1)
    showUser(image)
    (ret,img) = cv2.threshold(image, 127, 255, cv2.THRESH_BINARY)
    if (ret == False) :
        plog( "Thresholding failed?")
        return None
    if (img == None) :
        plog( "img is None after binary threshold in contrast")
    showUser(img)
    return img

def addhalf(img,b,c) :
    return cv2.add(cv2.multiply(img[:,:,b],0.5),
                   cv2.multiply(img[:,:,c],0.5))

def integrate(iter,a,b,c):
    img = grabFrame()
    plog("NEW FRAME")
    img[:,:,a] = cv2.subtract(img[:,:,a],addhalf(img,b,c))
    for i in range(iter): # Integrate primary minus half-other-colors
        f = grabFrame()
        img[:,:,a] = cv2.add(img[:,:,a], cv2.subtract(f[:,:,a],addhalf(f,b,c)))
    return img

def decorate(img) :
    (h,w,_) = img.shape
    y = h/2
    x = w/2
    label = str((y,x))
    cv2.putText(img,label, (x-40,y+7),cv2.FONT_HERSHEY_SIMPLEX, 0.5,(255,255,255),1)
    cv2.putText(img,"l", (x-60,y),cv2.FONT_HERSHEY_SIMPLEX, 0.5,(0,255,0),1)
    cv2.putText(img,"r", (x+60,y),cv2.FONT_HERSHEY_SIMPLEX, 0.5,(0,255,0),1)
    cv2.putText(img,"u", (x,y-60),cv2.FONT_HERSHEY_SIMPLEX, 0.5,(0,255,0),1)
    cv2.putText(img,"d", (x,y+60),cv2.FONT_HERSHEY_SIMPLEX, 0.5,(0,255,0),1)
    cv2.rectangle(img,(x+20,y-20),(x-20,y+20),(100,100,255),1)
    showUser(img)
    return img

def adjustCamera(dx,dy) :
    plog("Error "+str((dx,dy)))
    if (abs(dx) > 5):
        if dx < 0 :
            moveCamera(textcmds['left'],dx)
        else :
            moveCamera(textcmds['right'],dx)
    if (abs(dy) > 5):
        if dy < 0 :
            moveCamera(textcmds['down'],dy)
        else :
            moveCamera(textcmds['up'],dy)
    if (abs(dx) < 6 and abs(dy) < 6):
        return True
    return False

if __name__ == "__main__" :
    if ('--help' in sys.argv or 'help' in sys.argv or '-h' in sys.argv):
        print("usage: center.py [verbose] [--ip <ipaddr>]")
        exit(0)
    if ('--verbose' in sys.argv or 'verbose' in sys.argv or '-v' in sys.argv):
        logging = True
    if ('--ip' in sys.argv):
        for i in range(len(sys.argv)) :
            if ('--ip' ==  sys.argv[i]):
                ip = sys.argv[i+1]
    cv2.namedWindow("camera", cv2.CV_WINDOW_AUTOSIZE)
    cv2.moveWindow("camera", 200,50)
    lightLevel()
    img = grabFrame()
    print(str(img.shape))
    (h,w,d) = img.shape
    cy = h/2
    cx = w/2
    while(1) :
        img = integrate(8,2,0,1) # Integrate X8 Red(2) minus Blue(0) and Green(1)
        img = decorate(img)
        showUser(img)
        bbs = monoblobs(img[:,:,2]) # Look for red blobs
        if (len(bbs) == 0) :
            continue
        for r in bbs:
            cv2.rectangle(img,(r[0],r[1]),(r[0]+r[2],r[1]+r[3]),(255,160,120),1)
        print(str(bbs))
        (deltay, deltax) = nearest_error(bbs, cy, cx)
        cv2.line(img,(cx,cy),(cx-deltax,cy-deltay),(0,0,255),1)
        showUser(img)
        plog("Error "+str((deltax,deltay)))
        if (adjustCamera(deltax,deltay)) :
            time.sleep(5)
            exit(0)
                
            
