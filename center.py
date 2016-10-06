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

ip = '172.16.3.234'
userpwd = '&user=scrapsec&pwd=lakewould'
picCmd = '/snapshot.cgi?resolution=32&user=admin&pwd=lakewould'

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
        cv2.imshow("camera", image)
        key = cv.WaitKey(time)
        if (key in keycmds.keys()) :
            moveCamera(keycmds[key],100)
        elif key == 27:
            exit(0)
    
def nearest_error(bbs, centerx, centery) :
    min_x_plus_y = 100000
    minbb = bbs[0]
    for r in bbs:
        distance = abs(r[0]-centerx)*abs(r[0]-centerx) + abs(r[1]-centery)*abs(r[1]-centery)
        if distance < min_x_plus_y :
            minbb = r
            min_x_plus_y = distance
    xerror = centerx - minbb[0]
    yerror = centery - minbb[1]
    plog("Nearest blob is " + str(minbb))
    return (xerror,yerror)
    
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
    (ret,img) = cv2.threshold(image, 170, 255, cv2.THRESH_BINARY)
#    (ret,img) = cv2.threshold(image, 127, 255, cv2.THRESH_BINARY)
    if (ret == False) :
        plog( "Thresholding failed?")
        return None
    if (img == None) :
        plog( "img is None after binary threshold in contrast")
    showUser(img)
    return img
    
if __name__ == "__main__" :
    if ('--verbose' in sys.argv or 'verbose' in sys.argv or '-v' in sys.argv):
        logging = True
    cv2.namedWindow("camera", cv2.CV_WINDOW_AUTOSIZE)
    cv2.moveWindow("camera", 200,50)
    ledMode(0)
    lightLevel()
    img = grabFrame()
    (w,h,d) = img.shape
    centerx = w/2
    centery = h/2
    while(1) :
        img = grabFrame()
        plog("NEW FRAME")
        img[:,:,0] = cv2.multiply(img[:,:,0],0.5)
        img[:,:,1] = cv2.multiply(img[:,:,1],0.5)
        img[:,:,2] = cv2.subtract(img[:,:,2],cv2.add(cv2.multiply(img[:,:,0],0.5),
                                                     cv2.multiply(img[:,:,1],0.5)))
        for i in range(8):
            red2 = grabFrame()
            img[:,:,0] = cv2.add(img[:,:,0], cv2.subtract(red2[:,:,2],
                                                cv2.add( cv2.multiply(red2[:,:,0],0.5),
                                                        cv2.multiply(red2[:,:,1],0.5))))
            img[:,:,1] = cv2.add(img[:,:,1], cv2.subtract(red2[:,:,2],
                                                cv2.add( cv2.multiply(red2[:,:,0],0.5),
                                                         cv2.multiply(red2[:,:,1],0.5))))
            img[:,:,2] = cv2.add(img[:,:,2], cv2.subtract(red2[:,:,2],
                                                cv2.add( cv2.multiply(red2[:,:,0],0.5),
                                                         cv2.multiply(red2[:,:,1],0.5))))
        centerpoint = str((centery,centerx))
        cv2.putText(img,centerpoint, (centery-40,centerx+7),cv2.FONT_HERSHEY_SIMPLEX, 0.5,(255,255,255),1)
        cv2.putText(img,"l", (centery-60,centerx),cv2.FONT_HERSHEY_SIMPLEX, 0.5,(0,255,0),1)
        cv2.putText(img,"r", (centery+60,centerx),cv2.FONT_HERSHEY_SIMPLEX, 0.5,(0,255,0),1)
        cv2.putText(img,"u", (centery,centerx-30),cv2.FONT_HERSHEY_SIMPLEX, 0.5,(0,255,0),1)
        cv2.putText(img,"d", (centery,centerx+30),cv2.FONT_HERSHEY_SIMPLEX, 0.5,(0,255,0),1)
        cv2.rectangle(img,(centery+20,centerx-20),(centery-20,centerx+20),(100,100,255),1)
        showUser(img)
        (b,g,r) = cv2.split(img)
        bbs = monoblobs(r)
        if (len(bbs) == 0) :
            img = None
            continue
        for r in bbs:
            cv2.rectangle(img,(r[0],r[1]),(r[0]+r[2],r[1]+r[3]),(255,160,120),1)
        showUser(img)
        (deltax, deltay) = nearest_error(bbs, centery, centerx)
        cv2.rectangle(img,(centerx+deltax-10,centery+deltay-10),(centerx+deltax+10,centery+deltay+10),(0,0,255),1)
        plog("Error "+str((deltax,deltay)))
        if (abs(deltax) > 5):
            if deltax < 0 :
                moveCamera(textcmds['left'],deltax)
            else :
                moveCamera(textcmds['right'],deltax)
        if (abs(deltay) > 5):
            if deltay < 0 :
                moveCamera(textcmds['down'],deltay)
            else :
                moveCamera(textcmds['up'],deltay)
        if (abs(deltax) < 6 and abs(deltay) < 6):
            time.sleep(5)
            exit(0)
        img = None
                
            
