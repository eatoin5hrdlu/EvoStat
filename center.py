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


def plog(s) :
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

def moveCamera(cmd) :
    cmd = "http://" + ip + "/decoder_control.cgi?command=" + cmd + "&onestep=5" + userpwd
    plog(cmd)
    try:
        urllib2.urlopen(urllib2.Request(cmd))
    except urllib2.URLError, msg :
        plog("camera "+str(msg)+"-"+ip)

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
keycmds  = { 65362 : '0', 65364 : '2', 65363: '4', 65361 : '6' }
textcmds = { 'up' : '0', 'down': '2', 'left': '4', 'right' : '6' }

def showUser(image, time=1000) :
        cv2.imshow("camera", image)
        key = cv.WaitKey(time)
        if (key in keycmds.keys()) :
            moveCamera(keycmds[key])
        elif key == 27:
            exit(0)
    
def nearest_error(bbs, centerx, centery) :
    min_x_plus_y = 100000
    minbb = bbs[0]
    for r in bbs:
        distance = abs(r[0]-centerx)*abs(r[0]-centerx) + abs(r[1]-centery)*abs(r[1]-centery)
        print("Distance "+str(distance))
        if distance < min_x_plus_y :
            minbb = r
            min_x_plus_y = distance
    xerror = centerx - minbb[0]
    yerror = centery - minbb[1]
    return (xerror,yerror)
    
# Return blobs in monochrome image with width and height between min and max
def monoblobs(img, con=(1,1.8,-70), minDim=3, maxDim=12) :
    """IP cameras like 2X(Erode->Dilate->Dilate) erodeDilate(img,2,1,2)
    USB camera likes single erode->dilate cycle erodeDilate(img,1,1,1)
    TODO: Automate variation of these parameters to get a good reading"""
    (it, sc, off) = con
    con = contrast(img, iter=it, scale=sc, offset=off)
    gray = erodeDilate(con, 1, 1, 2)
    gray2 = cv2.adaptiveThreshold(gray,255,cv2.ADAPTIVE_THRESH_GAUSSIAN_C,cv2.THRESH_BINARY,11,2)
    contours, _ = cv2.findContours(gray2, cv2.RETR_LIST, cv2.CHAIN_APPROX_SIMPLE)
    toosmall = 0
    toolarge = 0
    bbs = []
    for c in contours:
        rect = cv2.boundingRect(c)
        plog("Considering contour : "+str(rect))
        if (rect[1] < 3) :
            plog("Too high ")
            continue
        if rect[2] < minDim or rect[3] < minDim:  
            plog(str(rect)+ " too small MinDIM="+str(minDim))
            toosmall += 1
            continue
        elif    rect[2]>maxDim or rect[3]>maxDim :
            plog(str(rect)+ " too large MAXDIM="+str(maxDim))
            toolarge += 1
            continue
        else :
            plog( "SIZE OKAY   " + str(rect) + "\n")
            bbs.append(rect)

    plog(str(toosmall)+" too small "+str(toolarge)+" too large")
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
    
if __name__ == "__main__" :
    cv2.namedWindow("camera", cv2.CV_WINDOW_AUTOSIZE)
    lightLevel()
    img = grabFrame()
    (w,h,d) = img.shape
    centerx = w/2
    centery = h/2
    while(1) :
        img = grabFrame()
        centerpoint = str((centery-30,centerx+7))
        cv2.putText(img,centerpoint, (centery-40,centerx+7),cv2.FONT_HERSHEY_SIMPLEX, 0.5,(255,255,255),1)
        cv2.putText(img,"l", (centery-60,centerx),cv2.FONT_HERSHEY_SIMPLEX, 0.5,(0,255,0),1)
        cv2.putText(img,"r", (centery+60,centerx),cv2.FONT_HERSHEY_SIMPLEX, 0.5,(0,255,0),1)
        cv2.putText(img,"u", (centery,centerx-30),cv2.FONT_HERSHEY_SIMPLEX, 0.5,(0,255,0),1)
        cv2.putText(img,"d", (centery,centerx+30),cv2.FONT_HERSHEY_SIMPLEX, 0.5,(0,255,0),1)
        cv2.rectangle(img,(centery+50,centerx-20),(centery-50,centerx+20),(100,100,255),1)
        showUser(img)
        (b,g,r) = cv2.split(img)
        red = cv2.subtract(cv2.multiply(r,2),cv2.add(b/2,g/2))
        showUser(red)
#        red = cv2.subtract(cv2.multiply(red, 3),100)
#        showUser(red)
        bbs = monoblobs(red)
        for r in bbs:
            cv2.rectangle(img,(r[0],r[1]),(r[0]+r[2],r[1]+r[3]),(255,160,120),1)
        showUser(img,5000)
        (deltax, deltay) = nearest_error(bbs, centery, centerx)
        cv2.rectangle(img,(centerx+deltax-10,centery+deltay-10),(centerx+deltax+10,centery+deltay+10),(0,0,255),1)
        print("Error "+str((deltax,deltay)))
        if (abs(deltax) > 20):
            if deltax < 0 :
                print("move left")
                moveCamera(textcmds['left'])
            else :
                print("move right")
                moveCamera(textcmds['right'])
        if (abs(deltay) > 20):
            if deltay < 0 :
                print("move down")
                moveCamera(textcmds['down'])
            else :
                print("move up")
                moveCamera(textcmds['up'])
                
            
