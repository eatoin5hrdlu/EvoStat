#!/usr/bin/python -u
#!C:/cygwin/Python27/python -u
#!C:/Python27/python -u
# Regions lead with y  (y1,x1,y2,x2) where (y1,x1) is (uppermost,leftmost)
# OpenCV blob,contour algorithms return (X,Y,Width,Height) not(x,y,x2,y2)
#
from __future__ import print_function
from contrast import makeTkSliders

import sys, os, time, socket, subprocess, re, traceback

from os  import popen
import glob
import base64, urllib2
from suppress_stdout_stderr import suppress_stdout_stderr
import numpy as np
import cv2
import cv2.cv as cv
import evocv2
from util import *

def on_exit(nLagoons) :
    print(termIntList('levels',[80, 60, 40, 20]))
    exit(0)

levelPhase = 1
rbox =  [100,150,200,300]
bbfp = None
threshold = 1100

def on_mouse(event, x, y, flags, params):
    global rbox
    if event == cv.CV_EVENT_LBUTTONDOWN:
        rbox[0] = x
        rbox[1] = y
    elif event == cv.CV_EVENT_LBUTTONUP:
        rbox[2] = x
        rbox[3] = y
        if (not bbfp is None):
            bbfp.write("("+str(rbox)+")")
            bbfp.flush()
            
#
# IPCamera knows about different IP cameras (as well as USB cams) and
# can find the IP address from a MAC address (requiring linux and superuser and time)
# This process will report the IP address which should be edited into evo.settings file
# along with with lagoon locations and other things which won't be changing.
#
# Fluorescence module does color integration (returns saturation cycle)
# Blob detection module ( find lagoon/cellstat coordinates)
# import ipcam, fluor, blob, level

class ipCamera(object):
    """USB, Wired Ethernet or WiFi IP camera discovery and control.
       This class requires a file './hostname.settings' for now.
       This object contain the configuration information and knows about
       lagoon dimensions, lighting, angle of camera, etc."""

    def __init__(self):
        self.params = None
        if 'lux' in sys.argv:
            threshold = 200
        self.params = settings()
        self.usbcam = None
        if isinstance(self.params['camera'],int) :
            self.usbcam = cv2.VideoCapture(self.params['camera'])
            time.sleep(0.1)
            (rval,img) = self.usbcam.read()
            if (not rval):
                plog("VideoCapture test returned "+str(rval))
                exit(4)
            self.ip = None
        else :
            plog("No IP camera support at present")
        self.evocv2  = evocv2.EvoCv(self.usbcam,
                                  self.params,
                                  color=1,
                                  minsize=self.params['lagoonWidth']/2, 
                                  maxsize=self.params['lagoonHeight'])

    def lagoonImage(self):
        (x1,y1,x2,y2) = self.params['lagoonRegion']
        image = self.usbcam.grab()
        if (image == None) :
            plog("camera(fail).")
            exit(0)
        self.exportImage(image)
        return image[x1:x2,y1:y2,:] # cropped for lagoons
#        return image

    def showThisColor(color) :
        frame = ipcam.evocv2.grab()
        picked = frame[:,:,color]  # Start with selected color image
        while True:
            temp =  ipcam.evocv2.grab()
            halfothers = cv2.addWeighted(temp[:,:,(color+1)%3], 0.5, temp[:,:,(color+2)%3], 0.5, 0 )
#        picked=cv2.addWeighted(picked,0.9, cv2.subtract(temp[:,:,color],halfothers), 0.95, 0)
            picked=cv2.add(picked,cv2.subtract(temp[:,:,color],halfothers))
            if not frame == None :
                labelImage(picked,color)
                if (picked != None) :
                    self.evocv2.userShow(picked)
                else :
                    plog("picked slice was None")

    def labelImage(self, img, color) :
        colors = {0:"blue", 1:"green", 2:"red" }
        cv2.putText(img,colors[color],(10,80),cv2.FONT_HERSHEY_PLAIN,4.0,(240,80,200),2)

    def updateLevels(self, height, brect, bbrects, contrast, color=1) :
        """Level is percentage of vessel height. To use mL as unit
        we should add scaling param to <hostname>.pl"""
        plog(">>>>>>>>>>updateLevels>>>>>>>>>>\n")
        if (bbrects == None):
            return [1]
        plog(str(brect))
        plog(str(bbrects))
        (gy,gx,_,_) = brect
        goodRead = 0
        Levels = []
        while (goodRead != len(bbrects)) :
            goodRead = 0
            frame = self.evocv2.grab()
            for r in bbrects:
                plog("height="+str(height))
                headroom = height-r[3]
                plog("headroom="+str(headroom))
                # Rectangles have xywh need x1,y1,x2,y2
                plog("start with: " + str(r))
                # Add headroom(vessel height) to reflect fill level
                c1 = max(1,gy+r[1]-headroom)
                bb = (c1, gx+r[0], gy+r[1]+r[3], gx+r[0]+r[2])
                plog("convert to: " + str(bb))
                subh = self.evocv2.croppedImage(bb)[:,:,color]
                plog("grabbed cropped image"+str(subh.shape))
                self.evocv2.showUser(subh)
                plog("shown")
                (it, sc, off) = contrast
                subi = self.evocv2.contrast(subh,iter=it,scale=sc,offset=off)
                lvl = self.evocv2.level(subi)
                if (lvl == None or lvl > 999) :
                    plog("level detection failed")
                    self.evocv2.showUser(subi)
                    break
                if (lvl > 0 and lvl < height) :   # Level is in range
                    Levels.append(100*(height - lvl)/height)   # as percentage
                    showLevel(frame, bb, lvl,(0,0,255))
                    goodRead = goodRead + 1
                    if (frame != None) :
                        self.evocv2.showUser(frame,multiplier=8)
                    else :
                          plog("frame was None after showLevel")
                else :
                    plog(str(lvl) + " out of range :" + str(bb))
                    return None
            if (goodRead != len(bbrects)) :
                plog(str(goodRead) + " good level reads")
        self.exportImage(frame)
        return Levels

    def updateLagoons(self, brect, needed, color=1) :
        """Blob detection to locate vessels. Call before updateLevels()."""
        plog("updateLagoons")
        sbbs = []
        blobs = []
        tries = 3
        while (len(sbbs) < needed and tries > 0) :
            frame = self.evocv2.croppedImage(brect)
            plog("Frame shape:" + str(frame.shape))
            bbs = self.evocv2.blobs(frame,color,contrast=self.params['lagoonContrast'])
            plog("BLOBS: " + str(bbs))
            sbbs = self.evocv2.blobs2outlines(bbs)
            plog("OUTLINES: " + str(sbbs))
            if (len(sbbs) >= needed) :  # Return the requested number of outlines
                return sbbs[:needed]
            else :
                tries = tries - 1
                plog("Needed " + str(needed) + " bbs, but got " + str(len(sbbs)))
                for bb in sbbs:
                    cv2.rectangle(frame,(bb[0],bb[1]),(bb[0]+bb[2],bb[1]+bb[3]),(0,0,255),2)
                if (frame is None) :
                    plog("frame was None after drawing bbs")
                else :
                    self.evocv2.showUser(frame)
        return None


    def exportImage(self, image) :
        global levelPhase
        (x1,y1,x2,y2) = self.params['lagoonRegion']
        (cx1,cy1,cx2,cy2) = self.params['cellstatRegion']
        filename = "mypic"+str(levelPhase)+".jpg"
        file2 = "./web/phagestat.png"
        if (image != None ) :
            cv2.rectangle(image,(y1,x1),(y2,x2),(250,250,0),2)
            cv2.rectangle(image,(cy1,cx1),(cy2,cx2),(0,200,200),2)
            cv2.imwrite(filename,cv2.resize(image,self.params['imageSize']))
            os.system("convert "+filename+" "+file2)
            newSnapshot = None
                    
    def drawLagoons(self, image) :
        global toggle
        global lagoon
        cler = [cv.Scalar(0,0,255,255),cv.Scalar(0,255,255,255),cv.Scalar(255,0,0,255),cv.Scalar(255,0,255,255)]
        i = 0
        for bb in lagoon.values():
            cv2.rectangle(image,(bb[0],bb[1]),(bb[0]+bb[2],bb[1]+bb[3]),cler[i%4],1)
            cv2.circle(image,(bb[0],bb[1]),5,cler[(i+1)%4],2)
            i = i + 1
        if (image != None) :
            self.evocv2.showUser(image)
        else :
            plog(">>>>drawLagoons>>>\nimage was None in drawLagoons\n>>>>>>")

    def bioBlobs(self, color, (x1,y1,x2,y2)) :
        """Bio-luminescence detection. Sum images until MAXFRAMES and note saturation points"""
        frame = None
        while(frame == None) :
            frame = ipcam.lagoonImage()
        picked = frame[:,:,color]  # Start with selected color image
        cycle = 0
        while True:
            cycle = cycle + 1
            temp = None
            tries = 0
            while (temp == None and tries < 10) :
                temp =  ipcam.evocv2.grab()
                if (temp == None) :
                    plog("Failed to get image from camera")
                    tries = tries + 1
                    time.sleep(100)
            if (temp == None) :
                plog("Giving up on camera connection")
                return None
                
            halfothers = cv2.addWeighted(temp[:,:,(color+1)%3], 0.5, temp[:,:,(color+2)%3], 0.5, 0 )
            picked=cv2.addWeighted(picked,0.9, cv2.subtract(temp[:,:,color],halfothers), 0.9, 0)
            sat = 0
            total = (x2-x1)*(y2-y1)
            lit = cv2.countNonZero(cv2.subtract(picked[y1:y2,x1:x2], 128))
            sat = cv2.countNonZero(cv2.subtract(picked[y1:y2,x1:x2], 250))
            plog(str(sat) + " saturated " + str(lit) + " detected out of " + str(total) + " at cycle " + str(cycle))
            cv2.rectangle(picked,(x1,y1),(x2,y2),255)
            if not frame == None :
                self.labelImage(picked,cler)
                self.evocv2.showUser(picked)

    def release(self) :
        if (self.usbcam != None) :
            self.usbcam.release()
            cv2.destroyAllWindows()

    def readLevels(bbox, needed, minDim, maxDim, minLevel, maxLevel) :
        previous = [] # START OF LAGOON LEVELS
        notDone = True
        tries = 3;
        while(notDone and tries > 0) :
            containers = self.updateContainers(needed) # blob contours shown for 4 seconds
            if (containers == None) :
                tries = tries - 1
                continue
            if ( self.updateContainerLevels(containers) == None) :
                tries = tries - 1
                continue
            for i in range(len(previous)) :
                for k in Levels.keys() :
                    if (abs(previous[i][k]-Levels[k]) > 0.1) :
                        plog("Odd man at " + str(i))
                        new = []
                        for j in range(len(previous)) :
                            if (j != i) :
                                new.append(previous[k])
                            else :
                                new.append(Levels)
                        plog("Replacing the odd man")
                        previous = new
                        continue
                plog("Current levels were close to previous " + str(len(previous)))
                if (len(previous) < needed) :
                    previous.append(Levels)
                    plog("added one to working list")
                howmany = len(previous)
                if (len(previous) == needed) :
                    plog("That's all we need")
                    notDone = False
                else:
                    plog(str(howmany) + " : " + str(previous))

            if (tries == 0) :
                on_exit(params['numLagoons'])
            return Levels

        

# End of ipCamera Class

def setupCamera() :
    cam = ipCamera()
    if ((cam.params['debugpause'] > 10) or ('locate' in sys.argv)) :
        with suppress_stdout_stderr() :
            cv2.namedWindow("camera", cv2.CV_WINDOW_AUTOSIZE)
        if cv2.__dict__['moveWindow'] != None :
            cv2.moveWindow("camera", 100, 0)
        else :
            plog("no moveWindow in cv2, update your OpenCV installation.")
    return cam

def dark(image) :
    global threshold
    totallight = np.average( tuple(ord(i) for i in image.tostring()) )
    if ( totallight < threshold ) :
        return True
    if ('baseline' in sys.argv):
        plog("msg('Must be dark to create baseline (camera heat image)').")
        exit(0)
    return False

# We only call getFluor if it is dark, and then we check
# to see that it is still dark before writing lux values

def getFluor(ipcam) :
    basefile = './baseline.jpg'
    baseline = None
    result = None
    cntr = 0
    if (not os.path.exists('lagoons')) :
        plog("msg('Luminosity mode: program must be run at least once with the lights on to locate lagoons').")
        exit(0)
    lagoon = eval(open('lagoons','r').read())
    if (os.path.exists(basefile)) :
        baseline = cv2.split(cv2.imread(basefile))[1]
    elif ( not 'baseline' in sys.argv) :
        plog("msg('Run: ipcam.py baseline  with no bioluminescence present to create dark(heat) image file').")
    frames = ipcam.params['frames']
    orig = ipcam.lagoonImage()
    fluor = orig[:,:,1]               # FIRST GREEN IMAGE
    while(cntr < frames) :
        orig = ipcam.lagoonImage()
        (bl, gr, rd) = cv2.split(orig)
        fluor = cv2.add(fluor, cv2.subtract(gr,cv2.add(bl/2,rd/2)))
        text = str(cntr) + "/" + str(frames)
        cv2.rectangle(fluor,(20,50), (160,0), 0, -1)
        cv2.putText(fluor,text, (20,40), cv2.FONT_HERSHEY_SIMPLEX, 1, 255,3)
        self.evocv2.showUser(fluor)
        cntr = cntr + 1
    if ('baseline' in sys.argv):
        plog("Creating baseline file")
        cv2.imwrite(basefile, fluor)
    else :
        self.evocv2.showUser(fluor)
        fluor = cv2.subtract(fluor,baseline)
        self.evocv2.showUser(fluor)
    if (dark(ipcam.lagoonImage())) :
        for k in lagoon.keys():
            bb = lagoon[k]   # Bounding box relative to cropped 'lagoonImage'
            subi = fluor[bb[1]:bb[1]+bb[3], bb[0]:bb[0]+bb[2]]
            plog(k + "(" + str(np.average( tuple(ord(i) for i in subi.tostring()))) + ").\n")

def incrColor(color,plus) :
    (b,g,r) = color
    if (b>plus):
        nb = b-plus
    else :
        nb = b
    if (g>plus):
        ng = g-plus
    else :
        ng = g
    if (r>plus):
        nr = r-plus
    else :
        nr = r
    return (nb,ng,nr)

def showBox(img, bb, colorin, size) :
    (x1,y1,x2,y2) = bb
    color = incrColor(colorin,-90)
    showCoord(img, (x1,y1), color, size)
    showCoord(img, (x2,y2), color, size)
    cv2.line(img,(x1,y2),(x2,y2),color,size)
    cv2.line(img,(x1,y1),(x2,y1),incrColor(color,30),size)
    cv2.line(img,(x1,y1),(x1,y2),incrColor(color,60),size)
    cv2.line(img,(x2,y1),(x2,y2),incrColor(color,80),size)

def rotateImage(img, angle=90):
    """+-90 degree rotations are fast and do not crop"""
    if (angle == 90) :
        return(cv2.flip(cv2.transpose(img),flipCode=0))
    elif (angle == -90) :
        return(cv2.flip(cv2.transpose(img),flipCode=1))
    else :
        center = (img.shape[1]/2.0,img.shape[0]/2.0)
        rotate = cv2.getRotationMatrix2D(center, angle, 1.0)
        return cv2.warpAffine(img, rotate, (img.shape[1], img.shape[0]))
            
def showCoord(img,pt,color,size) :
    cv2.putText(img,str(pt), pt, cv2.FONT_HERSHEY_PLAIN,size,color,size)

def showLevel(img, bb, lvl, color) :
    height = bb[2]-bb[0]
    width = bb[3]-bb[1]
    (px,py) = ( bb[3]-width/2, bb[0]+lvl )
    pc = 100-100*lvl/height
    cv2.line(img,(px,py),(px+width,py), color, 2)
    cv2.putText(img,"  "+str(pc)+"%",(px,py),
                cv2.FONT_HERSHEY_SIMPLEX, 0.5, color,1)

if __name__ == "__main__" :
    plog("openCV('" + str(cv2.__version__) + "').")
    ipcam = setupCamera()
    (rval, img) = ipcam.usbcam.read()
    if (not rval) :
        plog("camera read failed")
        exit(0)
    if (ipcam.params['rotate']) :
        img = rotateImage(img, ipcam.params['rotate'])
    plog(str(img.shape))
    if ('gen' in sys.argv) :   # Python .settings file updated
        ipcam.exportImage(img)
        exit(0)
    # EvoStat removes temporary pics at the beginning of the run
    # Here we toggle the two files between runs of this program
    if ('fluor' in sys.argv) :      # Add frames to detect bio-luminescence
        getFluor(ipcam)
        exit(0)
    if ('contrast' in sys.argv) :   # Interactive tool to find contrast settings
        makeTkSliders(ipcam.params['lagoonContrast'])
        exit(0)
    if ('snapshot' in sys.argv) :
        cv2.imwrite('phagestat.jpg',img)
        exit(0)
    if ('locate' in sys.argv):      # Help user find lagoon and cellstat regions
        cv.SetMouseCallback('camera', on_mouse, 0) # Set mouse handler
        bbfp = open('bbox.txt','a')
        for f in range(400) :
            img = ipcam.evocv2.grab()
            if (nullImage(img,"main:locate")) :
                exit(0)
            else :
                (x1,y1,x2,y2) = rbox
                showCoord(img, (x1,y1), (240,80,200),1)
                showCoord(img, (x2,y2), (240,80,200),1)
                cv2.line(img,(x1,y2),(x2,y2),(0,0,255),2)
                cv2.line(img,(x1,y1),(x2,y1),(0,255,255),2)
                cv2.line(img,(x1,y1),(x1,y2),(0,255,0),2)
                cv2.line(img,(x2,y1),(x2,y2),(255,0,0),2)
                (y1,x1,y2,x2) = ipcam.params['lagoonRegion']
                (cy1,cx1,cy2,cx2) = ipcam.params['cellstatRegion']
                showBox(img,(x1,y1,x2,y2),(0,255,0),2)
                showBox(img,(cx1,cy1,cx2,cy2),(0,200,200),2)
                ipcam.evocv2.showUser(img)
        bbfp.close()
        bbfp = None   # END OF LOCATE/BBOX HELPER
        exit(0)
    blue = 0
    green = 1
    red = 2
    if ( 'alllevels' in sys.argv ) : # Read lagoon levels
        plog("alllevels: " + str(img.shape))
        brect = ipcam.params['lagoonRegion']
        quarterWidth = (brect[3]-brect[1])/4
        eighthWidth = quarterWidth/2
        plog("QUARTER width " + str(quarterWidth))
        llist = []
        for i in range(ipcam.params['numLagoons']) :
            x1 = brect[1] + i * eighthWidth
            x2 = brect[1] + (i+1)*eighthWidth
            lbb = (brect[0], x1, brect[2], x2)
            llev = ipcam.evocv2.getLevel( img,
                                          lbb,
                                          green,
                                          ipcam.params['lagoonContrast'])
            plog("LBB " + str(lbb) + " level " + str(llev))
            showLevel(img, lbb, llev, (128,128,255))
            llist.append(llev)
        levelPhase = 1
        brect = ipcam.params['cellstatRegion']
        num = 1
        contrast = ipcam.params['cellstatContrast']
        lev = ipcam.evocv2.getLevel(img, brect, red, contrast)
        plog("CBB " + str(brect) + " level " + str(lev))
        showLevel(img, brect, lev, (255,255,0))
        ipcam.exportImage(img)
        llist.insert(0,lev)
        print(termIntList('levels',llist))
    ipcam.release()

