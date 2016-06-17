from __future__ import print_function
import os, sys, time, traceback
from util import *
import numpy as np
import cv2
import cv2.cv as cv

class EvoCv(object):
    """Utilities for EvoStat visual feedback
    Status images for Web access:
           Normal lighting
           Meniscus lighting
           Dark (bio-luminescence)
       Liquid Level Detection:
           Blob detection,
              Single-color emphasis (default green),
              Camera Settings,
              Parameterized cycles of Contrast, Erode, Dilation
           Lagoon Outlines
           Horizontal line (liquid level) detection
       BioLuminescence:
           Multiple Image addition: Sum(Green - (Blue+Red)/2)
           Saturation detection"""

    def __init__(self, usbcam, params, color=1, minsize=24, maxsize=160):
        """Optional color, and min/max dimension setting"""
        self.usbcam = usbcam
        self.params = params
	self.color = color   # default color of interest is Green
        self.maxWidth = 60   # All blobs are reduced to a strip this wide
	self.minDim = minsize
	self.maxDim = maxsize
        self.kernal = np.ones((2,2),np.uint8)
        self.alpha   = np.array([  1.2 ])
        self.beta    = np.array([ -60 ])
	self.theta = 1
	self.phi = 1
	self.maxIntensity = 255.0

    def erodeDilate(self,img,iter=1,erode=1,dilate=1) :
        for i in range(iter):
            img = cv2.erode(img,self.kernal,iterations=erode)
            img = cv2.dilate(img,self.kernal,iterations=dilate)
        return img
                
    def set_minsize(self, minsize) :
	self.minDim = minsize

    def set_maxsize(self, maxsize) :
	self.maxDim = maxsize

    def set_color(self, color) :
	self.color = color

    def rotateImage(self, img, angle=90):
        """+-90 degree rotations are fast and do not crop"""
        if (angle == 90) :
            return(cv2.flip(cv2.transpose(img),flipCode=0))
        elif (angle == -90) :
            return(cv2.flip(cv2.transpose(img),flipCode=1))
        else :
            center = (img.shape[1]/2.0,img.shape[0]/2.0)
            rotate = cv2.getRotationMatrix2D(center, angle, 1.0)
            return cv2.warpAffine(img, rotate, (img.shape[1], img.shape[0]))

    def contrast(self, image, iter=1, scale=2.0, offset=-80) :
        if (image == None) :
            plog("contrast called with null Image")
        for i in range(iter) :
            plog("Try contrast "+str((iter,scale,offset)))
            if (image == None) :
                plog("contrast loop: Image is None")
            else :
                self.showUser(image)
                image = cv2.add(cv2.multiply(image,scale),offset)
                if (image == None) :
                    plog( "image(None) after add/mulitply in contrast!")
            image = self.erodeDilate(image, 1, 1, 1)
        self.showUser(image,label= ("cdone",image.shape[0]/2,image.shape[1]/2) )
        (ret,img) = cv2.threshold(image, 127, 255, cv2.THRESH_BINARY)
        if (ret == False) :
            plog( "Thresholding failed?")
            return None
        if (img == None) :
            plog( "img is None after binary threshold in contrast")
        self.showUser(img)
        # ret value is threshold (127.0) not True - False
        return img
    
    def emphasis(self, img, color=1, scale=2, fraction=0.5) :
        """Return monochrome image with selected color scaled
           minus sum of the fractions of the other colors.
           Where color is Blue (0), Green (1-default), or Red (2)"""
        plog("Color="+str(color))
        return cv2.subtract(cv2.multiply(img[:,:,color],scale),
                            cv2.multiply(cv2.add( img[:,:,(color+1)%3],
                                                  img[:,:,(color+2)%3]),fraction))
    
    def showUser(self, img, label=(".",0,0),multiplier=1) :
        if (self.params['debugpause'] > 10) :
            (text, y, x) = label
            cv2.putText(img,text, (y,x),cv2.FONT_HERSHEY_SIMPLEX, 0.5,(255,0,255),1)
            cv2.imshow("camera", img)
            if cv.WaitKey(multiplier*self.params['debugpause']) == 27:
                 exit(0)

    def level(self, img) :
        """Return the uppermost horizontal line in the image (e.g. liquid level)
           Returns:   -1 when there is a problem with the data
                    1000 when there is no line within proper range"""
        if (img == None) :
            plog("Level detector called with invalid image")
            return -1
        (h,w) = img.shape
        if (h == 0 or w == 0) :
            plog( "Level called with degenerate image SHAPE" + str(img.shape))
            return -1
        img = self.contrast(img)
        if (img==None) :
            plog("Contrast returned None  (shape =" + str(img.shape))
            return -1
        edges = cv2.Canny(img, 90, 100)
        if (edges == None) :
            plog("Bad Canny output so not calling HoughLinesP in level()")
            return -1
        # distance resolution, angle resolution, 
        alllines = cv2.HoughLinesP(edges, 2, np.pi/2.0, 1, 8, 4)
        if (alllines == None) :
            plog("No horizontal lines found in image")
            return -1
        topline = 1000 + len(alllines)
        plog( "ALLINES " + str(alllines))
        for lines in alllines:
            plog( "LINES " + str(lines))
            for l in lines : # Find the highest (minY) line (not on the edge)
                plog( "LINE " + str(l))
                if (l[1] == l[3]) : # Horizontal?
                    if ( l[1] < topline and l[1] > 5 and l[1] < h-5) :
                        plog( "\nHighest so far: " + str(l))
                        topline = l[1]
                    else :
                        plog( " NOT RIGHT " + str(l) + " H = " + str(h))
        plog("level(): topline coordinate is: "+str(topline))
        return topline

    def blobs(self, img, color=1, contrast=(3,2,-70)) :
        """IP cameras like 2X(Erode->Dilate->Dilate) erodeDilate(img,2,1,2)
           USB camera likes single erode->dilate cycle erodeDilate(img,1,1,1)
           TODO: Automate variation of these parameters to get a good reading"""
        (it, sc, off) = contrast
#	emp = self.emphasis(img,color)  # Creates monochrome from color
	emp = img[:,:,color]
        con = self.contrast(emp, iter=it, scale=sc, offset=off)
	gray = self.erodeDilate(con, 1, 1, 2)
	gray2 = cv2.adaptiveThreshold(gray,255,cv2.ADAPTIVE_THRESH_GAUSSIAN_C,cv2.THRESH_BINARY,11,2)
        contours, _ = cv2.findContours(gray2, cv2.RETR_LIST, cv2.CHAIN_APPROX_SIMPLE)
        plog( str(len(contours)) + " contours ( ")
        toosmall = 0
        toolarge = 0
	bbs = []
	for c in contours:
		rect = cv2.boundingRect(c)
                plog("Considering contour : "+str(rect))
                if (rect[1] < 3) :
                    plog("Too high ")
                    continue
		if rect[2] < self.minDim:  # vertical dim(3) is never too small (nearly empty vessel!)
                    plog(str(rect)+ " too small MinDIM="+str(self.minDim))
                    toosmall += 1
                    continue
		elif    rect[2]>self.maxDim or rect[3]>self.maxDim :
                    plog(str(rect)+ " too large MAXDIM="+str(self.maxDim))
                    toolarge += 1
                    continue
		else :
                    if (rect[2] > self.maxWidth) :  # Limit width and center
                        margin = (rect[2]-self.maxWidth)/2
                        adjusted = (margin+rect[0],rect[1],self.maxWidth,rect[3])
                        plog( "WIDTH/CENTER ADJUSTED " + str(adjusted))
                        bbs.append(adjusted)
                    else :
                        plog( "SIZE OKAY   " + str(rect) + "\n")
                        bbs.append(rect)

        plog(") "+str(toosmall)+" too small "+str(toolarge)+" too large")
        for r in bbs:
            cv2.rectangle(img,(r[0],r[1]),(r[0]+r[2],r[1]+r[3]),(255,255,255),2)
        self.showUser(img)
	return bbs

    def rotateImage(self, img, angle=90):
        """+-90 degree rotations are fast and do not crop"""
        if (angle == 90) :
            return(cv2.flip(cv2.transpose(img),flipCode=0))
        elif (angle == -90) :
            return(cv2.flip(cv2.transpose(img),flipCode=1))
        else :
            center = (img.shape[1]/2.0,img.shape[0]/2.0)
            rotate = cv2.getRotationMatrix2D(center, angle, 1.0)
            return cv2.warpAffine(img, rotate, (img.shape[1], img.shape[0]))

    def grab(self):
        if (not self.usbcam is None) :
            try :
                (rval, img) = self.usbcam.read()
                if (rval) :
                    pass
                else :
                    plog("Return value from usbcam.read() was "+str(rval))
                    exit(0)
            except e:
                plog("Failed to grab image from USB camera" + str(e))
        if (self.params['rotate']) :
            img = self.rotateImage(img, self.params['rotate'])
        return img

    def croppedImage(self, brect):
        plog("Evocv cropping "+str(brect))
        (y1,x1,y2,x2) = brect
        image = self.grab()
        plog(str(image.shape))
        self.showUser(image)
        if (image == None) :
            plog("camera(fail).")
            exit(0)
        cimg = image[y1:y2,x1:x2,:]
        plog(str(cimg.shape))
        self.showUser(cimg)
        return cv2.copyMakeBorder(cimg, 2,2,2,2, cv2.BORDER_CONSTANT,(0,0,0))

    def cropImage(self, image, brect):
        if (image is None) :
            plog("cropImage given None image")
            exit(0)
        plog("Evocv cropping "+str(brect))
        (y1,x1,y2,x2) = brect
        self.showUser(image)
        if (image == None) :
            plog("camera(fail).")
            exit(0)
        cimg = image[y1:y2,x1:x2,:]
        plog(str(cimg.shape))
        self.showUser(cimg)
        return cv2.copyMakeBorder(cimg, 2,2,2,2, cv2.BORDER_CONSTANT,(0,0,0))

# BB (bounding box) is (y1, x1, y2, x2)
# Color  is one of [0,1,2] [Blue, Green, Red]
# Contrast = (#Iterations, Multiply, Offset(usually negative)) try (3, 1.35, -70)

    def getLevel(self, image, bb, color, contrast) :
        """Level value is a y position in the region"""
        plog("getLevel " + str(bb))
        frame = self.cropImage(image, bb)
        mono = frame[:,:,color]
        self.showUser(mono)
        (it, sc, off) = contrast
        mono = self.contrast(mono,iter=it,scale=sc,offset=off)
        if (mono == None) :
            plog("Image is None after contrast in getLevel")
        self.showUser(mono)
        lvl = self.level(mono)
        if (lvl == -1):
            return -1
        plog("getLevel() "+str(lvl))
        if (lvl == None or lvl > bb[2] ) :
            plog("getLevel failed")
        if (lvl > 0 and lvl < bb[2]) : # Level in range
            return lvl
        plog(str(lvl) + " out of range :" + str(bb))
        return 0

    def sliceColor(img, color):
        if (isinstance(colors,(int,long))) :
            mono = img[color]
        else :
            mono is img[colors[0]]
            for c in colors[1:] :
                mono is cv2.add(mono,img[c])
        return(mono)

    def expandRectangles(bbs, height) :
        rects = []
        for bb in bbs :
            (x, y, w, h) = bb
            ypos = y-(height-h)
            rects.append((x, ypos, x+w, ypos+height))
        return rects

    def findOutlines(self, bb, names, color=1, contrast=(3,2,-70)) :
        """Blob detection locates multiple vessels. Call this before updateLevels()."""
        sbbs = []
        while (len(sbbs) < len(names)):
            img = self.croppedImage(bb)[:,:,color]
            self.showUser(img)
            bbs = self.blobs(img, color, contrast) # Find colored blobs
            obbs = self.blobs2outlines(bbs)        # Sort left to right removing redundancies
            if (len(obbs) < len(names)):
                plog("Not enough blobs in image")
                self.showUser(img)
        self.drawBbs(img, obbs)
        return expandRectangles(obbs,bb[2]-bb[0])
        rects = expandRectangles(obbs,bb[2]-bb[0])
        self.drawRects(img, rects)
        return rects

    def blobs2outlines(self, bbs) :
        """The bottom edges of identified blobs should line up.
        In EvoStat these are the bottoms of the lagoons.  The tops vary
        as they represent the liquid levels, so we create a set of
        outlines to include maxiumum fill levels. These are the only 
        regions of interest for horizontal line detection"""
        sbbs = [b for a,b in sorted((tup[0], tup) for tup in bbs)]
        vessels = []
        for bb in sbbs :
            if (len(vessels) == 0) :
                vessels.append(bb)
            else :
                prevbb = vessels[len(vessels)-1]
                if bb[0] > (prevbb[0]+prevbb[2]) :
                    vessels.append(bb)
                else:
                    plog(str(bb) + " redundant: not added to list")
        return vessels

    def drawBbs(self, image, outlines) :
        # Create a set of colors so the outlines are distinct
        cset = [cv.Scalar(0,0,255,255),cv.Scalar(0,255,255,255),cv.Scalar(255,0,0,255),cv.Scalar(255,0,255,255)]
        i = 0
        for bb in outlines :
            cv2.rectangle(image,(bb[0],bb[1]),(bb[0]+bb[2],bb[1]+bb[3]),cset[i%4],1)
            cv2.circle(image,(bb[0],bb[1]),5,cset[(i+1)%4],2)
            i = i + 1
        self.showUser(image)

    def drawRects(self, image, outlines) :
        # Create a different set of colors so the outlines are distinct
        cset = [cv.Scalar(128,0,255,255),cv.Scalar(255,0,0,255),cv.Scalar(0,255,128,255),cv.Scalar(0,0,255,255)]
        i = 0
        for r in outlines :
            cv2.rectangle(image,(r[0],r[1]),(r[2],r[3]),cset[i%4],1)
            cv2.circle(image,(r[0],r[1]),5,cset[(i+1)%4],2)
            i = i + 1
        self.showUser(image)
