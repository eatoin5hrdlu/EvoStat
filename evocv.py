from __future__ import print_function
import os, sys, time, traceback
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
        self.maxWidth = 60   # All lagoon areas will be reduced to a strip this wide
	self.minDim = minsize
	self.maxDim = maxsize
        self.kernal = np.ones((5,5),np.uint8)
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
            print("contrast called with null Image",file=sys.stderr)
        for i in range(iter) :
            print("Try contrast "+str((iter,scale,offset)), file=sys.stderr)
            if (image == None) :
                print("contrast loop: Image is None",file=sys.stderr)
            else :
                self.showUser(image)
                image = cv2.add(cv2.multiply(image,scale),offset)
                if (image == None) :
                    print( "image(None) after add/mulitply in contrast!",file=sys.stderr)
        self.showUser(image,label= ("cdone",image.shape[0]/2,image.shape[1]/2) )
        (ret,img) = cv2.threshold(image, 127, 255, cv2.THRESH_BINARY)
        if (ret == False) :
            print( "Thresholding failed?",file=sys.stderr)
            return None
        if (img == None) :
            print( "img is None after binary threshold in contrast",file=sys.stderr)
        self.showUser(img)
        # ret value is threshold (127.0) not True - False
        return img
    
    def emphasis(self, img, color=1, scale=2, fraction=0.5) :
        """Return monochrome image with selected color scaled
           minus sum of the fractions of the other colors.
           Where color is Blue (0), Green (1-default), or Red (2)"""
        print("Color="+str(color),file=sys.stderr)
        return cv2.subtract(cv2.multiply(img[:,:,color],scale),
                            cv2.multiply(cv2.add( img[:,:,(color+1)%3],
                                                  img[:,:,(color+2)%3]),fraction))
    
    def showUser(self, img, label=(".",0,0)) :
        if (self.params['debugpause'] > 10) :
            (text, y, x) = label
            cv2.putText(img,text, (y,x),cv2.FONT_HERSHEY_SIMPLEX, 0.5,(255,0,255),1)
            cv2.imshow("camera", img)
            if cv.WaitKey(self.params['debugpause']) == 27:
                 exit(0)

    def level(self, img) :
        """Return the uppermost horizontal line in the image (e.g. liquid level)
           Returns:   -1 when there is a problem with the data
                    1000 when there is no line within proper range"""
        if (img == None) :
            print("Level detector called with invalid image",file=sys.stderr)
            return -1
        (h,w) = img.shape
        if (h == 0 or w == 0) :
            print( "Level called with degenerate image SHAPE" + str(img.shape),file=sys.stderr)
            return -1
        img = self.contrast(img)
        if (img==None) :
            print( "Contrast returned None  (shape =" + str(img.shape), file=sys.stderr)
            return -1
        edges = cv2.Canny(img, 90, 100)
        if (edges == None) :
            print( "Bad Canny output so not calling HoughLinesP in level()", file=sys.stderr)
            return -1
        # distance resolution, angle resolution, 
        alllines = cv2.HoughLinesP(edges, 2, np.pi/2.0, 1, 8, 4)
        if (alllines == None) :
            print( "No horizontal lines found in image", file=sys.stderr)
            return -1
        topline = 1000 + len(alllines)
        print( "ALLINES " + str(alllines), file=sys.stderr)
        for lines in alllines:
            print( "LINES " + str(lines), file = sys.stderr)
            for l in lines : # Find the highest (minY) line (not on the edge)
                print( "LINE " + str(l), file=sys.stderr)
                if (l[1] == l[3]) : # Horizontal?
                    if ( l[1] < topline and l[1] > 5 and l[1] < h-5) :
                        print( "\nHighest so far: " + str(l), file=sys.stderr)
                        topline = l[1]
                    else :
                        print( " NOT RIGHT " + str(l) + " H = " + str(h), file=sys.stderr)
        print("evocv.level(): topline coordinate is: ",str(topline),file=sys.stderr)
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
        print( str(len(contours)) + " contours ( ", file=sys.stderr)
        toosmall = 0
        toolarge = 0
	bbs = []
	for c in contours:
		rect = cv2.boundingRect(c)
                print("Considering contour : "+str(rect),file=sys.stderr)
		if rect[3] < self.minDim:  # vertical dim is never too small (nearly empty vessel!)
                    print(str(rect)+ " too small MinDIM="+str(self.minDim),file=sys.stderr)
                    toosmall += 1
                    continue
		elif    rect[2]>self.maxDim or rect[3]>self.maxDim :
                    print(str(rect)+ " too large MAXDIM="+str(self.maxDim),file=sys.stderr)
                    toolarge += 1
                    continue
		else :
                    if (rect[2] > self.maxWidth) :  # Limit width and center
                        margin = (rect[2]-self.maxWidth)/2
                        adjusted = (margin+rect[0],rect[1],self.maxWidth,rect[3])
                        print( "WIDTH/CENTER ADJUSTED " + str(adjusted), file=sys.stderr)
                        bbs.append(adjusted)
                    else :
                        print( "SIZE OKAY   " + str(rect) + "\n", file=sys.stderr)
                        bbs.append(rect)

        print(") "+str(toosmall)+" too small "+str(toolarge)+" too large",file=sys.stderr)
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
                    print("Return value from usbcam.read() was "+str(rval),file=sys.stderr)
                    exit(0)
            except e:
                print("Failed to grab image from USB camera" + str(e),file=sys.stderr)
        if (self.params['rotate']) :
            img = self.rotateImage(img, self.params['rotate'])
        return img
        return None


    def croppedImage(self, brect):
        print("Evocv cropping "+str(brect),file=sys.stderr)
        (y1,x1,y2,x2) = brect
        image = self.grab()
        if (image == None) :
            print("camera(fail).", file=sys.stderr)
            exit(0)
        cimg = image[y1:y2,x1:x2,:]
        print("Evocv cropped resulting in shape: "+str(cimg.shape),file=sys.stderr)
        return cimg

# BB (bounding box) is (y1, x1, height, width) ??    
# Color  is one of [0,1,2] [Blue, Green, Red]
# Contrast = (#Iterations, Multiply, Offset(usually negative)) try (3, 1.35, -70)

    def getLevel(bb, color, contrast) :
        """Level value is a percentage of the region height"""
        badRead = True
        percentage = 0
        print("getLevel " + str(bb), file=sys.stderr)
        while (badRead) :
            frame = self.croppedImage(bb)
            mono = frame[:,:,color]
            self.showUser(mono)
            (it, sc, off) = contrast
            mono = self.contrast(mono,iter=it,scale=sc,offset=off)
            print("Image is None after contrast in getLevel",file=sys.stderr)
            self.showUser(mono)
            lvl = self.level(mono)
            print("evocv.getLevel() "+str(lvl),file=sys.stderr)
            if (lvl == None or lvl > bb[2] ) :
                print("getLevel failed", file=sys.stderr)
            if (lvl > 0 and lvl < bb[2]) : # Level in range
                H = bb[2]-bb[0]
                percentage = 100*(H - lvl)/H
                badRead = None
                cv2.putText(frame,str(percentage)+"%", (0,lvl-4),
                            cv2.FONT_HERSHEY_SIMPLEX, 0.75, (255,0,255),2)
                cv2.line(frame,(0,lvl),(bb[2],lvl), (255,0,255),2)
                self.showUser(frame)
            else :
                print(str(lvl) + " out of range :" + str(bb), file=sys.stderr)
        return percentage

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
                print("Not enough blobs in image",file=sys.stderr)
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
                    print(str(bb) + " redundant: not added to list",file=sys.stderr)
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
