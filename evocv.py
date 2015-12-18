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

    def __init__(self, color=1, minsize=24, maxsize=160):
        """Optional color, and min/max dimension setting"""
	self.color = color   # default color of interest is Green
        self.maxWidth = 60   # All lagoon areas will be reduced to a strip this wide
	self.minDim = minsize
        self.debug = ''
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
                
    def debugString(self) :
	return self.debug

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

    def contrast(self, image, iter=1, scale=2.0, offset=-100) :
        if (image == None) :
            self.debug = self.debug + "contrast called with null Image"
        for i in range(iter) :
            if (image == None) :
                self.debug = self.debug + "contrast loop: Image is None"
            else :
                image = cv2.add(cv2.multiply(image,scale),offset)
        if (image == None) :
            self.debug = self.debug + "image is None after add/mulitply in contrast!"
        self.showUser(image,200)
        (ret,img) = cv2.threshold(image, 127, 255, cv2.THRESH_BINARY)
        if (ret == False) :
            self.debug = self.debug + "Thresholding failed?"
            return None
        if (img == None) :
            self.debug = self.debug + "img is None after binary threshold in contrast"
        self.showUser(img, 200)
        # ret value is threshold (127.0) not True - False
        return img

    def emphasis(self, img, scale=2, fraction=0.5) :
        """Return monochrome image with doubled(scale) selected color
           minus half of the other two colors(/fraction) added together.
           Where color is Blue (0), Green (1-default), or Red (2)"""
        return cv2.subtract(cv2.multiply(img[:,:,self.color],scale),
                            cv2.multiply(cv2.add( img[:,:,(self.color+1)%3],
                                                  img[:,:,(self.color+2)%3]),fraction))
    
    def showUser(self, image, pause=0) :
        if (image != None) :
            if (pause != 0) :
                cv2.imshow("camera",image)
                if cv.WaitKey(pause) == 27:
                    exit(0)
        else :
            print "showUser called with null image (None)"
            traceback.print_stack()


    def level(self, img, pause=0) :
        """Return the uppermost horizontal line in the image (e.g. liquid level)
           Returns:   -1 when there is a problem with the data
                    1000 when there is no line within proper range"""
        if (img == None) :
            print "Level detector called with invalid image"
            return -1
        (h,w) = img.shape
        if (h == 0 or w == 0) :
            self.debug = self.debug + "Level called with degenerate image SHAPE" + str(img.shape)
            return -1
        img = self.contrast(img)
        if (img==None) :
            self.debug = self.debug + "Contrast returned None  (shape =" + str(img.shape)
            return -1
        edges = cv2.Canny(img, 90, 100)
        if (edges == None) :
            self.debug = self.debug + "Bad Canny output so not calling HoughLinesP in level()"
            return -1
        alllines = cv2.HoughLinesP(edges, 2, 3.1415926/2.0, 1, 16, 4)
        if (alllines == None) :
            self.debug = self.debug + "No horizontal lines found in image"
            return -1
        topline = 1000 + len(alllines)
        self.debug = self.debug + "ALLINES " + str(alllines)
        for lines in alllines:
            self.debug = self.debug + "LINES " + str(lines)
            for l in lines : # Find the highest (minY) line (not on the edge)
                self.debug = self.debug + "LINE " + str(l)
                if (l[1] == l[3]) : # Horizontal?
                    if ( l[1] < topline and l[1] > 5 and l[1] < h-5) :
                        self.debug = self.debug + "\nHighest so far: " + str(l)
                        topline = l[1]
                    else :
                        self.debug = self.debug + " NOT RIGHT " + str(l) + " H = " + str(h)
#        if (topline > 999) :
#            print "EVOCV: " + self.debug + "END EVOCV"
#        else:
#            self.debug = ""
        return topline

    def blobs(self, img, pause=0) :
        """IP cameras like 2X(Erode->Dilate->Dilate) erodeDilate(img,2,1,2)
           USB camera likes single erode->dilate cycle erodeDilate(img,1,1,1)
           TODO: Automate variation of these parameters to get a good reading"""
	emp = self.emphasis(img)
#        con = self.contrast(emp,iter=1)
        con = self.contrast(emp,iter=1,scale=1.4, offset=-50)
	gray = self.erodeDilate(con, 1, 1, 1)
	gray2 = cv2.adaptiveThreshold(gray,255,cv2.ADAPTIVE_THRESH_GAUSSIAN_C,cv2.THRESH_BINARY,11,2)
        contours, _ = cv2.findContours(gray2, cv2.RETR_LIST, cv2.CHAIN_APPROX_SIMPLE)
	if (pause != 0) :
            self.debug = self.debug + str(len(contours)) + " contours ( "
        toosmall = 0
        toolarge = 0
	bbs = []
	for c in contours:
		rect = cv2.boundingRect(c)
		if rect[2] < self.minDim or rect[3] < self.minDim:
                    toosmall += 1
                    continue
		elif    rect[2]>self.maxDim or rect[3]>self.maxDim or rect[2]<self.minDim or rect[3]<self.minDim:
                    toolarge += 1
                    continue
		else :
                    if (rect[2] > self.maxWidth) :  # Limit width and center
                        margin = (rect[2]-self.maxWidth)/2
                        self.debug = self.debug + "WIDTH/CENTER ADJUSTED " + str(rect) + "\n"
                        bbs.append((margin+rect[0],rect[1],self.maxWidth,rect[3]))
                    else :
                        self.debug = self.debug + "SIZE OKAY   " + str(rect) + "\n"
                        bbs.append(rect)

        if (pause != 0) :
            self.debug = self.debug + ") "+str(toosmall)+" too small "+str(toolarge)+" too large\n"
            pen = (255,255,255) # White
            for r in bbs:
                cv2.rectangle(img,(r[0],r[1]),(r[0]+r[2],r[1]+r[3]),pen,2)
            self.showUser(img,pause)
        self.debug = ">>>>>>blobs>>>>>>>\n" + self.debug + ">>>>>" + str(len(bbs)) + ">>>>>>>>>>>>>"
	return bbs


