#!/usr/bin/python -u
from __future__ import print_function
import os, sys, time
from util import *
import numpy as np
import cv2
import cv2.cv as cv

class Secchi(object):
    """Linear version of a Secci Disk:
    Black and white bands on a receeding stairstep"""

    def __init__(self, usbcam, params, color=2):
        """Optional color, and min/max dimension setting"""
        self.usbcam = usbcam
        self.params = params
	self.color = color   # default color is Red (600nM)
        self.maxWidth = 60   # Default band width
        self.maxHeight = 30  # Default band height
        self.kernal = np.ones((2,2),np.uint8)
        self.alpha   = np.array([  1.2 ])
        self.beta    = np.array([ -60 ])
	self.theta = 1
	self.phi = 1
	self.maxIntensity = 255.0

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
    
    def emphasis(self, img, color=2, scale=2, fraction=0.5) :
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
            if img.shape[1] < 100 :
                img = cv2.resize(img,None,fx=4, fy=4, interpolation = cv2.INTER_CUBIC)
            cv2.imshow("camera", img)
            if cv.WaitKey(self.params['debugpause']) == 27:
                 exit(0)

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
        plog("Evocv cropping(y1,x1,y2,x2) "+str(brect))
        (y1,x1,y2,x2) = brect
        image = self.grab()
        plog("croppedImage input: " + str(image.shape))
        self.showUser(image)
        if (image == None) :
            plog("camera(fail).")
            exit(0)
        cimg = image[y1:y2,x1:x2,:]
        plog("croppedImage output: " + str(cimg.shape))
        plog(str(cimg.shape))
        self.showUser(cimg)
        return cv2.copyMakeBorder(cimg, 2,2,2,2, cv2.BORDER_CONSTANT,(0,0,0))

    def turbidity(self, img) :
        brect = self.params['secchiRegion']
        (y1,x1,y2,x2) = brect
        osx = (x2-x1)/9
        band = (y2-y1)/7
        osy = band/4
        cimg = self.croppedImage(brect)
        vals = []
        for i in range(6) :
            offset = band*i
#            (B,G,R,A) = cv2.mean(cimg[offset:offset+band,:,:])
            (B,G,R,A) = cv2.mean(cimg[
            osy+band*i:band*(i+1)-osy,
            osx+3:((x2-x1)-osx),
            :])
            vals.append(int(R))
        for i in range(6) :
            cv2.rectangle(cimg,(osx+3,osy+band*i),((x2-x1)-osx,band*(i+1)-osy),(34*i,200,255-30*i),1)
        big = cv2.resize(cimg,None,fx=4, fy=4, interpolation = cv2.INTER_CUBIC)
        self.showUser(big)
        print(termIntList("rawdata",vals))
        print(termIntList("deltas",self.deltas(vals)))
        return vals

    def deltas(self,vals) :
        dels = []
        for i in range(len(vals)/2) :
            dels.append(vals[2*i]-vals[2*i+1])
        return dels
        
def on_exit() :
    print(termInt('turbidity',999))
    exit(0)

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
            
def labelImage(self, img, color) :
    colors = {0:"blue", 1:"green", 2:"red" }
    cv2.putText(img,colors[color],(10,80),cv2.FONT_HERSHEY_PLAIN,4.0,(240,80,200),2)

if __name__ == "__main__" :
    plog("openCV('" + str(cv2.__version__) + "').")
    with suppress_stdout_stderr() :
        cv2.namedWindow("camera", cv2.CV_WINDOW_AUTOSIZE)
        if cv2.__dict__['moveWindow'] != None :
            cv2.moveWindow("camera", 100, 0)
    params = None
    configFile = socket.gethostname() + ".settings"
    if os.path.isfile(configFile) :
        params = eval(open(configFile, 'r').read())
    if (params == None) :
        print("requires a <hostname>.settings configuration file")
        exit(0)
    usbcam = cv2.VideoCapture(params['camera'])
    (rval, img) = usbcam.read()
    if (not rval) :
        plog("camera read failed")
        exit(0)
    secchi = Secchi(usbcam, params)
    if (params['rotate']) :
        img = secchi.rotateImage(img, params['rotate'])
    if ('gen' in sys.argv) :   # Python .settings file updated
        exportImage(img)
        exit(0)
    time.sleep(0.1)
    (rval,img) = usbcam.read()
    if (not rval):
        plog("VideoCapture test returned "+str(rval))
        exit(4)
    print(termIntList("turbidity",secchi.turbidity(img)))
    usbcam.release()
    cv2.destroyAllWindows()
    

