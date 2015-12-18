import os, time
import numpy as np
import cv2
import cv2.cv as cv

class Level(object):
    """Returns the y-value of the topmost horizontal line in a cropped image.
    The algorithm does less work finding vertical lines only, so we will
    rotate the image and then re-interpret the x coordinate as y"""

    alpha   = np.array([  1.2 ])
    beta    = np.array([ -60 ])

    def __init__(self,color=1) :
	self.color = color

    def rotateImage(self, img, angle):
        center = (img.shape[1]/2.0,img.shape[0]/2.0)
        rotate = cv2.getRotationMatrix2D(center, angle, 1.0)
        rotated = cv2.warpAffine(img, rotate, (img.shape[1], img.shape[0]))
        return rotated

    def contrast(self, image, iter=1, scale=2.0, offset=-100) :
        for i in range(iter) :
            image = cv2.add(cv2.multiply(image,scale),offset)
        (ret,img) = cv2.threshold(image, 127, 255, cv2.THRESH_BINARY)
        # ret value is threshold (127.0) not True - False
        return img


    def level(self, img) :
        if (img == None) :
            print "Level detector called with invalid image"
            return -1
        (h,w) = img.shape
        img = self.contrast(img)
        if (img==None) :
            print "Contrast returned None"
            return -1
        edges = cv2.Canny(img, 90, 100)
        if (edges == None) :
            print "Bad Canny output for HoughLinesP in level()"
            return -1
        alllines = cv2.HoughLinesP(edges, 2, 3.1415926/2.0, 1, 16, 4)
        if (alllines == None) :
            print "No horizontal lines found in image"
            return -1
        topline = 1000
        for lines in alllines:
            for l in lines : # Find the highest (minY) line (not on the edge)
                if (l[1] == l[3]) : # Horizontal?
                    if ( l[1] < topline and l[1] > 5 and l[1] < h-5) :
                        topline = l[1]
        return topline



