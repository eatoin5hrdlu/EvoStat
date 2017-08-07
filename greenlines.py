#!/usr/bin/python -u
#!C:/cygwin/Python27/python -u
from __future__ import print_function
import sys, os, time, socket
# To get rid of spurious messages from openCV library
from suppress_stdout_stderr import suppress_stdout_stderr
import numpy as np
import cv2
import cv2.cv as cv

debug = True
######## Data Structures
# BB (bounding box) is (y1, x1, y2, x2)
( blue, green, red, maxIntensity ) = ( 0, 1, 2, 255.0 )
colors = ["blue", "green", "red"]

def paintColor(img,color) :
    if (len(img.shape) == 3) :
        tricolor = [180,180,180]
        tricolor[color] = 250
        return (tuple(tricolor))
    else :
        return color

# Contrast = (#Iterations, Multiplier, (negative) Offset)
# E.g. (3, 1.35, -70)

# Line recognition is tuned by contrast (<>.settings) and this kernal
kernal = np.ones((2,2),np.uint8)
bigkernal = np.ones((4,4),np.uint8)

############# UTILITIES

def plog(str) :  
    if (False): # make this True for debug output
        print("      --"+str, file=sys.stderr)

def termIntList(f,l) :
    if (l == None ):
        plog("Empty argument list: " + f)
        return f+"."
    return f+"("+", ".join([str(i) for i in l])+")."

def termPairList(f,l) :
    if (l == None ):
        plog("Empty argument list: " + f)
        return f+"."
    return f+"("+", ".join([str(i[1])+":"+str(i[0]) for i in l])+")."

def icheck(image, who) :
    if (image == None) :
        plog("Image equal to None in " + who)
        exit(4)
    if (len(image.shape) == 2) :
        (h,w) = image.shape
    else :
        (h,w,d) = image.shape
    if (h == 0 or w == 0) :
        plog(who + ": Image shape is degenerate: " + str(img.shape))
        exit(4)

def settings() :
    for root in sys.argv:  # Argument specifies .setting file
        file = root + ".settings"
        if os.path.isfile(file) :
            return(eval(open(file,'r').read()))
    file = socket.gethostname() + ".settings"
    if os.path.isfile(file) :
        return(eval(open(file, 'r').read()))
    plog("Expected  <hostname>.settings file")
    exit(0)

def showUser(img, label=(".",0,0),multiplier=1) :
    if (params['debugpause'] > 10) :
        (text, y, x) = label
        cv2.putText(img,text, (y,x),
                    cv2.FONT_HERSHEY_SIMPLEX, 0.5,(255,0,255),1)
        cv2.imshow("camera", img)
        if cv.WaitKey(multiplier*params['debugpause']) == 27:
            exit(0)

def showdb(img, delay=500) :
    if (debug) :
        cv2.imshow("camera", img)
        if cv.WaitKey(delay) == 27:
            exit(0)

def exportImage(image) :
    if (image != None ) :
        (cy1,cx1,cy2,cx2) = params['cellstatRegion']
        cv2.rectangle(image,(cx1,cy1),(cx2,cy2),(0,200,200),2)
        (y1,x1,y2,x2) = params['lagoonRegion']
        cv2.rectangle(image,(x1,y1),(x2,y2),(250,250,0),2)
        for i in range(4):
            ((lx1,ly1),(lx2,ly2)) = lagoon_level_bbox_thirds(i)
            cv2.rectangle(image,(lx1,ly1),(lx2,ly2),
                          (100, 255-i*60, i*60), 2)
        name = "./web/phagestat.jpg"
        cv2.imwrite(name,cv2.resize(image,params['imageSize']))
    
def release() :
    global cam
    if (cam != None) :
        cam.release()
    cv2.destroyAllWindows()

############### IMAGE ENHANCEMENTS
def erodeDilate(img,iter=1,erode=1,dilate=1) :
    for i in range(iter):
        img = cv2.erode(img,kernal,iterations=erode)
        img = cv2.dilate(img,kernal,iterations=dilate)
    return img

def erodeDilatek(img,k,iter=1,erode=1,dilate=1) :
    for i in range(iter):
        img = cv2.erode(img,k,iterations=erode)
        img = cv2.dilate(img,k,iterations=dilate)
    return img

def dilateErode(img,iter=1, dilate=1, erode=1) :
    kernal = np.ones((2,2),np.uint8)
    for i in range(iter):
        img = cv2.dilate(img,kernal,iterations=dilate)
        img = cv2.erode(img,kernal,iterations=erode)
    return img

def contrast(image, thresh, iter=1, scale=2.0, offset=-80) :
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
    showdb(image)
    (ret,img) = cv2.threshold(image, thresh, 255, cv2.THRESH_BINARY)
    showdb(img)
    if (not ret) :
        plog( "Thresholding failed?")
        return None
    if (img is None) :
        plog( "img is None after binary threshold in contrast")
    return img

def contrastOnly(image, thresh, iter=1, scale=2.0, offset=-80) :
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
    showdb(image)
    (ret,img) = cv2.threshold(image, thresh, 255, cv2.THRESH_BINARY)
    if (not ret) :
        plog( "Thresholding failed?")
        return None
    if (img is None) :
        plog( "img is None after binary threshold in contrast")
    return img


def amplify(num, c=2, fraction=0.7) :
    """Return accumulated monochrome image minus a fraction of
    the sum of the other colors."""
    img = grab()
    showdb(img,3000)
    mono = img[:,:,c]
    mono = cv2.subtract(mono,
                        cv2.multiply(cv2.add( img[:,:,(c+1)%3],
                                          img[:,:,(c+2)%3]),fraction))
    for i in range(num) :
        img = grab()
        mono2 = img[:,:,c]
        mono2 = cv2.subtract(mono2,
                    cv2.multiply(cv2.add( img[:,:,(c+1)%3],
                                          img[:,:,(c+2)%3]),fraction))
        mono = cv2.add(mono,mono2)
    showdb(mono,3000)
    return mono

def rotateImage(img, angle=90):
    """+-90 deg are fast and do not crop"""
    if (angle == 0) :
        return img
    if (angle == 90) :
        return(cv2.flip(cv2.transpose(img),flipCode=0))
    elif (angle == -90) :
        return(cv2.flip(cv2.transpose(img),flipCode=1))
    else :
        center = (img.shape[1]/2.0,img.shape[0]/2.0)
        rotate = cv2.getRotationMatrix2D(center, angle, 1.0)
        return cv2.warpAffine(img, rotate,
                              (img.shape[1],img.shape[0]))

def showLines(img, y, color) :
    if (len(img.shape) == 3):
        (w,h,d) = img.shape
    else :
        (w,h) = img.shape
    for i in range(len(y)):
        cv2.line(img,(w/3,y[i]),(30+w/3,y[i]), color, 2)
        cv2.putText(img,str(y[i]),(10+w/2,y[i]),
                cv2.FONT_HERSHEY_SIMPLEX, 0.6, color,2)
        if (i>0) :
            delta = abs(y[i-1]-y[i])
            if (delta < 30):
                cv2.putText(img,str(delta),(50+w/2,y[i]-delta/2),
                            cv2.FONT_HERSHEY_SIMPLEX, 0.5, color,2)
                
    return(img)

def hlines(img, minlen=12) :
    """Return horizontal line levels"""
    global positions
    positions = []
    icheck(img,"hlines")
    (h,w) = img.shape
    retLines = []
    edges = cv2.Canny(img, 90, 130)
    showdb(edges,400)
    if (edges == None) :
        plog("level: Bad Canny output. Not calling HoughLines")
        return -1
    alllines = cv2.HoughLinesP(edges,rho = 2,theta = np.pi/2.0,threshold = 4,minLineLength = minlen, maxLineGap = 2)
    if (alllines == None) :
        return retLines
    for lines in alllines:
        for l in lines : # Find min Y line (not on the edge)
            if (l[1] == l[3]) : # Horizontal, not near top or bottom
                if ( l[1] < h-5 and l[1] > 5 and l[1] < h-5) :
                    retLines.append(l[1])
                    positions.append(tuple([l[1], (l[0]+l[2])/2]))
    return sorted(retLines)

def getNLevels(c, quan) :
    global referenceImage
    """Level value is a y position in the region"""
    lvls = []
    while(not len(lvls) == quan ):
        mono = amplify(2,c)
        showdb(mono)
        (ret,cimg) = cv2.threshold(mono, 220, 255, cv2.THRESH_BINARY)
        showdb(cimg)
        cimg = cv2.erode(cimg,np.ones((2,2),np.uint8),1)
        showdb(cimg)
        cimg = cv2.dilate(cimg,np.ones((2,8),np.uint8),4)
        hls = hlines(cimg,minlen=8)
        lvls = condense(hls)
        if (quan == 2):  # Hack Blue for now
            quan = len(lvls)
    return lvls

def getOneLevel(color, thresh, con, quan) :
    """Level value is a y position in the region"""
    lvls = []
    (it, sc, off) = con
    while(not len(lvls) == quan ):
        mono = amplify(3, color, fraction=0.5)
        showdb(mono)
        mono = contrast(mono, thresh, iter=it, scale=sc, offset=off)
        showdb(mono)
        mono = cv2.dilate(mono,np.ones((2,8),np.uint8),2)
        showdb(mono)
        rawlevels = hlines(mono,minlen=6)
        lvls = filter_levels(rawlevels)
    return(lvls)

def grab():
    global cam, params
    for ifile in sys.argv :
        if (ifile.endswith('.jpg')) :
            return cv2.imread(ifile) # Ignore rotation parameter
    (rval,img) = cam.read()
    if (rval):
        plog(str(params['rotate']))
        showUser(img)
        if (params['rotate']) :
            return rotateImage(img, params['rotate'])
        else :
            return img
    return None

def lagoon_level_bbox_xy(lnum) :
    global params
    (y1,x1,y2,x2) = params['lagoonRegion']
    w4 = int((x2-x1)/4)
    w8 = w4/2
    w16 = w8/2
    left = x1 + w16 + 8
    right = x1 + w16 + w8 - 8
    return ((left+(lnum*w4), y1+8),(right+(lnum*w4),y2-8))

def condense(lst) :
    lst.sort()
    extraneous = []
    for i in range(len(lst)-2):
        if abs(lst[i]-lst[i+2]) < 30 :
            extraneous.append(lst[i+1])
    return [num for num in lst if num not in extraneous]

def filter_levels(lst) :
    lst.sort()
    if len(lst) > 1 :
        return [ lst[0], lst[-1] ]
    extraneous = []
    for i in range(len(lst)-2):
        if abs(lst[i]-lst[i+2]) < 50 :
            extraneous.append(lst[i+1])
    return [num for num in lst if num not in extraneous]

def lagoon_level_bbox_thirds(lnum) :
    global params
    (y1,x1,y2,x2) = params['lagoonRegion']
    w3 = int((x2-x1)/3)
    w6 = w3/1.5
    w12 = w6/3
    left = x1 + w12 + 8
    right = x1 + w12 + w6 - 8
    return ( ( int(left+(lnum*w3)), int(y1+8) ),
             ( int(right+(lnum*w3)), int(y2-8) ) )

def get_positions(ylist) :
    global positions
    plist = []
    for y in ylist:
        for p in positions:
            if (y == p[0]):
                plist.append(p)
    return plist

def camSettle(n) :
    global cam
    """Initial frames can be split or underexposed"""
    for i in range(n) :
        cam.read()


        
def delta(reticules, levels) :
    for l in levels:
        for i,j in zip(reticules[0::2],reticules[1::2]) :
            avg = (i+j)/2
            if abs(avg-l) < 30 :
                print(str(l-avg))

if __name__ == "__main__" :
    global referenceImage
    global positions
    params = settings()
    if ('show' in sys.argv) :
        params['debugpause'] = 1000
    cam = cv2.VideoCapture(params['camera'])
    camSettle(3)
    referenceImage = grab()
    if (True) :
        with suppress_stdout_stderr() :
            cv2.namedWindow("camera", cv2.CV_WINDOW_AUTOSIZE)
            if cv2.__dict__['moveWindow'] != None :
                cv2.moveWindow("camera", 100, 100)
            else :
                plog("Update OpenCV (can't find moveWindow)")
    i1 = grab()
    showdb(i1)
    reticules = getNLevels(red, 4)
    print(termIntList('reticule', reticules))
    showLines(referenceImage,reticules,paintColor(referenceImage,red))
    vlevels = getOneLevel(green, 120, (1, 1.5, -70), 2)
    print(termPairList('levels', get_positions(vlevels)))
    showLines(referenceImage,vlevels,paintColor(referenceImage,green))
    name = "./web/phagestat.jpg"
    (he,wi,de) = referenceImage.shape
    cv2.putText(referenceImage,time.asctime(time.localtime()),(wi/10,he/2),
                cv2.FONT_HERSHEY_SIMPLEX, 1, (255,210,180),2)
    cv2.imwrite(name,cv2.resize(referenceImage,params['imageSize']))
    release()
    delta(reticules, vlevels)

