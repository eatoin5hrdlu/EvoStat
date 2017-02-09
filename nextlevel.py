#!C:/cygwin/Python27/python -u
#!/usr/bin/python -u
from __future__ import print_function
import sys, os, time, socket
from suppress_stdout_stderr import suppress_stdout_stderr
import numpy as np
import cv2
import cv2.cv as cv

######## Data Structures
# BB (bounding box) is (y1, x1, y2, x2)
( blue, green, red, maxIntensity ) = ( 0, 1, 2, 255.0 )
# Contrast = (#Iterations, Multiplier, (negative) Offset)
# E.g. (3, 1.35, -70)

# Line recognition is tuned by contrast (<>.settings) and this kernal
# For low-light conditions, try calling addImages() in main
kernal = np.ones((2,2),np.uint8)

############# UTILITIES

def plog(str) :  
    if (False): # make this True for debug output
        print("      --"+str, file=sys.stderr)

def termIntList(f,l) :
    if (l == None ):
        plog("Empty argument list: " + f)
        return f+"."
    return f+"("+", ".join([str(i) for i in l])+")."

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

def exportImage(image) :
    if (image != None ) :
        (cy1,cx1,cy2,cx2) = params['cellstatRegion']
        cv2.rectangle(image,(cx1,cy1),(cx2,cy2),(0,200,200),2)
        (y1,x1,y2,x2) = params['lagoonRegion']
        cv2.rectangle(image,(x1,y1),(x2,y2),(250,250,0),2)
        for i in range(4):
            ((lx1,ly1),(lx2,ly2)) = lagoon_level_bbox_xy(i)
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

def dilateErode(img,iter=1, dilate=1, erode=1) :
    kernal = np.ones((2,2),np.uint8)
    for i in range(iter):
        img = cv2.dilate(img,kernal,iterations=dilate)
        img = cv2.erode(img,kernal,iterations=erode)
    return img

def contrast(image, iter=1, scale=2.0, offset=-80) :
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
    (ret,img) = cv2.threshold(image, 127, 255, cv2.THRESH_BINARY)
    showUser(img,label= ("cdone",img.shape[0]/2,img.shape[1]/2) )
    img = dilateErode(img, 3, 2, 1)
    if (not ret) :
        plog( "Thresholding failed?")
        return None
    if (img is None) :
        plog( "img is None after binary threshold in contrast")
    showUser(img)
    return img

def contrast2(image, iter=1, scale=2.0, offset=-80) :
    for i in range(iter) :
        image = cv2.add(cv2.multiply(image,scale),offset)
    (ret,img) = cv2.threshold(image, 127, 255, cv2.THRESH_BINARY)
    return(erodeDilate(img, 1, 1, 1))
    
def emphasis(img, color=1, scale=2, fraction=0.5) :
    """Return monochrome image with selected color scaled
    minus sum of the fractions of the other colors.
    Where color is Blue (0), default Green (1), or Red (2)"""
    plog("Color="+str(color))
    return cv2.subtract(
        cv2.multiply(img[:,:,color],scale),
        cv2.multiply(cv2.add( img[:,:,(color+1)%3],
                              img[:,:,(color+2)%3]),fraction))

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

def showCoord(img,pt,color,size) :
    cv2.putText(img,str(pt), pt,
                cv2.FONT_HERSHEY_PLAIN,size,color,size)

def showLevel(img, bb, lvl, color) :  # Return distance from top
    height = bb[2]-bb[0]
    w2 = bb[3]-bb[1]/2
    (px,py) = ( bb[3]-w2, bb[0]+lvl )
    pc = 100-100*lvl/height
    cv2.line(img,(px,py),(px+w2,py), color, 2)
    cv2.putText(img,"  "+str(pc)+"%",(px-10,py-8),
                cv2.FONT_HERSHEY_SIMPLEX, 0.75, color,2)
    return(pc)

def croppedImage(brect):
    image = grab()
    return(cropImage(image,brect))

def cropImage(image, brect):
    icheck(image,"cropImage")
    (y1,x1,y2,x2) = brect
    cimg = image[y1:y2,x1:x2,:]
    plog(str(cimg.shape))
    showUser(cimg)
    return cv2.copyMakeBorder(cimg, 2,2,2,2, cv2.BORDER_CONSTANT,(0,0,0))

def level(img) :
    """Return uppermost horizontal line (liquid level)
              -1 for problems with the image
              1000 if no line is within range"""
    icheck(img,"level")
    (h,w) = img.shape
    edges = cv2.Canny(img, 90, 100)
    if (edges == None) :
        plog("level: Bad Canny output. Not calling HoughLines")
        return -1
    alllines = cv2.HoughLinesP(edges, 2, np.pi/2.0, 1, 8, 4)
    if (alllines == None) :
        plog("No horizontal lines found in image")
        return -1
    topline = 1000 + len(alllines)
    plog( "ALLINES " + str(alllines))
    for lines in alllines:
        plog( "LINES " + str(lines))
        for l in lines : # Find min Y line (not on the edge)
            plog( "LINE " + str(l))
            if (l[1] == l[3]) : # Horizontal?
                if ( l[1] < topline and l[1] > 5 and l[1] < h-5) :
                    plog( "\nHighest so far: " + str(l))
                    topline = l[1]
                else :
                    plog( " NOT RIGHT " + str(l) + " H = " + str(h))
    return topline

def getLevel(image, bb, color, con) :
    """Level value is a y position in the region"""
    frame = cropImage(image, bb)
    mono = frame[:,:,color]
    showUser(mono)
    plog("contrast tuple = " + str(con))
    (it, sc, off) = con
    lvl = level(contrast(mono,it,sc,off))
    if (lvl == -1):
        return -1
    plog("getLevel() "+str(lvl))
    if (lvl == None or lvl > bb[2] ) :
        plog("getLevel failed")
    if (lvl > 0 and lvl < bb[2]) : # Level in range
        return lvl
    plog(str(lvl) + " out of range :" + str(bb))
    return 0

def addImages(img, num) :
    new = cv2.add(grab(), img)
    for i in range(num-1):
        new = cv2.add(new, grab())
    return new

def grab():
    global cam, params
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

if __name__ == "__main__" :
    plog("openCV('" + str(cv2.__version__) + "').")
    params = settings()
    cam = cv2.VideoCapture(params['camera'])
    for i in range(30) : # Discard first (split) frame and
        cam.read()       # let exposure settings settle
    if (params['debugpause'] > 10) :
        with suppress_stdout_stderr() :
            cv2.namedWindow("camera", cv2.CV_WINDOW_AUTOSIZE)
            if cv2.__dict__['moveWindow'] != None :
                cv2.moveWindow("camera", 100, 0)
            else :
                plog("Update OpenCV (can't find moveWindow)")
    img = grab()
    img2 = img           # img2 = addImages(img,4) # add frames
    plog("alllevels: " + str(img2.shape))
    brect = params['cellstatRegion']
    lev = getLevel(img, brect, blue, params['cellstatContrast'])
    plog("CBB " + str(brect) + " level " + str(lev))
    llist = [ showLevel(img, brect, lev, (255,255,0)) ]
    for i in range(params['numLagoons']) :
        ( (lx1,ly1), (lx2,ly2) ) = lagoon_level_bbox_xy(i)
        lbb = (ly1,lx1,ly2,lx2)
        llev = getLevel( img2,
                         lbb,
                         green,
                         params['lagoonContrast'])
        plog("LBB " + str(lbb) + " level " + str(llev))
        llist.append(showLevel(img, lbb, llev, (128,128,255)))
    (wi,he,de) = img.shape
    cv2.putText(img,time.asctime(time.localtime()),(wi/16,3*he/4),
                cv2.FONT_HERSHEY_SIMPLEX, 1, (255,255,255),2)
    exportImage(img)
    print(termIntList('levels',llist))
    release()

