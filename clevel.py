#!/usr/bin/python -u
#!C:/cygwin/Python27/python -u

from __future__ import print_function
import sys, os, time, socket, glob, subprocess, getopt
# To get rid of spurious messages from openCV library
from suppress_stdout_stderr import suppress_stdout_stderr
import numpy as np
import cv2
import cv2.cv as cv
from shutil import copyfile

image_count = 0

import os, time
# Example: .maskspec
#[('reticule', 0,     1.4,  -80,  180,    0,   0,   3,  0.4),
# ('host0',   ((110, 340), (170, 410)), 5, 6 ),
# ('lagoon1', ((340, 110), (410,  210)), 7,  4 )]

def check_maskspec() :
    global prevmoddate
    global plist
    mfile = './.maskspec'
    moddate = os.stat(mfile)[8]
    if moddate != prevmoddate :
        prevmoddate = moddate
        try :
            plist = eval(open(mfile, 'r').read())
        except(IOError):
            print("No .maskspec file defined")
        

######## Critical Parameters
# Contrast (iterations, mul, subtract) ( 1,  1.28,  -80 )
# Threshold   (50%)                      127
# Symmetrical (2x2) erode iterations     3
# Asymmetrical (2x4) dilation iterations 2
defaultParams = [ (1,2),      # Contrast Iterations
                  (1.2,1.5),  # Contrast Scale
                  (-80.01,-60.01),  # Contrast Offset
                  (120.01,144.01),  # Contrast Threshold
                  (1,3),      # Erode Iterations
                  (1,2)]      # Dilate Iterations


######## Data Structures
# BB (bounding box) is ( (uly, ulx), (lry, lrx) )
# Levels are usually just y values
# Coordinates are frequently (y,x)
# Upper left corner is y = 0 (larger y going down)
# Points must be (x,y) for openCV drawing functions

( blue, green, red, maxIntensity ) = ( 0, 1, 2, 255.0 )
colors = ["blue", "green", "red"]
imageName = "./web/phagestat.jpg"
frameLocation = "/tmp/timelapse/"
    
def paintColor(img,color) :
    if (len(img.shape) == 3) :
        tricolor = [150,150,150]
        tricolor[color] = 250
        return (tuple(tricolor))
    else :
        return color

############# UTILITIES

def plog(str) :  
    if (debug): # make this True for debug output
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
    return f+"("+", ".join([str(i[0])+":"+str(i[1]) for i in l])+")."

def termTripleList(f,l) :
    if (l == None ):
        plog("Empty argument list: " + f)
        return f+"."
    return f+"("+", ".join([str(i[0])+":"+str(i[1])+":"+str(i[2]) for i in l])+")."

def termNumberList(f,l) :
    outstr = ""
    for i in range(len(l)) :
        outstr = outstr + "{0}({1},{2}).\n".format(f, i, l[i][2])
    return outstr


def icheck(image, who) :
    if (image == None) :
        plog("Image equal to None in " + who)
        exit(13)
    if (len(image.shape) == 2) :
        (h,w) = image.shape
    else :
        (h,w,d) = image.shape
    if (h == 0 or w == 0) :
        plog(who + ": Image shape is degenerate: " + str(img.shape))
        exit(14)

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

def showdb(img, delay=500) :
    if (debug) :
        cv2.imshow("camera", img)
        if cv.WaitKey(delay) == 27:
            exit(0)

def make_movie() :
    out = '/home/peter/src/EvoStat/web/timelapse.avi'
    cmd=['ffmpeg','-y','-framerate','2','-i','%05dm.jpg','-codec','copy',out]
    with suppress_stdout_stderr() :
        subprocess.call(cmd,cwd=frameLocation)

def save_frames(name, num) :
    for i in range(num) :
        img = grab()
        imgname = name + str(i) + ".jpg"
        cv2.imwrite(imgname, cv2.resize(img,params['imageSize']))
    
def movie_file(name) :
    numstart = len(frameLocation)
    type = 'jpg'
    next_file = frameLocation + '00000m.jpg'
    numend = numstart + 5
    imfiles = glob.glob(frameLocation+'*m.'+type)
    if (len(imfiles)>0) :
        last_file = max(imfiles, key=os.path.getctime)
        seq = int(last_file[numstart:numend]) + 1
        if (seq%10 == 0) :
            make_movie()
        next_file =  last_file[:-10]+"{0:0>5}".format(seq)+"m."+type
    copyfile(name, next_file)

def release() :
    global cam
    if (cam != None) :
        cam.release()
    cv2.destroyAllWindows()

def amplify(num, c=2, fraction=0.7) :
    """Return accumulated monochrome image minus a fraction of
    the sum of the other colors."""
    img = grab()
    mono = img[:,:,c]
    showdb(mono)
    mono = cv2.subtract(mono,
                        cv2.multiply(cv2.add( img[:,:,(c+1)%3],
                                          img[:,:,(c+2)%3]),fraction))
    showdb(mono)
    for i in range(num) :
        plog("amplify: iteration "+str(i))
        img = grab()
        mono2 = img[:,:,c]
        showdb(mono2)
        mono2 = cv2.subtract(mono2,
                    cv2.multiply(cv2.add( img[:,:,(c+1)%3],
                                          img[:,:,(c+2)%3]),fraction))
        showdb(mono2)
        mono = cv2.add(mono,mono2)
        showdb(mono)
    showdb(mono,1000)
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

def showLevels(img, y, color, dir=1, width=2) :
    if (len(img.shape) == 3):
        (w,h,d) = img.shape
    else :
        (w,h) = img.shape
    if (dir == 1):
        off = 30
    else :
        off = -60
    for i in range(len(y)):
        cv2.line(img,(w/3,y[i]),(30+w/3,y[i]), color, width)
        cv2.putText(img,str(y[i]),(off+w/3,y[i]),
                cv2.FONT_HERSHEY_SIMPLEX, 0.6, color,2)
        if (i>0) :
            delta = abs(y[i-1]-y[i])
            if (delta < 30):
                cv2.putText(img,str(delta),(50+w/2,y[i]-delta/2),
                            cv2.FONT_HERSHEY_SIMPLEX, 0.5, color,2)
                
    return(img)

# Find longest vertical line, x value is center
# divide horizontal lines into left or right of center
# take max y from left and min y from right.
#
# Returns ({ 0, 1, 2 }, vlength)
def level_data(img, minlen=12) :
    """Level Information"""
    global report
    leftofcenter = []
    rightofcenter = []
    positions = []
    report = ""
    icheck(img,"level_data")
    (h,w) = img.shape
    edges = cv2.Canny(img, 90, 130)
    showdb(edges,2000)
    if (edges == None) :
        plog("level: Bad Canny output. Not calling HoughLines")
        return (0, 0)
    alllines = cv2.HoughLinesP(edges,rho = 2,theta = np.pi/2.0,threshold = 4,minLineLength = minlen, maxLineGap = 2)
    if (alllines == None) :
        return (0,0)
    maxvert = 0
    maxvertpos = 0
    numbervert = 0
    centerx = -1
    state = 0
    for lines in alllines:
        for l in lines :
            if (l[0] == l[2]) : # Vertical line
                numbervert = numbervert + 1
                vlen = l[1]-l[3]
                if (vlen > maxvert) :
                    centerx = l[0]
                    maxvert = vlen
                    maxvertpos = l[0]
    report = report + "num vert = "+str(numbervert)+" centerx="+str(centerx)+" max vert="+str(maxvert)
    numberhoriz = 0
    maxy = -1
    miny = 1000
    if (numbervert == 0) : # Check for one horizontal line
        for lines in alllines:
            for l in lines : # Find max Y line (not on the edge)
                if (l[1] == l[3]) : # Horizontal
                    hcen = (l[0]+l[2])/2
                    numberhoriz = numberhoriz + 1
                if ( l[1]>maxy ) :
                    maxy = l[1]
                    leftofcenter = ( hcen, l[1] )
                    state = 1
                    report = report + " hl"+str(hcen)
        return (state, 0)
    leftofcenter = None
    rightofcenter = None
    for lines in alllines:
        for l in lines : # Find min Y line (not on the edge)
            if (l[1] == l[3]) : # Horizontal, not near top or bottom
                numberhoriz = numberhoriz + 1
                if ( l[1] < h-3 and l[1] > 3 and l[1] < h-3) :
                    hcen = (l[0]+l[2])/2
                    if ( (hcen < centerx) and l[1]>maxy ) :
                        maxy = l[1]
                        leftofcenter = ( hcen, l[1] )
                        report = report + "left " + str(hcen)
                    if ( (hcen > centerx) and l[1]<miny ) :
                        miny = l[1]
                        rightofcenter = ( hcen, l[1] )
                        report = report + "right " + str(hcen)
    if (leftofcenter is not None) :
        state = state + 1
    if (rightofcenter is not None) :
        state = state + 1
    report = report + str((state, maxvert))
    return (state, maxvert)

#  rp =  ('reticule', 0, 1.4, -80, 164, 1, 4, 3, 0.4)
def prepareImage(c, rp) :
    (name, cit, scale, offset, thresh, eit, dit, amp, frac) = rp
    mono = amplify(amp, c, fraction=frac)
    (ret,cimg) = cv2.threshold(mono, thresh, 255, cv2.THRESH_BINARY)
    showdb(cimg)
#    cimg = cv2.dilate(cimg,np.ones((2,8),np.uint8),1)
    cimg = cv2.erode(cimg,np.ones((3,1),np.uint8),3)
    showdb(cimg)
    return cimg

def grab():
    global cam, params, image_count
    rval = None
    for ifile in sys.argv :
        if (ifile.endswith('.jpg')) :
            return cv2.imread(ifile) # Ignore rotation parameter
    tries = 5
    while (rval is None and tries > 0) :
        (rval,img) = cam.read()
        image_count = image_count + 1
        tries = tries - 1
    if (tries == 0) :
        exit(12)
    if (rval):
        showdb(img)
        if (params['rotate']) :
            return rotateImage(img, params['rotate'])
        else :
            return img
    return None

def condense_positions(positions, minspace=5) :
    newpositions = [positions[0]]
    for i in range(len(positions)-1):
        delt = abs(positions[i][1]-positions[i+1][1])
        if delt > minspace :
            newpositions.append(positions[i+1])
    return newpositions

#        if delt < minspace + 2 :
#            print(str(positions[i+1][1])+" barely qualified at delta="+str(delt))

def camSettle(n) :
    global cam
    """Initial frames can be split or underexposed"""
    for i in range(n) :
        cam.read()

def imageOut():
    """Adds a blue-grey timestamp to the current referenceImage
       saves it to the imageName (/tmp/timelapse/NNNNNm.jpg) file
       and periodically (every 10 frames), makes a movie"""
    global imageName
    (he,wi,de) = referenceImage.shape
    cv2.putText(referenceImage,time.asctime(time.localtime()),(wi/10,he/2),
                cv2.FONT_HERSHEY_SIMPLEX, 1, (255,210,200),2)
    cv2.putText(referenceImage,lasttemp(),(wi/10,4*he/7),
                cv2.FONT_HERSHEY_SIMPLEX, 0.8, (250,210,255),2)
    cv2.imwrite("./web/tmp.jpg", cv2.resize(referenceImage,params['imageSize']))
    os.rename("./web/tmp.jpg", imageName)
    movie_file(imageName)

def get_camera() :
    global cam
    cam = None
    for n in [0, 2, 1] :
        camera = '/dev/video'+str(n)
        if os.path.exists(camera) :
            cam = cv2.VideoCapture(n)
            break
    if cam == None :
        print("No camera? Check for one of /dev/video[0,1,2]")
        exit(0)
    camprofile = socket.gethostname()
    if 'growth' in sys.argv :
        camprofile += "g"
    cmdstr =  "/usr/bin/uvcdynctrl -L "+ camprofile + ".gpfl --device="+camera
    plog(cmdstr)
    with suppress_stdout_stderr() :
        os.system(cmdstr)
#        os.system("./camreset quickcam")
#        time.sleep(1)
    camSettle(3)   # save_frames('sample',10) to create sample set

def lasttemp() :
    cmd = ['/bin/bash', './lasttemps.sh']
    p = subprocess.Popen(cmd, stdout=subprocess.PIPE)
    (result, err) = p.communicate('')
    result.replace('88.8 C','No Data')
    return result

def crop(img, bbox) :
    ((y1,x1),(y2,x2)) = bbox
    return img[y1:y2,x1:x2]

# Apply function() to subimage transform output to original coordinates

def processRegion(image, bbox, minlen, name) :
    cropped = crop(image, bbox)
    return level_data(cropped, minlen)

def boxat(pimg, x, y) :
    cv2.rectangle(pimg,(x-8,y-8),(x+8,y+8),255,1)
    cv2.putText(pimg,str(y),(x+20,y), cv2.FONT_HERSHEY_SIMPLEX, 0.5, 255,1)

def colorboxat(img, x, y, minspace) :
    cv2.rectangle(img,(x-8,y-minspace),(x+8,y+minspace),(100,180,180),1)
    cv2.putText(img,str(y),(x+20,y), cv2.FONT_HERSHEY_SIMPLEX, 0.5, (100,200,200),1)
    
laststate =  {'host0' : 'under', 'lagoon1' : 'under' }
lastchange = {'host0' : 0,       'lagoon1' : 0 }

def monitor() :
    global referenceImage
    global report
    global plist
    global lastvlen
    states = { 0:"under",  1 : "full", 2:"over", 3: "error3", 4:"error4"}
    pimg = prepareImage(green, plist[0])
    for (name, bbox, minlen, minspace) in plist[1:] :
        (newstate, vlen) = processRegion(pimg, bbox, minlen, name)
        cv2.rectangle(referenceImage,(bbox[0][1],bbox[0][0]),(bbox[1][1],bbox[1][0]),(200,200,0),1)
        cv2.rectangle(referenceImage,bbox[0][::-1],bbox[1][::-1],(200,200,0),1)
        now  = int(time.time())
        if (((abs(vlen-lastvlen)>2) and newstate < 2) or (laststate[name] != newstate)) :
            lastvlen = vlen
            laststate[name] = newstate
            elapsed = now - lastchange[name]
            lastchange[name] = now
            ts = str(elapsed)
            print(report)
            print(name+"("+states[newstate]+"," +str(vlen)+").")
    imageOut()

def newReferenceImage() :
    global referenceImage
    referenceImage = None
    tries = 10
    while (referenceImage == None and tries > 0) :
        referenceImage = grab()
        tries = tries - 1
        if (referenceImage == None) :
            time.sleep(0.2)
    if tries == 0 :
        print("Failed to get image from camera")
        exit(10)
    
if __name__ == "__main__" :
    global debug
    global plist
    global prevmoddate
    global lastvlen
    prevmoddate = 0
    debug = False
    debug = 'show' in sys.argv
    global cycletime
    cycletime = 90  # Default
    optlist, args = getopt.gnu_getopt(sys.argv, 'c:')
    for o, a in optlist :
        if (o == '-c'):
            try :
                cycletime = float(a)
            except:
                print("Argument to -c [cycletime] must be a number")
    get_camera()
    check_maskspec()  # Loads plist
    params = settings()
    if (debug) :
        with suppress_stdout_stderr() :
            cv2.namedWindow("camera", cv2.CV_WINDOW_AUTOSIZE)
            if cv2.__dict__['moveWindow'] != None :
                cv2.moveWindow("camera", 100, 100)
            else :
                plog("Update OpenCV (can't find moveWindow)")
    now = int(time.time())
    lastchange = {'host0' : now, 'lagoon1' : now }
    time.sleep(3)
    lastvlen = 0
    while(1) :
        newReferenceImage()
        check_maskspec()    # Reloads plist if .maskspec changed
        monitor()
        time.sleep(cycletime)
        
