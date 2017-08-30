#!/usr/bin/python -u
#!C:/cygwin/Python27/python -u
from __future__ import print_function
import sys, os, time, socket, glob, subprocess
# To get rid of spurious messages from openCV library
from suppress_stdout_stderr import suppress_stdout_stderr
import numpy as np
import cv2
import cv2.cv as cv

from shutil import copyfile

debug = False
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
# BB (bounding box) is (y1, x1, y2, x2)
( blue, green, red, maxIntensity ) = ( 0, 1, 2, 255.0 )
colors = ["blue", "green", "red"]
imageName = "./web/phagestat.jpg"

frameLocation = "/tmp/timelapse"
    
def paintColor(img,color) :
    if (len(img.shape) == 3) :
        tricolor = [100,100,100]
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

def get_previous() :
    plist = None
    try :
        plist = eval(open('./.previous','r').read())
    except(IOError):
        print("No previous level information")
    if (plist == None) :
        print("creating PLIST")
        (it, sc1, of1, ei, di, ai1, af) = ( 1, 1.85, -80, 2, 2, 4, 0.7 )
        (sc2, of2, ai2) = (1.35, -80, 3)
        pt1 = (35,200)
        bb1 = ( (10,100), (100,400) )
        pt2 = (375,200)
        bb2 = ( (300,100), (400,300) )
        plist = [('host0', pt1, bb1, it, sc1, of1, ei, di, ai1, af  ),
                 ('lagoon1', pt2, bb2, it, sc2, of2, ei, di, ai2, af  )]
        save_plist(plist)
    return plist

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
        
def make_movie() :
    out = '/home/peter/src/EvoStat/web/timelapse.avi'
    cmd=['ffmpeg','-y','-framerate','2','-i','%05dm.jpg','-codec','copy',out]
    with suppress_stdout_stderr() :
        subprocess.call(cmd,cwd=frameLocation)

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


def amplify0(num, c=2, fraction=0.7) :
    """Return accumulated monochrome image minus a fraction of
    the sum of the other colors."""
    img = grab()
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
    showdb(mono,1000)
    return mono

def amplify(bbox, num, c=2, fraction=0.7) :
    """Return accumulated monochrome image minus a fraction of
    the sum of the other colors."""
    fullimg = grab()
    ( (uly,ulx), (lry, lrx) ) = bbox
    img = fullimg[uly:lry,ulx:lrx,:]
    mono = img[:,:,c]
    mono = cv2.subtract(mono,
                        cv2.multiply(cv2.add( img[:,:,(c+1)%3],
                                          img[:,:,(c+2)%3]),fraction))
    for i in range(num) :
        fullimg2 = grab()
        img = fullimg[uly:lry,ulx:lrx,:]
        mono2 = img[:,:,c]
        mono2 = cv2.subtract(mono2,
                    cv2.multiply(cv2.add( img[:,:,(c+1)%3],
                                          img[:,:,(c+2)%3]),fraction))
        mono = cv2.add(mono,mono2)
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

def showLines(img, y, color, width=2) :
    if (len(img.shape) == 3):
        (w,h,d) = img.shape
    else :
        (w,h) = img.shape
    for i in range(len(y)):
        cv2.line(img,(w/3,y[i]),(30+w/3,y[i]), color, width)
        cv2.putText(img,str(y[i]),(10+w/2,y[i]),
                cv2.FONT_HERSHEY_SIMPLEX, 0.6, color,2)
        if (i>0) :
            delta = abs(y[i-1]-y[i])
            if (delta < 30):
                cv2.putText(img,str(delta),(50+w/2,y[i]-delta/2),
                            cv2.FONT_HERSHEY_SIMPLEX, 0.5, color,2)
                
    return(img)

def showLinePositions(img, xy, color) :
    w = img.shape[0]
    h = img.shape[1]
    for (px,py) in xy:
        cv2.line(img,(px,py),(px,py), color, 2)
        cv2.putText(img,str(py),(20+px,py),
                cv2.FONT_HERSHEY_SIMPLEX, 0.6, color,2)
    return(img)

# Returns list of (x,y) (centroids of lines)
def hlines(img, minlen=12) :
    """Return horizontal line levels"""
    global positions
    positions = []
    icheck(img,"hlines")
    (h,w) = img.shape
    retLines = []
    edges = cv2.Canny(img, 90, 130)
    showdb(edges,2000)
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
                    positions.append(tuple( [ (l[0]+l[2])/2, l[1] ] ) )
    return sorted(retLines)

def horiz_lines(img, minlen=12) :
    """Return horizontal line levels"""
    global positions
    positions = []
    icheck(img,"hlines")
    (h,w) = img.shape
    retLines = []
    edges = cv2.Canny(img, 90, 130)
    showdb(edges,2000)
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
                    retLines.append(tuple([(l[0]+l[2])/2, l[1] ]))
                    positions.append(tuple( [ (l[0]+l[2])/2, l[1] ] ) )
    retLines.sort(key=lambda x: x[1])  # Sort on Y value (top to bottom)
    return retLines

def getReticules(c) :
    global referenceImage
    """Level value is a y position in the region"""
    lvls = []
    tries = 5
    bbox = ((0,referenceImage.shape[0]),(0,referenceImage.shape[1]))
    while(not len(lvls) == 4 and tries > 0):
        mono = amplify0(2,c, fraction=0.8) # default is .7
        showdb(mono)
        (ret,cimg) = cv2.threshold(mono, 220, 255, cv2.THRESH_BINARY)
        showdb(cimg)
        cimg = cv2.erode(cimg,np.ones((2,2),np.uint8),1)
        showdb(cimg)
        cimg = cv2.dilate(cimg,np.ones((2,8),np.uint8),4)
        hls = hlines(cimg,minlen=8)
        lvls = condense(hls)
        tries = tries - 1
    if tries == 0 :
        imageOut()
        print("reticule(null).")
        exit(5)
    return lvls

def getLevels(color, thresh, con, quan, reticules) :
    """Level value is a y position in the region"""
    lvls = []
    (it, sc, off) = con
    tries = 3
    while(not len(lvls) == quan and tries > 0):
        mono = amplify(1, color, fraction=0.5)
        showdb(mono)
        mono = contrast(mono, thresh, iter=it, scale=sc, offset=off)
        showdb(mono)
        mono = cv2.erode(mono,np.ones((2,2),np.uint8),3)
        mono = cv2.dilate(mono,np.ones((2,8),np.uint8),2)
        showdb(mono)
        rawlevels = horiz_lines(mono,minlen=6)
        lvls = nearest(reticules, rawlevels)
        tries = tries - 1
    if tries == 0 :
        imageOut()
        print("level_detection(null).")
        exit(4)
    return(lvls)

def grab():
    global cam, params
    rval = None
    for ifile in sys.argv :
        if (ifile.endswith('.jpg')) :
            return cv2.imread(ifile) # Ignore rotation parameter
    tries = 5
    while (rval is None and tries > 0) :
        (rval,img) = cam.read()
        tries = tries - 1
    if (tries == 0) :
        exit(12)
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

def filter_levels(lst) : # (px,py,y1,y2)
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
            if (y == p[1]):
                plist.append(p)
    return plist

def camSettle(n) :
    global cam
    """Initial frames can be split or underexposed"""
    for i in range(n) :
        cam.read()

def xydeltay(reticules, xyLevels) :
    dy = []
    for (x,y) in xyLevels:
        for i,j in zip(reticules[0::2],reticules[1::2]) :
            avg = (i+j)/2
            if abs(avg-y) < 64 : # Level must be near reticule
                dy.append(tuple([x,y,y-avg]))
    return dy

def nearest(reticules, hlines) :
    rlevels = []
    for i,j in zip(reticules[0::2],reticules[1::2]) :
        avg = (i+j)/2
        hlines.sort(key=lambda l: abs(l[1]-avg))
        levels = []
        for h in hlines :
            levels.append(tuple([h[0],h[1]]))
        rlevels.append(tuple([i,j,levels]))
    return rlevels

def nearest_reticule(reticule, hlines) :
    (i,j) = reticule
    avg = (i+j)/2
    hlines.sort(key=lambda l: abs(l[1]-avg))
    return tuple([hlines[0][1], hlines[0][0]])

def sortOutLevels(xydeltayLevels) :
    allLevels = sorted(xydeltayLevels)  # Sorted by horizontal position
    if (len(allLevels) == 0) :
        print("nolevels.")
        return
    cellstat = (None,1000,None)
    for (x,y,dy) in allLevels:     # Find least y (Host Cell Level)
        if (y < cellstat[1]) :
            cellstat = (x,y,dy)
    allLevels.remove(cellstat)  # Remove it to get Lagoon list
    print(termNumberList("host",[cellstat]))
    allLevels.insert(0, tuple([4,5,10]))
    print(termNumberList("lagoon", allLevels))

def printRLevels(rlevels) :
    if not ( len(rlevels) == 2 ) :
        imageOut()
        exit(7)
    (hy1, hy2, h) = rlevels[0]
    if (len(h) == 0) :
        imageOut()
        exit(8)
    print(termIntList("host", [h[0][0],h[0][1],hy1,hy2]))
    showLines(referenceImage,[h[0][1]],paintColor(referenceImage,blue))
    (ly1, ly2, lgs) = rlevels[1]
    lgs.sort(key=lambda x: x[0])  # Sort on X value (left to right)
    l = lgs[0]
    print(termIntList("lagoon", [l[0],l[1],ly1,ly2]))
    showLines(referenceImage,[l[1]],paintColor(referenceImage,green))
#    for l in lgs :
#        print(termIntList("lagoon", [l[0],l[1],ly1,ly2]))

def imageOut():
    global imageName
    (he,wi,de) = referenceImage.shape
    cv2.putText(referenceImage,time.asctime(time.localtime()),(wi/10,he/2),
                cv2.FONT_HERSHEY_SIMPLEX, 1, (255,210,180),2)
    cv2.imwrite(imageName,cv2.resize(referenceImage,params['imageSize']))
    movie_file(imageName)

def processRegion(image, bbox, function) :
    # Apply function() to subwindow (bbox) of image
    # to find features
    # Return feature coordinates relative to original image
    cropped = crop(img, bbox)
    features = function(cropped)
    transformed = []
    for feature in features:
        transformed.append(transform(feature, bbox, image.shape))
    return transformed

def hunt(low, high) :
    if ( isinstance(high,float) and isinstance(low,float) ):
        delta = (high-low)/2.0
        center = (high+low)/2.0
        lvals = [center]
        if not delta == 0 :
            for x in np.arange(0.0,delta,delta/5.0) :
                lvals.append(center+x)
                lvals.append(center-x)
    else :
        center = (high+low)/2
        lvals = [center]
        for x in range(low,high,1) :
            lvals.append(center+x)
            lvals.append(center-x)
    return lvals

def vary(params) :
    vparams = []
    for (p) in params:
        if isinstance(p,float) :
            low = p - p/2
            high = p + p/2
        else :
            low = p - 1
            high = p + 1
        vparams.append(hunt(low,high))
    start = [vparams[i][0] for i in range(len(vparams))]
    yield start
    for i in range(len(vparams)-1) :
        column = vparams[i+1]
        for j in range(len(column)-1) :
            yield start[0:i+1] + [column[j+1]] + start[i+2:]

def processRegion2(image, bbox, function, params, n) :
    """Apply function() to subwindow (bbox) of image to find N
    features and return them in original coordinates
    Each BBOX can have min/max values for each of:
             CONTRAST:  (iteration, scale, offset) (1, 1.28,-80)
             THRESHOLD:  thresh                       127
             ERODE    :  erode_iteration                3
             DIALATE  :  dilation_iteration             2
    After three solutions, return params of middle solution
    """
    transformed = []
    features = function(crop(image, bbox),*params)
    if (len(features) == n) :
        for feature in function(crop(image, bbox)):
            transformed.append(transform(feature, bbox, image.shape))
        return transformed
    return None

def save_plist(plist) :
    f = open('./.previous','w')
    f.write(str(plist))
    f.close()

def getALevel(color, threshold, p, reticule) :
    """Level value is a y position in the region"""
    lvl = None
    for varp in vary(p[3:]) :
        vp = tuple( [ p[0], p[1], p[2] ] + varp )
        if (debug) :
            print(" variation: " + str(vp),file=sys.stderr)
        (name, oldpt, bbox, it, sc, off, ei, di, ai, af) = vp
        mono = amplify(bbox, ai, color, fraction=af)
        showdb(mono)
        mono = contrast(mono, threshold, iter=it, scale=sc, offset=off)
        showdb(mono)
        mono = cv2.erode(mono,np.ones((2,2),np.uint8),ei)
        mono = cv2.dilate(mono,np.ones((2,8),np.uint8),di)
        showdb(mono)
        rawlevels = horiz_lines(mono,minlen=6)
        if len(rawlevels) < 1 :
            continue
        lvl = nearest_reticule(reticule, rawlevels)
        vp = tuple([name,lvl,bbox,it,sc,off,ei,di,ai,af])
        break
    imageOut()
    return(vp)

if __name__ == "__main__" :
    global referenceImage
    global positions
    with suppress_stdout_stderr() :
        os.system("./camreset quickcam")
        time.sleep(1)
        os.system("/usr/bin/uvcdynctrl -L aristotle.gpfl --device=/dev/video0")
    plist = get_previous()
    params = settings()
    debug = 'show' in sys.argv
    cam = cv2.VideoCapture(params['camera'])
    camSettle(3)
    referenceImage = None
    tries = 10
    while (referenceImage == None and tries > 0) :
        time.sleep(0.1)
        referenceImage = grab()
        tries = tries - 1
    if tries == 0 :
        exit(10)
    if (debug) :
        with suppress_stdout_stderr() :
            cv2.namedWindow("camera", cv2.CV_WINDOW_AUTOSIZE)
            if cv2.__dict__['moveWindow'] != None :
                cv2.moveWindow("camera", 100, 100)
            else :
                plog("Update OpenCV (can't find moveWindow)")
    i1 = grab()
    showdb(i1)
    reticules = getReticules(red)
    showLines(referenceImage,reticules,paintColor(referenceImage,red),width=3)
    newplist = []
    retlist = []
    different = False
    for i in range(len(plist)) :
        ret = tuple( [ reticules[2*i] , reticules[2*i+1] ] )
        n = getALevel(green, 127, plist[i], ret)
        print("level("+n[0]+","
              +str(n[2][0][0]+n[1][0])+","
              +str(ret[0])+","
              +str(ret[1])+").")
        newplist.append(n)
        for j in range(len(plist[0]))[2:] :
            if (plist[i][j] != n[j]):
                different = True
    if (different) :
        save_plist(newplist)

