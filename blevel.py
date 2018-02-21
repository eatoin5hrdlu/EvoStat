#!/usr/bin/python -u
#!C:/cygwin/Python27/python -u

# minlen for green horizontal lines changed from 6 to 10 to avoid reflection
# minlen for reticules is still 8
#
# See condense_positions
# lines must be further apart than two detected lines from each horiz feature
#
from __future__ import print_function
import sys, os, time, socket, glob, subprocess
# To get rid of spurious messages from openCV library
from suppress_stdout_stderr import suppress_stdout_stderr
import numpy as np
import cv2
import cv2.cv as cv
from shutil import copyfile

debug = False
image_count = 0
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

# Contrast = (#Iterations, Multiplier, (negative) Offset)
# E.g. (3, 1.35, -70)

# Line recognition is tuned by contrast (<>.settings) and this kernal
kernal = np.ones((2,2),np.uint8)
bigkernal = np.ones((4,4),np.uint8)

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

def get_previous() :
    plist = None
    prevfile = './.maskspec'
    plog("Using :" + prevfile)
    try :
        plist = eval(open(prevfile, 'r').read())
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
    global imageName
    if (image != None ) :
        (cy1,cx1,cy2,cx2) = params['cellstatRegion']
        cv2.rectangle(image,(cx1,cy1),(cx2,cy2),(0,200,200),2)
        (y1,x1,y2,x2) = params['lagoonRegion']
        cv2.rectangle(image,(x1,y1),(x2,y2),(250,250,0),2)
        for i in range(4):
            ((lx1,ly1),(lx2,ly2)) = lagoon_level_bbox_thirds(i)
            cv2.rectangle(image,(lx1,ly1),(lx2,ly2),
                          (100, 255-i*60, i*60), 2)
        cv2.imwrite("./web/tmp.jpg", cv2.resize(image,params['imageSize']))
        os.rename("./web/tmp.jpg", imageName)
        
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
        showdb(image,4000)
        image = cv2.add(cv2.multiply(image,scale),offset)
        showdb(image,4000)
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

# Returns list of (x,y) (centroids of lines)
def hlines(img, minlen=12) :
    """Return horizontal line levels"""
    positions = []
    icheck(img,"hlines")
    (h,w) = img.shape
    edges = cv2.Canny(img, 90, 130)
    showdb(edges,2000)
    if (edges == None) :
        plog("level: Bad Canny output. Not calling HoughLines")
        return []
    alllines = cv2.HoughLinesP(edges,rho = 2,theta = np.pi/2.0,threshold = 4,minLineLength = minlen, maxLineGap = 2)
    if (alllines == None) :
        return positions
    for lines in alllines:
        for l in lines : # Find min Y line (not on the edge)
            if (l[1] == l[3]) : # Horizontal, not near top or bottom
                if ( l[1] < h-3 and l[1] > 3 and l[1] < h-3) :
                    positions.append(tuple( [ (l[0]+l[2])/2, l[1] ] ) )
                    print("length is "+ str(l[2]-l[0]))
    positions.sort(key= lambda l: l[1])
    return positions

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
    retLines.sort(key=lambda l: l[1])  # Sort on Y value (top to bottom)
    positions.sort(key=lambda l: l[1])  # Sort on Y value (top to bottom)
    return retLines

#  rp =  ('reticule', 0, 1.4, -80, 164, 1, 4, 3, 0.4)
def prepareImage(c, rp) :
    (name, cit, scale, offset, thresh, eit, dit, amp, frac) = rp
    mono = amplify(amp, c, fraction=frac)
    (ret,cimg) = cv2.threshold(mono, thresh, 255, cv2.THRESH_BINARY)
    return cimg
#    cimg = cv2.erode(cimg,np.ones((2,2),np.uint8),eit)
#    cimg = cv2.dilate(cimg,np.ones((2,8),np.uint8),dit)

def getPairs(cimg, minlen) :
    global referenceImage
    positions = hlines(cimg,minlen)
    showdb(cimg)
    if ( len(positions) > 1 ) :
        positions = condense_positions(positions)
    return positions

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

def condense_positions(positions) :
    newpositions = [positions[0]]
    for i in range(len(positions)-1):
        if abs(positions[i][1]-positions[i+1][1]) > 5 :
            newpositions.append(positions[i+1])
    return newpositions

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

def nearest_y(reticule, y, hlines) :
    hlines.sort(key=lambda l: abs(l[1]-y))
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

def save_plist(plist) :
    f = open('./.newmask','w')
    f.write('# (Vessel, oldPt, BBox,')
    f.write('#     Contrast-iter, Contrast-mul, Contrast-off,')
    f.write('#     Erode-iter, Dilate-iter, Amplify-iter, Amp-factor )')
    f.write('#e.g. (host0, (x,y), ((uy,lx),(ly,rx)), 1, 1.2, -80, 2, 2, 3, 0.7)')
    f.write(str(plist))
    f.close()
    g = open('./web/conlog.txt','a')
    g.write( time.asctime(time.localtime()) )
    g.write(str(plist))
    g.close()

def showBBox(params, color) :
    ( (yul,xul), (ylr,xlr) ) = params[2]
    cv2.rectangle(referenceImage,(xul,ylr),(xlr,yul),color,2)

def showSpots(ptlist, bbox, color) :
    for (x,y) in ptlist :
        ( (uly,ulx), (lry, lrx) ) = bbox
        tx = ulx + x
        ty = uly + y
        cv2.rectangle(referenceImage,(tx-4,ty-1),(tx+4,ty+1),color,2)
    
def showVesselLevel(who, y) :
    color = paintColor(referenceImage,red)
    if (who[:-1] == 'host') :
        color = paintColor(referenceImage,blue)
    elif (who[:-1] == 'lagoon') :
        color = paintColor(referenceImage,green)
    else :
        print("unrecognized vessel " + who, file=sys.stderr)
    showLevels(referenceImage,[y],color,dir=-1)

def total(image) :
    return np.average( tuple(ord(i) for i in image.tostring()) )

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

def processRegion(image, bbox, minlen, function) :
    cropped = crop(image, bbox)
    showdb(cropped,10000)
    features = function(cropped, minlen)
#   Translate to original coordinates (bbox pts are ( (y1,x1),(y2,x2) )
    return [ ( pt[0]+bbox[0][1], pt[1]+bbox[0][0] ) for pt in features ]

def boxat(img, x, y) :
    cv2.rectangle(pimg,(x-8,y-8),(x+8,y+8),255,1)
    cv2.putText(pimg,str(y),(x+20,y), cv2.FONT_HERSHEY_SIMPLEX, 0.5, 255,1)
    
if __name__ == "__main__" :
    global referenceImage
    global positions
    debug = 'show' in sys.argv
    get_camera()
    plist = get_previous()
    params = settings()
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
    pimg = prepareImage(green, plist[0])
    for (name, bbox, minlen) in plist[1:] :
        features = processRegion(pimg, bbox, minlen, getPairs)
        cv2.rectangle(pimg,(bbox[0][1],bbox[0][0]),(bbox[1][1],bbox[1][0]),255,1)
        for (x,y) in features :
            boxat(pimg,x,y)
        showdb(pimg,delay=10000)
        print(name + str(features))

