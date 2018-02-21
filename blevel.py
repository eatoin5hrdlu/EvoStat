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

# Returns list of (x,y) (centroids of lines)
def horizontal_lines(img, minlen=12) :
    """Return horizontal line levels"""
    positions = []
    icheck(img,"horizontal_lines")
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
                    print("length is "+ str(l[2]-l[0]) + " y = ", l[1])
    positions.sort(key= lambda l: l[1])
    return positions

#  rp =  ('reticule', 0, 1.4, -80, 164, 1, 4, 3, 0.4)
def prepareImage(c, rp) :
    (name, cit, scale, offset, thresh, eit, dit, amp, frac) = rp
    mono = amplify(amp, c, fraction=frac)
    (ret,cimg) = cv2.threshold(mono, thresh, 255, cv2.THRESH_BINARY)
    return cimg
#    cimg = cv2.erode(cimg,np.ones((2,2),np.uint8),eit)
#    cimg = cv2.dilate(cimg,np.ones((2,8),np.uint8),dit)

def getPairs(cimg, minlen, minspace) :
    global referenceImage
    positions = horizontal_lines(cimg,minlen)
    showdb(cimg)
    if ( len(positions) > 1 ) :
        positions = condense_positions(positions, minspace)
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

def condense_positions(positions, minspace=5) :
    newpositions = [positions[0]]
    for i in range(len(positions)-1):
        delt = abs(positions[i][1]-positions[i+1][1])
        if delt > minspace :
            newpositions.append(positions[i+1])
        if delt < minspace + 2 :
            print(str(positions[i+1][1])+" barely qualified at delta="+str(delt))
    return newpositions

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

def processRegion(image, bbox, minlen, minspace, function) :
    cropped = crop(image, bbox)
    showdb(cropped,10000)
    features = function(cropped, minlen, minspace)
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
    for (name, bbox, minlen, minspace) in plist[1:] :
        features = processRegion(pimg, bbox, minlen, minspace, getPairs)
        cv2.rectangle(pimg,(bbox[0][1],bbox[0][0]),(bbox[1][1],bbox[1][0]),255,1)
        for (x,y) in features :
            boxat(pimg,x,y)
        showdb(pimg,delay=10000)
        print(name + str(features))

