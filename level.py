#!/usr/bin/python -u
#!C:/cygwin/Python27/python -u
#
# Automation of this process now includes:
# To identify the correct affine transformation to eliminate rotational distortion,
# we applying a range of small +- rotations and find longest horizontal lines
# This rotation can be different for each of the vessels, but will not change unless
# something is moved.
# While this will not happen often during a normal experimental run, it
# It is not hard to detect this human interference, as the light level inside
# the EvoStat changes dramatically.  The level detection algorithm will simply idle
# when the light level indicates an open EvoStat. Once the images are dark, the affine
# calibration will be performed once.
# 
#
#
#
#
#
#
#

from __future__ import print_function
import  sys, os, time, socket, glob, subprocess, getopt
# To get rid of spurious messages from openCV library
from suppress_stdout_stderr import suppress_stdout_stderr
import numpy as np
import cv2
import cv2.cv as cv
from shutil import copyfile

referenceImage = None
image_count = 0

# There is a bit of dense code ( nested list comprehensions )
# The output of the Hough algorithm is a list of lists of bboxs
# They are degenerate bboxes in the sense that they represent vertical
# and horizontal lines so either b[0]==b[2] (vertical) or b[1]==b[3] (horizontal)
#
# I have loops to extract:
#        ( length, x position) for each vertical line
#  and   (height(y), length, x position(centroid)) for each horizontal line

def comps( all ) :
    """ all is output of HoughLinesP() """
    for v in [( l[1]-l[3], l[0] ) for ls in all for l in ls if l[0]==l[2] ] :
        print("vertical "+str(v))
    for h in [(l[1], (l[0]+l[2])/2, l[2]-l[0] ) for ls in all for l in ls if l[1]==l[3] ] :
        print("horizontal "+str(h))

# External parameter in .maskspec file  (example)
#[('reticule', 0,     1.4,  -80,  180,    0,   0,   3,  0.4),
# ('host0',   ((110, 340), (170, 410)), 5, 6 ),
# ('lagoon1', ((340, 110), (410,  210)), 7,  4 )]
#
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

############# UTILITIES

def plog(str) :  
    if (debug): # make this True for debug output
        print("      --"+str, file=sys.stderr)

def icheck(image, who) :
    if (image is None) :
        plog("Image equal to None in " + who)
        exit(13)
    if (len(image.shape) == 2) :
        (h,w) = image.shape
        plog("2D image: " + str(image.shape))
    else :
        (h,w,d) = image.shape
        plog("3D image: " + str(image.shape))
    if (h == 0 or w == 0) :
        plog(who + ": Image shape is degenerate: " + str(image.shape))
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

def newReferenceImage() :
    global referenceImage
    referenceImage = None
    tries = 10
    while (referenceImage is None and tries > 0) :
        referenceImage = grab()
        tries = tries - 1
        if (referenceImage is None) :
            time.sleep(0.2)
        else :
            cv2.imwrite("./web/sample.jpg",referenceImage)
    if tries == 0 :
        print("Failed to get image from camera")
        exit(10)
    
def showdb(img, delay=200) :
    if (debug) :
        cv2.imshow("camera", img)
        if cv.WaitKey(delay) == 27:
            exit(0)

def make_movie() :
    for format in ['.mp4','.avi'] :
        out = os.path.abspath('~/src/EvoStat/web/timelapse'+format)
        cmd=['ffmpeg','-y','-framerate','2','-pattern_type','glob','-i','*.jpg','-vcodec','mpeg4',out]
        print(cmd)
        subprocess.call(cmd,cwd=frameLocation)
        print("made "+format)
#       with suppress_stdout_stderr() :

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
        if ((seq%10 == 0) or ('-m' in sys.argv)) :
            print("make movie "+str(seq))
            make_movie()
            print("made movie(s)")
        next_file =  last_file[:-10]+"{0:0>5}".format(seq)+"m."+type
    copyfile(name, next_file)

def release() :
    global cam
    if (cam != None) :
        cam.release()
    cv2.destroyAllWindows()

############# UTILITIES

############# IMAGE PROCESSING

def amplify(num, c=2, fraction=0.7) :
    """Return accumulated monochrome image minus a fraction of
    the sum of the other colors."""
    img = grab()
    mono = img[:,:,c]
    mono = cv2.subtract(mono,
                        cv2.multiply(cv2.add( img[:,:,(c+1)%3],
                                          img[:,:,(c+2)%3]),fraction))
    for i in range(num) :
        plog("amplify: iteration "+str(i))
        img = grab()
        mono2 = img[:,:,c]
        mono2 = cv2.subtract(mono2,
                    cv2.multiply(cv2.add( img[:,:,(c+1)%3],
                                          img[:,:,(c+2)%3]),fraction))
        mono = cv2.add(mono,mono2)
    return mono


def rotateImage(img, angle=90):
    """+-90 deg are fast and do not crop"""
    if (angle == -90) :
        return(cv2.flip(cv2.transpose(img),flipCode=0))
    elif (angle == 90) :
        return(cv2.flip(cv2.transpose(img),flipCode=1))
    else :
        (h,w) = img.shape[:2]
        print("Rotating by "+ str(angle) + "shape: "+ str(img.shape))
        center = (h/2, w/2)
        rot_matrix = cv2.getRotationMatrix2D(center, angle, scale=1.0)
        rimg = cv2.warpAffine(img, rot_matrix, (w,h) )
        print("shape= "+ str(rimg.shape))
        return rimg

# COUNTING HORIZONTAL BLOBS
# Examine the gaps between the horizontal lines
# to decide how many groups there are.
# Each group should consist of the artifacts associated with one visible line.

# Calculate the distance between line fragments to figure out which ones
# are artifacts of a bigger, visible blob. The distances between blobs
# should be somewhat consistent, so we're going to assume that once we've
# identified the maximum gap between groups, any gap less than 66% (2/3)
# of this, could be intra-group noise.

def delta_list(list, threshold) :
    if (len(list) < 2) :
        return len(list)  # No data = 0 lines, one item = 1
    print(list)
    delta = []
    for i in range(1,len(list)) :  # More than one line, group by gaps
        delta.append(list[i][0]-list[i-1][0])
    delta.sort(reverse=True)   # Largest gaps first
    print(delta)
    lines = 1
    for n in delta :
        if n > threshold :
            lines = lines + 1
        else :
            return lines
    return lines

def visible( all, threshold) :
    """ Takes output of HoughLinesP and counts major horizontal lines  """
    return delta_list(sorted([(l[1],(l[0]+l[2])/2,l[2]-l[0]) for ls in all for l in ls if l[1]==l[3]]), threshold)

def level_count(img, delta_threshold=7, minlen=12) :
    """Level Information: Canny(i,lowthresh,highthresh,kernel)"""
    icheck(img,"level_count")
    (h,w) = img.shape
    edges = cv2.Canny(img, 100, 180, apertureSize=5)
    showdb(edges,delay=2000)
    if (edges is None) :
        return 0
    alllines = cv2.HoughLinesP(edges,rho = 1,theta = np.pi/2.0,threshold = 4,minLineLength = minlen, maxLineGap = 4)
    if (alllines is None) :
        return 0
    return(visible(alllines,delta_threshold))


# MAIN ALGORITHM :
# Find longest vertical line, x value is center
# divide horizontal lines into left or right of center
# take max y (lowest) from left and min y (highest) from right.
#
# Returns (state = { 0, 1, 2 }, vlength)
def level_data(img, minlen=12) :
    """Level Information: Canny(i,lowthresh,highthresh,kernel)"""
    icheck(img,"level_data")
    (h,w) = img.shape
    edges = cv2.Canny(img, 100, 180, apertureSize=5)
    plog("EDGES")
    showdb(edges,delay=2000)
    if (edges is None) :
        return (0, 0)
    alllines = cv2.HoughLinesP(edges,rho = 1,theta = np.pi/2.0,threshold = 4,minLineLength = minlen, maxLineGap = 4)
    if (alllines is None) :
        return (0,0)
    if (debug) :
        comps(alllines)
    maxvert = 0
    numbervert = 0
    centerx = -1
    for (vlen,x) in [( l[1]-l[3], l[0] ) for ls in alllines for l in ls if l[0]==l[2] ] :
        numbervert = numbervert + 1
        if (vlen > maxvert) :
            centerx = x
            maxvert = vlen
    if (numbervert > 0) :
        plog("longest vertical is "+str(vlen))
    numberhoriz = 0
    maxy = -1
    miny = 1000
    leftofcenter = None
    rightofcenter = None
    if (numbervert == 0) : # Check for one horizontal line
        for (y,hcen,hlen) in [(l[1],(l[0]+l[2])/2, l[2]-l[0] ) for ls in alllines for l in ls if l[1]==l[3] ] :
            numberhoriz = numberhoriz + 1
            if ( y > maxy ) :
                maxy = y
                leftofcenter = ( hcen, y )
    else :  # We saw the vertical, so their might be two horizontals
        for (y,hcen,hlen) in [(l[1],(l[0]+l[2])/2, l[2]-l[0] ) for ls in alllines for l in ls if l[1]==l[3] ] :
            numberhoriz = numberhoriz + 1
            if ( (hcen < centerx) and y > maxy ) :
                maxy = y
                leftofcenter = ( hcen, y )
            if ( (hcen > (centerx-2) ) and y < miny ) :
                miny = y
                rightofcenter = ( hcen, y )
# 0:"under",  1 : "full",  2:"over",  3: "error3", 4:"error4"
    state = 0
    if (leftofcenter is not None) :
        state = 1
    if ((numbervert > 0) and (rightofcenter is not None)) :
        state = 2
    return (state, maxvert)

#  rp =  ('reticule', 0, 1.4, -80, 164, 1, 4, 3, 0.4)
def prepareImage(c, rp) :
    (name, cit, scale, offset, thresh, eit, dit, amp, frac) = rp
    mono = amplify(amp, c, fraction=frac)
    (ret,cimg) = cv2.threshold(mono, thresh, 255, cv2.THRESH_BINARY)
    cimg = cv2.dilate(cimg,np.ones((2,8),np.uint8),dit)
    showdb(cimg,1000)
    cimg = cv2.erode(cimg,np.ones((3,1),np.uint8),eit)
    showdb(cimg,1000)
    return cimg

def grab():
    global cam, params, image_count
    rval = None
    for ifile in sys.argv :
        if (ifile.endswith('.jpg')) :
            return cv2.imread(ifile)
#            return rotateImage( cv2.imread(ifile), 4.0)
    tries = 5
    while (rval is None and tries > 0) :
        (rval,img) = cam.read()
        image_count = image_count + 1
        tries = tries - 1
    if (tries == 0) :
        exit(12)
    if (rval):
        if (params['rotate']) :
            return rotateImage(img, params['rotate'])
        else :
            return img
    return None

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
    global referenceImage
    (he,wi,de) = referenceImage.shape
    cv2.putText(referenceImage,time.asctime(time.localtime()),(wi/16,he/2),
                cv2.FONT_HERSHEY_SIMPLEX, 1.8, (255,210,200),4)
    cv2.putText(referenceImage,lasttemp(),(wi/11,4*he/7),
                cv2.FONT_HERSHEY_SIMPLEX, 1.5, (250,210,255),4)

    tmpimg = cv2.resize(referenceImage,params['imageSize'])
    if not os.path.exists(frameLocation) :
        os.mkdir(frameLocation)
    plog("before showdb")
    showdb(tmpimg, delay=10000)
    plog("after showdb")
    cv2.imwrite("./web/tmp.jpg", cv2.resize(referenceImage,params['imageSize']))
    os.rename("./web/tmp.jpg", imageName)
    movie_file(imageName)

def nocam() :
    for ifile in sys.argv :
        if (ifile.endswith('.jpg')) :
            return True
    return False

def get_camera() :
    global cam
    cam = None
    if (nocam()) :
        plog("no camera")
        return
    plog("nocam returned False")
    for n in [1, 0, 2] :
        camera = '/dev/video'+str(n)
        if os.path.exists(camera) :
            cam = cv2.VideoCapture(n)
            break
    if cam == None :
        print("No camera? Check for one of /dev/video[0,1,2]")
        exit(0)
    plog("cam not None")
    camprofile = socket.gethostname()
    plog(str(cam.get(cv2.cv.CV_CAP_PROP_FRAME_WIDTH)))
    plog(str(cam.get(cv2.cv.CV_CAP_PROP_FRAME_HEIGHT)))
    cam.set(cv2.cv.CV_CAP_PROP_FRAME_WIDTH,1920)
    cam.set(cv2.cv.CV_CAP_PROP_FRAME_HEIGHT,1080)
    plog(str(cam.get(cv2.cv.CV_CAP_PROP_FRAME_WIDTH)))
    plog(str(cam.get(cv2.cv.CV_CAP_PROP_FRAME_HEIGHT)))
    camprofile += "w"
    if 'growth' in sys.argv :
        camprofile += "g"
    for opt in sys.argv :
        if opt.startswith('od') :
            camprofile = 'normal'
    cmdstr =  "/usr/bin/uvcdynctrl -L "+ camprofile + ".gpfl --device="+camera
    print(cmdstr)
    plog(cmdstr)
    with suppress_stdout_stderr() :
        os.system(cmdstr)
#        os.system("./camreset quickcam")
#        time.sleep(1)
    camSettle(3)   # save_frames('sample',10) to create sample set

def lasttemp() :
    if not os.path.exists('./lasttemps.sh') :
        return "No temperature information"
    cmd = ['/bin/bash', './lasttemps.sh']
    p = subprocess.Popen(cmd, stdout=subprocess.PIPE)
    (result, err) = p.communicate('')
    result.replace('88.8 C','No Data')
    return result

def crop(img, bbox) :
    ((y1,x1),(y2,x2)) = bbox
    return img[y1:y2,x1:x2]

# Apply function() to subimage transform output to original coordinates

def processRegion(image, bbox, mlen, delta_thresh, name) :
    plog("processRegion " + str(bbox) +  "shape: "+ str(image.shape))
    cropped = crop(image, bbox)
    return level_count(cropped, delta_threshold=delta_thresh, minlen=mlen)

def boxat(pimg, x, y) :
    cv2.rectangle(pimg,(x-8,y-8),(x+8,y+8),255,2)
    cv2.putText(pimg,str(y),(x+20,y), cv2.FONT_HERSHEY_SIMPLEX, 0.5, 255,1)

def colorboxat(img, x, y, minspace) :
    cv2.rectangle(img,(x-8,y-minspace),(x+8,y+minspace),(100,180,180),1)
    cv2.putText(img,str(y),(x+20,y), cv2.FONT_HERSHEY_SIMPLEX, 0.5, (100,200,200),1)
    
laststate =  {'host0' : 'under', 'lagoon1' : 'under' }
lastchange = {'host0' : 0,       'lagoon1' : 0 }

# Overall light level tells us if the EvoStat is open or closed(dark)
def evostat_light() :
    return np.mean(cv2.cvtColor(referenceImage, cv2.COLOR_BGR2HLS)[2])

# Monitor prints out new level readings only
# when they differ from the previous reading
def monitor() :
    global referenceImage
    global plist
    pimg = prepareImage(green, plist[0])
    plog(str(referenceImage.shape))
    plog("monitor")
    plog(str(plist[1:]))
    for (name, bbox, minlen, deltath, minspace) in plist[1:] :
        line_count = processRegion(pimg, bbox, minlen, deltath, name)
        cv2.rectangle(referenceImage,(bbox[0][1],bbox[0][0]),(bbox[1][1],bbox[1][0]),(255,200,255),2)
        cv2.rectangle(referenceImage,
                      (bbox[0][1]+minlen, bbox[0][0]-minlen/2),
                      (bbox[0][1]+2*minlen,bbox[0][0]-minlen/2),(255,255,0),3)
        now  = int(time.time())
        elapsed = now - lastchange[name]
        lastchange[name] = now
        print(name+"("+str(line_count)+","+str(elapsed)+").")
    imageOut()

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
    optlist, args = getopt.gnu_getopt(sys.argv, 'c:m')
    for o, a in optlist :
        if (o == '-c'):
            try :
                cycletime = float(a)
            except:
                print("N must be a number in cycletime option -cN")
    get_camera()
    check_maskspec()     # Loads plist
    params = settings()  # Optional image rotation and movie frame size from old settings
    if (debug) :
        with suppress_stdout_stderr() :
            cv2.namedWindow("camera", cv2.CV_WINDOW_AUTOSIZE)
            if cv2.__dict__['moveWindow'] is not None :
                cv2.moveWindow("camera", 100, 100)
            else :
                plog("Update OpenCV (can't find moveWindow)")
    now = int(time.time())
    lastchange = {'host0' : now, 'lagoon1' : now }
    time.sleep(3)
    for opt in sys.argv:
        if opt.startswith('od') :
            cv2.imwrite('./web/' + opt + '.jpg', grab())
            exit(0)
    if 'snap' in sys.argv :
        one = grab()
        cv2.imwrite("./temp.jpg", cv2.resize(one,params['imageSize']))
        exit(0)
    lastvlen = 0
    while(1) :
        newReferenceImage()           # Take a picture
        if ( evostat_light() > 127 ): # EvoStat is open, don't bother image processing
            plog("  TOO MUCH LIGHT....sleeping")
            time.sleep(cycletime)     # and no frames produced for timelapse movie (okay?)
            continue
        check_maskspec()              # Reload plist if .maskspec changed
        monitor()                     # Image Processing
        time.sleep(cycletime)

# OLD
# cellstatRegion(130,190,360,220),
#       lagoonRegion(600,200,638,300),
# NEW
# (name,    cit, scale, offset, thresh, eit, dit, amp, frac) = rp
#[('reticule', 0,     1.4,  -80,  180,    0,   0,   3,  0.4),
#    for (name, bbox, minlen, delta_threshold, minspace) in plist[1:] :
# ('host0',   ((110, 340), (170, 410)), 16, 6, 6 ),
# ('lagoon1', ((340, 110), (410,  210)),16, 8, 4 )]
#!/usr/bin/python -u


    
