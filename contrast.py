#!/usr/bin/python -u
#!C:/cygwin/Python27/python.exe -u
#!C:/Python27/python -u

from __future__ import print_function
def plog(str) :
    print(str, file=sys.stderr)

from suppress_stdout_stderr import suppress_stdout_stderr
import time, sys
import numpy as np
import cv2
import cv2.cv as cv
from Tkinter import *

gc_color = 1
gc_iter = None
gc_scale = None
gc_offset = None
gc_camera = None

def show(img) :
    cv2.imshow("camera", img)
    if cv.WaitKey(2000) == 27:
        exit(0)
    
def contrast(image, iter=1, scale=2.0, offset=-80) :
    if (image == None) :
        plog("contrast called with null Image")
    for i in range(iter) :
        plog("Try contrast "+str((iter,scale,offset)))
        if (image == None) :
            plog("contrast loop: Image is None")
        else :
            show(image)
        image = cv2.add(cv2.multiply(image,scale),offset)
        if (image == None) :
            plog("image(None) after add/mulitply in contrast!")
        show(image)
        
        (ret,img) = cv2.threshold(image, 127, 255, cv2.THRESH_BINARY)
        if (ret == False) :
            plog("Thresholding failed?")
            return None
    if (img == None) :
        plog( "img is None after binary threshold in contrast")
    return(img)

def setred() :
    global gc_color
    gc_color = 2

def setblue() :
    global gc_color
    gc_color = 0

def setgreen() :
    global gc_color
    gc_color = 1

def showContrast() :
    """Check contrast algorithm using three values from TkInter console"""
    global gc_color
    global gc_scale
    global gc_iter
    global gc_offset
    (rval, frame) = gc_camera.read()
    show(frame)
    greyimage = frame[:,:,gc_color]
    s = float(gc_scale.get())/100.0
    o = gc_offset.get()
    i = gc_iter.get()
    show(greyimage)
    greyimage = contrast(greyimage,iter=i,scale=s,offset=o)
    show(greyimage)

def makeTkSliders(contrast) :
    global gc_iter
    global gc_scale
    global gc_offset
    global gc_camera
    master = Tk()
    master.minsize(250,150)
    master.title("Contrast Setting")
    iter_label = Label(master,text="Number of Iterations")
    iter_label.pack()
    gc_iter = Scale(master, from_=1, to=4, orient=HORIZONTAL)
    (iter, scale, offset) = contrast
    gc_iter.set(iter)
    gc_iter.pack()
    
    scale_label = Label(master,text="Scaling Multiplier")
    scale_label.pack()
    gc_scale = Scale(master, from_=0, to=300, orient=HORIZONTAL)
    gc_scale.set(int(100*scale))
    gc_scale.pack()
    
    offset_label = Label(master,text="Negative Offset after Scale")
    offset_label.pack()
    gc_offset = Scale(master, from_=-100, to=0, orient=HORIZONTAL)
    gc_offset.set(offset)
    gc_offset.pack()
    
    Button(master, text='Red',  command=setred).pack()
    Button(master, text='Green', command=setgreen).pack()
    Button(master, text='Blue', command=setblue).pack()
    Button(master, text='Show', command=showContrast).pack()
    Button(master, text='Quit', command=exit).pack()
    with suppress_stdout_stderr() :
        cv2.namedWindow("camera", cv2.CV_WINDOW_AUTOSIZE)
    if (gc_camera is None) :
        gc_camera = cv2.VideoCapture(0)
        plog("Opened device 0 for contrast" + str(gc_camera))
    if (gc_camera is None) :
        gc_camera = cv2.VideoCapture(1)
        plog("Opened device 1 for contrast" + str(gc_camera))
    if (gc_camera is None) :
        plog("No camera available on [0,1]")
        exit(0)
    time.sleep(0.1)
    master.mainloop()

if __name__ == "__main__" :
    makeTkSliders((3,2.0,-70))

    

