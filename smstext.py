#!/usr/bin/python
import smtplib, os, sys, time
from email.mime.image      import MIMEImage
from email.mime.multipart  import MIMEMultipart
from email.mime.text       import MIMEText
COMMASPACE = ', '

import subprocess

# We need actual IP to send EvoStat URLs to our correspondents
# The old way, retained for quaintness
wgetip = "bash -c \"ipconfig | grep -Eo 'IPv4.*: ?([0-9]*\.){3}[0-9]*' | sed -E \\\"s/IPv4[^0-9]*(([0-9]+\.){3}[0-9]*).*/\\\\1/\\\""

machineip    = ['hostname','-I']

# The following usually necessary, and imply that we have
# port forwarding: EvoStats use ports 12846, 12847 (default), 21848...
curlip       = ['curl','http://canhazip.com']
getoutsideip = ['dig','+short','myip.opendns.com','@resolver1.opendns.com']

proc = subprocess.Popen(getoutsideip, stdout=subprocess.PIPE)
myip = proc.stdout.read().strip().split()[0]
print "[", myip, "]"

url="http://"+myip+":21847/web/pathe.pl"
print "URL["+url+"]"
secrets = eval(open('secrets.py').read())

# fake it
# secrets = { 'login':'8wan5hrdlu', 'password': '<passwd>' }

carriers = { 'a' : '@mms.att.net',
             't' : '@tmomail.net',
             'v' : '@vtext.com',
             'vt': '@vtext.com',  #text
             'vp': '@vzwpix.com',  #pictures
             'n' : '@messaging.nextel.com',
             's' : '@messaging.sprintpcs.com',
             'Alltel' : '@message.alltel.com',
'Ampd Mobile' : '@vtext.com',
'Cingular' : '@mobile.mycingular.com',
'SunCom' : '@tms.suncom.com',
'VoiceStream' : '@voicestream.net',
'US Cellulart' : '@email.uscc.net', # text
'US Cellularp' : '@mms.uscc.net', #pictures
'Cricket' : '@mms.mycricket.com',
'Virgin' : '@vmobl.com',
'Cingular' : '@cingularme.com', #not sure
'Boost Mobile' : '@myboostmobile.com',
'Einstein PCS' : '@einsteinmms.com' }

car = 'vp'
num = '9194525098'
onion = 'http://innatrixuqtlyuxk.onion/web/pathe.pl'
mess = onion + "\n" + url +"\n" + os.popen('tail -9 /home/peter/src/EvoStat/evostat.report').read()

htmlmess = """\
<html>
<a href="http://innatrixuqtlyuxk.onion/web/pathe.pl">Aristotle</a>
<pre>
""" + "\n" + mess + "</pre>\n</html>"

if (len(sys.argv) < 2) :
    print 'smstext [atvs] NNNNNNNNNN "message"';
    print 'e.g. for [v]erizon use:   smstext v 9194525098'
#    exit()
elif (len(sys.argv) == 3) :
    car = sys.argv[1]
    num = sys.argv[2]
else :
    num = sys.argv[1]

# SMTP Port 25?
server = smtplib.SMTP( "smtp.gmail.com", 587 )
server.starttls()
server.login( secrets['login'], secrets['password'] )

# Create the container (outer) email message.
if (car == 'vp' or car == 'a') :
    if (len(sys.argv) == 2) :
        msg = MIMEMultipart('alternative')
    else :
        msg = MIMEMultipart()
    msg.attach(MIMEImage(open("web/phagestat.jpg", 'rb').read()))
    if (len(sys.argv) == 2) :
        msg.attach(MIMEText(htmlmess,'html'))
    else :
        msg.attach(MIMEText(mess,'plain'))
else :
    msg = MIMEText(mess,'plain')
    
msg['Subject'] = 'PhageStatus'
msg['From'] = 'phagestat@gmail.com'
if (len(sys.argv) == 2) :
    msg['To'] = num
else :
    msg['To'] = num + carriers[car]

# Careful, this debug print prints the whole uuencoded picture
# print "server.sendmail( 'phagestat@gmail.com', "+num+carriers[car]+", "+msg.as_string()+" )"

server.sendmail('phagestat@gmail.com', num+carriers[car], msg.as_string())
server.quit()

