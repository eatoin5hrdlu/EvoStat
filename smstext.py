#!/usr/bin/python -u
#!C:/cygwin/Python27/python -u
import smtplib, os, sys, time
from email.mime.image      import MIMEImage
from email.mime.multipart  import MIMEMultipart
from email.mime.text       import MIMEText
COMMASPACE = ', '

secrets = eval(open('secrets.py').read())
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
mess = os.popen('tail -9 /home/peter/src/EvoStat/evostat.report').read()
             
if (len(sys.argv) < 2) :
    print 'smstext [atvs] NNNNNNNNNN "message"';
    print 'e.g. for [v]erizon use:   smstext v 9194525098'
#    exit()
else :
    car = sys.argv[1]
    num = sys.argv[2]

# SMTP Port 25?
server = smtplib.SMTP( "smtp.gmail.com", 587 )
server.starttls()
server.login( secrets['login'], secrets['password'] )

# Create the container (outer) email message.
if (car == 'vp' or car == 'a') :
    msg = MIMEMultipart()   #   MIMEText(mess, 'plain')
    msg.attach(MIMEImage(open("phagestat1.png", 'rb').read()))
    msg.attach(MIMEText(mess,'plain'))
else :
    msg = MIMEText(mess,'plain')
    
msg['Subject'] = 'PhageStatus'
msg['From'] = 'phagestat@gmail.com'
msg['To'] = num + carriers[car]

# Careful, this debug print prints the whole uuencoded picture
# print "server.sendmail( 'phagestat@gmail.com', "+num+carriers[car]+", "+msg.as_string()+" )"

server.sendmail('phagestat@gmail.com', num+carriers[car], msg.as_string())
server.quit()

