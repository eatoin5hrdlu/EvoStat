#!C:/cygwin/Python27/python
import smtplib, sys, time
from email.mime.image      import MIMEImage
from email.mime.multipart  import MIMEMultipart
from email.mime.text       import MIMEText
COMMASPACE = ', '

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
mess = '\n' + "temperature 38C\nturbidity(OD600) 0.38\npH 7.2\n"
             
if (len(sys.argv) < 2) :
    print 'smstext [atvs] NNNNNNNNNN "message"';
    print 'e.g. for [v]erizon use:   smstext v 9194525098 "hi peter"'
#    exit()
else :
    car = sys.argv[1]
    num = sys.argv[2]
    mess = sys.argv[3]

# SMTP Port 25?
server = smtplib.SMTP( "smtp.gmail.com", 587 )
server.starttls()
server.login( 'phagestat', '5ageass!3' )

# Create the container (outer) email message.
if (car == 'vp') :
    msg = MIMEMultipart()   #   MIMEText(mess, 'plain')
    msg.attach(MIMEImage(open("phagestat.png", 'rb').read()))
    msg.attach(MIMEText(mess,'plain'))
else :
    msg = MIMEText(mess,'plain')
    
msg['Subject'] = 'PhageStatus'
msg['From'] = 'phagestat@gmail.com'
msg['To'] = num + carriers[car]
               
# print "server.sendmail( 'phagestat@gmail.com', " + num + carriers[car] + ", " + msg.as_string() + " )"
server.sendmail('phagestat@gmail.com', num+carriers[car], msg.as_string())
server.quit()

