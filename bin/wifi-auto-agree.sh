#!/bin/sh
curl -si http://cnn.com |
awk '/^Location:/ { print $2 }' |
sed '1!d' |
awk -F'switch_url=|&' '
$2 !~ /^https/ { print "echo good to go" ; exit }
{ print "curl -X POST " $2 " -d buttonClicked=4 -d Submit=Agree | grep You.can.now" }
' |
sh
