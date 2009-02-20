ssh martijn@pooh.zoo.cs.uu.nl killall -v -g DazzleEditor
ssh martijn@pooh.zoo.cs.uu.nl killall -v -g startDazzleEditor
ssh martijn@pooh.zoo.cs.uu.nl "nohup ./proxima/proxima/scripts/startDazzleEditor  < /dev/null > /dev/null 2>&1 &"
pause