ssh martijn@pooh.zoo.cs.uu.nl killall -v -g DazzleEditor
ssh martijn@pooh.zoo.cs.uu.nl killall -v -g startDazzleEditor
ssh martijn@pooh.zoo.cs.uu.nl "nohup ./proxima/proxima/scripts/startDazzleEditor  < /dev/null > dazzleEditorOut.txt 2>&1 &"
pause