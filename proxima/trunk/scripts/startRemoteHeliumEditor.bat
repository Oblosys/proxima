ssh martijn@pooh.zoo.cs.uu.nl killall -v -g HeliumEditor
ssh martijn@pooh.zoo.cs.uu.nl killall -v -g startHeliumEditor
ssh martijn@pooh.zoo.cs.uu.nl "nohup ./proxima/proxima/scripts/startHeliumEditor < /dev/null > heliumEditorOut.txt 2>&1 &"
pause