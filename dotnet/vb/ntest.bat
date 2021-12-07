echo off

cd %1
nmake -f makefile.win
.\%1.exe %2 %3 %4
pause
del .\%1.exe
cd ..



