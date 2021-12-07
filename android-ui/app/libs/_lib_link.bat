rem Run this script to create links to the Android binaries. 
rem
rem This script requires junction.exe utility.
rem 
rem If you don't have junction installed, you can download it at the following link
rem https://docs.microsoft.com/en-us/sysinternals/downloads/junction
rem
rem Alternatively, manually copy these folders into this directory
rem 
JUNCTION     arm64-v8a 		..\..\..\..\bin\android\arm64-v8a
JUNCTION     armeabi 		..\..\..\..\bin\android\armeabi
JUNCTION     armeabi-v7a 	..\..\..\..\bin\android\armeabi-v7a
JUNCTION     mips 			..\..\..\..\bin\android\mips
JUNCTION     mips64 		..\..\..\..\bin\android\mips64
JUNCTION     x86 			..\..\..\..\bin\android\x86
JUNCTION     x86_64 		..\..\..\..\bin\android\x86_64
