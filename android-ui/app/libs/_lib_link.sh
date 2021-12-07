# Run this script to create symbolic links to the Android binaries. 
#
# Alternatively, manually copy these folders into this directory
# 
function JUNCTION() {
	ln -sf $2 $1
}

JUNCTION     arm64-v8a 		../../../../bin/android/arm64-v8a
JUNCTION     armeabi 		../../../../bin/android/armeabi
JUNCTION     armeabi-v7a 	../../../../bin/android/armeabi-v7a
JUNCTION     mips 			../../../../bin/android/mips
JUNCTION     mips64 		../../../../bin/android/mips64
JUNCTION     x86 			../../../../bin/android/x86
JUNCTION     x86_64 		../../../../bin/android/x86_64
