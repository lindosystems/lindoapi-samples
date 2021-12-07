#!/bin/sh
###########################################################################
#
# Tests the JNI (Java Native Interface) for LINDO API
# Copyright (c) 2002-2011, Lindo Systems Inc.
#
# $Id: jtest.sh 237 2017-06-12 21:28:12Z mka $
###########################################################################

############################################################################  
if [ "$1" = "" ]; then
	echo usage: jtest [path_to_example] 
	echo
	echo ex:   "jtest ex_lp1"
	exit 0
fi

# Read and set LS_MAJOR and LS_MINOR values
echo Reading version file '../../include/lsversion.sh'
if [ ! -f ../../include/lsversion.sh ]; then
	echo Version file not found at specified path
	echo Quitting...
	exit 0
fi
. ../../include/lsversion.sh


# Set PLATFORM
. ../../include/platform.inc

if [ "$PLATFORM" = "" ]; then
	echo
	echo PLATFORM variables needs to be set.
	echo Quitting...
	exit 0
fi

############################################################################  
if [ "$LINDOAPI_HOME" = "" ]; then
	echo
	echo LINDOAPI_HOME variable should point to your LINDO API path. 
	echo You can do one of the following
	echo "1. Modify your startup script (e.g. ~/.bashrc) to source it from"
	echo "   <local_path_lindoapi>/bin/$PLATFORM/lindoapivars.sh"
	echo "2. Type 'export LINDOAPI_HOME=<path_to_lindoapi>' at command shell."
	echo Quitting...
	#exit 0
fi

if [ ! -d $LINDOAPI_HOME/bin ]; then
	echo
	echo LINDOAPI_HOME=$LINDOAPI_HOME does not appear to be a valid installation. 
	echo Quitting...
	#exit 0
fi

###
# set binary folder
BINPATH=../../bin/"$PLATFORM"

if [ -d ../../obj ]; then
	. binpath.inc
fi




############################################################################  
JAVA_MEM="-Xms128m -Xmx512m"
#JAVA_MEM=${JAVA_MEM}" -Xcheck:jni"

if [ "$JAVA_HOME" = "" ]; then
	echo
	echo JAVA_HOME variable needs to be set.. 
	echo You can do one of the following
	echo "1. Specify 'JAVA_HOME=<path_to_java>' in your login script, or"
	echo "2. Type 'export JAVA_HOME=<path_to_java>' at command shell."
	echo Quitting...
	exit 0
fi

if [ ! -d $JAVA_HOME ]; then
	echo
	echo JAVA_HOME=$JAVA_HOME does not exist. 
	echo Quitting...
	exit 0
fi

# Set platform specific options, if any required.
if [ "$PLATFORM" = "linux32arm" ]; then
	JAVA_OPT="$JAVA_MEM -d32 -Djava.library.path=$BINPATH"
fi
if [ "$PLATFORM" = "linux32" ]; then
	JAVA_OPT="$JAVA_MEM -d32 -Djava.library.path=$BINPATH"
fi
if [ "$PLATFORM" = "linux64" ]; then	
	JAVA_OPT="$JAVA_MEM -d64 -Djava.library.path=$BINPATH"
fi
if [ "$PLATFORM" = "solaris32" ]; then
	JAVA_OPT="$JAVA_MEM -d32 -Djava.library.path=$BINPATH"
fi
if [ "$PLATFORM" = "solaris64" ]; then
	JAVA_OPT="$JAVA_MEM -d64 -Djava.library.path=$BINPATH"
fi
if [ "$PLATFORM" = "solaris32x86" ]; then
	JAVA_OPT="$JAVA_MEM -d32 -Djava.library.path=$BINPATH"
fi
if [ "$PLATFORM" = "solaris64x86" ]; then
	JAVA_OPT="$JAVA_MEM -d64 -Djava.library.path=$BINPATH"
fi
if [ "$PLATFORM" = "osx32x86" ]; then
	JAVA_OPT="$JAVA_MEM -d32 -Djava.library.path=$BINPATH"
fi
if [ "$PLATFORM" = "osx64x86" ]; then
	JAVA_OPT="$JAVA_MEM -d64 -Djava.library.path=$BINPATH"
fi



###########################################################################  
echo "Platform: $PLATFORM"  
echo "Java: $JAVA_HOME"
#cd $1

############################################################################  
echo "Compiling $1..."

"$JAVA_HOME"/bin/javac -classpath ../../lib/"$PLATFORM"/lindo${LS_MAJOR}_${LS_MINOR}.jar $1/$1.java 

############################################################################  
echo "Running..."
echo Java Options..
echo $JAVA_OPT -classpath ../../lib/"$PLATFORM"/lindo${LS_MAJOR}_${LS_MINOR}.jar:$1 $1 $2 $3 $4
"$JAVA_HOME"/bin/java  $JAVA_OPT -classpath ../../lib/"$PLATFORM"/lindo${LS_MAJOR}_${LS_MINOR}.jar:$1 $1 $2 $3 $4

############################################################################  
rm $1/*.class
#cd ..



