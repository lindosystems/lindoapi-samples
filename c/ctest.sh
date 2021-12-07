#!/bin/sh

# set PLATFORM
source ../../include/platform.inc

# Optionally, update environment variables
#source ../../bin/$PLATFORM/lindoapivars.sh

############################################################################  
if [ "$LINDOAPI_HOME" = "" ]; then
	echo
	echo LINDOAPI_HOME variable should point to your LINDO API path. 
	echo You can do one of the following
	echo "1. Modify your startup script (e.g. ~/.bashrc) to source it from"
	echo "   <local_path_lindoapi>/bin/$PLATFORM/lindoapivars.sh"
	echo "2. Type 'export LINDOAPI_HOME=<path_to_lindoapi>' at command shell."
	echo Quitting...
	exit 0
fi

if [ ! -d $LINDOAPI_HOME/bin ]; then
	echo
	echo LINDOAPI_HOME=$LINDOAPI_HOME does not appear to be a valid installation. 
	echo Quitting...
	exit 0
fi

###
# Optionally, use $LD_LIBRARY_PATH to load libraries
#
#LD_LIBRARY_PATH="$LINDOAPI_HOME/bin/$PLATFORM:$LD_LIBRARY_PATH"
#export LD_LIBRARY_PATH
#

MAKE=make
if [ `uname` = "SunOS" ]; then
	MAKE=gmake
fi

if [ "$1" = "" ]; then
	echo
        echo usage: ctest.sh [ex_path]
        echo
elif [ "$1" = "clean" ]; then
	EXLIST=`ls $EXDIR | grep -e ex_`	
	for i in $EXLIST
	do
		echo cleaning up
	        rm $i/*.obj $i/*.o $i/*.exe
	done
else
	cd $1
	$MAKE -f makefile.unx PLATFORM=$PLATFORM COMMON_FLAGS=$COMMON_FLAGS
	./$1 $2 $3 
	$MAKE -f makefile.unx clean
	cd ..
fi
