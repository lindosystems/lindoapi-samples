#
# This file is included from makefile.win in each sample's subfolder
#

# Get versions of libraries
include ../../../include/lsversion.sh

# Name tag for x64 binaries
!IF "$(PLATFORM)" == ""
!MESSAGE Warning: PLATFORM is not specified. Using PLATFORM=win64
!MESSAGE Warning: For 32-bit run nmake as follows
!MESSAGE Warning: nmake makefile.win PLATFORM=win32
!MESSAGE
#!ERROR
PLATFORM=win64
!ENDIF
!IF "$(PLATFORM)" == "win64"
BITS=64_
!ELSE
PLATFORM=win32
BITS=
!ENDIF 

IFLAGS = -I"..\..\..\include" 
DFLAGS = -D_LINDO_DLL_

all : $(EXAMPLE).obj $(EXAMPLE).exe
$(EXAMPLE).obj : $(EXAMPLE).c
	cl -c $(DFLAGS) $(IFLAGS) $(EXAMPLE).c

$(EXAMPLE).exe : ..\..\..\lib\$(PLATFORM)\lindo$(BITS)$(LS_MAJOR)_$(LS_MINOR).lib $(EXAMPLE).obj
	cl $(EXAMPLE).obj ..\..\..\lib\$(PLATFORM)\lindo$(BITS)$(LS_MAJOR)_$(LS_MINOR).lib -Fe$(EXAMPLE).exe 

clean:
	del /q *.obj *.exe