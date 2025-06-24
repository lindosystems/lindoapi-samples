#
# This file is included from makefile.unx in each sample's subfolder
#

# Set APIPATH to LINDOAPI_HOME if LINDOAPI_HOME is a non-empty environment variable, otherwise fallback to ../../..
APIPATH = $(if $(LINDOAPI_HOME),$(LINDOAPI_HOME),../../..)


include $(APIPATH)/include/lsversion.sh

OBJ_EXT=o
OBJPATH=.
SRCPATH=.
LIBPATH=$(APIPATH)/bin/$(PLATFORM)

###########################################################################
# set flags
###########################################################################
ifeq ($(PLATFORM),linux32)
	LFLAGS= $(COMMON_FLAGS) -liomp5 -llindo  -lmosek -lconsub3 -lpthread -Wl,-rpath-link,$(LIBPATH) -Wl,-rpath,$(LIBPATH)
	CFLAGS= $(COMMON_FLAGS) -c -O3
	IFLAGS= -I $(APIPATH)/include 
	CCOM  = gcc -m32
	LINK  = gcc -m32
endif

ifeq ($(PLATFORM),linux64)
	LFLAGS= $(COMMON_FLAGS) -llindo64  -lmosek64 -lconsub3 -lpthread -Wl,-rpath-link,$(LIBPATH) -Wl,-rpath,$(LIBPATH) -lm 
	CFLAGS= $(COMMON_FLAGS) -c -O3 
	IFLAGS= -I $(APIPATH)/include 
	CCOM  = gcc -m64
	LINK  = gcc -m64
endif

ifeq ($(PLATFORM),linux64arm)
	LFLAGS= $(COMMON_FLAGS) -llindo64  -lmosek64 -lconsub3 -lpthread -Wl,-rpath-link,$(LIBPATH) -Wl,-rpath,$(LIBPATH) -lm 
	CFLAGS= $(COMMON_FLAGS) -c -O3 
	IFLAGS= -I $(APIPATH)/include 
	CCOM  = gcc 
	LINK  = gcc 
endif

ifeq ($(PLATFORM),linux64ia)
	LFLAGS= $(COMMON_FLAGS) -llindo64  -lmosek64 -lconsub3 -lpthread -lc -ldl -lm -Wl,-rpath-link,$(LIBPATH) -Wl,-rpath,$(LIBPATH)
	CFLAGS= $(COMMON_FLAGS) -c -O3
	IFLAGS= -I $(APIPATH)/include 
	CCOM  = gcc
	LINK  = ld
endif

ifeq ($(PLATFORM),solaris32x86)
	LFLAGS= $(COMMON_FLAGS) -llindo  -lmosek  -lconsub3 -lsocket -lnsl -lintl -lthread -lpthread -lc -ldl -lm -lsunperf -lfsu -Rpath:$(LIBPATH)
	CFLAGS= $(COMMON_FLAGS) -c -O3
	IFLAGS= -I $(APIPATH)/include 
	CCOM  = cc -m32
	LINK  = ld -m32
endif

ifeq ($(PLATFORM),solaris64x86)
	LFLAGS= $(COMMON_FLAGS) -llindo64  -lmosek64  -lconsub3 -lsocket -lnsl -lintl -lthread -lpthread -lc -ldl -lm -lsunperf -lfsu -lsunmath -Rpath:$(LIBPATH)
	CFLAGS= $(COMMON_FLAGS) -c -O3  
	IFLAGS= -I $(APIPATH)/include 
	CCOM  = cc -m64
	LINK  = ld -m64
endif

ifeq ($(PLATFORM),hpux32)
	LFLAGS= $(COMMON_FLAGS) -llindo  -lmosek -lconsub3 -lpthread -lc -lm -Wl,+s
	CFLAGS= $(COMMON_FLAGS) +DD32 +DAportable 
	IFLAGS= -I $(APIPATH)/include 
	CCOM  = cc
	LINK  = ld
endif

ifeq ($(PLATFORM),hpux64)
	LFLAGS= $(COMMON_FLAGS) -llindo64  -lmosek64 -lconsub3 -lpthread -lc -lm
	CFLAGS= $(COMMON_FLAGS) +DD64 +DA2.0W +DS2.0W 
	IFLAGS= -I $(APIPATH)/include 
	CCOM  = cc
	LINK  = ld
endif

ifeq ($(PLATFORM),osx64x86)
	LFLAGS= $(COMMON_FLAGS) -llindo64  -pthread  -lSystem -lc -ldl -lm -Wl,-rpath,@loader_path/$(LIBPATH) -Xlinker -bind_at_load
	CFLAGS= $(COMMON_FLAGS) -c -O3
	IFLAGS= -I $(APIPATH)/include 
	CCOM  = gcc -m64
	LINK  = gcc -m64
	OBJ_EXT=os
endif

ifeq ($(PLATFORM),osx32x86)
	LFLAGS= $(COMMON_FLAGS) -llindo  -pthread -lc -ldl -lm -Wl,-rpath,@loader_path/$(LIBPATH) -Xlinker -bind_at_load
	CFLAGS= $(COMMON_FLAGS) -c -O3
	IFLAGS= -I $(APIPATH)/include 
	CCOM  = gcc -m32
	LINK  = gcc -m32
	OBJ_EXT=os
endif

ifeq ($(PLATFORM),win32)
	LFLAGS= $(COMMON_FLAGS) -llindo$(LS_MAJOR)_$(LS_MINOR)  -lmosek$(MSK_MAJOR)_$(MSK_MINOR) -lconopt3 -lpthread 
	CFLAGS= $(COMMON_FLAGS) -c -O3 -D_LINDO_DLL_
	IFLAGS= -I $(APIPATH)/include 
	CCOM  = gcc -m32
	LINK  = ld -m32
endif

ifeq ($(PLATFORM),win64)
	LFLAGS= $(COMMON_FLAGS) -llindo64_$(LS_MAJOR)_$(LS_MINOR)  -lmosek64_$(MSK_MAJOR)_$(MSK_MINOR) -lconopt3 -lpthread 
	CFLAGS= $(COMMON_FLAGS) -c -O3 -D_LINDO_DLL_
	IFLAGS= -I $(APIPATH)/include 
	CCOM  = gcc -m64
	LINK  = ld -m64
endif


###########################################################################
# designate executable binary
###########################################################################
BINNAME=$(OBJPATH)/$(EXAMPLE)

OBJS_BIN= $(OBJPATH)/$(EXAMPLE).$(OBJ_EXT)

all:  $(LIB_OBJS) $(BINNAME) 
	@echo "APIPATH is set to: $(APIPATH)"
clean: 
	rm *.$(OBJ_EXT)
	rm $(EXAMPLE)

.PHONY: DEF_PLATFORM

DEF_PLATFORM:
ifndef PLATFORM
    $(error PLATFORM is undefined, try 'make -f makefile.unx PLATFORM=linux64')
endif

###########################################################################
# make objects and executables
###########################################################################

$(OBJPATH)/$(EXAMPLE).$(OBJ_EXT): $(SRCPATH)/$(EXAMPLE).c
	$(CCOM) $(CFLAGS) $(IFLAGS) $(SRCPATH)/$(EXAMPLE).c -o $(OBJS_BIN)

$(BINNAME):  $(OBJS_BIN)
	$(CCOM) -o $@  $(OBJS_BIN) -L$(LIBPATH) $(LFLAGS)
ifeq ($(PLATFORM),osx64x86)
	install_name_tool -change @loader_path/liblindo64.dylib @loader_path/$(LIBPATH)/liblindo64.dylib $(BINNAME)
endif
ifeq ($(PLATFORM),osx32x86)
	install_name_tool -change @loader_path/liblindo.dylib @loader_path/$(LIBPATH)/liblindo.dylib $(BINNAME)
endif