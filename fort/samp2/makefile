###########################################################################
# @makefile 
#
# Linking against LINDO-API libraries on Linux/Unix Platforms.
#
# Copyright (c) 2001-2002, Lindo Systems Inc.
###########################################################################


#PLATFORM=linux32
PLATFORM=linux64

EXAMPLE = samp2
OBJ_EXT=o
OBJPATH=
BINPATH=
LIBPATH=../../../bin/$(PLATFORM)/
SRCPATH=./


###########################################################################
# set flags
###########################################################################
ifeq ($(PLATFORM),linux64)
	LINKLINDO=-llindo64 -lmosek64 -lconsub3 -lpthread -ldl -Wl,-rpath,$(LIBPATH)
	CFLAGS= -c -O3 -132 -Dx64
	CCOM=/opt/intel/fce/10.1.013/bin/ifort
endif

ifeq ($(PLATFORM),linux32)
	LINKLINDO=-llindo -lmosek -lconsub3 -lpthread -ldl -Wl,-rpath,$(LIBPATH)
	CFLAGS= -c -O3 -132 -DNODECOR
	CCOM=/opt/intel/fc/10.1.013/bin/ifort
endif


###########################################################################
# designate executable binary
###########################################################################
BINNAME=$(BINPATH)$(EXAMPLE)

OBJS_BIN= $(OBJPATH)$(EXAMPLE).$(OBJ_EXT)

all:  $(LIB_OBJS) $(BINNAME)
clean: 
	rm *.o
	rm $(EXAMPLE)

###########################################################################
# make objects and executables
###########################################################################

$(OBJPATH)$(EXAMPLE).$(OBJ_EXT): $(SRCPATH)$(EXAMPLE).for
	$(CCOM) $(CFLAGS) $(SRCPATH)$(EXAMPLE).for -o $(OBJS_BIN)

$(BINNAME):  $(OBJS_BIN)
	$(CCOM) -o $@  $(OBJS_BIN) -L $(LIBPATH) $(LINKLINDO)

