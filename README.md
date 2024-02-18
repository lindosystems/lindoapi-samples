
# LINDO API  Sample Programs

                      Copyright (c) 2024

         LINDO Systems, Inc.           312.988.7422
         1415 North Dayton St.         info@lindo.com
         Chicago, IL 60622             http://www.lindo.com



This directory contains several programming examples that illustrate the use of 
LINDO API using a high level programming language. The majority of the examples 
are in C/C++. Samples in other languages, such as Visual Basic, .NET, Delphi, 
Fortran 90 and Java/J++ are also given. 


**IMPORTANT**: The header files required by each programming language are located in 
lindoapi\include directory. These headers contain macro definitions and function 
prototypes (calling sequences) for each programming language. For a detailed 
description of available LINDO API functions, please refer to your user manual.


Each sample is located in a separate directory along with a MAKEFILE and/or an 
IDE Project (for Windows only) to build the application. Depending on your 
platform, use makefile.unx (for OSX and Linux) or makefile.win (for Windows).

## C/C++ applications
Change directory to lindoapi\samples\c and type the following at command prompt. 

		DOS shell
		c\> ctest.bat ex_nlp1

		Unix-Linux shells
		$ ctest.sh ex_nlp1

**Note**: When testing C/C++ samples on unix-like platforms, make sure to

1. edit and modify ./c/platform.inc (included by ./c/ctest.sh) so that
the target platform is specified correctly. 

2. set platform-specific flags you need (such as -m64 for compiling and 
linking in 64-bit mode with gcc). $COMMON_FLAGS is reserved for this purpose.

3. set PLATFORM=<platform_name> at command line with if you are *not* using 
ctest.sh but using 'make' explictly inside applications folder. E.g.

		$ cd lindoapi/samples/c/ex_samp1
	
		$ make -f makefile.unx PLATFORM=linux32

## Java applications
Change directory to lindoapi\samples\java and type the following at command prompt. 

        DOS shell
		c:\> jtest.bat ex_nlp1

        Unix-Linux shells
		$ jtest.sh ex_nlp1

## .NET applications
Change the directory to lindoapi\samples\dotnet\vb or lindoapi\samples\dotnet\cs and
and type the following at command promt.

        DOS shell
		c\> ntest.bat ex_mps filename

## VB6 or F90 or Delphi applications
VB6, F90 and Delphi samples should be built and run using the associated IDE projects. 

## Python and R
Refer to lindoapi/Python and lindoapi/R folders.

## Julia applications
Refer to lindoapi/samples/julia