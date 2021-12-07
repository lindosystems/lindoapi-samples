@echo off

rem ###########################################################################
rem #
rem # Tests the JNI (Java Native Interface) for LINDO API
rem # Copyright (c) 2002-2010, Lindo Systems Inc.
rem #
rem # $Id: jtest.bat 193 2014-08-22 17:27:50Z svn $
rem ###########################################################################

rem Read and set LS_MAJOR and LS_MINOR values
echo Reading version file '..\..\include\lsversion.sh'
if not exist ..\..\include\lsversion.sh goto noversion
for /f "delims=" %%x in (..\..\include\lsversion.sh) do (set "%%x")

set OLDPATH=%PATH%

if "%1"=="" goto usage
if not exist %1 goto nosample

rem ############################################################################
rem Specify platform at command line as an environment variable (or
rem set it here: possible values are win32, win64)

rem set PLATFORM=win32
rem set PLATFORM=win64
if "%PLATFORM%"=="" goto noplatform
echo Platform: %PLATFORM%

rem ############################################################################
rem Developer sources
if not exist ../../obj goto setapihome
@call binpath.bat
goto setpath

:setapihome
rem ############################################################################
rem Check if LINDO API installation exists (or set it here).

rem set LINDOAPI_HOME=d:/usr/lindoapi/8.0

if "%LINDOAPI_HOME%"=="" goto nolindohome
if not exist %LINDOAPI_HOME%\bin goto badlindohome
set BINPATH=%LINDOAPI_HOME%\bin\%PLATFORM%

:setpath
set OLDPATH=%PATH%
set PATH=%BINPATH%;%PATH%

rem ############################################################################
rem If not already set, this is where to set JAVA_HOME according to 
rem your installation.

rem if "%PLATFORM%"=="win32" set JAVA_HOME="D:\usr\Java\x86\default"
rem if "%PLATFORM%"=="win64" set JAVA_HOME="D:\usr\Java\x64\default"

if "%JAVA_HOME%"=="" goto nojavahome
echo %JAVA_HOME%
if not exist %JAVA_HOME% goto badjavahome


:javaopt
rem set JAVA_MEM=-Xms128m -Xmx512m -Xcheck:jni
set JAVA_MEM=-Xms128m -Xmx512m
if "%PLATFORM%"=="win64" set JAVA_OPT=%JAVA_MEM% -Djava.library.path=%BINPATH%
if "%PLATFORM%"=="win32" set JAVA_OPT=%JAVA_MEM% -Djava.library.path=%BINPATH%

rem cd %1
echo Compiling %1
%JAVA_HOME%\bin\javac.exe -classpath ..\..\lib\%PLATFORM%\lindo%LS_MAJOR%_%LS_MINOR%.jar %1\%1.java 
echo Running %1 ...
echo Java Options...
echo %JAVA_OPT%  -classpath ..\..\lib\%PLATFORM%\lindo%LS_MAJOR%_%LS_MINOR%.jar;%1 %1 %2 %3 %4
%JAVA_HOME%\bin\java.exe %JAVA_OPT%  -classpath ..\..\lib\%PLATFORM%\lindo%LS_MAJOR%_%LS_MINOR%.jar;%1 %1 %2 %3 %4
del %1\*.class
rem cd ..
goto end


rem ############################################################################
:usage
@echo usage: jtest [path_to_example] 
@echo ex:   jtest ex_lp1
goto end


rem ############################################################################
:nojavahome
@echo JAVA_HOME variable needs to be set. 
@echo Quitting...
goto end

rem ############################################################################
:badjavahome
@echo JAVA_HOME=%JAVA_HOME% does not exist. 
@echo Quitting...
goto end

rem ############################################################################
:nolindohome
@echo LINDOAPI_HOME variable needs to be set. 
@echo Quitting...
goto end


rem ############################################################################
:badlindohome
@echo LINDOAPI_HOME=%LINDOAPI_HOME% does not appear to be a valid installation. 
@echo Quitting...
goto end


rem ############################################################################
:noplatform
@echo PLATFORM environment variable should be set prior to running the sample.
@echo Type 'set PLATFORM=win32' or 'set PLATFORM=win64' before running jtest.
@echo Quitting...
goto end

rem ############################################################################
:nosample
@echo Specified sample '%1' does not exist.
@echo Quitting...
goto end

rem ############################################################################
:noversion
@echo Version file not found at specified path
@echo Quitting...
goto end

:end
set PATH=%OLDPATH%
