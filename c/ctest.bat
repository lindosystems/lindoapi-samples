@echo off
echo.
setlocal enableextensions
set SAVPATH=%cd%

if "%1"=="" goto usage
if "%1"=="ex_samp3" goto skip
if "%1"=="ex_samp4" goto skip
if not exist %1 goto noex

rem choose default platform 
if "%PLATFORM%"=="" goto choose_platform
goto xrun

:choose_platform
rem set PLATFORM=win32
set PLATFORM=win64
echo Warning PLATFORM not specified. Using PLATFORM=%PLATFORM%

:xrun
if not exist %LINDOAPI_HOME%\bin\%PLATFORM% goto noplatform

echo Building application %1 for %PLATFORM%...
cd %1
nmake -f makefile.win PLATFORM="%PLATFORM%"

echo Running.
if not exist %1.exe goto nobin
%1 %2 %3 %4

goto end

:nobin
echo.
echo Error: Application was not built. 
if exist %1.obj echo Possible cause: Current compiler cannot be used to build a %PLATFORM% application.
goto end

:noplatform
echo Error: binaries for platform %PLATFORM% not found
goto end

:noex
echo Error: sample directory '%1' does not exist.
goto end

:usage
echo usage: ctest [ex_path [platform]]
echo.
echo examples:
echo    ctest ex_lp2 win32
echo    ctest ex_nlp1 win64
goto end

:skip
echo Run the Visual C/C++ project (%1\stafflnd.dsw)

:end
if exist %1.obj del %1.obj
if exist %1.exe del %1.exe
cd %SAVPATH%
endlocal 