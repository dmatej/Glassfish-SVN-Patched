@echo off
REM Copyright 1997-2008 Sun Microsystems, Inc.  All rights reserved.
REM Use is subject to license terms.
REM Always use JDK 1.6 or higher
REM Depends on Java from ..\glassfish\config\asenv.bat
setlocal
call "%~dp0..\glassfish\config\asenv.bat"
if "%AS_JAVA%x" == "x" goto UsePath
set JAVA="%AS_JAVA%\bin\java"
goto run
:UsePath
set JAVA=java
:run
echo "java is %JAVA%"
 %JAVA% -jar "%~dp0..\glassfish\modules\admin-cli.jar" %*

