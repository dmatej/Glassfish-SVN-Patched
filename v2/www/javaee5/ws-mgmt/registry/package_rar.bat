@REM -- This file for use on Windows systems
@REM ---------------------------------------------------------
@echo off
echo. 
echo.
echo ********** Script to package JWSDP 1.6 rar for GlassFish ****************
echo  Printing default usage message 
echo To run this script. Make sure you do the following :
echo Copy ra.xml to c:\tmp directory
echo Copy the script in c:\tmp directory
echo Set JWSDP_HOME to point to JWSDP directory 
echo Set JAVA_HOME 
echo "*************************************************************************
echo. 
echo.

:checkJWSDPHOME
if not "%JWSDP_HOME%" == "" goto checkJAVAHOME
echo Set JWSDP_HOME environment variable

:checkJAVAHOME
if not "%JAVA_HOME%" == "" goto checkRAR
echo Set JAVA_HOME environment variable

:checkRAR
if exist c:\tmp\ra.xml goto createRAR
echo ra.xml missing!
echo Copy the new ra.xml to c:\tmp directory
goto end

:createRAR
set RARTMP=c:\tmp\soar
echo Creating the rar under c:\tmp\soar
mkdir %RARTMP%
set reg=%JWSDP_HOME%\registry\lib
copy %reg%\soar-jaxr-ra.jar %RARTMP%
copy %reg%\oasis-*.jar %RARTMP%
copy %reg%\omar-common.jar %RARTMP%
copy %reg%\jaxr-ebxml.jar %RARTMP%
copy %JWSDP_HOME%\jwsdp-shared\lib\commons-logging.jar %RARTMP%


set jar=%JAVA_HOME%\bin\jar
cd %RARTMP% 
%jar% xvf soar-jaxr-ra.jar
rm soar-jaxr-ra.jar
rm %RARTMP%\ra.xml %RARTMP%\META-INF\SUN* %RARTMP%\META-INF\pack.properties
copy c:\tmp\ra.xml %RARTMP%\META-INF
cd %RARTMP%
%jar% cvf soar.rar META-INF com commons-logging.jar oasis-regrep.jar omar-common.jar jaxr-ebxml.jar oasis-saml1.1.jar oasis-saml2.0.jar
cp soar.rar c:\tmp
cd ..
rm -rf %RARTMP%


:end



		



