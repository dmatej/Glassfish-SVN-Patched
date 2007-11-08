REM
REM Compiles the sample code.
REM

REM AMX APIs are found in appserv-ext.jar
set CP=appserv-ext.jar
set SRC_DIR=amxsamples

echo Compiling AMX Samples...
del %SRC_DIR%\*.class
javac -cp %CP% %SRC_DIR%\*.java -Xlint -deprecation -g -source 1.5

echo DONE




