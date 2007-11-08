REM These jar files must be present!
set CP=.;appserv-ext.jar;javaee.jar

set SAMPLE_MAIN=amxsamples.SampleMain
set PROPS=SampleMain.properties

java -ea -esa -cp %CP% %SAMPLE_MAIN% %PROPS%

