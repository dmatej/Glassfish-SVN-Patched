REM Last updated 03 October 2005, Lloyd L Chambers
REM Use this script to generate the sample code and javadoc to go with it

REM AMX APIs are found in appserv-ext.jar
set CP=.;appserv-ext.jar;javaee.jar;../../../admin-core/mbeanapi/build/
set SRC_DIR=./amxsamples

set LINK_URL=http://java.sun.com/j2se/1.5.0/docs/api
set NO_QUALIFIER=-noqualifier java.lang:java.io:java.util:javax.management:javax.management.remote
set OPTIONS=-linksource -breakiterator -sourcepath %SRC_DIR% -link %LINK_URL% -classpath %CP% -source 1.5 %NO_QUALIFIER%

echo Generating AMX Samples Javadoc...


javadoc -d javadoc -protected %OPTIONS% amxsamples/*.java

echo DONE

