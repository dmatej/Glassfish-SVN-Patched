# !/bin/sh
# Last updated 27 October 2004, Lloyd L Chambers
# Use this script to generate the sample code and javadoc to go with it
#


# AMX APIs are found in appserv-ext.jar
export CP_SEP=":"
export CP=".${CP_SEP}../../../admin-core/mbeanapi/build/"
export SRC_DIR=./amxsamples

LINK_URL=http://java.sun.com/j2se/1.5.0/docs/api
NO_QUALIFIER="-noqualifier java.lang:java.io:java.util:javax.management:javax.management.remote"
OPTIONS="-linksource -breakiterator -sourcepath $SRC_DIR -link $LINK_URL -classpath $CP -source 1.5 $NO_QUALIFIER"

echo Generating AMX Samples...

rm -rf javadoc
javadoc -d javadoc -protected $OPTIONS amxsamples/*.java

echo DONE

