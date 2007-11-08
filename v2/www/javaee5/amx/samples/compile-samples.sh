#
# Compiles the sample code.
#

# AMX APIs are found in appserv-ext.jar
export CP="appserv-ext.jar"
export SRC_DIR=./amxsamples

echo Compiling AMX Samples...
rm -f $SRC_DIR/*.class
javac -cp $CP $SRC_DIR/*.java -Xlint -deprecation

echo DONE




