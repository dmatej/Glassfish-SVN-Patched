#
# Runs the sample command line.
#

export CP=".:./appserv-ext.jar:./javaee.jar"

export SAMPLE_MAIN=amxsamples.SampleMain
export PROPS=SampleMain.properties

java -ea -esa -cp $CP $SAMPLE_MAIN $PROPS





