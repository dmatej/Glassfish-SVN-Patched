# These jar files must be present!
export STD_JARS="jmxri.jar;jmxremote.jar;javax77.jar"
export CP=".;amx-client.jar;$STD_JARS"

java -cp $CP samples.amx.SampleMain SampleMain.properties

