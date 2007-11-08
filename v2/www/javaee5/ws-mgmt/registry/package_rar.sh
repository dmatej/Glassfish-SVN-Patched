#!/bin/sh
echo 
echo
echo "********** Script to package JWSDP 1.6 rar for GlassFish ****************"
echo " Printing default usage message "
echo "To run this script. Make sure you do the following :"
echo "Copy ra.xml to /tmp directory"
echo "Copy the script in /tmp directory"
echo "Set JWSDP_HOME to point to JWSDP directory "
echo "Set JAVA_HOME " 
echo "*************************************************************************"
echo 
echo 

if [ -z "$JWSDP_HOME" ];  
	then
		echo "Set JWSDP_HOME environment variable"
		exit 1
fi
		
if [ -z "$JAVA_HOME" ];  
	then
		echo "Set JAVA_HOME environment variable"
		exit 1
fi		
if test ! -r /tmp/ra.xml
	then 
		echo "ra.xml missing!" 
		echo "Copy the new ra.xml to /tmp directory"
		exit 1
fi
RARTMP=/tmp/soar
echo "Creating the rar under /tmp/soar.rar"
mkdir $RARTMP
reg=$JWSDP_HOME/registry/lib
cp $reg/soar-jaxr-ra.jar $RARTMP
cp $reg/oasis-*.jar $RARTMP
cp $reg/omar-common.jar $RARTMP
cp $reg/jaxr-ebxml.jar $RARTMP
cp $JWSDP_HOME/jwsdp-shared/lib/commons-logging.jar $RARTMP

jar=$JAVA_HOME/bin/jar
(cd $RARTMP; $jar xvf soar-jaxr-ra.jar; rm soar-jaxr-ra.jar)
rm $RARTMP/ra.xml $RARTMP/META-INF/SUN* $RARTMP/META-INF/pack.properties
cp /tmp/ra.xml $RARTMP/META-INF
(cd $RARTMP; $jar cvf soar.rar META-INF com commons-logging.jar oasis-regrep.jar omar-common.jar jaxr-ebxml.jar oasis-saml1.1.jar oasis-saml2.0.jar; cp soar.rar /tmp)
rm -rf $RARTMP
