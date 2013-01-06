#!/bin/sh -ex

# MODULE is relative path of module to be promoted wrt fighterfish dir.
# We hard code the module name to avoid having to update the hudson job
# everytime we want to rollback. This also allows us better tracking.
MODULE=sample/uas/api
# Directory relative to WORKSPACE where last failed promotion job ran.
DIRNAME=20130105_232930

if [ "$MODULE" = "" ]
then
 echo "Module name missing"
 exit 1
fi

cd $WORKSPACE/$DIRNAME/$MODULE
 
mvn -Dhttps.proxyHost=www-proxy.us.oracle.com -Dhttps.proxyPort=80 -B release:rollback
