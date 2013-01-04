#!/bin/sh -ex

# MODULE is relative path of module to be promoted wrt fighterfish dir.
# We hard code the module name to avoid having to update the hudson job
# everytime we want to rollback. This also allows us better tracking.
MODULE=sample/parent-pom

if [ "$MODULE" = "" ]
then
 echo "Module name missing"
 exit 1
fi

# Directory where last failed promotion job ran.
cd $WORKSPACE/20130104_074059/$MODULE
 
mvn -Dmaven.repo.local=$WORKSPACE/repository -Dhttps.proxyHost=www-proxy.us.oracle.com -Dhttps.proxyPort=80 -B release:rollback
