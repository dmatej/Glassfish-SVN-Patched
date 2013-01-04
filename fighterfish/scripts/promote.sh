#!/bin/sh -ex

# MODULE is relative path of module to be promoted wrt fighterfish dir.
# We hard code the module name to avoid having to update the hudson job
# everytime we want to promote. This also allows us better tracking.
MODULE=sample/parent-pom

if [ "$MODULE" = "" ]
then
 echo "Module name missing"
 exit 1
fi

GPG_PASSPHRASE=$1
if [ "$GPG_PASSPHRASE" = "" ]
then
 echo "GPG Passphrase must be provided or you must change the script to run in interactive mode."
 exit 1
fi

# Create a temporary dir to checkout the module(s) that need to be released.
TS=`date +%Y%m%d_%H%M%S`
mkdir $WORKSPACE/$TS
cd $WORKSPACE/$TS
svn co https://svn.java.net/svn/glassfish~svn/trunk/fighterfish/$MODULE $MODULE

cd $MODULE
mvn -Dmaven.repo.local=$WORKSPACE/repository -Dhttps.proxyHost=www-proxy.us.oracle.com -Dhttps.proxyPort=80 -B -DtagBase=https://svn.java.net/svn/glassfish~svn/tags/fighterfish-releases -DtagNameFormat=@{project.groupId}.@{project.artifactId}-@{project.version} -Dgpg.passphrase=$GPG_PASSPHRASE release:prepare
 
mvn -Dmaven.repo.local=$WORKSPACE/repository -Dhttps.proxyHost=www-proxy.us.oracle.com -Dhttps.proxyPort=80 -Dgpg.passphrase=$GPG_PASSPHRASE -B release:perform
