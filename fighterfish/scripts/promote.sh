#!/bin/sh -ex

# Update this everytime you want to promote one or more modules.
# Use space a delim while specifying more than one module dir path and quote the entire variable.
# We hard code the module name to avoid having to update the hudson job
# everytime we want to promote. This also allows us better tracking.
MODULES="\
 module/parent-pom \
"

GPG_PASSPHRASE=$1 #Take as argument for security reasons

function promote_one_module() {
    # MODULE is relative path of module to be promoted wrt fighterfish dir.
    MODULE=$1

    echo promoting $MODULE

    if [ "$MODULE" = "" ]
    then
     echo "Module name missing"
     exit 1
    fi

    GPG_PASSPHRASE=$2
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

    # We don't use any separate maven local repo, because I don't know how to make release plugin use it in forked processes the special maven repo.
    # So, we use the default one.
    mvn -Dhttps.proxyHost=www-proxy.us.oracle.com -Dhttps.proxyPort=80 -B -DtagBase=https://svn.java.net/svn/glassfish~svn/tags/fighterfish-releases -DtagNameFormat=@{project.groupId}.@{project.artifactId}-@{project.version} -Dgpg.passphrase=$GPG_PASSPHRASE release:prepare 
     
    # We don't use any separate maven local repo, because I don't know how to make release plugin use it in forked processes the special maven repo.
    # So, we use the default one.
    mvn -Dhttps.proxyHost=www-proxy.us.oracle.com -Dhttps.proxyPort=80 -Dgpg.passphrase=$GPG_PASSPHRASE -B release:perform 
}

for MODULE in $MODULES 
do
    promote_one_module $MODULE $GPG_PASSPHRASE
done

