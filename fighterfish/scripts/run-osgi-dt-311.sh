#!/bin/sh -x
if [ $# -eq 0 ] 
then
 echo "Usage: $0 <glassfish.home>"
 exit 1
fi
GF_HOME=$1
shift 1
WS=~/WS/ff/fighterfish-gf3.1.1/test/test.it
mkdir ${GF_HOME}/osgi/felix/conf/
cp ${GF_HOME}/config/osgi.properties ${GF_HOME}/osgi/felix/conf/config.properties
mkdir ${GF_HOME}/osgi/equinox/configuration/
cp ${GF_HOME}/config/osgi.properties ${GF_HOME}/osgi/equinox/configuration/config.ini
OUT=${WS}/t.log
touch ${OUT}
if [ -w ${OUT} ]
then
 rm ${OUT}
 #echo "Running tests on Equinox"
 #redirect.sh ${OUT} `which mvn` -o -f ${WS}/pom.xml clean test -P-Felix -PEquinox -Dglassfish.home=${GF_HOME} $*
 #rm -rf ${WS}/surefire-reports-Equinox || true
 #mv ${WS}/target/surefire-reports ${WS}/surefire-reports-Equinox
 #rm ${GF_HOME}/osgi/equinox/configuration/config.ini
 echo "Running tests on Felix"
 redirect.sh ${OUT} `which mvn` -o -f ${WS}/pom.xml clean test -Dglassfish.home=${GF_HOME} $*
 rm -rf ${WS}/surefire-reports-Felix || true
 mv ${WS}/target/surefire-reports ${WS}/surefire-reports-Felix
 rm ${GF_HOME}/osgi/felix/conf/config.properties
 echo "Summary:" 
 grep "Tests run" ${OUT}
else
 echo "can't write t.log."
 exit 1
fi
