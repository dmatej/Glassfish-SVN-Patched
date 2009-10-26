#! /bin/sh

#
# Copyright 2003 Sun Microsystems, Inc. All rights reserved.
# SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
#


if [ -z "$JAVA_HOME" ]
then
JAVACMD=`which java`
if [ -z "$JAVACMD" ]
then
echo "Cannot find JAVA. Please set your PATH."
exit 1
fi
JAVA_BINDIR=`dirname $JAVACMD`
JAVA_HOME=$JAVA_BINDIR/..
fi

JAVACMD=$JAVA_HOME/bin/java

cp=./lib/ant.jar:./lib/jaxp.jar:./lib/crimson.jar:$JAVA_HOME/lib/tools.jar

$JAVACMD -classpath $cp:$CLASSPATH org.apache.tools.ant.Main "$@"
