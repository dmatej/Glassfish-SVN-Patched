#!/bin/sh +x
#
# DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
#
# Copyright (c) 2013 Oracle and/or its affiliates. All rights reserved.
#
# The contents of this file are subject to the terms of either the GNU
# General Public License Version 2 only ("GPL") or the Common Development
# and Distribution License("CDDL") (collectively, the "License").  You
# may not use this file except in compliance with the License.  You can
# obtain a copy of the License at
# https://glassfish.dev.java.net/public/CDDL+GPL_1_1.html
# or packager/legal/LICENSE.txt.  See the License for the specific
# language governing permissions and limitations under the License.
#
# When distributing the software, include this License Header Notice in each
# file and include the License file at packager/legal/LICENSE.txt.
#
# GPL Classpath Exception:
# Oracle designates this particular file as subject to the "Classpath"
# exception as provided by Oracle in the GPL Version 2 section of the License
# file that accompanied this code.
#
# Modifications:
# If applicable, add the following below the License Header, with the fields
# enclosed by brackets [] replaced by your own identifying information:
# "Portions Copyright [year] [name of copyright owner]"
#
# Contributor(s):
# If you wish your version of this file to be governed by only the CDDL or
# only the GPL Version 2, indicate your decision by adding "[Contributor]
# elects to include this software in this distribution under the [CDDL or GPL
# Version 2] license."  If you don't indicate a single choice of license, a
# recipient has the option to distribute your version of this file under
# either the CDDL, the GPL Version 2 or to extend the choice of license to
# its licensees as provided above.  However, if you add GPL Version 2 code
# and therefore, elected the GPL Version 2 license, then the option applies
# only if the new code is made subject to such option by the copyright
# holder.
#

# This is a simple script that we use to run OSGi Compliance Tests.
# We use the same file for all test suites, viz: 
# org.osgi.test.cases.http, 
# org.osgi.test.cases.transaction, 
# org.osgi.test.cases.webcontainer, 
# org.osgi.test.cases.jdbc, 
# etc.
#
# This script uses following properties which are supplied to it from command line:
#
# gfhome -  specifies where glassfish has been installed.
# cthome - specifies where OSGi CT has been checked out
# ctname - refers to the OSGi CT test suite that we want to run. e.g., http|transaction|webcontainer
# testclass - refers to the individual JUnit test class that you may want to execute. i
# When this is not specified, all tests are executed
#
# Author: sanjeeb.sahoo@oracle.com
# Date: 23 Feb 2013

set -e

print_usage() {
 echo "Usage: $0 <glassfish.home> <ct.home> <ct.name> [test.class]"
 echo "e.g.: $0 /tmp/gf $HOME/WS/osgi/r4v42-final transaction SimpleUserTransactionTest"
 echo "If you don't supply the test class name, all tests from the test suite will get executed."
 echo "Test results will be reported in `pwd`/reports/osgi-ct.xml"
 exit 1
}

if [ "$1" = "--help" ] || [ $# -lt 3 ]
then
 print_usage
fi

gfhome=${1}
cthome=${2}
ctname=${3}
testclass=${4}

scriptdir=`dirname $0`

if [ -d reports ]
then
echo Reusing existing reports dir.
else
 mkdir reports
fi
reportfile=`pwd`/reports/osgi-ct.xml

# Make sure cthome ends with '/'
# We get some strange errors if we have // in bundle path, so we do this magic here.
echo ${cthome} | grep '/$'
if [ $? -ne 0 ]
then
 cthome=${cthome}/
fi

if [ "${testclass}" != "" ]
then
 extraopt="-test org.osgi.test.cases.${ctname}.${testclass}"
fi

# Extract the bnd runtime jar from the uber bnd jar
(cd /tmp; jar xvf ${cthome}/licensed/repo/biz.aQute.bnd/biz.aQute.bnd-latest.jar aQute/bnd/test/biz.aQute.runtime.jar)

classpath=/tmp/aQute/bnd/test/biz.aQute.runtime.jar:${gfhome}/osgi/felix/bin/felix.jar:${cthome}licensed/repo/com.springsource.junit/com.springsource.junit-3.8.2.jar:$JAVA_HOME/lib/tools.jar
debug="-Xdebug -Xnoagent -Djava.compiler=NONE -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=9009"

# We set all the properties used for all test suites. That means a few extra proprties are set, but 
# that does not harm. It allows us to have just one script for all suites.
# Pl note that we need to set the bundles first before passing the target bundle, else http CT won't run.
# It makes sense since bundles are used to set up the environment for the target bundle.
options="${debug} \
 -classpath ${classpath} \
 aQute.junit.runtime.Target \
 -export junit.framework;version=3.8 \
 -set org.osgi.framework.storage /tmp/osgi-ct-cache \
 -set org.osgi.framework.storage.clean onFirstInit \
 -set org.osgi.framework.bundle.parent framework \
 -set osgi.support.multipleHosts true \
 -set org.osgi.service.webcontainer.hostname 127.0.0.1 \
 -set org.osgi.service.webcontainer.http.port 8080 \
 -set org.glassfish.osgihttp.ContextPath / \
 -set com.sun.aas.installRoot ${gfhome} \
 -set gosh.args --nointeractive \
 -bundle $HOME/.m2/repository/org/glassfish/fighterfish/test.ct.misc.bundle.javax.servlet-api_2.5/1.0.0-SNAPSHOT/test.ct.misc.bundle.javax.servlet-api_2.5-1.0.0-SNAPSHOT.jar \
 -bundle ${cthome}/cnf/repo/org.osgi.impl.service.log/org.osgi.impl.service.log-1.3.2.jar \
 -bundle ${gfhome}/modules/glassfish.jar \
 -bundle $HOME/.m2/repository/org/glassfish/fighterfish/test.ct.misc.bundle.delay/1.0.0-SNAPSHOT/test.ct.misc.bundle.delay-1.0.0-SNAPSHOT.jar \
 -target ${cthome}org.osgi.test.cases.${ctname}/generated/org.osgi.test.cases.${ctname}.jar \
 -report ${reportfile} \
 ${extraopt}"

echo Executing the command: [java ${options}]
java ${options} || true

# Show results
echo "Test results are kept in ${reportfile}"
${scriptdir}/find-failed-ct-test.sh ${reportfile} | tee /tmp/failed
