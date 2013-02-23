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
# This script uses following options which are supplied to it from command line:
#
# gfhome - specifies where glassfish has been installed.
# cthome - specifies where OSGi CT has been checked out
# ctname - refers to the OSGi CT test suite that we want to run. e.g., http|transaction|webcontainer
#
# Author: sanjeeb.sahoo@oracle.com
# Date: 23 Feb 2013

set -e
print_usage() {
 echo "Usage: $0 <glassfish.home> <OSGi CT Home> <CT Name> [bnd file name]"
 echo "e.g.: $0 /tmp/glassfish3/glassfish /space/ss141213/WS/osgi/r4v42-final transaction"
 echo "Test results will be reported in `pwd`/reports/"
 exit 1
}

if [ "$1" = "--help" ] || [ $# -lt 3 ]
then
 print_usage
fi

gfhome=${1}
cthome=${2}
ctname=${3}
bndfile=${4:-osgi-ct.bnd}

# We set glassfish.home, ct.home and ct.name properties as they are referenced in bnd files.
options="-Dglassfish.home=${gfhome} -Dct.home=${cthome} -Dct.name=${ctname} \
  -jar ${cthome}/licensed/repo/biz.aQute.bnd/biz.aQute.bnd-latest.jar runtests ${bndfile}"
echo Executing cmd: [java ${options}]
java ${options}

# bnd generates report in ./reports/<base name of bnd file without extension>.xml
# So, we have to calculate report file path.
# Refer to http://www.cyberciti.biz/faq/unix-linux-extract-filename-and-extension-in-bash/ for following variable expansion tricks
bndfilebasename=${bndfile##*/}
bndfilenamewithoutextension=${bndfilebasename%.*}
reportfile=`pwd`/reports/${bndfilenamewithoutextension}.xml
summaryfile=`dirname ${reportfile}`/summary.txt
echo "Test results are kept in ${reportfile} and ${summaryfile}"
scriptdir=`dirname $0`
set +e
${scriptdir}/find-failed-ct-test.sh ${reportfile} > ${summaryfile} 2>&1
failed=$?
cat ${summaryfile}
exit ${failed}

