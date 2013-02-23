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

# A simple script that parses report file produced by bnd and produces a summary of
# total no. of failed tests and total no. of tests.
# It returns zero if there are no failures, else it returns an exist status which is same 
# as no. of failed tests. If no tests have run, then it returns -1.
# Author: sanjeeb.sahoo@oracle.com

EXPECTED_ARGS=1
if [ $# -ne $EXPECTED_ARGS ]
then
 echo "Usage: $0 <path to report file. e.g., reports/bnd.xml>"
 exit 1
fi
result=`grep -e failure -e error $1 | cut -d "=" -f2 | cut -d "'" -f2 | cut -d "(" -f1 | grep test `
total=`grep "test name" $1 | wc -l`
failed=`echo $result | wc -w`
echo $result
echo failed/total =  $failed/$total 
if [ $total -eq 0 ]
then
 echo no tests ran!
 exit -1
else
 exit $failed
fi

