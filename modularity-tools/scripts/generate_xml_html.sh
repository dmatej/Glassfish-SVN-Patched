#!/bin/bash
#
# DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
#
# Copyright (c) 2010-2012 Oracle and/or its affiliates. All rights reserved.
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


# Usage: ./example.sh TARGET_DIRECTORY FILE_NAME_MATCH TYPE OUTPUT_DIR OUTPUT_XML_FILE_NAME OUTPUT_HTML_FILE_NAME

REPO_DIR=$1
MATCH=$2
TYPE=$3
OUTPUT_DIR=$4
OUTPUT_FILE_NAME=$5
OUTPUT_HTML=$6
XSL_TEMPLATE=""
HTML_TEMPLATE=""

if [ "$TYPE" == "MODULE" ];
then
XSL_TEMPLATE="MODULE_TEMPLATE.xsl"
HTML_TEMPLATE="MODULE_TEMPLATE.html"
else 
if [ "$TYPE" == "TIME" ];
then
XSL_TEMPLATE="TIME_TEMPLATE.xsl"
HTML_TEMPLATE="TIME_TEMPLATE.html"
else
if [ "$TYPE" == "MEMORY" ];
then
XSL_TEMPLATE="MEMORY_TEMPLATE.xsl"
HTML_TEMPLATE="MEMORY_TEMPLATE.html"
else
if [ "$TYPE" == "STATE" ];
then
XSL_TEMPLATE="STATE_TEMPLATE.xsl"
fi
fi
fi
fi

if [ ! -d $OUTPUT_DIR ]
then
mkdir -p $OUTPUT_DIR
fi

#Generate unified XML and copy xsl
cat "./XSL/XML_HEADER_TEMPLATE.xml" | sed "s/REPLACE_ME/$XSL_TEMPLATE/"  > $OUTPUT_DIR/$OUTPUT_FILE_NAME
sed -ie '$d' $OUTPUT_DIR/$OUTPUT_FILE_NAME
sed -ie '$d' $OUTPUT_DIR/$OUTPUT_FILE_NAME
cat `find  $REPO_DIR -name "$MATCH"` >> $OUTPUT_DIR/$OUTPUT_FILE_NAME
echo -e "</root>" >> $OUTPUT_DIR/$OUTPUT_FILE_NAME
cp ./XSL/$XSL_TEMPLATE $OUTPUT_DIR

#Generate HTML for Graphs.
if [ "$TYPE" != "STATE" ];
then
cat "./HTML/$HTML_TEMPLATE" | sed "s/REPLACE_ME/$OUTPUT_FILE_NAME/"  > $OUTPUT_DIR/$OUTPUT_HTML
fi


