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


#./example.sh GF_HOME ID(REVISION) TARGET_DIRECTORY STATS_XML_FILE_NAME TIME_XML_FILE_NAME MEMORY_XML_FILE_NAME LOG_FILE_NAME

WORKSPACE=$WORKSPACE
GF_HOME=$1
revision=$2
REPO_DIR=$3
STATS_XML_FILE=$4
TIME_XML_FILE=$5
MEMORY_XML_FILE=$6
MODULES_OUTPUT_FILE=$7
MODULE_STATES="READY;RES;NEW"
LOG_DIR="$GF_HOME/domains/domain1/logs"
ITER=5
TIME_FILE="$REPO_DIR/snap.txt"

if [ "$revision" == "" ]; then 
revision="UNKNOWN"
fi

if [ ! -d $REPO_DIR ]
then
mkdir -p $REPO_DIR
fi

#Change the log level to FINE to get dumps in server.log on GF StartUp.
grep -q "javax.enterprise.system.core.level" $GF_HOME/domains/domain1/config/logging.properties
if [ $? -eq 0 ]
then
 sed -i 's/javax.enterprise.system.core.level=INFO/javax.enterprise.system.core.level=FINE/g' $GF_HOME/domains/domain1/config/logging.properties
else
 echo "javax.enterprise.system.core.level=FINE" >> $GF_HOME/domains/domain1/config/logging.properties
fi


#Check if  any of the already running GF process
x=`jps -mlvV | grep ASMain | awk '{ print $1 }'`; if [ -z "$x" ]; then echo "no AS process running"; else echo "GF Already running!" $x; exit; fi;

COMMENT="\nTEST for Build ID:$revision STARTED AT `date`"
echo -e "\n$COMMENT" >> $REPO_DIR/$MODULES_OUTPUT_FILE

SLEEP_SECS1=2; #time to sleep between start-stop cycles
SLEEP_SECS2=5; #time to sleep between start and stop. This is needed because server does a lot of back ground task after it is started.

echo RUNNING $ITER ITERATIONS AGAINST $GF_HOME AND OUTPUT WILL BE WRITTEN TO $MODULES_OUTPUT_FILE.
sleep $SLEEP_SECS1
cd $GF_HOME

banner=""
banner1=""
TAB="\t"
str=""
str1=""
str2=""
mes=""
net="0"
memory="0"
total_count="0"
ready_count="0"
resolved_count="0"
new_count="0"

# Running desired Number of Iterations

for i in `seq 1 $ITER`
do
rm -rf $LOG_DIR/server.log
banner="$banner TRY$i$TAB"
/usr/bin/time -o $TIME_FILE --format "%E" ./bin/asadmin start-domain
sed -e '/Module Status Report Ends/,$d' $LOG_DIR/server.log | grep 'OSGiModuleImpl:: Bundle' | sed "s/^.*Bundle = //" > $REPO_DIR/results_before.txt
module_count=`wc -l < $REPO_DIR/results_before.txt`
TAB="\t"
echo "Total number of Modules: $module_count"
str1="BEFORE$TAB$module_count"
banner1="$TAB TOTAL"
old_ifs=$IFS
IFS=";"
for word in $MODULE_STATES; do
count=`grep -c $word $REPO_DIR/results_before.txt`
banner1="$banner1$TAB$word"
str1="$str1$TAB$count"
echo "Number of Modules in $word STATE: $count "  
done
IFS=$old_ifs

echo -e "\n$banner1 \n$str1" >> $REPO_DIR/$MODULES_OUTPUT_FILE

lapse=`head -n 1 $TIME_FILE`
str="$str$lapse$TAB"
tim=`head -n 1 $TIME_FILE | sed "s/^.*://" | bc`
net=`echo "$net+$tim" | bc`
int=`jps | grep ASMain | cut -f 1 -d " "`
mem=`ps -p $int -o rss=`
mem=`echo "$mem/4" | bc`
mes="$mes$mem$TAB"
memory=`echo "$memory+$mem" | bc`

sleep $SLEEP_SECS2

sed -e '1,/Module Status Report Begins/d' $LOG_DIR/server.log | sed -e '1,/Module Status Report Begins/d' | grep 'OSGiModuleImpl:: Bundle' | sed "s/^.*Bundle = //" > $REPO_DIR/results_after.txt

module_count=`wc -l < $REPO_DIR/results_after.txt`

echo "Total number of Modules: $module_count"
str2="AFTER$TAB$module_count"
total_count=`echo "$total_count+$module_count" | bc`

old_ifs=$IFS
IFS=";"
for word in $MODULE_STATES; do
count=`grep -c $word $REPO_DIR/results_after.txt`
str2="$str2$TAB$count"
echo "Number of Modules in $word STATE: $count "  

if [ "$word" == "READY" ]; then
ready=$count
else
if [ "$word" == "RES" ]; then
res=$count
else
new=$count
fi
fi
done
IFS=$old_ifs

ready_count=`echo "$ready_count+$ready" | bc`
resolved_count=`echo "$resolved_count+$res" | bc`
new_count=`echo "$new_count+$new" | bc`

echo -e $str2 >> $REPO_DIR/$MODULES_OUTPUT_FILE

./bin/asadmin stop-domain
sleep $SLEEP_SECS1
done

rm -rf $REPO_DIR/snap.txt
rm -rf $REPO_DIR/results_*.txt

echo -e "\n$banner" >> $REPO_DIR/$MODULES_OUTPUT_FILE
echo -e $str >> $REPO_DIR/$MODULES_OUTPUT_FILE
echo -e $mes >> $REPO_DIR/$MODULES_OUTPUT_FILE


total_count=`echo "$total_count/$ITER" | bc`
ready_count=`echo "$ready_count/$ITER" | bc`
resolved_count=`echo "$resolved_count/$ITER" | bc`
new_count=`echo "$new_count/$ITER" | bc`

# Generate XML file containing Module Stats

XMLFILE=$REPO_DIR/$STATS_XML_FILE

prepare="<stat>\n<build_id>$revision</build_id>\n<total>$total_count</total>\n<ready>$ready_count</ready>\n<resolved>$resolved_count</resolved>\n<new>$new_count</new>\n</stat>"
echo -e $prepare > $XMLFILE

tim=`echo "scale=2; $net/$ITER" | bc`
org=$tim
tim=`echo "0.5+$tim" | bc`
tim=`echo $tim | sed "s/\..*//"`
memory=`echo "$memory/$ITER" | bc`
org_mem=$memory
memory=`echo "0+$memory" | bc`

###################XML File Generation###########
FILE3="$REPO_DIR/$TIME_XML_FILE"

prepare="<stat>\n<build_id>$revision</build_id>\n<time>$org</time>\n</stat>"
echo -e $prepare > $FILE3

FILE4="$REPO_DIR/$MEMORY_XML_FILE"

prepare="<stat>\n<build_id>$revision</build_id>\n<memory>$org_mem</memory>\n</stat>"
echo -e $prepare >> $REPO_DIR/$MEMORY_XML_FILE

############XML for Modules names & States##########

FILE="$REPO_DIR/module_states_startup.xml"

if [ -f $FILE ];
then
echo "File $FILE exists"
rm $FILE
echo -e "<Modules>" > $FILE
echo -e "</Modules>" >> $FILE
else
echo "File $FILE does not exists"
echo -e "<Modules>" > $FILE
echo -e "</Modules>" >> $FILE
fi

old_ifs=$IFS
IFS=";"
for word in $MODULE_STATES; do

sed -e '1,/Module Status Report Begins/d' $LOG_DIR/server.log | sed -e '1,/Module Status Report Begins/d' | grep 'OSGiModuleImpl:: Bundle' | grep $word | sed "s/^.*Bundle = \[//" | sed "s/ \[.*$//"> $REPO_DIR/temp.txt
while read line      
do
sed -ie '$d' $FILE
prepare="<Module>\n<state>$word</state>\n<name>$line</name>\n</Module>\n</Modules>"
echo -e $prepare >> $FILE
done < $REPO_DIR/temp.txt
done
IFS=$old_ifs
rm $REPO_DIR/temp.txt
















