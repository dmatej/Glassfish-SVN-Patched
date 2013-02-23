#!/bin/sh
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
