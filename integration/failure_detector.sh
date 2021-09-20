#!/bin/bash

FILE=$1

usage(){
  echo "$0 <file output to evaluate>"
  echo "$0 output.json"
  exit 1
}

#Make sure an operator has been passed
if [ -z "$FILE" ] ;
then
  usage
fi


FAILURE_INDICATOR="--- FAIL"

FAILURE_LINES=`grep -- "${FAILURE_INDICATOR}" ${FILE} | wc -l`

if [ $FAILURE_LINES -gt 0 ] ;
then
  echo "${FAILURE_LINES} Failures detected in ${FILE}"
  exit 1
else
  exit 0
fi
