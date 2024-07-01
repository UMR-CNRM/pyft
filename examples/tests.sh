#!/bin/bash

which pyft_tool.py 2>&1 >/dev/null
if [ $? -ne 0 ]; then
  echo "pyft_tool.py not found!"
  exit 3
fi

function usage {
  echo "Usage: $0 [--update] [--debug] [file]\*"
  echo "  --update to update reference instead of comparing to it"
  echo "  --debug copy resulting file in the example directory when different from the reference"
  echo "  If file names are present on the command line, tests are limited to these files"
}
update='n'
debug='n'
tests=""
while [ -n "$1" ]; do
  case "$1" in
    '-h') usage; exit;;
    '--update') update='y';;
    '--debug') debug='y';;
    *) tests+=" $1";;
  esac
  shift
done

#Temporary directory
TMP_LOC=$(mktemp -d)
trap "\rm -rf $TMP_LOC" EXIT

#Copy all files in the temporary directory
DIR="$(cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)"
cp $DIR/*.F90 $DIR/*.h $TMP_LOC
cd $TMP_LOC

FAIL=""
ERROR="" #Crash

if [ "$tests" == "" ]; then
  tests=$(ls *_before.F90 *_checkOK.F90 *_checkKO.F90 2>/dev/null)
fi

#Transformations and checks
for file in $tests; do
  if [[ $file == *_before.F90 ]]; then
    name=$(echo $file | rev | cut -c 12- | rev)
  elif [[ $file == *_checkOK.F90 || $file == *_checkKO.F90 ]]; then
    name=$file
  fi
  ref=${name}_after.F90
  trans=${name}_trans.F90
  transfo=$(grep "!#PYFT transfo: " $file | cut -c 17-)
  if [ "$transfo" == "" ]; then
    ERROR+=" $name"
  else
    [ "$debug" == 'y' ] && jsonpath="$DIR/" || jsonpath=""
    output=$(pyft_tool.py --wrapH --tree . --descTree ${jsonpath}desctree.json --logLevel error \
             $file $trans $transfo 2>&1)
    res=$?
    if [[ $file == *_before.F90 ]]; then
      #Transformation must execute without error and must give the same result as reference
      [ "$output" != "" ] && echo "$output"
      if [ $res -ne 0 ]; then
        ERROR+=" $name"
      else
        if [ "$update" == 'y' ]; then
          cp $trans $DIR/$ref
        else
          cmp $trans $ref
        fi
        if [ $? -ne 0 ]; then
          [ "$debug" == 'y' ] && cp $trans $DIR/
          FAIL+=" $name"
        fi
      fi
    elif [[ $file == *_checkOK.F90 ]]; then
      #Transformation must execute without error
      [ "$output" != "" ] && echo "$output"
      if [ $res -ne 0 ]; then
        FAIL+=" $name"
      fi
    elif [[ $file == *_checkKO.F90 ]]; then
      #Transformation must execute with error
      if [ $res -eq 0 ]; then
        FAIL+=" $name"
      fi
    fi
  fi
done

#Results
stat=0
if [ "$ERROR" != "" ]; then
  echo "Errors occurred on: $ERROR"
  stat=1
fi
if [ "$FAIL" != "" ]; then
  echo "Some tests failed: $FAIL"
  stat=$(($stat+2))
fi

exit $stat
