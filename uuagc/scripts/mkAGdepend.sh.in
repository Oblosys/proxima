#!/bin/bash

# script mkAGdepend borrowed from the Generic Haskell build system

# special purpose version of mkAGdepend to be used by GH build system
#
# mkAGdepend <target path> <source path> <source files>

SED="sed"
GREP="grep"

if test "$1" == "."; then
    pretarget=
else 
    pretarget="$1/"
fi
presource=$2/
shift 2

calcdeps () {
    depfiles=$(${GREP} '^INCLUDE' $3 | ${SED} -e "s/INCLUDE \"\(.*\)\"/\1/g")
    for dep in ${depfiles}; do
      if [ -f $2/$dep ]; then
        echo "$1 : $2/$dep"
        calcdeps "$1" "$2" "$2/$dep"
      fi
    done
}    

for file in ${*/#/$presource}; do

    dpre=$(dirname ${file})
    dpost=${pretarget}$(basename $file) # hacked 05.01.2003
    target=$(echo ${dpost} | ${SED} -e 's/l\?ag$/hs/g') # was file 05.01.2003
#    etarget=$(echo ${target} | ${SED} -e 's/\//\\\//g')
#    epath=$(echo ${dpre} | ${SED} -e 's/\//\\\//g')
#    echo file: $file presource: $presource
#    echo dpre: $dpre dpost: $dpost target: $target
#    echo etarget: $etarget epath: $epath
#    ${GREP} '^INCLUDE' ${file} | ${SED} -e "s/INCLUDE \"\(.*\)\"/${etarget} : ${epath}\/\1/g";
    calcdeps ${target} ${dpre} ${file}

done
