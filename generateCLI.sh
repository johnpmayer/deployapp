#!/bin/sh

# set -x

cat Queries.hs | grep 'Query ::' | while read line
do
    
    funcName=`echo $line | sed 's/Query ::.*$//'`
    nParams=`echo $line | awk -F '->' '{print NF - 1}'`
    
    echo $funcName
    echo "Usage: $nParams parameters: $line"
    
    appendArgs=""
    count=1
    
    while [ $count -le $nParams ]
    do
        appendArgs+=' $'$count
        count=`expr $count + 1 `
    done
    
    echo $appendArgs

    echo "echo \"${funcName}Query $appendArgs\" | ghci Queries.hs" > scripts/$funcName
    
done
