#!/bin/sh

# set -x

cat Queries.hs | grep 'Query ::' | while read line
do
    
    funcName=`echo $line | sed 's/Query ::.*$//'`
    nParams=`echo $line | awk -F '->' '{print NF - 1}'`

    appendArgs=""
    count=1
    
    while [ $count -le $nParams ]
    do
        appendArgs+=' $'$count
        count=`expr $count + 1 `
    done
    
    tmpFile="${funcName}.output"
    scriptFile=scripts/$funcName
    >$scriptFile
    echo "#!/bin/sh" >>$scriptFile
#    echo "set -x" >>$scriptFile
    echo "#Usage: $nParams parameters: $line" >>$scriptFile
    expr="${funcName}Query $appendArgs >>= (writeFile \\\"$tmpFile\\\") . show"
    echo "ghc Queries.hs -e \"$expr\" 1>&2" >> $scriptFile
    echo "cat $tmpFile" >>$scriptFile
    echo "echo" >> $scriptFile
    echo "rm $tmpFile" >>$scriptFile
    chmod +x $scriptFile
    
done
