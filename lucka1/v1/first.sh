#! /bin/bash

agg=0
i=0

while read -r line; do
    Numbers=$(echo "$line" | sed 's/[a-Z]*//g')
    i=$(($i+1))
    First=$(echo ${Numbers:0:1})
    Last=$(echo ${Numbers:(-1):1})
    Result=$(($First*10+$Last))
    agg=$(($agg+$Result))

    echo "$line - $Result"
done < 1-input.txt
echo "$agg"