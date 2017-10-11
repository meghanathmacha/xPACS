#!/bin/sh
job_limit () {
    # Test for single positive integer input
    if (( $# == 1 )) && [[ $1 =~ ^[1-9][0-9]*$ ]]
    then

        # Check number of running jobs
        joblist=($(jobs -rp))
        while (( ${#joblist[*]} >= $1 ))
        do

            # Wait for any job to finish
            command='wait '${joblist[0]}
            for job in ${joblist[@]:1}
            do
                command+=' || wait '$job
            done
            eval $command
            joblist=($(jobs -rp))
        done
   fi
}

cd Part2
rm Train1/DFile*

echo "Decision of 1"

subs=($(awk -F , '{ a[$10]++ } END { for (b in a) { gsub(/[ \t]+$/, "", b); print b } }' 'selectpacks1.csv'))
for i in "${subs[@]}"
do
	echo $i
	subid="$i"
	subid="$(echo -e "${subid}" | tr -d '[:space:]')"
	echo -e "subid='${subid}'"
  	matlab -nodisplay -nodesktop -r "calculateDecisionSubs("$subid",'breasttrain1.csv','selectpacks1.csv','breasttrain1.csv',1)" &
  	job_limit 50
done