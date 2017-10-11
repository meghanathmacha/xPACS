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
cp breast* Part2/.
cd Part2
rm Train1/MFile*

echo "Running for CV 1"

subs=($(awk -F , '{ a[$9]++ } END { for (b in a) { gsub(/[ \t]+$/, "", b); print b } }' 'breasttrain1subs.csv'))
for i in "${subs[@]}"
do
	echo $i
	subid="$i"
	subid="$(echo -e "${subid}" | tr -d '[:space:]')"
	echo -e "subid='${subid}'"
  	matlab -nodisplay -nodesktop -r "calculateEllipsoidsSub("$subid",'breasttrain1.csv','breasttrain1subs.csv',1)" &
  	job_limit 50
done