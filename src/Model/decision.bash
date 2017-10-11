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
rm Train2/DFile*
rm Train3/DFile*
rm Train4/DFile*
rm Train5/DFile*
rm Train1/TFile*
rm Train2/TFile*
rm Train3/TFile*
rm Train4/TFile*
rm Train5/TFile*

echo "Decision of 1"

subs=($(awk -F , '{ a[$10]++ } END { for (b in a) { gsub(/[ \t]+$/, "", b); print b } }' 'selectpacks1.csv'))
for i in "${subs[@]}"
do
	echo $i
	subid="$i"
	subid="$(echo -e "${subid}" | tr -d '[:space:]')"
	echo -e "subid='${subid}'"
  	matlab -nodisplay -nodesktop -r "calculateDecisionSubs("$subid",'breasttrain1.csv','selectpacks1.csv','breastvalid1.csv',1)" &
  	job_limit 50
done
echo "Running for CV 2"
subs=($(awk -F , '{ a[$10]++ } END { for (b in a) { gsub(/[ \t]+$/, "", b); print b } }' 'selectpacks2.csv'))
for i in "${subs[@]}"
do
	echo $i
	subid="$i"
	subid="$(echo -e "${subid}" | tr -d '[:space:]')"
	echo -e "subid='${subid}'"
  	matlab -nodisplay -nodesktop -r "calculateDecisionSubs("$subid",'breasttrain2.csv','selectpacks2.csv','breastvalid2.csv',2)" &
  	job_limit 50
done


echo "Running for CV 3"
subs=($(awk -F , '{ a[$10]++ } END { for (b in a) { gsub(/[ \t]+$/, "", b); print b } }' 'selectpacks3.csv'))
for i in "${subs[@]}"
do
	echo $i
	subid="$i"
	subid="$(echo -e "${subid}" | tr -d '[:space:]')"
	echo -e "subid='${subid}'"
  	matlab -nodisplay -nodesktop -r "calculateDecisionSubs("$subid",'breasttrain3.csv','selectpacks3.csv','breastvalid3.csv',3)" &
  	job_limit 50
done

echo "Running for CV 4"
subs=($(awk -F , '{ a[$10]++ } END { for (b in a) { gsub(/[ \t]+$/, "", b); print b } }' 'selectpacks4.csv'))
for i in "${subs[@]}"
do
	echo $i
	subid="$i"
	subid="$(echo -e "${subid}" | tr -d '[:space:]')"
	echo -e "subid='${subid}'"
  	matlab -nodisplay -nodesktop -r "calculateDecisionSubs("$subid",'breasttrain4.csv','selectpacks4.csv','breastvalid4.csv',4)" &
  	job_limit 50
done

echo "Running for CV 5"

subs=($(awk -F , '{ a[$10]++ } END { for (b in a) { gsub(/[ \t]+$/, "", b); print b } }' 'selectpacks5.csv'))
for i in "${subs[@]}"
do
	echo $i
	subid="$i"
	subid="$(echo -e "${subid}" | tr -d '[:space:]')"
	echo -e "subid='${subid}'"
  	matlab -nodisplay -nodesktop -r "calculateDecisionSubs("$subid",'breasttrain5.csv','selectpacks5.csv','breastvalid5.csv',5)" &
  	job_limit 50
done
