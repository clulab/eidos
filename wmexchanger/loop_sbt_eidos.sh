sleep=30

while ! ./wmexchanger/loop_sbt_eidos_once.sh
do
  echo "It failed, so I am trying again in ${sleep} seconds."
  sleep ${sleep}
done
