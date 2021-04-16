baseDir=../corpora/loop
threads=4
memory=-Xmx16g
sleep=30

export _JAVA_OPTIONS=${memory}
while ! sbt "wmexchanger/runMain org.clulab.wm.wmexchanger.wmeidos.EidosLoopApp ${baseDir}/input ${baseDir}/output ${baseDir}/input/done ${threads}"
do
  echo "It failed, so I am trying again in ${sleep} seconds."
  sleep ${sleep}
done

