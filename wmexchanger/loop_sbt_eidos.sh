baseDir=../corpora/loop
sleep=30

default_threads=4
default_memory=-Xmx20g

threads="${eidos_threads:-$default_threads}"
memory="${eidos_memory:-$default_memory}"

echo threads is ${threads}
echo memory is ${memory}

export _JAVA_OPTIONS=${memory}
while ! sbt "wmexchanger/runMain org.clulab.wm.wmexchanger.wmeidos.EidosLoopApp ${baseDir}/input ${baseDir}/output ${baseDir}/input/done ${threads}"
do
  echo "It failed, so I am trying again in ${sleep} seconds."
  sleep ${sleep}
done
