baseDir=../corpora/loop
sleep=30

# These are fallback positions in case the environment variables aren't set.
threads=4
memory=-Xmx20g

export _JAVA_OPTIONS=${memory}
while ! sbt "wmexchanger/runMain org.clulab.wm.wmexchanger.wmeidos.EidosLoopApp ${baseDir}/input ${baseDir}/output ${baseDir}/input/done ${threads}"
do
  echo "It failed, so I am trying again in ${sleep} seconds."
  sleep ${sleep}
done

