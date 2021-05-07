set baseDir=../corpora/loop
set threads=4
set memory=-Xmx16g

sbt "wmexchanger/runMain org.clulab.wm.wmexchanger.wmconsumer.KafkaConsumerLoopApp app.topic=dart.cdr.streaming.updates app.outputDir=${baseDir}/input/kafka" &
sbt "wmexchanger/runMain org.clulab.wm.wmexchanger.wmconsumer.RestConsumerLoopApp ${baseDir}/input/kafka ${baseDir}/input ${baseDir}/input/kafka/done" &
sbt "wmexchanger/runMain org.clulab.wm.wmexchanger.wmproducer.RestProducerLoopApp ${baseDir}/output ${baseDir}/output/done" &
# This doesn't work because sbt is not re-entrant, at least not so far.
sbt "wmexchanger/runMain org.clulab.wm.wmexchanger.wmeidos.EidosLoopAppLauncher wmexchanger 4 ${baseDir}/input ${baseDir}/output ${baseDir}/input/done ${threads} ${memory}" &


while ! sbt "wmexchanger/runMain org.clulab.wm.wmexchanger.wmeidos.EidosLoopApp ${baseDir}/input ${baseDir}/output ${baseDir}/input/done ${threads}"
do
  echo "It failed, so I'm trying again in 10 seconds."
  sleep 10
done

