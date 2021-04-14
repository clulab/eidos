set baseDir=../corpus/loop
set threads=4
set memory=-Xmx16g
set java_memory=-J%memory%
set eidos_java_options=%memory% -Dfile.encoding=UTF-8

sbt "wmexchanger/runMain org.clulab.wm.wmexchanger.wmconsumer.KafkaConsumerLoopApp app.topic=dart.cdr.streaming.updates app.outputDir=%baseDir%/input/kafka" &
sbt "wmexchanger/runMain org.clulab.wm.wmexchanger.wmconsumer.RestConsumerLoopApp %baseDir%/input/kafka %baseDir%/input %baseDir%/input/kafka/done" &
sbt "wmexchanger/runMain org.clulab.wm.wmexchanger.wmproducer.RestProducerLoopApp %baseDir%/output %baseDir%/output/done" &

REM sbt %java_memory% "wmexchanger/runMain org.clulab.wm.wmexchanger.wmeidos.EidosLoopApp %baseDir%/input %baseDir%/output %baseDir%/input/done %threads%" &
set _JAVA_OPTIONS=%eidos_java_options%
sbt "wmexchanger/runMain org.clulab.wm.wmexchanger.wmeidos.EidosLoopApp %baseDir%/input %baseDir%/output %baseDir%/input/done %threads%" &
