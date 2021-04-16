set baseDir=../corpora/loop
set threads=4
set memory=-Xmx16g
set assembly=eidos-assembly-1.1.0-SNAPSHOT.jar:eidos-wmexchanger_2.12-1.1.0-SNAPSHOT.jar

java -cp ${assembly} org.clulab.wm.wmexchanger.wmconsumer.KafkaConsumerLoopApp app.topic=dart.cdr.streaming.updates app.outputDir=${baseDir}/input/kafka &
java -cp ${assembly} org.clulab.wm.wmexchanger.wmconsumer.RestConsumerLoopApp ${baseDir}/input/kafka ${baseDir}/input ${baseDir}/input/kafka/done &
java -cp ${assembly} org.clulab.wm.wmexchanger.wmproducer.RestProducerLoopApp ${baseDir}/output ${baseDir}/output/done &
# This doesn't work because wmexchanger can't be assembled properly.
java -cp ${assembly} org.clulab.wm.wmexchanger.wmeidos.EidosLoopAppJavaLauncher 4 ${baseDir}/input ${baseDir}/output ${baseDir}/input/done ${threads} ${memory}" &
