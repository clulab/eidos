baseDir=../corpora/loop

export _JAVA_OPTIONS=-Xmx2g
sbt ";set ThisBuild/offline := true ;wmexchanger/runMain org.clulab.wm.wmexchanger.wmconsumer.KafkaConsumerLoopApp app.topic=dart.cdr.streaming.updates app.outputDir=${baseDir}/input/kafka"

