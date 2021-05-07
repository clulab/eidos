baseDir=../corpora/loop

export _JAVA_OPTIONS=-Xmx1g
sbt ";set ThisBuild/offline := true ;wmexchanger/runMain org.clulab.wm.wmexchanger.wmconsumer.RestConsumerLoopApp ${baseDir}/input/kafka ${baseDir}/input ${baseDir}/input/kafka/done"
