baseDir=../corpora/loop

sbt "wmexchanger/runMain org.clulab.wm.wmexchanger.wmconsumer.RestConsumerLoopApp ${baseDir}/input/kafka ${baseDir}/input ${baseDir}/input/kafka/done"

