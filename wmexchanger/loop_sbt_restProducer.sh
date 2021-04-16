baseDir=../corpora/loop

sbt "wmexchanger/runMain org.clulab.wm.wmexchanger.wmproducer.RestProducerLoopApp ${baseDir}/output ${baseDir}/output/done"

