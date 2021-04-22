baseDir=../corpora/loop

sbt ";set ThisBuild/offline := true ;wmexchanger/runMain org.clulab.wm.wmexchanger.wmproducer.RestProducerLoopApp ${baseDir}/output ${baseDir}/output/done"

