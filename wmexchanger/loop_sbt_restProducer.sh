baseDir=../corpora/loop

export _JAVA_OPTIONS=-Xmx2g
sbt ";set ThisBuild/offline := true ;wmexchanger/runMain org.clulab.wm.wmexchanger.wmproducer.RestProducerLoopApp ${baseDir}/output ${baseDir}/output/done"

