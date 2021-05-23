./loop_sbt_configure.sh

echo REST_PRODUCER_INPUT_DIR is ${REST_PRODUCER_INPUT_DIR}
echo REST_PRODUCER_DONE_DIR is ${REST_PRODUCER_DONE_DIR}

export _JAVA_OPTIONS=-Xmx1g
sbt ";set ThisBuild/offline := true ;wmexchanger/runMain org.clulab.wm.wmexchanger.wmproducer.RestProducerLoopApp"
