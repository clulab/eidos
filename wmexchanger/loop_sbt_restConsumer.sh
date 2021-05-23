./loop_sbt_configure.sh

echo REST_CONSUMER_INPUT_DIR is ${REST_CONSUMER_INPUT_DIR}
echo REST_CONSUMER_OUTPUT_DIR is ${REST_CONSUMER_OUTPUT_DIR}
echo REST_CONSUMER_DONE_DIR is ${REST_CONSUMER_DONE_DIR}

export _JAVA_OPTIONS=-Xmx1g
sbt ";set ThisBuild/offline := true ;wmexchanger/runMain org.clulab.wm.wmexchanger.wmconsumer.RestConsumerLoopApp"
