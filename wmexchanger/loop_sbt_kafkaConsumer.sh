./loop_sbt_configure.sh

KAFKA_APP_TOPIC="${KAFKA_APP_TOPIC:-dart.cdr.streaming.updates}"
KAFKA_APP_OUTPUT_DIR="${KAFKA_APP_OUTPUT_DIR:-$KAFKA_CONSUMER_OUTPUT_DIR}"

# These are the variables the program actually uses.
echo KAFKA_APP_TOPIC is ${KAFKA_APP_TOPIC}
echo KAFKA_APP_OUTPUT_DIR is ${KAFKA_APP_OUTPUT_DIR}

export _JAVA_OPTIONS=-Xmx512m
sbt ";set ThisBuild/offline := true ;wmexchanger/runMain org.clulab.wm.wmexchanger.wmconsumer.KafkaConsumerLoopApp"
