# Run this before loop_sbt_configure.sh or docker-compose.

# This one is for production.
export KAFKA_HOSTNAME=wm-ingest-pipeline-streaming-1.prod.dart.worldmodelers.com
# This one is for testing.
# export KAFKA_HOSTNAME=uat-ingest-pipeline-streaming-1.prod.dart.worldmodelers.com

export KAFKA_CONSUMER_BOOTSTRAP_SERVERS=${KAFKA_HOSTNAME:-localhost}:9093
export KAFKA_APP_TOPIC=dart.cdr.streaming.updates

# This one is for production.
export REST_HOSTNAME=wm-ingest-pipeline-rest-1.prod.dart.worldmodelers.com
# This one is for testing.
# export REST_HOSTNAME=uat-ingest-pipeline-rest-1.prod.dart.worldmodelers.com

export EIDOS_VERSION=julyEmbed1
export ONTOLOGY_VERSION=2.3

export EIDOS_USERNAME=eidos
# Add this elsewhere.
# export EIDOS_PASSWORD=

export EIDOS_BASE_DIR=../corpora/julyEmbed
