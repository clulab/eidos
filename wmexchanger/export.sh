export KAFKA_CONSUMER_BOOTSTRAP_SERVERS=wm-ingest-pipeline-streaming-1.prod.dart.worldmodelers.com:9093
export KAFKA_APP_TOPIC=dart.cdr.streaming.updates
export REST_CONSUMER_SERVICE=https://uat-ingest-pipeline-rest-1.prod.dart.worldmodelers.com/dart/api/v1/cdrs
export REST_PRODUCER_SERVICE=https://uat-ingest-pipeline-rest-1.prod.dart.worldmodelers.com/dart/api/v1/readers/upload

export EIDOS_VERSION=julyEmbed
export ONTOLOGY_VERSION=2.2

export EIDOS_USERNAME=eidos
# export EIDOS_PASSWORD=

#export EIDOS_BASE_DIR=../corpora/julyEmbed/61538
export EIDOS_BASE_DIR=../corpora/julyEmbed/534
