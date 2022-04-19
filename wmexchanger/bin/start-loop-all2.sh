#!/usr/bin/env bash

# These came from ../loop_sbt_configure.sh.  Values should be exported
# so that Java can access them except for values that are used directly.

default_base_dir="../corpora/corpus"

# Coordinate servers
export REST_CONSUMER_DOCUMENT_SERVICE=${REST_CONSUMER_DOCUMENT_SERVICE:-http://localhost/dart/api/v1/cdrs}
export REST_CONSUMER_ONTOLOGY_SERVICE=${REST_CONSUMER_ONTOLOGY_SERVICE:-http://localhost/dart/api/v1/ontologies}
export REST_PRODUCER_SERVICE=${REST_PRODUCER_SERVICE:-http://localhost/dart/api/v1/readers/upload}

# Coordinate credentials.
export REST_CONSUMER_USERNAME="${EIDOS_USERNAME}"
export REST_CONSUMER_PASSWORD="${EIDOS_PASSWORD}"
export REST_PRODUCER_USERNAME="${EIDOS_USERNAME}"
export REST_PRODUCER_PASSWORD="${EIDOS_PASSWORD}"
export CURL_PRODUCER_USERNAME="${EIDOS_USERNAME}"
export CURL_PRODUCER_PASSWORD="${EIDOS_PASSWORD}"
export KAFKA_USERNAME="${EIDOS_USERNAME}"
export KAFKA_PASSWORD="${EIDOS_PASSWORD}"

# Coordinate directories.
EIDOS_BASE_DIR="${EIDOS_BASE_DIR:-$default_base_dir}"
EIDOS_INPUT_SUBDIR="${EIDOS_INPUT_SUBDIR:-input}"
EIDOS_OUTPUT_SUBDIR="${EIDOS_OUTPUT_SUBDIR:-output}"
DONE_SUBDIR="${DONE_SUBDIR:-done}"
KAFKA_SUBDIR="${KAFKA_SUBDIR:-kafka}"

DOCUMENT_SUBDIR="${DOCUMENT_SUBDIR:-documents}"
ONTOLOGY_SUBDIR="${ONTOLOGY_SUBDIR:-ontologies}"
READING_SUBDIR="${READING_SUBDIR:-readings}"

# Set up the pipeline.

# For kafka, input comes from the network.
export KAFKA_CONSUMER_OUTPUT_DIR="${KAFKA_CONSUMER_OUTPUT_DIR:-$EIDOS_BASE_DIR/$EIDOS_INPUT_SUBDIR/$KAFKA_SUBDIR}"

REST_CONSUMER_INPUT_DIR="${REST_CONSUMER_INPUT_DIR:-$KAFKA_CONSUMER_OUTPUT_DIR}"
REST_CONSUMER_OUTPUT_DIR="${REST_CONSUMER_OUTPUT_DIR:-$EIDOS_BASE_DIR/$EIDOS_INPUT_SUBDIR}"
REST_CONSUMER_DONE_DIR="${REST_CONSUMER_DONE_DIR:-$REST_CONSUMER_INPUT_DIR/$DONE_SUBDIR}"

EIDOS_INPUT_DIR="${EIDOS_INPUT_DIR:-$REST_CONSUMER_OUTPUT_DIR}"
EIDOS_OUTPUT_DIR="${EIDOS_OUTPUT_DIR:-$EIDOS_BASE_DIR/$EIDOS_OUTPUT_SUBDIR}"
EIDOS_DONE_DIR="${EIDOS_DONE_DIR:-$EIDOS_INPUT_DIR/$DONE_SUBDIR}"

REST_PRODUCER_INPUT_DIR="${REST_PRODUCER_INPUT_DIR:-$EIDOS_OUTPUT_DIR}"
REST_PRODUCER_OUTPUT_DIR="${REST_PRODUCER_OUTPUT_DIR:-$EIDOS_BASE_DIR}"
REST_PRODUCER_DONE_DIR="${REST_PRODUCER_DONE_DIR:-$REST_PRODUCER_INPUT_DIR/$DONE_SUBDIR}"

DOCUMENT_DIR="${DOCUMENT_DIR:-$EIDOS_BASE_DIR/$DOCUMENT_SUBDIR}"
ONTOLOGY_DIR="${ONTOLOGY_DIR:-$EIDOS_BASE_DIR/$ONTOLOGY_SUBDIR}"
READING_DIR="${READING_DIR:-$EIDOS_BASE_DIR/$READING_SUBDIR}"

echo "Starting Eidos"

SLEEP=30
DEFAULT_THREADS=4
DEFAULT_EIDOS_MEMORY="-Xmx20g"

THREADS="${EIDOS_THREADS:-$DEFAULT_THREADS}"
MEMORY="${EIDOS_MEMORY:-$DEFAULT_EIDOS_MEMORY}"

export _JAVA_OPTIONS=-Xmx1g ; ./bin/rest-producer-loop-app-2 -Dlogback.configurationFile=restProducer.xml "${REST_PRODUCER_INPUT_DIR}" "${REST_PRODUCER_OUTPUT_DIR}" "${REST_PRODUCER_DONE_DIR}" &
sleep 10
export _JAVA_OPTIONS=-Xmx1g ; ./bin/rest-consumer-loop-app-2 -Dlogback.configurationFile=restConsumer.xml "${REST_CONSUMER_INPUT_DIR}" "${REST_CONSUMER_OUTPUT_DIR}" "${REST_CONSUMER_DONE_DIR}" "${DOCUMENT_DIR}" "${ONTOLOGY_DIR}" "${READING_DIR}" &
sleep 5
export _JAVA_OPTIONS=-Xmx512m ; ./bin/kafka-consumer-loop-app-2 -Dlogback.configurationFile=kafkaConsumer.xml &
sleep 5

export _JAVA_OPTIONS=${MEMORY}
export JAVA_OPTS=${MEMORY}

# The container ends when eidos stops running.
while ! ./bin/eidos-loop-app-2 -Dlogback.configurationFile=eidos.xml "${EIDOS_INPUT_DIR}" "${EIDOS_OUTPUT_DIR}" "${EIDOS_DONE_DIR}" "${DOCUMENT_DIR}" "${ONTOLOGY_DIR}" "${READING_DIR}" "${THREADS}"
do
  echo "It failed, so I am trying again in ${SLEEP} seconds."
  sleep ${SLEEP}
done
