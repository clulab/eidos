#!/usr/bin/env bash

# These came from ../loop_sbt_configure.sh.  Values should be exported
# so that Java can access them except for values that are used directly.

default_base_dir="../corpora/loop"

# Coordinate servers
export REST_CONSUMER_SERVICE=https://${REST_HOSTNAME:-localhost}/dart/api/v1/cdrs
export REST_PRODUCER_SERVICE=https://${REST_HOSTNAME:-localhost}/dart/api/v1/readers/upload

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
export EIDOS_BASE_DIR="${EIDOS_BASE_DIR:-$default_base_dir}"
export EIDOS_INPUT_SUBDIR="${EIDOS_INPUT_SUBDIR:-input}"
export EIDOS_OUTPUT_SUBDIR="${EIDOS_OUTPUT_SUBDIR:-output}"
export DONE_SUBDIR="${DONE_SUBDIR:-done}"
export KAFKA_SUBDIR="${KAFKA_SUBDIR:-kafka}"

# Set up the pipeline.

# For kafka, input comes from the network.
export KAFKA_CONSUMER_OUTPUT_DIR="${KAFKA_CONSUMER_OUTPUT_DIR:-$EIDOS_BASE_DIR/$EIDOS_INPUT_SUBDIR/$KAFKA_SUBDIR}"

export REST_CONSUMER_INPUT_DIR="${REST_CONSUMER_INPUT_DIR:-$KAFKA_CONSUMER_OUTPUT_DIR}"
export REST_CONSUMER_OUTPUT_DIR="${REST_CONSUMER_OUTPUT_DIR:-$EIDOS_BASE_DIR/$EIDOS_INPUT_SUBDIR}"
export REST_CONSUMER_DONE_DIR="${REST_CONSUMER_DONE_DIR:-$REST_CONSUMER_INPUT_DIR/$DONE_SUBDIR}"

export EIDOS_INPUT_DIR="${EIDOS_INPUT_DIR:-$REST_CONSUMER_OUTPUT_DIR}"
export EIDOS_OUTPUT_DIR="${EIDOS_OUTPUT_DIR:-$EIDOS_BASE_DIR/$EIDOS_OUTPUT_SUBDIR}"
export EIDOS_DONE_DIR="${EIDOS_DONE_DIR:-$EIDOS_INPUT_DIR/$DONE_SUBDIR}"

# For the rest producer, output goes to the network.
export REST_PRODUCER_INPUT_DIR="${REST_PRODUCER_INPUT_DIR:-$EIDOS_OUTPUT_DIR}"
export REST_PRODUCER_DONE_DIR="${REST_PRODUCER_DONE_DIR:-$REST_PRODUCER_INPUT_DIR/$DONE_SUBDIR}"

echo "Starting Eidos"

SLEEP=30
DEFAULT_THREADS=4
DEFAULT_EIDOS_MEMORY="-Xmx20g"

THREADS="${EIDOS_THREADS:-$DEFAULT_THREADS}"
EIDOS_MEMORY="${EIDOS_MEMORY:-DEFAULT_EIDOS_MEMORY}"

export _JAVA_OPTIONS=-Xmx1g ; ./bin/rest-producer-loop-app "${REST_PRODUCER_INPUT_DIR}" "${REST_PRODUCER_DONE_DIR}" &
sleep 10
export _JAVA_OPTIONS=-Xmx1g ; ./bin/rest-consumer-loop-app "${REST_CONSUMER_INPUT_DIR}" "${REST_CONSUMER_OUTPUT_DIR}" "${REST_CONSUMER_DONE_DIR}" &
sleep 5
export _JAVA_OPTIONS=-Xmx512m ; ./bin/kafka-consumer-loop-app &
sleep 5

export _JAVA_OPTIONS=${DEFAULT_EIDOS_MEMORY}
export JAVA_OPTS=${DEFAULT_EIDOS_MEMORY}

# The container ends when eidos stops running.
while ! ./bin/eidos-loop-app "${EIDOS_INPUT_DIR}" "${EIDOS_OUTPUT_DIR}" "${EIDOS_DONE_DIR}" "${THREADS}"
do
  echo "It failed, so I am trying again in ${SLEEP} seconds."
  sleep ${SLEEP}
done
