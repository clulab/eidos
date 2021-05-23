./loop_sbt_configure.sh

default_threads=4
default_memory=-Xmx20g

EIDOS_THREADS="${EIDOS_THREADS:-$default_threads}"
EIDOS_MEMORY="${EIDOS_MEMORY:-$default_memory}"

echo EIDOS_INPUT_DIR is ${EIDOS_INPUT_DIR}
echo EIDOS_OUTPUT_DIR is ${EIDOS_OUTPUT_DIR}
echo EIDOS_DONE_DIR is ${EIDOS_DONE_DIR}
echo EIDOS_THREADS is ${EIDOS_THREADS}
echo EIDOS_MEMORY is ${EIDOS_MEMORY}

export _JAVA_OPTIONS=${memory}
sbt ";set ThisBuild/offline := true ;wmexchanger/runMain org.clulab.wm.wmexchanger.wmeidos.EidosLoopApp"
