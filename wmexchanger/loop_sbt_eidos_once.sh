baseDir=../corpora/loop

default_threads=4
default_memory=-Xmx20g

threads="${eidos_threads:-$default_threads}"
memory="${eidos_memory:-$default_memory}"

echo threads is ${threads}
echo memory is ${memory}

export _JAVA_OPTIONS=${memory}
sbt ";set ThisBuild/offline := true ;wmexchanger/runMain org.clulab.wm.wmexchanger.wmeidos.EidosLoopApp ${baseDir}/input ${baseDir}/output ${baseDir}/input/done ${threads}"
