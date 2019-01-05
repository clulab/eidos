result=1
#app=org.clulab.wm.eidos.apps.ExitCode
app=org.clulab.wm.eidos.apps.FilteredExtractMetaFromDirectory
inputDir=../corpora/Doc500/txt
outputDir=../corpora/Doc500/jsonld-sh
metaDir=../corpora/Doc500/meta-sh
# Use 3/4 of processors, up to 1/2 of memory
# Amy 20 198
#memory=-Xmx99g
#threads=15
# Clara 72 264
#memory=-Xmx132g
#threads=54
# Jenny 48 264
#memory=-Xmx132g
#threads=36
# River 16 132
#memory=-Xmx66g
#threads=12
# Sager 8 32
memory=-Xmx16g
threads=6
# 2 is our special secret and may indicate disk full
while [ $result -ne 0 ] && [ $result -ne 2 ]; do
    java $memory -cp eidos-assembly-0.2.2-SNAPSHOT.jar $app $inputDir $outputDir $metaDir $threads
    result=$?
    echo
    echo "<<<<<<< Exit code was $result >>>>>>>"
    echo
    sleep 2m
done
