# These are started in reverse order so that the pipeline is mostly ready when the first document is retrieved.
sbt ";set ThisBuild/offline := true ;wmexchanger/runMain org.clulab.wm.wmexchanger.utils.HelloWorld"
./wmexchanger/loop_sbt_restProducer2.sh &
sleep 10
./wmexchanger/loop_sbt_restConsumer2.sh &
sleep 5
./wmexchanger/loop_sbt_kafkaConsumer2.sh &
sleep 5
# The container ends when eidos stops running.
./wmexchanger/loop_sbt_eidos2.sh

