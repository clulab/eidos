# Copy this file to the main eidos directory so that the paths match up.
# These are started in reverse order so that the pipeline is mostly ready when the first document is retrieved.
./wmexchanger/loop_sbt_restProducer.sh &
sleep 10
./wmexchanger/loop_sbt_eidos.sh &
sleep 5
./wmexchanger/loop_sbt_restConsumer.sh &
sleep 5
./wmexchanger/loop_sbt_kafkaConsumer.sh &
read -p "Press enter to stop the container. "
