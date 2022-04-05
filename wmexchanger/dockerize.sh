rm -r ./wmexchanger/target/universal/*
# sbt "project wmexchanger" "dist"
cd ./wmexchanger/target/universal
unzip eidos-wmexchanger*.zip
mv eidos-wmexchanger*/bin eidos-wmexchanger*/lib .
mkdir eidos
mv ./lib/org.clulab.eidos-*.jar eidos
rm eidos-wmexchanger*.zip
rm -r eidos-wmexchanger* scripts
cd ../..
docker build -f ./Docker/DockerfileLoopDist2 -t clulab/eidos-dart .