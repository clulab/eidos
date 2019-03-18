FROM ubuntu:16.04

# Install base packages
RUN apt-get update && apt-get install -y \
    build-essential \
    software-properties-common \ 
    dialog \
    git \
    graphviz \
    libgraphviz-dev \
    vim

# Install Java
RUN add-apt-repository -y ppa:webupd8team/java
RUN echo "oracle-java8-installer shared/accepted-oracle-license-v1-1 select true" | debconf-set-selections
RUN apt-get update && apt-get install -y \
    oracle-java8-installer 
RUN export JAVA_HOME="/usr/lib/jvm/java-8-oracle/"
RUN export PATH="$JAVA_HOME/bin:$PATH"    

# Install Scala
WORKDIR /
RUN apt-get remove scala-library scala
RUN wget http://scala-lang.org/files/archive/scala-2.12.7.deb
RUN dpkg -i scala-2.12.7.deb
RUN apt-get update && apt-get install scala
RUN apt-get install apt-transport-https -y
RUN echo "deb https://dl.bintray.com/sbt/debian /" |  tee -a /etc/apt/sources.list.d/sbt.list
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
RUN apt-get update && apt-get install sbt -y

# Clone the Eidos repo
RUN git clone https://github.com/clulab/eidos.git
WORKDIR /eidos
RUN wget https://s3.amazonaws.com/world-modelers/data/vectors.txt
RUN mv vectors.txt src/main/resources/org/clulab/wm/eidos/english/w2v/
RUN sed -i 's/useW2V = false/useW2V = true/' src/main/resources/eidos.conf
RUN sed -i 's/useW2V = false/useW2V = true/' src/main/resources/reference.conf
RUN sed -i 's/glove.840B.300d.txt/vectors.txt/' src/main/resources/eidos.conf
RUN sbt assembly

# Run Web Service
EXPOSE 9000
ENTRYPOINT ["sbt", "webapp/run"]
