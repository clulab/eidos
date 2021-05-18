# Eidos Docker

## webapp

This directory contains several Dockerfiles which generate containers that run the Eidos web application and web service.

* Dockerfile - This should match the next and remains for compatability purposes
* DockerfileRun - This runs the webapp through sbt in development mode
* DockerfileRunProd - This runs the webapp through sbt in production mode
* DockerfileStage - This runs the webapp directly in production mode

From this directory you can build the container with:

```
docker build -f <Dockerfile> . -t eidos-webservice
```

You can run the container with:

```
docker run -id -p 9000:9000 eidos-webservice
```

This launches the container and exposes port 9000. You can then navigate to `localhost:9000` to access the web application and may submit requests to `localhost:9000/process_text` as described [here](https://github.com/clulab/eidos#web-service).

### Configuration
Currently, this container is built with grounding activated (`useW2V = true`) and Google's pre-trained vectors are used, not GloVe.


## wmexchanger

A different set of Dockerfiles is used to create the image for the wmexchanger subproject:

* DockerfileLoop - This runs the entire build within the image, which takes a long time but does not require local Scala development tools
* DockerfileBaseDependencies - This version copies files from the local build and then also runs `sbt update` in the image to produce a base image
* DockerfileLoopMultiStage - This copies the files again to pick up any changes and further runs `sbt dist` in the image to generate the programs that need to run

`DockerfileLoop` is meant to be build relative to the `Docker` directory so that it doesn't need to index all the files in the main directory that won't be transferred to the image.  The build command is therefore something like
```shell
docker build -f ./Docker/DockerfileLoop ./Docker -t clulab/eidos-loop
```

The two other files form a pair in which the first is used periodically to make a first approximation of the final image and the second accounts for any differences that have accrued in since then.  `git` is not called within the image, so the current files should be on the local computer, the correct branch should be checked out, the commits all staged, etc.  It would be good to remove extraneous files, like those produced by `sbt assembly`, so that they don't find their way to an image.  Typical commands to build the image are
```shell
docker build -f ./Docker/DockerfileBaseDependencies . -t eidos:base
docker build -f ./Docker/DockerfileLoopMultiStage . -t clulab/eidos-stream
```
