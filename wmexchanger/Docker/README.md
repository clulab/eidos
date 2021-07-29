# Eidos


## Docker for wmexchanger

This directory contains multiple docker files that work alone or in tandem to produce slightly different images that can be chosen based on need.

* One-stage built and run with `sbt` in image
  * DockerfileLoop
* Two-stage built with `sbt` in image, run from `Java` (Two Six version)
  * DockerfileBaseDependencies
  * DockerfileLoopMultiStage
* Two-stage built with `sbt` in image, run from `Java` (CLU Lab version)
  * DockerfileLoopBase
  * DockerfileLoopEidos
* One-stage built with `sbt` on host, run from `Java`
  * DockerfileLoopDist

### One-stage built and run with sbt in image

`DockerfileLoop` runs the entire build within the image, which takes a long time but does not require local `Scala` development tools.  The resulting image is larger then necessary because all the build tools are included.  Startup is also slower because `sbt` is used to call `Java`.

`DockerfileLoop` is meant to be built relative to the `Docker` directory so that it doesn't need to index all the files in the main directory that won't be transferred to the image.  The build command is therefore something like
```shell
$ cd wmexchanger
$ docker build -f ./Docker/DockerfileLoop -t clulab/eidos-loop ../Docker
```

### Two-stage built with `sbt` in image, run from `Java` (Two Six version)

This was contributed by Two Six and should remain here mostly unchanged so that it can continue to be used in their system.

`DockerfileBaseDependencies` copies files from the local build and then also runs `sbt update` in the image to produce a base image

`DockerfileLoopMultiStage` copies the files again to pick up any changes and further runs `sbt dist` in the image to generate the programs that need to run

The two files form a pair in which the first is used periodically to make a first approximation of the final image and the second accounts for any differences that have accrued in since then.  `git` is not called within the image, so the current files should be on the local computer, the correct branch should be checked out, the commits all staged, etc.  It would be good to remove extraneous files, like those produced by `sbt assembly`, so that they don't find their way to an image.  Typical commands to build the image are
```shell
$ cd wmexchanger
$ docker build -f ./Docker/DockerfileBaseDependencies -t eidos:base ..
$ docker build -f ./Docker/DockerfileLoopMultiStage -t clulab/eidos-stream ..
```

### Two-stage built with `sbt` in image, run from `Java` (CLU Lab version)

The files here are based on the design by Two Six, described above, but they have been further refined.

`DockerfileLoopBase` copies files from the local build and then also runs `sbt update` in the image to produce a base image

`DockerfileLoopEidos` copies the files again to pick up any changes and further runs `sbt dist` in the image to generate the programs that need to run


The two files form a pair in which the first is used periodically to make a first approximation of the final image and the second accounts for any differences that have accrued in since then.  `git` is not called within the image, so the current files should be on the local computer, the correct branch should be checked out, the commits all staged, etc.  It would be good to remove extraneous files, like those produced by `sbt assembly`, so that they don't find their way to an image.  Typical commands to build the image are
```shell
$ cd wmexchanger
$ docker build -f ./Docker/DockerfileLoopBase -t eidos:base ..
$ docker build -f ./Docker/DockerfileLoopEidos -t clulab/eidos-stream ..
```

### One-stage built with `sbt` on host, run from `Java`

`DockerfileLoopDist` copies files already prepared on the host to an image that runs `bash` and `Java`.  No special development tools are otherwise added to the image.  `sbt` needs to be installed and run on the host.  The commands might look like
```shell
$ sbt "project wmexchanger" "dist"
$ cd ./wmexchanger/target/universal
$ unzip wmexchanger*.zip
$ mv wmexchanger*/bin wmexchanger*/lib .
$ cd ../..
$ docker build -f ./Docker/DockerfileLoopDist -t clulab/eidos-loop .
```

## Parameters

Quite a few parameters need to be passed to the `wmexchanger` programs and most of them start as environment variables which are then referenced in configuration files or fotwarded to scripts as command line parameters.  See particularly those used in `export.sh`.  Once set, they can be included in the `docker run` command, but more typically they are used with `docker-compose up`.  This series of commands often does the job:
```shell
$ source ./export.sh
$ export EIDOS_PASSWORD=<EidosPassword>
$ docker-compose -f ./Docker/docker-compose-eidos.yml up
```
