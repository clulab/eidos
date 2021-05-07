IMAGE_PREFIX = docker.causeex.com/dart
IMAGE_NAME = eidos
IMG := $(IMAGE_PREFIX)/$(IMAGE_NAME)

ifndef CI_COMMIT_REF_NAME
	APP_VERSION := "latest"
else ifeq ("$(CI_COMMIT_REF_NAME)", "master")
	APP_VERSION := "latest"
else
	APP_VERSION := $(shell cat version.sbt | cut -d\" -f2 | cut -d '-' -f1)
endif

docker-build:
	docker build -f ./Docker/DockerfileLoopMultiStage -t $(IMG):$(APP_VERSION) .

docker-push: docker-build
	docker push $(IMG):$(APP_VERSION)
