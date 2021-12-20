IMAGE_PREFIX = eidos
IMAGE_NAME = eidos
IMG := $(IMAGE_PREFIX)/$(IMAGE_NAME)

docker-build:
	docker build -f ./wmexchanger/Docker/DockerfileLoopMultiStage -t $(IMG):$(APP_VERSION) .

docker-push: docker-build
	docker push $(IMG):$(APP_VERSION)
