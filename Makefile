.PHONY: build buildApp start stop healthcheck

DOCKER := $(shell command -v docker 2> /dev/null)

healthcheck:
ifndef DOCKER
	@echo "Docker isn't installed!!! please install docker : https://www.docker.com/products/container-runtime"
endif
	@echo "everything is good"

build: ./Dockerfile
	sudo docker build -t gmihtt/main_server .

buildApp: ./docker-compose.yml
	sudo docker-compose build

start: ./docker-compose.yml
	sudo docker-compose up -d

stop: ./docker-compose.yml
	sudo docker-compose down


