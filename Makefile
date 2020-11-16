.PHONY: build buildApp start stop pull healthcheck

DOCKER := $(shell command -v docker 2> /dev/null)

healthcheck:
ifndef DOCKER
	@echo "Docker isn't installed!!! please install docker : https://www.docker.com/products/container-runtime"
endif
	@echo "everything is good"

build: ./Dockerfile
	sudo docker build -t gmihtt/multir_server .

buildApp: ./docker-compose.yml
	sudo docker-compose build

start: ./docker-compose.yml
	sudo docker-compose up -d

pull: ./docker-compose.yml
	sudo docker pull gmihtt/server
	sudo docker pull gmihtt/multir_server
	sudo docker pull gmihtt/server8081
	sudo docker pull mongo
	sudo docker pull redis

stop: ./docker-compose.yml
	sudo docker-compose down


