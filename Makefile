.PHONY: build start stop clean healthcheck delete

DOCKER := $(shell command -v docker 2> /dev/null)

PORT := 8080:8080

clean:
	rm -rf ./files ./var

healthcheck:
ifndef DOCKER
	@echo "Docker isn't installed!!! please install docker : https://www.docker.com/products/container-runtime"
endif
	@echo "everything is good"

build: clean ./Dockerfile
	sudo docker build -t gmihtt/server .

start: ./Dockerfile
	sudo docker run -dp $(PORT) -it --name run-server --rm gmihtt/server

stop: ./Dockerfile
	sudo docker stop run-server

delete: clean ./Dockerfile
	sudo docker rm run-server


