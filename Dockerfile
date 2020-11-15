# Dockerfile
FROM ubuntu

RUN apt -y update && apt -y upgrade
RUN apt-get install -y curl
RUN curl -sSL https://get.haskellstack.org/ | sh

COPY . /usr/mainServer

RUN cd usr/mainServer && stack build

WORKDIR /usr/mainServer
CMD ["stack", "exec", "mainServer-exe"]
