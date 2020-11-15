# Dockerfile
FROM ubuntu

COPY . /usr/mainServer
RUN apt -y update && apt -y upgrade
RUN apt-get install -y curl
RUN curl -sSL https://get.haskellstack.org/ | sh
# RUN ./mainServer-exe
