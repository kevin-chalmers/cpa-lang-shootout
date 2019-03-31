FROM ubuntu
LABEL maintainer="k.chalmers@napier.ac.uk"
RUN apt-get update
RUN apt-get install wget -y
RUN wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
RUN apt-get install ./erlang-solutions_1.0_all.deb -y
RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install erlang -y
