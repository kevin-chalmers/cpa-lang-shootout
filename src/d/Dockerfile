FROM ubuntu
LABEL mainter="k.chalmers@napier.ac.uk"
RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install gdc -y
WORKDIR /tmp
COPY *.d /tmp/
RUN mkdir results
RUN gdc -O3 commstime.d -o commstime
RUN gdc -O3 selecttime.d -o selecttime
RUN gdc -O3 montecarlo.d -o montecarlo
ENV test=commstime
CMD "./${test}"
