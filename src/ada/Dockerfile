FROM ubuntu
LABEL mainter="k.chalmers@napier.ac.uk"
RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install -y gnat
WORKDIR /tmp
COPY *.adb /tmp/
RUN mkdir results
RUN gnatmake commstime.adb -o commstime
RUN gnatmake selecttime.adb -o selecttime
RUN gnatmake montecarlo.adb -o montecarlo
ENV test=commstime
CMD "./${test}"
