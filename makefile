ERL = erl
ERLC = erlc
ERLCFLAGS = -o
SRCDIR = src/erlang
BEAMDIR = build/erlang

all:
	mkdir -p $(BEAMDIR) ;
	$(ERLC) $(ERLCFLAGS) $(BEAMDIR) $(SRCDIR)/*.erl ;

run:
	${ERL} ${BEAMDIR} -run commstime commstime 0

clean:
	rm -rf $(BEAMDIR) ;
	rm -rf erl_crash.dump
