.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

# Modules to compile/include
MODS = 

all: compile

compile: ${MODS:%=%.beam}

# With subdirs, use:
#compile: ${MODS:%=%.beam} subdirs
#
#subdirs:
#	cd dir1; $(MAKE)
#	cd dir2; $(MAKE)

# Add special compilation requirements here
#special.beam: special.erl
#	${ERL} -Dflag1 -W0 special.erl

# Run application from makefile
#application: compile
#	${ERL} -pa Dir1 -s application start Arg1 Arg2

clean:
	rm -rf *.beam erl_crash.dump
	#cd dir1; $(MAKE) clean
	#cd dir2; $(MAKE) clean
