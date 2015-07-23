.PHONY = all,debug,clean,profile

OPTS = -use-ocamlfind
OPTOPTS = $(OPTS) -ocamlopt 'ocamlopt -inline 20'
POPTOPTS = $(OPTS) -ocamlopt 'ocamlopt -p -g'
PROFDIR = profile

TIME := $(shell date +"%Y.%m.%d-%H.%M.%S")

# If the first argument is "profile"...
ifeq (profile,$(firstword $(MAKECMDGOALS)))
  # use the rest as arguments for "run"
  PROF_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  # ...and turn them into do-nothing targets
  $(eval $(PROF_ARGS):;@:)
endif

all: cli.native
	rm cli.native
	rm -f cli
	ln -s _build/src/cli.native cli

cli.d.byte:
	ocamlbuild $(OPTS) cli.d.byte

cli.native:
	ocamlbuild $(OPTOPTS) cli.native

cli.p.native:
	ocamlbuild $(POPTOPTS) cli.p.native

profile: clean cli.p.native cli.native
	@echo First run profiles for time.
	./cli.p.native $(PROF_ARGS)
	if [ ! -d $(PROFDIR) ]; then mkdir $(PROFDIR); fi
	gprof cli.p.native > $(PROFDIR)/$(TIME).gprof
#@echo Second run profiles for space.
#valgrind --tool=massif --massif-out-file=$(PROFDIR)/$(TIME).massif ./cli.native $(PROF_ARGS)

debug: clean cli.d.byte

clean:
	rm -rf cli \#*\# *\~ *.cm* *.out *.native *.byte *.opt _build *.dot *.jpg parser.ml parser.mli lexer.ml *.pdf


