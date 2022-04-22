REBAR3_URL=https://s3.amazonaws.com/rebar3/rebar3
RELEASE=coronerl-${VSN}
SERVER_DIR=/home/bruce/coronerl/rel
SERVER_URL=ec2-3-72-11-195.eu-central-1.compute.amazonaws.com
SERVER_USER=bruce

ifeq ($(wildcard rebar3),rebar3)
REBAR3 = $(CURDIR)/rebar3
endif

REBAR3 ?= $(shell test -e `which rebar3` 2>/dev/null && which rebar3 || echo "./rebar3")

ifeq ($(REBAR3),)
REBAR3 = $(CURDIR)/rebar3
endif

.PHONY: deps test build

all: build test docs

build: $(REBAR3)
	@$(REBAR3) compile

$(REBAR3):
	wget $(REBAR3_URL) || curl -Lo rebar3 $(REBAR3_URL)
	@chmod a+x rebar3

deps:
	@$(REBAR3) get-deps

clean:
	@$(REBAR3) clean

clean-all:
	rm -rf $(CURDIR)/_build

distclean: clean
	@$(REBAR3) delete-deps

docs:
	@$(REBAR3) edoc

test:
#	@$(REBAR3) do ct, cover
	@$(REBAR3) eunit

release: test
	@$(REBAR3) as prod release

# Packs source code into tar, scp to EC2 instance,
# then runs the release script located at /scripts/release
# on EC2
deploy:
	@:$(call check_defined, VSN)
	mv $(CURDIR)/_build ..
	cd .. && tar -cvzf ${RELEASE}.tar coronerl
	cd .. && scp ${RELEASE}.tar ${SERVER_USER}@${SERVER_URL}:${SERVER_DIR}
	cd .. && rm ${RELEASE}.tar
	mv ../_build .
	ssh amazon-linux-bruce "${SERVER_DIR}/release ${VSN}"

# https://stackoverflow.com/questions/10858261/abort-makefile-if-variable-not-set
# Check that given variables are set and all have non-empty values,
# die with an error otherwise.
#
# Params:
#   1. Variable name(s) to test.
#   2. (optional) Error message to print.
check_defined = \
    $(strip $(foreach 1,$1, \
        $(call __check_defined,$1,$(strip $(value 2)))))
__check_defined = \
    $(if $(value $1),, \
      $(error You must define the $1$(if $2, ($2)) variable))