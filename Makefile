BUILD=`date +%FT%T%z`
LDFLAGS=-ldflags "-X main.buildTime=${BUILD}"
MAKE_HOME=${PWD}


install:
	cd cmd/bbi; go install ${LDFLAGS}

VT_TEST_RUNNERS = scripts/run-unit-tests
VT_TEST_RUNNERS += scripts/run-integration-tests
include internal/valtools/rules.mk
