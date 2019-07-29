BUILD=`date +%FT%T%z`
LDFLAGS=-ldflags "-X main.buildTime=${BUILD}"
MAKE_HOME=${PWD}


install:
	cd cmd/bbi; go install ${LDFLAGS}
