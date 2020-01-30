#!/bin/bash

#Write out the nonmem file to current dir
echo "${NONMEM_LICENSE}" | base64 -d > nonmem.lic
cp nonmem.lic /opt/NONMEM/nm74gf/license/nonmem.lic

#Clone operations
git clone https://github.com/metrumresearchgroup/babylontest.git
cd babylontest

go mod download


#Run the Non Parallel Tests
go test -run TestBabylonCompletesLocalExecution

#Sge basic tests
go test -run TestBabylonCompletesSGEExecution