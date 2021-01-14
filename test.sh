#!/bin/bash

#Write out the nonmem file to current dir
echo "$NONMEM_LICENSE"
echo "$NONMEM_LICENSE" > /opt/NONMEM/nm74gf/license/nonmem.lic

#Clone operations
git clone https://github.com/metrumresearchgroup/bbitest.git
cd bbitest

go mod download


#Run the Non Parallel Tests
go test -run TestBbiCompletesLocalExecution

#Sge basic tests
#go test -run TestBbiCompletesSGEExecution


#Install and setup bbi
