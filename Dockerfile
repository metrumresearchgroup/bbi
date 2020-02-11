FROM 906087756158.dkr.ecr.us-east-1.amazonaws.com/nonmem

COPY . /tmp/babylon
WORKDIR /tmp/babylon/cmd/bbi
RUN     GOOS=linux GOARCH=amd64 go build -o bbi main.go && \
        cp bbi /usr/local/bin

WORKDIR /tmp
RUN     git clone https://github.com/metrumresearchgroup/babylontest.git
WORKDIR /tmp/babylontest
ENTRYPOINT ""
CMD ["go","test","./...", "--json"]
