FROM 906087756158.dkr.ecr.us-east-1.amazonaws.com/nonmem

COPY . /tmp/bbi
WORKDIR /tmp/bbi/cmd/bbi
RUN     GOOS=linux GOARCH=amd64 go build -o bbi main.go && \
        cp bbi /usr/local/bin

WORKDIR /tmp
RUN     git clone https://github.com/metrumresearchgroup/bbitest.git
WORKDIR /tmp/bbitest
ENTRYPOINT ""
CMD ["go","test","./...", "--json"]
