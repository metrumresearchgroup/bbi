FROM 906087756158.dkr.ecr.us-east-1.amazonaws.com/nonmem-mpn:latest

# Setup user
COPY library-scripts/common-debian.sh /tmp/library-scripts/
RUN apt-get update && bash /tmp/library-scripts/common-debian.sh

ENV GOROOT=/usr/local/go \
    GOPATH=/go
ENV PATH=${GOPATH}/bin:${GOROOT}/bin:${PATH}
COPY library-scripts/go-debian.sh /tmp/library-scripts/
RUN apt-get update && rm /usr/local/bin/go && bash /tmp/library-scripts/go-debian.sh "latest" "${GOROOT}" "${GOPATH}"
