FROM dukeofubuntu/nonmem-go
LABEL Description="Container for Babylon execution"
#default to master
ARG BBI_BRANCH=master

WORKDIR /babylon
RUN     git clone https://github.com/metrumresearchgroup/babylon.git && \
        cd babylon && \
        git checkout ${BBI_BRANCH} && \
        cd cmd/bbi && \
        go build -o bbi main.go && \
        mv bbi /usr/local/bin

ENTRYPOINT ["bbi"]