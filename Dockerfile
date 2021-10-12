FROM ubuntu

RUN apt-get update
RUN apt-get install --yes curl python3 
RUN curl -L -O https://github.com/bazelbuild/bazelisk/releases/download/v1.10.1/bazelisk-linux-amd64 && mv bazelisk-linux-amd64 /usr/local/bin/bazelisk && chmod +x /usr/local/bin/bazelisk 
RUN USE_BAZEL_VERSION=a6e97a7e59e80bbe5dd4c63cf1c4f4802daa94ae bazelisk --version
RUN apt-get install --yes g++ make libtinfo5 git libgmp-dev
# RUN curl -sSL https://get.haskellstack.org/ | sh
