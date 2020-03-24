# Build stage 0
FROM amazonlinux:latest

RUN yum install -y git \
    gcc make automake autoconf libreadline-dev \
    libncurses-dev libssl-dev libyaml-dev \
    libxslt-dev libffi-dev libtool unixodbc-dev \
    unzip curl \
    openssl-devel ncurses-devel \
    libiodbc

RUN amazon-linux-extras install java-openjdk11

RUN git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.7.7 && \
    . $HOME/.asdf/asdf.sh && \
    asdf plugin add erlang && \
    asdf install erlang 22.3 && \
    asdf global erlang 22.3

# Set working directory
RUN mkdir /buildroot
WORKDIR /buildroot

# Copy our Erlang test application
COPY . coronerl
RUN chmod +x /buildroot/coronerl/rebar3
ENV PATH $PATH:/buildroot/coronerl

# And build the release
WORKDIR coronerl
RUN rebar3 as prod release

# Build stage 1
FROM amazonlinux:latest

# Install the released application
COPY --from=0 /buildroot/coronerl/_build/prod/rel/coronerl /coronerl

# Expose relevant ports
EXPOSE 8080
EXPOSE 8443

CMD ["/coronerl/bin/coronerl", "foreground"]