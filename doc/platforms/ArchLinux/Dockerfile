FROM base/archlinux

LABEL maintainer "yairchu@gmail.com"
LABEL Description="Docker image to check whether Lamdu successfuly builds on Arch Linux"

RUN pacman --sync --refresh

RUN pacman --noconfirm --sync git
RUN pacman --noconfirm --sync make
RUN pacman --noconfirm --sync stack
RUN pacman --noconfirm --sync rocksdb

# GHC dependency
RUN pacman --noconfirm --sync numactl

# GLFW dependencies
RUN pacman --noconfirm --sync mesa
RUN pacman --noconfirm --sync libxrandr
RUN pacman --noconfirm --sync libxi
RUN pacman --noconfirm --sync libxinerama
RUN pacman --noconfirm --sync libxcursor
RUN pacman --noconfirm --sync glew

# Install NodeJS to save time (so that it isn't built by stack build)
RUN pacman --noconfirm --sync nodejs

# Running `stack setup` and `stack build` seperately enables making
# better use of Docker's build cache when maintaining the Dockerfile
RUN stack setup --resolver ghc-8.2.1

RUN git clone --depth 1 https://github.com/lamdu/lamdu

WORKDIR "/lamdu"
RUN stack build -j1
