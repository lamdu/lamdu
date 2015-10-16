# Project Lamdu

[![Join the chat at https://gitter.im/lamdu/lamdu](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/lamdu/lamdu?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

This project aims to create a "next-generation", "live programming" environment that radically improves the programming experience.

See the [Main Page](http://peaker.github.io/lamdu/)


## Installation

#### osx

requires [stack](https://github.com/commercialhaskell/stack/releases), [brew](http://brew.sh/) and [git](https://git-scm.com/):

```shell
brew install ftgl leveldb
git clone --recursive https://github.com/lamdu/lamdu
cd lamdu
stack setup
stack install

```

#### ubuntu

requires [stack](https://github.com/commercialhaskell/stack/releases)
```shell
sudo apt-get update -qq
sudo apt-get install git libftgl-dev libleveldb-dev libglfw-dev libxrandr-dev libxi-dev libxcursor-dev libxinerama-dev -yq
git clone --recursive https://github.com/lamdu/lamdu
cd lamdu
stack setup
stack install

```
