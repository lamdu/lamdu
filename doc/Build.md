# Building Lamdu from source

## General notes

### NodeJS

To drastically speed up Lamdu's installation under any OS, you can install an appropriate version of NodeJS beforehand, such that `node` is in your `$PATH`. The version has to be at least 6.2.1.

Enter `node -v` into terminal. If NodeJS is installed (and in your `$PATH`), this will print your current version. If it isn't, you'll get an error.

If you do not install NodeJS, Lamdu's installation will build it from source.

## Platforms

### macOS

Using [haskell (stack)](https://www.haskell.org/downloads/), [homebrew](http://brew.sh/) and [git](https://git-scm.com/) (prerequirements):

```shell
brew install rocksdb
git clone --recursive https://github.com/lamdu/lamdu
cd lamdu
stack setup
stack install
~/.local/bin/lamdu
```

### ubuntu

Lamdu requires [stack](https://github.com/commercialhaskell/stack/releases) (version 1.6.1 or above):

To check your version of stack, use:

```shell
stack --version
```

and make sure that the version is at least 1.6.1. If stack is older than 1.6.1, you will need to upgrade stack. If you need to upgrade stack, use:

```shell
stack upgrade
hash -r
```

To install lamdu, run the following:

```shell
sudo apt-get update -qq
sudo apt-get install git nodejs zlib1g-dev libglew-dev librocksdb-dev -yq
sudo apt-get install libxxf86vm-dev libxrandr-dev libxi-dev libxcursor-dev libxinerama-dev -yq
git clone --recursive https://github.com/lamdu/lamdu
cd lamdu
stack setup
stack install
~/.local/bin/lamdu
```

NOTE: `~/.local/bin` should be in your `$PATH` for the upgraded `stack` to take effect.


### fedora

Optional: Install NodeJS with `sudo dnf insall nodejs`.
Please see the starred note under "NodeJS & Build Time".

Lamdu requires [stack](https://github.com/commercialhaskell/stack/releases) (version 1.6.1 or above):

To check your version of stack, use:

```shell
stack --version
```

and make sure that the version is at least 1.6.1. If stack is older than 1.6.1, you will need to upgrade stack. If you need to upgrade stack, use:

```shell
stack upgrade
hash -r
```
To install lamdu, run the following:

```shell
sudo dnf install -y gcc gcc-c++ gmp-devel libXrandr-devel libXi-devel
sudo dnf install -y libXcursor-devel mesa-libGL-devel libGLU-devel
sudo dnf install -y libXinerama-devel rocksdb-devel glew-devel zlib-devel
git clone --recursive https://github.com/lamdu/lamdu
cd lamdu
stack setup
stack install
~/.local/bin/lamdu
```

NOTE: `~/.local/bin` should be in your `$PATH` for the upgraded `stack` to take effect.

### arch linux

```shell
sudo pacman -Sy rocksdb libxrandr libxi libxcursor libxinerama stack make tar gcc awk libxxf86vm mesa mesa-demos
git clone --recursive https://github.com/lamdu/lamdu
cd lamdu
LD_PRELOAD=/usr/lib/libtcmalloc.so stack build
stack exec -- lamdu
```

### nix (any linux distribution)

Requires [Nix](https://nixos.org/nix/) and [flakes](https://nixos.wiki/wiki/flakes) to be enabled.

```shell
git clone --recursive https://github.com/lamdu/lamdu
cd lamdu
nix build .#Lamdu:exe:lamdu
```

### Windows

Install:

* [git](https://git-scm.com/)
* [stack](https://haskellstack.org/)
* [NodeJS](https://nodejs.org/en/)

Then run:

```shell
git clone https://github.com/lamdu/lamdu.git
cd lamdu
stack exec -- pacman -S mingw-w64-x86_64-rocksdb
stack build
```

If the installation of RocksDB fails due to signature verification, consider the work-around in https://github.com/msys2/MSYS2-packages/issues/2343#issuecomment-780121556
