# Building Lamdu from source

## General notes

### NodeJS

To drastically speed up Lamdu's installation under any OS, you can install an appropriate version of NodeJS beforehand, such that `node` is in your `$PATH`. The version has to be at least 6.2.1.

Enter `node -v` into terminal. If NodeJS is installed (and in your `$PATH`), this will print your current version. If it isn't, you'll get an error.

If you do not install NodeJS, Lamdu's installation will build it from source.

## Platforms

### macOS

requires an x86_64 [brew](http://brew.sh/) setup and [git](https://git-scm.com/):

```shell
arch -arch x86_64 /usr/local/bin/brew install rocksdb haskell-stack
git clone --recursive https://github.com/lamdu/lamdu
cd lamdu
stack setup
stack install
~/.local/bin/lamdu
```

### ubuntu

Then to install lamdu - requires [stack](https://github.com/commercialhaskell/stack/releases) (version 1.6.1 or above):

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

If the above fails at `stack setup` or `stack install`, it may because stack is older than 1.6.1. To upgrade stack, run the following commands:

```shell
stack upgrade
hash -r
```

NOTE: `~/.local/bin` should be in your `$PATH` for the upgraded `stack` to take effect.

### fedora

Optional: Install NodeJS with `sudo dnf insall nodejs`.
Please see the starred note under "NodeJS & Build Time".

requires [stack](https://github.com/commercialhaskell/stack/releases) (1.6.1 or above)

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

If the above fails at `stack setup` or `stack install`, it may because stack is older than 1.6.1. To upgrade stack, run the following commands:

```shell
stack upgrade
hash -r
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

requires [Nix](https://nixos.org/nix/)

```shell
git clone --recursive https://github.com/lamdu/lamdu
cd lamdu
nix-env -f default.nix -iA lamdu
```

### Windows

Install:

* [git](https://git-scm.com/)
* [stack](https://haskellstack.org/)
* [NodeJS](https://nodejs.org/en/)

Then run:

    git clone https://github.com/lamdu/lamdu.git

#### Workaround bindings-freetype-gl problem

(Todo: the underlying problem should be fixed and this section should subsequently be removed)

    git clone https://github.com/lamdu/bindings-freetype-gl
    cd bindings-freetype-gl
    stack exec bash prepare_submodule.sh
    cd ..

In `lamdu/stack.yaml`, add `'../bindings-freetype-gl'` as an `extra-dep`

#### Build Lamdu

    cd lamdu
    stack exec pacman -S mingw-w64-x86_64-rocksdb
    stack build

If the installation of RocksDB fails due to signature verification, consider the work-around in https://github.com/msys2/MSYS2-packages/issues/2343#issuecomment-780121556
