# Building Lamdu from source

## General notes

### NodeJS

To drastically speed up Lamdu's installation under any OS, you can install an appropriate version of NodeJS beforehand, such that `node` is in your `$PATH`. The version has to be at least 6.2.1.

Enter `node -v` into terminal. If NodeJS is installed (and in your `$PATH`), this will print your current version. If it isn't, you'll get an error.

If you do not install NodeJS, Lamdu's installation will build it from source.

## Platforms

### macOS

requires [brew](http://brew.sh/) and [git](https://git-scm.com/):

```shell
brew install leveldb haskell-stack
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
sudo apt-get install git nodejs zlib1g-dev libglew-dev libleveldb-dev -yq
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
sudo dnf install -y libXinerama-devel leveldb-devel glew-devel zlib-devel
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
sudo pacman -Sy leveldb libxrandr libxi libxcursor libxinerama stack make tar gcc awk libxxf86vm mesa mesa-demos
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
* [msys2](http://msys2.org/)
* [NodeJS](https://nodejs.org/en/)

In the Windows `cmd.exe` shell:

    cd <WORK-DIR>
    git clone https://github.com/fastogt/leveldb.git
    git clone https://github.com/lamdu/lamdu.git

In the msys2 mingw64 shell:

    pacman -S mingw-w64-x86_64-{cmake,make,gcc}

    cd <WORK-DIR>/leveldb
    cmake -G "MinGW Makefiles" .
    mingw32-make

In the Windows `cmd.exe` shell:

    cd <WORK-DIR>/lamdu
    stack build --extra-lib-dirs="%cd%"\..\leveldb --extra-include-dirs="%cd%"\..\leveldb\include
    stack exec lamdu
