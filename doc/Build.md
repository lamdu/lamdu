# Building Lamdu from source

## General notes

### NodeJS

To drastically speed up Lamdu's installation under any OS, you can install
an appropriate version of NodeJS beforehand, such that `node` is in your `$PATH`. The version has to be at least 6.2.1 but below 8.0.0 (due to the removal of tail call optimization from node at version 8) <sup>1</sup>.

Enter `node -v` into terminal. If NodeJS is installed (and in your `$PATH`),
this will print your current version. If it isn't, you'll get an error.

If you do not install NodeJS, Lamdu's installation will build it from
source.

<sup>
**1. For Fedora Users:**
Fedora packages have very long names. This may lead to some confusion.
Consider `nodejs-1:6.11.2-1.fc25.x86_64`.
This example indicates a NodeJS version of `6.11`, plus a little.
The `-1:` is not a part of the version.
</sup>

## Platforms

### macOS

requires [brew](http://brew.sh/) and [git](https://git-scm.com/):

```shell
brew install leveldb haskell-stack
git clone --recursive https://github.com/lamdu/lamdu
cd lamdu
stack setup
~/.local/bin/lamdu
```

### ubuntu

Optional: Install NodeJS from node's apt repository:

```shell
curl -sL https://deb.nodesource.com/setup_6.x | sudo -E bash -
sudo apt-get install -y nodejs
```

requires [stack](https://github.com/commercialhaskell/stack/releases) (version 1.6.1 or above)

```shell
sudo apt-get update -qq
sudo apt-get install git zlib1g-dev libglew-dev libleveldb-dev -yq
sudo apt-get install libxrandr-dev libxi-dev libxcursor-dev libxinerama-dev -yq
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

Optional: Install NodeJS with `sudo dnf insall nodjs`.
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

requires [stack](https://github.com/commercialhaskell/stack/releases) (1.6.1 or above)

```shell
sudo pacman -S leveldb glfw libxrandr libxi libxcursor libxinerama
git clone --recursive https://github.com/lamdu/lamdu
cd lamdu
stack setup
stack install
~/.local/bin/lamdu

```

### nix (any linux distribution)

requires [Nix](https://nixos.org/nix/)

```shell
git clone --recursive https://github.com/lamdu/lamdu
cd lamdu
nix-env -f default.nix -iA lamdu
```

### Windows

Note: this instructions are work-in-progress and don't yet work :(

Install NodeJS via [NVM for Windows](https://github.com/coreybutler/nvm-windows) (a NodeJS distribution), and then run in Windows' `cmd.exe` shell:

    nvm install 7.10.1
    nvm use 7.10.1

Now, to build Lamdu, install [stack](https://haskellstack.org/). Then, find its bundled msys2 shell, at a location that looks like `C:\Users\$USERNAME\AppData\Local\Programs\stack\x86_64-windows\msys2-*\msys2.exe`, and run the `msys2 shell`. In the msys2 shell:

    export PATH=$PATH:/mingw64/bin:/c/Program\ Files/nodejs:/c/Users/$USER/AppData/Roaming/local/bin

    pacman -S git make mingw-w64-x86_64-{cmake,gcc}

    # Install LevelDB (a dependency)
    git clone https://github.com/fastogt/leveldb.git
    cd leveldb
    cmake -G "MSYS Makefiles" .
    make
    # Work around "make install" not working
    cp libleveldb.a /usr/lib
    cp -R include/* /usr/include/
    cd ..

    git clone https://github.com/lamdu/lamdu.git
    cd lamdu
    stack build

    stack exec lamdu
