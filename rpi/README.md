# happy flowers Setup Guide

The following guide documents the steps to setting up the Haskell environment for happy flowers to work in.

## Table of Contents

1. [Requirements](#requirements)
2. [Setting Up the RPi](#setting-up-the-rpi)
3. [Setting Up a Cabal Sandbox](#setting-up-a-cabal-sandbox)
4. [Setting Up SQLite](#setting-up-sqlite)
5. [Configuration](#configuration)
6. [Cabal Tasks](#cabal-tasks)

## Requirements

To use this project you will need the following minimal requirements.

| Technology | Version  |
| ---------- | -------- |
| GHC        | 7.10.3   |
| cabal      | 1.22.9.0 |
| sqlite     | 3.14.0   |

## Setting Up the RPi

- As a basis for the setup, a standard Raspbian (jessie) installation is used and needs to be stored on an SD card. This is either preinstalled or part of the default RPi NOOBS. The RPi should now allow connection over SSH.

- Use `raspi-config` to enlarge the SD card partition and set the video / cpu memory split.

```ini
enlarge partition
memory split 16
```

- Update package resources using `sudo apt-get update`.

- Install a recent GHC binary. The binary needs to be built locally since the version offered by Debian is outdated. ARMv7 binaries are available on the [official Haskell site](https://goo.gl/4g1Ck1). The unpacked archive can then be built using `./configure` and `sudo make install`.

- GMP needs to be installed using `sudo apt-get install libgmp-dev`. This enables the use of `ghci`.

- The GHC compiler requires LLVM (>= 3.5). The easiest way is to download it from the [official site](https://goo.gl/VZj7b3) and copy the unpacked archive to `/usr/local/`.

- Just like with GHC, the latest available version of Cabal in Debian is outdated. This means that another custom installation is required. For this it is needed to clone the Cabal GitHub repository using `git clone https://github.com/haskell/cabal.git`. The cabal-install  directory contains an executable called bootstrap.sh which is only available on releases. Using `git checkout cabal-install-v1.22.9.0` and `EXTRA_CONFIGURE_OPTS="" ./bootstrap.sh`, Cabal is installed and made ready.

- In some cases the Cabal binary needs to be added to the path using `PATH="$HOME/.cabal/bin/:$PATH"`. All package sources can then be updated using `cabal update`.

## Setting Up a Cabal Sandbox

In order to set up the cabal sandbox, you have to run `cabal sandbox init` in the according folder.
Then you can run `cabal install` to install the packages, when installing `HPi` you have to pass the `--extra-lib-dirs` and `--extra-include-dirs` flags.

## Setting Up SQLite

To install SQLite on your specific OS you can follow the instructions [on this site](https://www.tutorialspoint.com/sqlite/sqlite_installation.htm).

- Create the database. Navigate to the `rpi` folder and run `sqlite3 happyflowers.db`.
- Create the necessary tables using `.read happyflowers.sql`.
- Insert data for application settings: `INSERT INTO settings (name, upper, lower, interval) VALUES ("...", 80, 40, 60);`.

## Configuration

The configuration is based on a file called `rpi.cfg` inside the `rpi` directory. It contains settings that change the behaviour of the application. The default configuration expects a `password` entry and a `frame` entry as follows:

```ini
password=1234
frame=14
```

The password is used for user authentication and the frame determines the timeframe for which historical data is displayed on the front end.

## Cabal Tasks

- `cabal install` installs all required dependencies.
- `cabal build` builds the project.
- `cabal run` builds and runs the project.
- `cabal haddock --executables` generates documentation using Haddock.

All tasks read the `rpi.cabal` config and are based off the `rpi` executable.
