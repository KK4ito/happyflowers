# happy flowers Setup Guide

The following guide documents the steps to setting up the Haskell environment and Raspberry Pi ('RPi') for happy flowers to work in.

## Table of Contents

1. [Requirements](#requirements)
2. [Setting Up the RPi](#setting-up-the-rpi)
3. [RPi Dependencies](#rpi-dependencies)
3. [Cabal Sandbox](#cabal-sandbox)
4. [SQLite](#sqlite)
5. [Configuration](#configuration)
6. [Cabal Tasks](#cabal-tasks)

## Requirements

To use this project you will need the following minimal requirements.

| Technology | Version  |
| ---------- | -------- |
| GHC        | 7.10.3   |
| Raspbian   | 8.0      |
| cabal      | 1.22.9.0 |
| sqlite     | 3.14.0   |

## Setting Up the RPi

The Raspberry Pi is based on an ARM architecture. The problem is that Haskell is not supported very well on these systems so a lot of additional configuration is needed. The following guide is mostly adapted from [this tutorial](https://github.com/blitzcode/hue-dashboard#raspberry-pi) by [Tim C. Schröder](https://github.com/blitzcode).

- As a basis for the setup, a standard Raspbian (jessie) installation is used and needs to be stored on an SD card. This is either preinstalled or part of the default RPi NOOBS. The RPi should now allow connection over SSH.

- Use `sudo raspi-config` to enlarge the SD card partition and set the video / cpu memory split.

```ini
enlarge partition
memory split 16
```

- Update package sources using `sudo apt-get update`.

- Install a recent GHC binary. The binary needs to be built locally since the version offered by Debian is outdated. ARMv7 binaries are available from the [official Haskell site](https://goo.gl/4g1Ck1).

```bash
wget http://downloads.haskell.org/~ghc/7.10.3/ghc-7.10.3-armv7-deb8-linux.tar.xz
tar xf ghc-7.10.3-armv7-deb8-linux.tar.xz
cd ghc-7.10.3
./configure
sudo make install
```

- Install GMP using `sudo apt-get install libgmp-dev`. This enables the use of `ghci`.

- The GHC compiler requires LLVM (>= 3.5). The easiest way is to download it from the [official site](https://goo.gl/VZj7b3).

```bash
wget http://llvm.org/releases/3.5.2/clang+llvm-3.5.2-armv7a-linux-gnueabihf.tar.xz
tar xf clang+llvm-3.5.2-armv7a-linux-gnueabihf.tar.xz
sudo cp -r clang+llvm-3.5.2-armv7a-linux-gnueabihf/* /usr/local/
```

- Just like with GHC, the latest available version of Cabal in Debian is outdated. This means that another custom installation is required. To do this, clone the Cabal GitHub repository.

```bash
git clone https://github.com/haskell/cabal.git
git checkout cabal-install-v1.22.9.0
cd cabal-install-v1.22.9.0
EXTRA_CONFIGURE_OPTS="" ./bootstrap.sh
```

- In some cases the Cabal binary needs to be added to the path using `PATH="$HOME/.cabal/bin/:$PATH"`. All package sources can then be updated using `cabal update`.

- Stack still does not support ARM systems natively so for the purpose of this project it was decided that Cabal would be sufficient.

By following all these steps you get a working Haskell environment you can use to run the happy flowers project.

## RPi Dependencies

Enable I2C on the RPi in order for the sensor to be able to read data from the moisture sensor.

- Open the RPi settings using `sudo raspi-config`.
- Select `9 Advanced Options`.
- Select `A7 I2C`.
- Confirm with `Yes` and `OK`.
- Save changes with `Finish`.

Change the baudrate as it too high by default.

- Append `dtparam=i2c1_baudrate=3814` to `/boot/config.txt`.

The `HPi` package that enables GPIO communication between the Haskell code and the RPi requries the bcm2835 C library. Install it using the following commands.

```bash
wget http://www.open.com.au/mikem/bcm2835/bcm2835-1.50.tar.gz
tar xvfz bcm2835-1.50.tar.gz
cd bcm2835-1.50
./configure
make
sudo make install
```

## Setting Up a Cabal Sandbox

In order to provide a clean and isolated environment this project uses a cabal sandbox.

- Run `cabal sandbox init`.
- Run `cabal install --extra-lib-dirs=/usr/local/lib/ --extra-include-dirs=/usr/local/include/`. The two flags are required for installing `HPi`.

Depdendencies are locked using `cabal freeze`. The locked versions are noted in the `cabal.config` file.

## Setting Up SQLite

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

The build is automatically configured based on whether it is built on a Debian ARM architecture or on another architecture. A Debian ARM system is considered to be the RPi, thus access to sensors is considered possible. All other systems use mocking functions to simulate the hardware communication.

- `cabal install` installs all required dependencies.
- `cabal build` builds the project.
- `cabal run` builds and runs the project.
- `cabal haddock --executables` generates documentation using Haddock.

All tasks read the `rpi.cabal` config and are based off the `rpi` executable.
