# happy flowers Setup Guide

## Requirements

To use this project you will need the following minimal requirements.
GHC 7.10.3
cabal 1.22.9.0
sqlite 3.14.0

## Setup RPi

- As a basis for the setup, a standard Raspbian (jessie) installation is used and needs to be stored on an SD card. This is either preinstalled or part of the default RPi NOOBS. The RPi should now allow connection over SSH.

- Use raspi-config to enlarge the SD card partition and set the video/cpu memory split.

```
enlarge partition
memory split 16
```

- Update package resources using `sudo apt-get update`.

- Install a recent GHC binary. The binary needs to be built locally since the version offered by Debian is outdated. ARMv7 binaries are available on the official Haskell site (https://goo.gl/4g1Ck1). The unpacked archive can then be built using `./configure` and `sudo make install`.

- GMP needs to be installed using `sudo apt-get install libgmp-dev`. This enables the use of ghci.

- The GHC compiler requires LLVM (>= 3.5). The easiest way is to download it from the official site (https://goo.gl/VZj7b3) and copy the unpacked archive to `/usr/local/`.

- Just like with GHC, the latest available version of Cabal in Debian is outdated. This means that another custom installation is required. For this it is needed to clone the Cabal GitHub repository using `git clone https://github.com/haskell/cabal.git`. The cabal-install  directory contains an executable called bootstrap.sh which is only available on releases. Using `git checkout cabal-install-v1.22.9.0` and `EXTRA_CONFIGURE_OPTS="" ./bootstrap.sh`, Cabal is installed and made ready.

- In some cases the Cabal binary needs to be added to the path using `PATH="$HOME/.cabal/bin/:$PATH"`. All package sources can then be updated using `cabal update`.

## Setup cabal sandbox

In order to set up the cabal sandbox, you have to run `cabal sandbox init` in the according folder.
Then you can run `cabal install` to install the packages, when installing HPi you have to pass the `--extra-lib-dirs` and `--extra-include-dirs` flags.

## Setup sqlite

To install sqlite on your specific OS you can follow the instructions on this site: https://www.tutorialspoint.com/sqlite/sqlite_installation.htm
We created this project on macOS and we did not have to install it separately as it already was installed.

First you need to create the database, to do that navigate in the terminal window to the according folder and run `sqlite3 happyflowers.db`
Then you can insert the necessary tables by running `.read happyflowers.sql`
Then you have to configure the settings by running `INSERT INTO SETTINGS VALUES (name,80,40,60);`
The first parameter name is for the name of the flower, the second is for the upper limit, the third for the lower limit and the last parameter is for the interval.

## Configuration

For the configuration of the password you have to create a rpi.cfg file containing the password as follows `password=abc`.
In this configuration file you can also specify the frame for how many days of data you want as follows `frame=140`, this means it gets the data for the last 140 days.

## build / run

In order to run the Haskell Code you will have to first run `cabal build` and then run `cabal run` to start the program.

## Haddock

To generate the documentation you can run `cabal haddock --executables`. This will generate documentation for all packages that are part of an executable package.
