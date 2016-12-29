1. [Requirements](#requirements)
2. [Setting Up the RPi](#setting-up-the-rpi)
3. [Setting Up a WiFi Connection](#setting-up-a-wifi-connection)
4. [Headless RPi](#headless-rpi)
5. [RPi Dependencies](#rpi-dependencies)

## Requirements

To use this project you will need the following minimal requirements.

| Technology     | Minimal Version     |
| -------------- | ------------------- |
| Raspberry Pi   | 3 Model B           |
| Raspbian       | 8.0 (jessie)        |
| bcm2835        | 1.50                |
| Motion         | 3.2.12              |
| Chirp!         | 2.7.1 (Sensor Mode) |
| USB Webcam     | n/a                 |
| USB Water Pump | n/a                 |

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

## Setting Up a WiFi Connection

The RPi can be configured to automatically connect to an available WiFi network on startup. In order to do this, edit the `wpa-supplicant` configuration:

```bash
sudo nano /etc/wpa_supplicant/wpa_supplicant.conf
```

Include the following lines:

```
network={
    ssid="Your_SSID"
    psk="Your_password"
}
```

More detailed information about connection options is available on the [Raspberry Pi website](https://www.raspberrypi.org/documentation/configuration/wireless/wireless-cli.md).

## Headless RPi

For this to work, the RPi needs to be connected to a local network and the accessing device needs to be on the same network. This means, that for development purposes, the internet connection of a computer could be shared with the RPi so that they are automatically on the same network.

In order to connect to the device, one needs to find the local IP of the RPi. There are multiple approaches to finding this address, e.g. `arp -a` in a UNIX terminal. The next step is connecting via SSH using `ssh pi@<ip>` with the RPi’s password, by default `raspberry`. The device is usually aliased as `raspberrypi.local` so that finding the IP can be circumvented in many cases.

A more detailed version of this tutorial is available [on the Raspberry Pi forums](https://www.raspberrypi.org/forums/viewtopic.php?t=74176).

## RPi Dependencies

### I2C

Enable I2C on the RPi in order for the sensor to be able to read data from the moisture sensor.

- Open the RPi settings using `sudo raspi-config`.
- Select `9 Advanced Options`.
- Select `A7 I2C`.
- Confirm with `Yes` and `OK`.
- Save changes with `Finish`.

Change the baudrate as it too high by default.

- Append `dtparam=i2c1_baudrate=3814` to `/boot/config.txt`.

A collection of tools to manage the I2C functionality can be installed with `sudo apt-get install i2c-tools`. This installs a binary called `i2cdetect`, which can be used to list all active I2C connections using `i2cdetect -y 1`.

### bcm2835

The `HPi` package that enables GPIO communication between the Haskell code and the RPi requries the bcm2835 C library. Install it using the following commands.

```bash
wget http://www.open.com.au/mikem/bcm2835/bcm2835-1.50.tar.gz
tar xvfz bcm2835-1.50.tar.gz
cd bcm2835-1.50
./configure
make
sudo make install
```

### Motion

The command `sudo apt-get install motion` installs `motion` on the RPi.  For the proper setup, the default configuration then needs to be changed slightly:

```
daemon 	              on
width	                1280
height	              1024
framerate	            100
output_pictures	      off
ffmpeg_output_movies	off
stream_maxrate	      100
stream_localhost	    off
webcontrol_localhost	off
```

The `width` and `height`settings are depending on the USB camera that is used for the project. The livestream can be started with the command `sudo service motion start`. In this project, motion is always run in daemon mode offering an accessible live stream on port 8081.
