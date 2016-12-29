# happy flowers Setup Guide

The following guide documents the steps to setting up the Haskell environment and Raspberry Pi ('RPi') for happy flowers to work in.

## Table of Contents

1. [Requirements](#requirements)
2. [Folder Organisation](#folder-organisation)
3. [Cabal Sandbox](#cabal-sandbox)
4. [SQLite](#sqlite)
5. [Configuration](#configuration)
6. [Cabal Tasks](#cabal-tasks)
7. [Atom Integration](#atom-integration)

## Requirements

To use this project you will need the following minimal requirements.

| Technology | Version  |
| ---------- | -------- |
| GHC        | 7.10.3   |
| cabal      | 1.22.9.0 |
| sqlite     | 3.14.0   |

## Folder Organisation

All Haskell code resides in the `backend/` directory. Most of the folder organisation is either mandated by the use of Haskell and Cabal or by following established best practices. Files marked as ‘local’ need to be created separately for each new installation as they should not be tracked by version control.

| Name             | Local | Description                                                    |
| ---------------- | ----- | -------------------------------------------------------------- |
| HappyFlowers/    | No    | Contains Haskell source code.                                  |
| happyflowers.db  | Yes   | SQLite database.                                               |
| happyflowers.sql | No    | Contains SQLite database setup.                                |
| cabal.config     | No    | Locks dependencies.                                            |
| Main.hs          | No    | Entrypoint for the Haskell back end application.               |
| rpi.cabal        | No    | Cabal configuration file listing dependencies and executables. |
| rpi.cfg          | Yes   | Internal configuration file for the application.               |

## Setting Up a Cabal Sandbox

In order to provide a clean and isolated environment this project uses a cabal sandbox.

- Run `cabal sandbox init`.
- Run `cabal install --extra-lib-dirs=/usr/local/lib/ --extra-include-dirs=/usr/local/include/`. The two flags are required for installing `HPi`.

Depdendencies are locked using `cabal freeze`. The locked versions are noted in the `cabal.config` file.

## Setting Up SQLite

- Create the database. Navigate to the `rpi` folder and run `sqlite3 happyflowers.db`.
- Create the necessary tables using `.read happyflowers.sql`.
- Insert data for application settings: `INSERT INTO settings (name, upper, lower, interval) VALUES ("...", 80, 40, 60);`

## Configuration

The configuration is based on a file called `rpi.cfg` inside the `rpi` directory. It contains settings that change the behaviour of the application. The default configuration expects a `password` entry and a `frame` entry as follows:

```ini
password=1234
frame=14
```

The password is used for user authentication and the frame determines the timeframe for which historical data is displayed on the front end.

## Cabal Tasks

The build can be configured to run in development mode, where sensors are simulated using mock functions, or production mode, where the real RPi sensors are accessed. The production environment only needs to be active when building on the RPi.

- `cabal configure -f Development` enables development mode.
- `cabal configure -f -Development` disables development mode.
- `cabal install` installs all required dependencies.
- `cabal build rpi` builds the project.
- `cabal run rpi` builds and runs the project.

All tasks read the [rpi.cabal](./rpi.cabal) config and are based off the `rpi` executable.

Running the project on the RPi requires administrator privileges. This does not work with cabal so the executable needs to be built using cabal and then started using `sudo ./dist/build/rpi/rpi`. In order to run the program in the background, append ` > /dev/null &` to the previous command.

Additionally, a separate executable used for testing only the I2C functionality is provided.

- `cabal build I2CTest` builds the test project.
- `cabal run I2CTest` builds and runs the test project.

## Atom Integration

Atom offers easy integration of linting output directly in the editor.

1. Install [linter](https://atom.io/packages/linter)
2. Install [linter-hlint](https://atom.io/packages/linter-hlint)
3. Install hlint using `cabal install --global hlint`

## Documentation

Generating the documentation is included in the cabal setup and can be performed using `cabal haddock --executables`. This generates documentation for all packages that are part of an executable package.

haddock can include documentation of dependencies so that internal documentation may reference other libraries. This requires passing the `--enable-documentation` flag during the cabal install step.

## Style Guide

* [Haskell Style Guide](https://github.com/tibbe/haskell-style-guide)
* [UPENN Style Guide](http://www.seas.upenn.edu/~cis552/12fa/styleguide.html)

There is no automatic enforcement of these practices, instead the code is reviewed for its conformance on a regular basis.

All Haskell code is linted using `hlint` and its available IDE integrations. All available options (`Default`, `Dollar`, `Generalise`) are used in order to find possible improvements throughout the codebase.
