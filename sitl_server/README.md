# SITL Server

## Dependencies

You will need to install the following tools in order to build the SITL server:

 * GHC
 * cabal-install
 * GNU Make

## Building

 1. Make sure that you have sync'd the submodules:

    ```
    $ git submodule update --init
    ```

 2. Change to the sitl_server directory, and run make:

    ```
    $ cd sitl_server
    $ make
    ```

    This will build [ArduPilot](https://github.com/diydrones/ardupilot), as well
    as the server, placing both in the build directory.
