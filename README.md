# Planning Synthesis

## Server Requirements

Building the SITL server requires a number of dependencies to be installed:
- ghc (version 7.8.4 or greater)
- cabal-install (version 1.22 or greater)
- python
- pip
- virtualenv
- gcc

Running the SITL server requires:
- tmux
- xterm
- realpath (via the coreutils package on ubuntu)

## Building the server

1. Install python dependencies for ArduPilot
   ```sh
   $ sudo apt-get install python-matplotlib python-serial python-wxgtk2.8 python-lxml
   $ sudo apt-get install python-scipy python-opencv ccache gawk git python-pip python-pexpect
   ```

1. Run cabal update
   ```sh
   $ cabal update
   ```
   
1. Make sure that `~/.cabal/bin` is in your `PATH`

1. Make sure that you have all submodules cloned
   ```sh
   $ git submodule update --init
   ```

2. Build the server (this will take a while)
   ```sh
   $ cd sitl_server
   $ make distclean
   $ make
   ```

## Running the server

* Run the `start-server.sh` script in the top-level of the repository
  ```sh
  $ ./start-server.sh
  ```

* By default the server will start a new instance of tmux, to manage the
  processes started by the SITL.  If you would like to detach from the server,
  you can just press `C-a d`.  You can also start the server in a detached
  state by using the word `detach` as the first argument to the
  `start-server.sh` script:
  ```sh
  $ ./start-server.sh detach
  ```

* You can reattach to a running server by giving the `attach` command to tmux:
  ```sh
  $ tmux attach
  ```

### Optional Simulation Configuration File

The simulation can be given a configuration file, that can specify additional
obstacles in the system, as well as the starting position of the controlled
instance.  The format of the file is as follows:

```yaml
controlled:
  home: "lat,lon,alt,heading"

obstacles:
  * name:   "name1"
    script: "path/to/script1"
    home:   "lat,lon,alt,heading"

    ...

  * name:   "nameN"
    script: "path/to/scriptN"
    home:   "lat,lon,alt,heading"
```

Note that you can leave out the obstacles section entirely.  The files
mentioned in the `script` field of an obstacle can be specified as a path that
is either relative to the configuration file, or absolute.

When running the server with the configuration file, pass the path to the file
as the argument to `start-server.sh`:

```sh
$ ./start-server.sh test.cfg
```

Now, when the client connects, it will need to use the `start_obstacles`
function to cause the obstacle quad-copter to start flying the path of
waypoints, but after that, should observe multiple positions present in the
output of `get_positions`.

### Dropping position packets

The `start-server.sh` script supports dropping position packets from obstacles
some percentage of the time.  This is specified with the `-d` option to
`start-server.sh`, which takes a value between 0 and 1. For example:
```sh
$ ./start-server.sh -d 0.2 test.cfg
```
will drop packets 20% of the time.

The effect of enabling this is that when packets are being dropped, the previous
location for an obstacle will be reported, instead of its current location.

## Connecting to the server

The `sitl_client` directory contains a single python module, named
`sitl_client.py`.  When run this module will execute a simple test program that
will connect to the server, move the quad-copter, then report its position for
60 seconds.
