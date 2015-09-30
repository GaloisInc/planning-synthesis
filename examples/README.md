This folder contains the files needed to run the example. Below are the descriptions of the folders:

**configuration**: contains the configuration file and the way points scripts for the obstacles. The configuration file should be used as an input when initiating the SITL.

**tulip**: Includes the python scripts for creating the reactive synthesis controllers in /sitl_client. The file controller_alternative.py shows an alternative way (and easier to read way) for encoding the problem for tulip. It uses systems transitions instead of LTL specifications to defined the allowed transitions for the controlled and uncontrolled objects.

**sitl_client**: Contains all the files needed to run the controller. The main controller is controller_demonew.py.

**Results**: Contains a description of the problem, plots and a demo.
