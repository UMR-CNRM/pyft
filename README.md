# Python-fortran-tool

This set of tools is primarily designed to be applied on the [PHYEX](https://github.com/UMR-CNRM/PHYEX)
repository. But they are normally generic enough to be used in other contexts.

The tools can be used with the command line (the pyft\_tool.py and pyft\_parallel\_tool.py utilities)
or as a python package. The documentation is in the doc directory and some examples are in the
examples directory. 

The fxtran binary is needed to use pyft. You can:
  - use an existing installation and make sure it can be found using your PATH variable;
  - use an existing installation providing the exact path with the '--parser' option;
  - automatically install fxtran using the INSTALL.sh script (installation method #1 below)

Prerequisites:
  - an internet connexion (with access to the github servers) is needed only for the installation of fxtran
  - python > 3.8 (but only tested with version 3.10)
  - a C compiler (tested with cc 11.4.0) to compile fxtran
  - some classical unix tools: make (for fxtran) and git

There are three installation methods, depending on your use.
Methods 2 and 3 needs an external installation of fxtran (which is not covered here).
On some systems, you may need to create a python virtual environment to use methods 2 and 3.

Method 1 (suitable for developpers and end users):
  - open a terminal on a system satisfying the prerequisites and enter the following commands
  - if you don't have a github ssh key or don't know what it is:
    > git clone https://github.com/UMR-CNRM/pyft.git
    > ./pyft/bin/INSTALL.sh
  - if you have a github ssh key:
    > git clone git@github.com:UMR-CNRM/pyft.git
    > ./pyft/bin/INSTALL.sh --ssh
  - then:
    > source pyft/bin/env.sh # to set PATH and PYTHONPATH

Method 2 (suitable for developpers and end users):
  - open a terminal on a system satisfying the prerequisites and enter the following commands
  - if you don't have a github ssh key or don't know what it is:
    > git clone https://github.com/UMR-CNRM/pyft.git
  - if you have a github ssh key:
    > git clone git@github.com:UMR-CNRM/pyft.git
  - > cd pyft; python3 -m pip install -e .

Method 3 (suitable for users who do not develop pyft):
  - open a terminal on a system satisfying the prerequisites and enter the following commands
  - python3 -m pip install pyft
