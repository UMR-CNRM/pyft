#!/usr/bin/env python3

"""
Main tool to apply in parallel source-to-source transformations using pyft
"""

# This script only contains a call to the mainParallel function
# It must remain unchanged to be equivalent to the entry point defined in pyproject.toml

from pyft.scripting import mainParallel  # pylint: disable=import-error

if __name__ == '__main__':
    mainParallel()
