"""
pyft is a python package, built on top of the fxtran tool, to manipulate
FORTRAN files to apply source-to-source transformations.
"""

# NAMESPACE should be defined first
NAMESPACE = 'http://fxtran.net/#syntax'

# Import the public part of the package
from . import util  # pylint: disable=wrong-import-position # noqa: F401 E402
from . import expressions  # pylint: disable=wrong-import-position # noqa: F401 E402
from .pyft import PYFT, conservativePYFT  # pylint: disable=wrong-import-position # noqa: F401 E402
from . import tree  # pylint: disable=wrong-import-position # noqa: F401 E402
