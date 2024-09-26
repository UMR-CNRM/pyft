#!/usr/bin/env python3

"""
This module contains the main PYFT class
PYFT is the file level class (read/write...)
"""

import os
import sys

from pyft.scope import PYFTscope
from pyft.tree import Tree
from pyft.util import (debugDecor, tostring, tofortran, fortran2xml,
                       setVerbosity, printInfos, PYFTError)


@debugDecor
def conservativePYFT(filename, parser, parserOptions, wrapH,
                     tree=None, verbosity=None):
    """
    Return a conservative PYFT object usable for tree manipulation
    :param filename: name of the file to open
    :param parser, parserOptions, wrapH: see the pyft class
    :param verbosity: if not None, sets the verbosity level
    :return: PYFT object
    """
    options = PYFT.DEFAULT_FXTRAN_OPTIONS if parserOptions is None else parserOptions
    options = options.copy()
    if len(set(options).intersection(('-no-include', '-noinclude'))) == 0:
        # We add the option to not include 'include files' when analysing the tree
        options.append('-no-include')
    pft = PYFT(filename, parser=parser, parserOptions=options, wrapH=wrapH,
               tree=tree, verbosity=verbosity)
    return pft


class PYFT(PYFTscope):
    """
    This class extends the PYFTscope one by adding file support (read/write)
    """
    DEFAULT_FXTRAN_OPTIONS = ['-construct-tag', '-no-include', '-no-cpp', '-line-length', '9999']
    MANDATORY_FXTRAN_OPTIONS = ['-construct-tag']

    def __init__(self, filename, output=None, parser=None, parserOptions=None, verbosity=None,
                 wrapH=False, tree=None, enableCache=False):
        """
        :param filename: Input file name containing FORTRAN code
        :param output: Output file name, None to replace input file
        :param parser: path to the fxtran parser
        :param parserOptions: dictionnary holding the parser options
        :param verbosity: if not None, sets the verbosity level
        :param wrapH: if True, content of .h file is put in a .F90 file (to force
                      fxtran to recognize it as free form) inside a module (to
                      enable the reading of files containing only a code part)
        :param tree: an optional Tree instance
        :param enableCache: True to cache node parents
        """
        if not sys.version_info >= (3, 8):
            # At least version 3.7 for ordered dictionary
            # At least verison 3.8 for namsepace wildcard (use of '{*}' in find or findall)
            raise PYFTError("pyft needs at least version 3.8 of python")
        self._filename = filename
        self._originalName = filename
        assert os.path.exists(filename), 'Input filename must exist'
        self._output = output
        self._parser = 'fxtran' if parser is None else parser
        tree = Tree() if tree is None else tree
        if parserOptions is None:
            self._parserOptions = self.DEFAULT_FXTRAN_OPTIONS.copy()
        else:
            self._parserOptions = parserOptions.copy()
        for tDir in tree.getDirs():
            self._parserOptions.extend(['-I', tDir])
        for option in self.MANDATORY_FXTRAN_OPTIONS:
            if option not in self._parserOptions:
                self._parserOptions.append(option)
        includesRemoved, xml = fortran2xml(self._filename, self._parser, self._parserOptions, wrapH)
        super().__init__(xml, enableCache=enableCache, tree=tree)
        if includesRemoved:
            self.tree.update(self)
        if verbosity is not None:
            setVerbosity(verbosity)

    @staticmethod
    def close():
        """
        Closes the FORTRAN file
        """
        printInfos()

    @property
    def xml(self):
        """
        Returns the xml as a string
        """
        return tostring(self.node)

    @property
    def fortran(self):
        """
        Returns the FORTRAN as a string
        """
        return tofortran(self.node)

    def renameUpper(self):
        """
        The output file will have an upper case extension
        """
        self._rename(str.upper)

    def renameLower(self):
        """
        The output file will have a lower case extension
        """
        self._rename(str.lower)

    def _rename(self, mod):
        """
        The output file will have a modified extension.
        :param mod: function to apply to the file extension
        """
        def _transExt(path, mod):
            filename, ext = os.path.splitext(path)
            return filename + mod(ext)
        if self._output is None:
            self._filename = _transExt(self._filename, mod)
        else:
            self._output = _transExt(self._output, mod)

    def write(self):
        """
        Writes the output FORTRAN file
        """
        with open(self._filename if self._output is None else self._output, 'w',
                  encoding='utf-8') as fo:
            fo.write(self.fortran)
        if self._output is None and self._filename != self._originalName:
            # We must perform an in-place update of the file, but the output file
            # name has been updated. Then, we must remove the original file.
            os.unlink(self._originalName)

    def writeXML(self, filename):
        """
        Writes the output XML file
        :param filename: XML output file name
        """
        with open(filename, 'w', encoding='utf-8') as fo:
            fo.write(self.xml)
