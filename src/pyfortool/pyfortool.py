#!/usr/bin/env python3

"""
This module contains the main PYFT class
PYFT is the file level class (read/write...)
"""

import os
import sys
from multiprocessing import Lock, RLock

from pyfortool.scope import PYFTscope
from pyfortool.tree import Tree, updateTree
from pyfortool.util import (debugDecor, tostring, tofortran, fortran2xml,
                            setVerbosity, printInfos, PYFTError)


@debugDecor
def conservativePYFT(filename, parser, parserOptions, wrapH,
                     tree=None, verbosity=None, clsPYFT=None):
    """
    Return a conservative PYFT object usable for tree manipulation
    :param filename: name of the file to open
    :param parser, parserOptions, wrapH: see the PYFT class
    :param tree: Tree instance or None
    :param verbosity: if not None, sets the verbosity level
    :param clsPYFT: PYFT class to use
    :return: PYFT object
    """
    options = PYFT.DEFAULT_FXTRAN_OPTIONS if parserOptions is None else parserOptions
    options = options.copy()
    if len(set(options).intersection(('-no-include', '-noinclude'))) == 0:
        # We add the option to not include 'include files' when analysing the tree
        options.append('-no-include')
    if clsPYFT is None:
        clsPYFT = PYFT
    pft = clsPYFT(filename, parser=parser, parserOptions=options, wrapH=wrapH,
                  tree=tree, verbosity=verbosity)
    return pft


def generateEmptyPYFT(filename, fortran=None, **kwargs):
    """
    Generates an empty PYFT scope
    :param filename: file name corresponding to this new PYFT scope
    :param fortran: fortran text to include
    :param **kwargs: other arguments for the PYFT class
    """
    with open(filename, 'w', encoding='utf-8') as fo:
        fo.write('SUBROUTINE FOO\nEND' if fortran is None else fortran)
    pft = PYFT(filename, **kwargs)
    if fortran is None:
        pft.remove(pft.find('.//{*}program-unit'))
    return pft


class PYFT(PYFTscope):
    """
    This class extends the PYFTscope one by adding file support (read/write)
    """
    DEFAULT_FXTRAN_OPTIONS = ['-construct-tag', '-no-include', '-no-cpp', '-line-length', '9999']
    MANDATORY_FXTRAN_OPTIONS = ['-construct-tag']
    SHARED_TREE = None  # Can be updated by setParallel
    NO_PARALLEL_LOCK = None  # Can be updated by setParallel
    PARALLEL_FILE_LOCKS = None  # Can be updated by setParallel

    @updateTree('signal')
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
        self.__class__.lockFile(filename)
        if not sys.version_info >= (3, 8):
            # At least version 3.7 for ordered dictionary
            # At least verison 3.8 for namsepace wildcard (use of '{*}' in find or findall)
            raise PYFTError("PyForTool needs at least version 3.8 of python")
        self._filename = filename
        self._originalName = filename
        assert os.path.exists(filename), 'Input filename must exist'
        self._output = output
        self._parser = 'fxtran' if parser is None else parser
        tree = Tree() if tree is None else tree
        if self.SHARED_TREE is not None:
            assert tree is not None, 'tree must be None if setParallel has been called'
            tree.copyFromOtherTree(self.SHARED_TREE)
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
            self.tree.signal(self)
        if verbosity is not None:
            setVerbosity(verbosity)

    @classmethod
    def setParallel(cls, tree, clsLock=None, clsRLock=None):
        """
        Prepare the class to be used to instanciate parallel objects
        :param tree: Tree object shared among processes
        :param clsLock: class to use for Lock (defaults to multiprocessing.Lock)
        :param clsRLock: class to use for RLock (defaults to multiprocessing.RLock)
        """
        if clsLock is None:
            clsLock = Lock
        if clsRLock is None:
            clsRLock = RLock
        cls.NO_PARALLEL_LOCK = clsRLock()
        cls.SHARED_TREE = tree
        cls.PARALLEL_FILE_LOCKS = {os.path.normpath(file): clsLock()
                                   for file in tree.getFiles()}

    @classmethod
    def lockFile(cls, filename):
        """
        Acquire lock for filename
        :param filename: name of the file whose lock must be acquired
        """
        filename = os.path.normpath(filename)
        # pylint: disable-next=unsupported-membership-test
        if cls.PARALLEL_FILE_LOCKS is not None and filename in cls.PARALLEL_FILE_LOCKS:
            # pylint: disable-next=unsubscriptable-object
            cls.PARALLEL_FILE_LOCKS[filename].acquire()

    @classmethod
    def unlockFile(cls, filename, silence=False):
        """
        Release lock for filename
        :param filename: name of the file whose lock must be acquired
        :param silence: do not raise exception if file is already unlocked
        """
        filename = os.path.normpath(filename)
        # pylint: disable-next=unsupported-membership-test
        if cls.PARALLEL_FILE_LOCKS is not None and filename in cls.PARALLEL_FILE_LOCKS:
            try:
                # pylint: disable-next=unsubscriptable-object
                cls.PARALLEL_FILE_LOCKS[filename].release()
            except ValueError:
                if not silence:
                    raise

    def __enter__(self):
        """
        Context manager
        """
        return self

    def __exit__(self, excType, excVal, excTb):
        """
        Context manager
        """
        self.close()

    def close(self):
        """
        Closes the FORTRAN file
        """
        printInfos()
        self.__class__.unlockFile(self.getFileName())

    @property
    def xml(self):
        """
        Returns the xml as a string
        """
        return tostring(self)

    @property
    def fortran(self):
        """
        Returns the FORTRAN as a string
        """
        return tofortran(self)

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
            fo.flush()  # ensuring all the existing buffers are written
            os.fsync(fo.fileno())  # forcefully writing to the file

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
