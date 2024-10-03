#!/usr/bin/env python3

"""
Main tool to apply in parallel source-to-source transformations using pyft
"""


import logging
import argparse
from multiprocessing.managers import BaseManager
from multiprocessing import Pool
import sys
import traceback

from pyft import PYFT  # pylint: disable=import-error
from pyft.tree import Tree  # pylint: disable=import-error
from pyft.util import PYFTError  # pylint: disable=import-error
# pylint: disable-next=import-error
from pyft.scripting import updateParser, getParserOptions, \
                           getDescTree, applyTransfo, getArgs


class MyManager(BaseManager):
    """
    Custom manager to deal with Tree instances
    """


MyManager.register('Tree', Tree)


def init(cls):
    """
    Pool initializer
    """
    # After many, many attempts, it seems very difficult (if not impossible)
    # to do without this global variable
    global PYFT  # pylint: disable=global-statement
    PYFT = cls


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Python FORTRAN tool', allow_abbrev=False,
                                     epilog="The argument order matters.")

    updateParser(parser, withInput=False, withOutput=False, withXml=False, withPlotCentralFile=True,
                 treeIsOptional=False, nbPar=True)
    commonArgs, getFileArgs = getArgs(parser)

    # Manager to share the Tree instance
    with MyManager() as manager:
        # Set-up the Tree instance
        sharedTree = getDescTree(commonArgs, manager.Tree)

        # Prepare PYFT to be used in parallel
        PYFT.setParallel(sharedTree)

        allFileArgs = {file: getFileArgs(file) for file in sharedTree.getFiles()}

        # Defined here to use the allFileArgs
        def task(filename):
            """
            Function to use on each file
            :param clsPYFT: PYFT class to use
            :param filename: file name
            """
            allArgs, orderedOptions = allFileArgs[filename]
            try:
                # Opening and reading of the FORTRAN file
                pft = PYFT(filename, filename, parser=allArgs.parser,
                           parserOptions=getParserOptions(allArgs), verbosity=allArgs.logLevel,
                           wrapH=allArgs.wrapH,
                           enableCache=allArgs.enableCache)

                # apply the transformation in the order they were specified
                for arg in orderedOptions:
                    logging.debug('Applying %s on %s', arg, filename)
                    applyTransfo(pft, arg, allArgs,
                                 filename if filename == allArgs.plotCentralFile else None)
                    logging.debug('  -> Done')

                # Writing
                if not allArgs.dryRun:
                    pft.write()

                # Closing and reporting
                pft.close()
                return (0, filename)

            except Exception as exc:  # pylint: disable=broad-except
                logging.error("The following error has occurred in the file %s", filename)
                PYFT.unlockFile(filename, silence=True)
                traceback.print_exception(exc, file=sys.stdout)
                sys.stdout.flush()
                return (1, filename)

        # Set-up the processes
        # allFileArgs = {file: getFileArgs(file) for file in sharedTree.getFiles()}
        logging.info('Executing in parallel on %i files with a maximum of %i processes',
                     len(allFileArgs), commonArgs.nbPar)
        with Pool(commonArgs.nbPar, initializer=init, initargs=(PYFT, )) as pool:
            result = pool.map(task, sharedTree.getFiles())

        # Writting the descTree object
        sharedTree.toJson(commonArgs.descTree)

        # General error
        errors = [item[1] for item in result if item[0] != 0]
        status = len(errors)
        if status != 0:
            logging.error('List of files with error:')
            for error in errors:
                logging.error('  - %s', error)
            raise PYFTError(f"Errors have been reported in {status} file(s).")
