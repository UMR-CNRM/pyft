#!/usr/bin/env python3

"""
Main tool to apply source-to-source transformations using pyft
"""

import logging
import argparse

from pyft import PYFT  # pylint: disable=import-error
# pylint: disable-next=import-error
from pyft.scripting import updateParser, getParserOptions, \
                           getDescTree, applyTransfo, getArgs

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Python FORTRAN tool', allow_abbrev=False,
                                     epilog="The argument order matters.")

    updateParser(parser, withInput=True, withOutput=True, withXml=True, withPlotCentralFile=False,
                 treeIsOptional=True, nbPar=False, restrictScope=True)
    args, orderedOptions = getArgs(parser)[1]()

    parserOptions = getParserOptions(args)
    descTree = getDescTree(args)

    try:
        # Opening and reading of the FORTRAN file
        pft = PYFT(args.INPUT, args.OUTPUT, parser=args.parser, parserOptions=parserOptions,
                   verbosity=args.logLevel, wrapH=args.wrapH, tree=descTree,
                   enableCache=args.enableCache)
        if args.restrictScope != '':
            pft = pft.getScopeNode(args.restrictScope)

        # apply the transformation in the order they were specified
        for arg in orderedOptions:
            logging.debug('Applying %s on %s', arg, args.INPUT)
            applyTransfo(pft, arg, args, plotCentralFile=args.INPUT)
            logging.debug('  -> Done')

        # Writing
        if descTree is not None:
            descTree.toJson(args.descTree)
        if args.xml is not None:
            pft.mainScope.writeXML(args.xml)
        if not args.dryRun:
            pft.mainScope.write()

        # Closing
        pft.mainScope.close()

    except:  # noqa E722
        # 'exept' everything and re-raise error systematically
        logging.error("The following error has occurred in the file %s", args.INPUT)
        raise
