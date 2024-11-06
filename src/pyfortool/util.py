"""
This module implements some tools to manipulate the xml
"""

import xml.etree.ElementTree as ET
from functools import wraps
import logging
import tempfile
import os
import subprocess
import time
import re

from pyfortool import NAMESPACE

################################################################################
# Verbosity, decorators and Exception

debugStats = {}


def debugDecor(func):
    """
    Defines a decorator to trace all function calling with arguments and results
    and count number of calls and time spent
    """
    @wraps(func)
    def wrapper(*args, **kwargs):
        # Logging call
        logger = logging.getLogger()
        if logger.isEnabledFor(logging.DEBUG):
            callstr = func.__name__ + \
                      '(' + ', '.join([str(a) for a in args] +
                                      [k + '=' + str(v) for (k, v) in kwargs.items()]) + ')'
            logging.debug('%s --> ...', callstr)
        else:
            callstr = None

        # Count and time
        if logger.isEnabledFor(logging.INFO):
            t0 = time.time()
        else:
            t0 = None

        # effective call
        result = func(*args, **kwargs)

        # Count and time
        if t0 is not None:
            # We test with t0 instead of the log level in case level evolved during func call
            if func.__name__ not in debugStats:
                debugStats[func.__name__] = dict(nb=0, totalTime=0)
            debugStats[func.__name__]['nb'] += 1
            duration = time.time() - t0
            debugStats[func.__name__]['totalTime'] += duration
            debugStats[func.__name__]['min'] = \
                min(duration, debugStats[func.__name__].get('min', duration))
            debugStats[func.__name__]['max'] = \
                max(duration, debugStats[func.__name__].get('max', duration))

        # logging result
        if callstr is not None:
            # We test with callstr instead of the log level in case level evolved during func call
            logging.debug('%s --> %s', callstr, str(result))

        return result
    return wrapper


def noParallel(func):
    """
    Defines a decorator that prevent this method to be executed in parallel on several files
    """
    @wraps(func)
    def wrapper(self, *args, **kwargs):
        if self.NO_PARALLEL_LOCK is not None:
            # Acquire lock, if any
            with self.NO_PARALLEL_LOCK:
                # We cannot use directly the shared version because the update
                # tries to send a whole PYFT object to the Manager and there are
                # issues with that (at least about the locks attached to the instance)
                # The code here allows to synchronize when there is a lock mechanism.
                # All the routines updating the tree must be decorated by noParallel
                if self.SHARED_TREE is not None:
                    self.tree.copyFromOtherTree(self.SHARED_TREE)
                result = func(self, *args, **kwargs)
                if self.SHARED_TREE is not None:
                    self.tree.copyToOtherTree(self.SHARED_TREE)
                return result
        else:
            return func(self, *args, **kwargs)
    return wrapper


def setVerbosity(level):
    """
    Set the verbosity level
    :param level: verbosity level used to set the logging module
    """
    logger = logging.getLogger()
    if isinstance(level, str):
        logger.setLevel(level=level.upper())
    else:
        logger.setLevel(level=level)


def printInfos():
    """
    Print statistics on methods and function usage
    """
    logger = logging.getLogger()
    if logger.isEnabledFor(logging.INFO):
        def _print(name, nb, vmin, vmax, mean):
            print('| ' + name.ljust(30) + '| ' + str(nb).ljust(14) + '| ' +
                  str(vmin).ljust(23) + '| ' + str(vmax).ljust(23) + '| ' +
                  str(mean).ljust(23) + '|')
        _print('Name of the function', '# of calls', 'Min (s)', 'Max (s)', 'Total (s)')
        for funcName, values in debugStats.items():
            _print(funcName, values['nb'], values['min'], values['max'], values['totalTime'])


class PYFTError(Exception):
    """
    Exceptions for PYFT
    """

################################################################################
# Conversions


def fortran2xml(fortranSource, parser='fxtran', parserOptions=None, wrapH=False):
    """
    :param fortranSource: a string containing a fortran source code
                          or a filename
    :param parser: path to the fxtran parser
    :param parserOptions: dictionnary holding the parser options
    :param wrapH: if True, content of .h file is put in a .F90 file (to force
                  fxtran to recognize it as free form) inside a module (to
                  enable the reading of files containing only a code part)
    :returns: (includesRemoved, xml) where includesRemoved indicates if an include
              was replaced by fxtran and xml is an ET xml document
    """
    # Namespace registration
    ET.register_namespace('f', NAMESPACE)

    # Default options
    if parserOptions is None:
        import pyfortool
        parserOptions = pyfortool.PYFT.DEFAULT_FXTRAN_OPTIONS

    # Call to fxtran
    renamed = False
    moduleAdded = False
    with tempfile.NamedTemporaryFile(buffering=0, suffix='.F90') as file:
        if os.path.exists(fortranSource):
            # tempfile not needed in this case if wrapH is False but I found easier to write code
            # like this to have only one subprocess call and automatic
            # deletion of the temporary file
            filename = fortranSource
            if wrapH and filename.endswith('.h'):
                renamed = True
                filename = file.name
                with open(fortranSource, 'r', encoding='utf-8') as src:
                    content = src.read()
                # renaming is enough for .h files containing SUBROUTINE or FUNCTION
                # but if the file contains a code fragment, it must be included in a
                # program-unit
                firstLine = [line for line in content.split('\n')
                             if not (re.search(r'^[\t ]*!', line) or
                             re.search(r'^[\t ]*$', line))][0]
                fisrtLine = firstLine.upper().split()
                if not ('SUBROUTINE' in fisrtLine or 'FUNCTION' in firstLine):
                    # Needs to be wrapped in a program-unit
                    moduleAdded = True
                    content = 'MODULE FOO\n' + content + '\nEND MODULE FOO'
                file.write(content.encode('UTF8'))
        else:
            filename = file.name
            file.write(fortranSource.encode('UTF-8'))
        xml = subprocess.run([parser, filename,
                             '-o', '-'] + parserOptions,
                             stdout=subprocess.PIPE, check=True,
                             encoding='UTF-8').stdout
        xml = ET.fromstring(xml, parser=ET.XMLParser(encoding='UTF-8'))
        if renamed:
            xml.find('./{*}file').attrib['name'] = fortranSource
        if moduleAdded:
            file = xml.find('./{*}file')
            programUnit = file.find('./{*}program-unit')
            # all nodes inside program-unit except 'MODULE' and 'END MODULE'
            for node in programUnit[1:-1]:
                file.append(node)
            # pylint: disable-next=undefined-loop-variable
            node.tail = node.tail[:-1]  # remove '\n' added before 'END MODULE'
            file.remove(programUnit)

    includesDone = False
    if len(set(['-no-include', '-noinclude']).intersection(parserOptions)) == 0:
        # fxtran has included the files but:
        # - it doesn't have removed the INCLUDE "file.h" statement
        # - it included the file with its file node
        # This code section removes the INCLUDE statement and the file node

        # Remove the include statement
        includeStmts = xml.findall('.//{*}include')
        for includeStmt in includeStmts:
            par = [p for p in xml.iter() if includeStmt in p][0]
            par.remove(includeStmt)
            includesDone = True
        # Remove the file node
        mainfile = xml.find('./{*}file')
        for file in mainfile.findall('.//{*}file'):
            par = [p for p in xml.iter() if file in p][0]
            index = list(par).index(file)
            if file.tail is not None:
                file[-1].tail = file.tail if file[-1].tail is None else (file[-1].tail + file.tail)
            for node in file[::-1]:
                par.insert(index, node)
            par.remove(file)

    return includesDone, xml


def tostring(doc):
    """
    :param doc: an ET object
    :return: xml as a string
    """
    return ET.tostring(doc, method='xml', encoding='UTF-8').decode('UTF-8')


def tofortran(doc):
    """
    :param doc: an ET object
    :return: a string representing the FORTRAN source code
    """
    # When fxtran encounters an UTF-8 character, it replaces it by *2* entities
    # We must first transform each of these entities to its corresponding binary value
    # (this is done by tostring), then we must consider the result as bytes
    # to decode these two bytes into UTF-8 (this is done by encode('raw_...').decode('UTF-8'))
    result = ET.tostring(doc, method='text', encoding='UTF-8').decode('UTF-8')
    try:
        result = result.encode('raw_unicode_escape').decode('UTF-8')
    except UnicodeDecodeError:
        filename = doc.find('.//{*}file').attrib['name']
        logging.warning("The file '%s' certainly contains a strange character", filename)
    return result

################################################################################
# Other


def isint(string):
    """
    :param string: string to test for intergerness
    :return: True if s represent an int
    """
    try:
        int(string)
    except ValueError:
        return False
    else:
        return True


def isfloat(string):
    """
    :param string: string to test for intergerness
    :return: True if s represent a real
    """
    try:
        float(string)
    except ValueError:
        return False
    else:
        return True

################################################################################
# Helper functions acting on the xml


def tag(elem):
    """
    :param elem: ET Element
    :return: the tag without the namespace
    """
    return elem.tag.split('}')[1]


def n2name(nodeN):
    """
    Helper function which returns the entity name enclosed in a N tag
    """
    return ''.join([e.text for e in nodeN.findall('./{*}n')])


def alltext(doc):
    """
    Helper function to iterate on all text fragment and join them
    :param doc: xml fragment
    """
    return ''.join(doc.itertext())


def nonCode(elem):
    """
    :param e: element
    :return: True if e is non code (comment, text...)
    """
    return tag(elem) in {'cnt', 'C', 'cpp'}


def isExecutable(elem):
    """
    :param e: element
    :return: True if element is executable
    """
    return ((isStmt(elem) or isConstruct(elem)) and
            tag(elem) not in ('subroutine-stmt', 'end-subroutine-stmt',
                              'function-stmt', 'end-function-stmt',
                              'use-stmt', 'T-decl-stmt', 'component-decl-stmt',
                              'T-stmt', 'end-T-stmt',
                              'data-stmt', 'save-stmt',
                              'implicit-none-stmt'))


def isConstruct(elem):
    """
    :param elem: element
    :return: True if element is a construct
    """
    return tag(elem).endswith('-construct')


def isStmt(elem):
    """
    :param elem: element
    :return: True if element is a statement
    """
    return tag(elem).endswith('-stmt')
