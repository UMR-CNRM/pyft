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


################################################################################
### Verbosity, decorators and Exception

debugStats = {}

def debugDecor(func):
    """
    Defines a decorator to trace all function calling with arguments and results
    and count number of calls and time spent
    """
    @wraps(func)
    def wrapper(*args, **kwargs):
        #Logging call
        logger = logging.getLogger()
        if logger.isEnabledFor(logging.DEBUG):
            callstr = func.__name__ + \
                      '(' + ', '.join([str(a) for a in args] + \
                                      [k + '=' + str(v) for (k, v) in kwargs.items()]) + ')'
            logging.debug(callstr + ' --> ...')

        #Count and time
        if logger.isEnabledFor(logging.INFO):
            t0 = time.time()

        #effective call
        result = func(*args, **kwargs)

        #Count and time
        if logger.isEnabledFor(logging.INFO):
            from pyft import util
            if not func.__name__ in util.debugStats:
                util.debugStats[func.__name__] = dict(nb=0, totalTime=0)
            util.debugStats[func.__name__]['nb'] += 1
            duration = time.time() - t0
            util.debugStats[func.__name__]['totalTime'] += duration
            util.debugStats[func.__name__]['min'] = min(duration, util.debugStats[func.__name__].get('min', duration))
            util.debugStats[func.__name__]['max'] = max(duration, util.debugStats[func.__name__].get('max', duration))

        #logging result
        if logger.isEnabledFor(logging.DEBUG):
            logging.debug(callstr + ' --> ' + str(result))

        #return result
        return result
    return wrapper

def set_verbosity(level):
    """
    Set the verbosity level
    :param level: verbosity level used to set the logging module
    """
    logger = logging.getLogger()
    if isinstance(level, str):
        logger.setLevel(level=level.upper())
    else:
        logger.setLevel(level=level)

def print_infos():
    logger = logging.getLogger()
    if logger.isEnabledFor(logging.INFO):
        def _print(name, nb, min, max, mean):
            print('| ' + name.ljust(30) + '| ' + str(nb).ljust(14) + '| ' + \
                  str(min).ljust(23) + '| ' + str(max).ljust(23) + '| ' + str(mean).ljust(23) + '|')
        _print('Name of the function', '# of calls', 'Min (s)', 'Maxi (s)', 'Total (s)')
        from pyft import util
        for funcName, values in util.debugStats.items():
            _print(funcName, values['nb'], values['min'], values['max'], values['totalTime'])

class PYFTError(Exception): pass

################################################################################
### Conversions

def fortran2xml(fortranSource, parser='fxtran', parserOptions=None, wrapH=False):
    """
    :param fortranSource: a string containing a fortran source code
                          or a filename
    :param parser: path to the fxtran parser
    :param parserOptions: dictionnary holding the parser options
    :param wrapH: if True, content of .h file is put in a .F90 file (to force
                  fxtran to recognize it as free form) inside a module (to
                  enable the reading of files containing only a code part)
    :returns: (ns, xml) where ns is a namespace dictionnary and xml
              is an ET xml document
    """
    #Namespace registration
    ns = {'f': 'http://fxtran.net/#syntax'}
    #Alternatively, we could load a first time to find out the namespaces, then reload
    #it after having registered the right namespace. The folowing code snippet
    #allows to capture the declared namespaces.
    #ns = dict([node for _, node in ET.iterparse(StringIO(self.xml), events=['start-ns'])])
    for k, v in ns.items():
        ET.register_namespace(k, v)

    #Default options
    if parserOptions is None:
        import pyft
        parserOptions = pyft.PYFT.DEFAULT_FXTRAN_OPTIONS

    #Call to fxtran
    renamed = False
    moduleAdded = False
    with tempfile.NamedTemporaryFile(buffering=0, suffix='.F90') as f:
        if os.path.exists(fortranSource):
            #tempfile not needed in this case if wrapH is False but I found easier to write code
            #like this to have only one subprocess call and automatic
            #deletion of the temporary file
            filename = fortranSource
            if wrapH and filename.endswith('.h'):
                renamed = True
                filename = f.name
                with open(fortranSource, 'r') as src:
                    content = src.read()
                #renaming is enough for .h files containing SUBROUTINE or FUNCTION
                #but if the file contains a code fragment, it must be included in a
                #program-unit
                firstLine = [l for l in content.split('\n') if not (re.search(r'^[\t ]*!', l) or
                                                                    re.search(r'^[\t ]*$', l))][0]
                fisrtLine = firstLine.upper().split()
                if not ('SUBROUTINE' in fisrtLine or 'FUNCTION' in firstLine):
                    #Needs to be wrapped in a program-unit
                    moduleAdded = True
                    content = 'MODULE FOO\n' + content + '\nEND MODULE FOO'
                f.write(content.encode('UTF8'))
        else:
            filename = f.name
            f.write(fortranSource.encode('UTF-8'))
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
            for node in programUnit[1:-1]: #all nodes inside program-unit except 'MODULE' and 'END MODULE'
                file.append(node)
            node.tail = node.tail[:-1] #remove '\n' added before 'END MODULE'
            file.remove(programUnit)

    includesDone = False
    if len(set(['-no-include', '-noinclude']).intersection(parserOptions)) == 0:
        #fxtran has included the files but:
        #- it doesn't have removed the INCLUDE "file.h" statement
        #- it included the file with its file node
        #This code section removes the INCLUDE statement and the file node

        #Remove the include statement
        includeStmts = xml.findall('.//{*}include')
        for includeStmt in includeStmts:
            par = [p for p in xml.iter() if includeStmt in p][0]
            par.remove(includeStmt)
            includesDone = True
        #Remove the file node
        mainfile = xml.find('./{*}file')
        for file in mainfile.findall('.//{*}file'):
            par = [p for p in xml.iter() if file in p][0]
            index = list(par).index(file)
            if file.tail is not None:
                file[-1].tail = file.tail if file[-1].tail is None else (file[-1].tail + file.tail)
            for node in file[::-1]:
                par.insert(index, node)
            par.remove(file)

    return ns, includesDone, xml

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
    #When fxtran encounters an UTF-8 character, it replaces it by *2* entities
    #We must first transform each of these entities to its corresponding binary value
    #(this is done by tostring), then we must consider the result as bytes
    #to decode these two bytes into UTF-8 (this is done by encode('raw_...').decode('UTF-8'))
    r = ET.tostring(doc, method='text', encoding='UTF-8').decode('UTF-8')
    try:
        r = r.encode('raw_unicode_escape').decode('UTF-8')
    except UnicodeDecodeError:
        filename = doc.find('.//{*}file').attrib['name']
        logging.warning("The file '{}' certainly contains a strange character".format(filename))
    return r

################################################################################
### Other

def isint(s):
    """
    :param s: string to test for intergerness
    :return: True if s represent an int
    """
    try:
        int(s)
    except ValueError:
        return False
    else:
        return True

def isfloat(s):
    """
    :param s: string to test for intergerness
    :return: True if s represent a real
    """
    try:
        float(s)
    except ValueError:
        return False
    else:
        return True

################################################################################
### Helper functions acting on the xml

def n2name(N):
    """
    Helper function which returns the entity name enclosed in a N tag
    """
    return ''.join([e.text for e in N.findall('./{*}n')])

def alltext(doc):
    """
    Helper function to iterate on all text fragment and join them
    :param doc: xml fragment
    """
    return ''.join(doc.itertext())

def non_code(e):
    """
    :param e: element
    :return: True if e is non code (comment, text...)
    """
    return e.tag.split('}')[1] in {'cnt', 'C', 'cpp'}

def isExecutable(e):
    """
    :param e: element
    :return: True if element is executable
    """
    return (isStmt(e) or isConstruct(e)) and \
           not e.tag.split('}')[1] in {'subroutine-stmt', 'end-subroutine-stmt',
                                       'function-stmt', 'end-function-stmt',
                                       'use-stmt', 'T-decl-stmt', 'component-decl-stmt',
                                       'T-stmt', 'end-T-stmt',
                                       'data-stmt', 'save-stmt',
                                       'implicit-none-stmt'}

def isConstruct(e):
    """
    :param e: element
    :return: True if element is a construct
    """
    return e.tag.endswith('-construct')

def isStmt(e):
    """
    :param e: element
    :return: True if element is a statement
    """
    return e.tag.endswith('-stmt')
