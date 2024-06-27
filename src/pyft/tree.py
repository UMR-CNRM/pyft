"""
This module contains the functions to browse the tree
"""

import glob
import os
import logging
import json
import subprocess
import xml.etree.ElementTree as ET
import copy
import re

from pyft.util import debugDecor, copy_doc, PYFTError, n2name

@debugDecor
def getDirs(tree):
    """
    :param tree: list of directories composing the tree or None
    :return: list of directories and subdirectories
    """
    r = []
    if tree is not None:
        for t in tree:
            r += glob.glob("**/")
    return r

@debugDecor
def getFiles(tree):
    """
    :param tree: list of directories composing the tree or None
    :return: list of directories and subdirectories
    """
    filenames = []
    for t in tree:
        for filename in glob.glob(t + '/**/*', recursive=True):
            if os.path.splitext(filename)[1] not in ('', '.json', '.fypp', '.txt'):
                #We only keep files with extension
                filenames.append(filename)
    return filenames

@debugDecor
def conservativePYFT(filename, parser, parserOptions, wrapH, addIncludes=False, tree=None):
    """
    Return a conservative PYFT object usable for tree manipulation
    :param filename: name of the file to open
    :param parser, parserOptions, wrapH: see the pyft class
    :param addIncludes: add includes file during the source file scanning
    :return: PYFT object
    """
    from pyft import PYFT
    options = PYFT.DEFAULT_FXTRAN_OPTIONS if parserOptions is None else parserOptions
    options = copy.copy(options)
    hasNoInclude = len(set(options).intersection(('-no-include', '-noinclude'))) != 0
    if addIncludes and hasNoInclude:
        #We remove the option
        for opt in ('-no-include', '-noinclude'):
            if opt in options:
                options.remove(opt)
    elif (not addIncludes) and (not hasNoInclude):
        #We add the option to not include 'include files' when analysing the tree
        options.append('-no-include')
    pft = PYFT(filename, parser=parser, parserOptions=options, wrapH=wrapH, tree=tree)
    if addIncludes:
        #We save the file with the includes
        pft.write()
    return pft

@debugDecor
def descTree(tree, descTree, parser=None, parserOptions=None, wrapH=False, addIncludes=False):
    """
    Build the description tree file
    :param tree: list of directories composing the tree or None (also used to include files if addIncludes is True)
    :param descTree: filename where the description of th tree will be stored
    :param parser, parserOptions, wrapH: see the pyft class
    :param addIncludes: add includes file during the source file scanning
    """
    def extract_string(text):
        text = text.strip()
        if text[0] in ('"', "'"):
            assert text[-1] == text[0]
            text = text[1, -1]
        return text

    if not os.path.exists(descTree):
        if tree is None:
            raise PYFTError('You must provide tree when descTree is set')
        result = {'cwd': os.getcwd(), 'compilation_tree': {}, 'execution_tree': {}, 'scopes': {}}

        useList = {}
        includeList = {}
        callList = {}
        funcList = {}
        #Loop on directory and files
        for filename in getFiles(tree):
            if os.path.isfile(filename):
                pft = conservativePYFT(filename, parser, parserOptions, wrapH,
                                        addIncludes=addIncludes, tree=tree)
                filename = filename[2:] if filename.startswith('./') else filename
                varList = pft.getVarList()

                #Fill scopes
                scopes = pft.getScopes(excludeContains=True)
                result['scopes'][filename] = []
                for scope in scopes:
                    #Scope found in file
                    result['scopes'][filename].append(scope.path)
                    #We add, to this list, the "MODULE PROCEDURE" declared in INTERFACE statements
                    if scope.path.split('/')[-1].split(':')[0] == 'interface':
                        for name in [n2name(N).upper()
                                     for moduleproc in scope.findall('./{*}procedure-stmt')
                                     for N in moduleproc.findall('./{*}module-procedure-N-LT/{*}N')]:
                            for s in scopes:
                                if re.search(scope.path.rsplit('/', 1)[0] + '/[a-zA-Z]*:' + name, s.path):
                                    result['scopes'][filename].append(scope.path + '/' + s.path.split('/')[-1])

                #Fill trees
                result['compilation_tree'][filename] = []

                includeList[filename] = {}
                useList[filename] = {}
                callList[filename] = {}
                funcList[filename] = {}
                for scope in scopes:
                    virtual = ET.Element('virtual')
                    virtual.extend(scope)
                    #Fill compilation_tree
                    #Includes give directly the name of the source file but possibly without the directory
                    includeList[filename][scope.path] = [f.text
                                                    for f in virtual.findall('.//{*}include/{*}filename')] #cpp
                    includeList[filename][scope.path].extend([extract_string(f.text)
                                               for f in virtual.findall('.//{*}include/{*}filename/{*}S')]) #FORTRAN

                    #For use statements, we need to scan all the files to know which one contains the module
                    useList[filename][scope.path] = virtual.findall('.//{*}use-stmt')

                    #Fill execution tree
                    #We need to scan all the files to find which one contains the subroutine/function
                    callList[filename][scope.path] = [n2name(c.find('./{*}procedure-designator/{*}named-E/{*}N')).upper()
                                                     for c in virtual.findall('.//{*}call-stmt')]
                    #We cannot distinguish function from arrays
                    funcList[filename][scope.path] = []
                    for name in [n2name(c.find('./{*}N')).upper()
                                 for c in virtual.findall('.//{*}named-E/{*}R-LT/{*}parens-R/../..')]:
                        #But we can exclude some names if they are declared as arrays
                        var = pft.findVar(name, scope.path, varList=varList)
                        if var is None or var['as'] is None:
                            funcList[filename][scope.path].append(name)


        #compilation_tree computation: include
        incInScope = {}
        for filename, incScopePaths in includeList.items():
            #Loop on scopes
            for scopePath, incList in incScopePaths.items():
                #Loop on each included file
                incInScope[scopePath] = []
                for inc in incList:
                    #Try to guess the right file
                    same = []
                    subdir = []
                    basename = []
                    #Loop on each file found in the source tree
                    for f in result['compilation_tree']:
                        if os.path.normpath(inc) == os.path.normpath(f):
                            #Exactly the same file name (including directories)
                            same.append(f)
                        elif (not os.path.isabs(f)) and \
                             os.path.realpath(inc) == os.path.realpath(os.path.join(os.path.dirname(inc), f)):
                            #The include statement refers to a file contained in the directory where inc is
                            subdir.append(f)
                        elif os.path.basename(inc) == os.path.basename(f):
                            #Same name excluding the directories
                            basename.append(f)
                    if len(same) > 1: same = subdir = basename = []
                    if len(subdir) > 1: subdir = basename = []
                    if len(basename) > 1: basename = []
                    found = True
                    if len(same) > 0:
                        incFilename = same[0]
                    elif len(subdir) > 0:
                        incFilename = subdir[0]
                    elif len(basename) > 0:
                        incFilename = basename[0]
                    else:
                        #We haven't found the file in the tree, we keep the inc untouched
                        found = False
                        incFilename = inc
                    result['compilation_tree'][filename].append(incFilename)
                    if found: incInScope[scopePath].append(incFilename)

        #compilation_tree computation: use
        for filename, uList in useList.items():
            #Loop on each use statement
            for use in [use for l in uList.values() for use in l]:
                moduleScopePath = 'module:' + n2name(use.find('./{*}module-N/{*}N')).upper()
                #Loop on scopes to find the module
                found = []
                for f, scopes in result['scopes'].items():
                    if moduleScopePath in scopes:
                        found.append(f)
                if len(found) == 1:
                    result['compilation_tree'][filename].append(found[0])
                else:
                    logging.warning(('Several or none file containing the scope path {scopePath} ' +
                                     'have been found for file {filename}'
                                    ).format(scopePath=moduleScopePath, filename=filename))

        #compilation_tree: cleaning (uniq values)
        for filename, depList in result['compilation_tree'].items():
            result['compilation_tree'][filename] = list(set(depList))

        #execution_tree: call statements
        allScopes = [scopePath for _, l in result['scopes'].items() for scopePath in l]
        result['execution_tree'] = {scopePath: [] for scopePath in allScopes}
        for canonicKind, progList in (('sub', callList), ('func', funcList)):
            for filename, callScopes in progList.items():
                #Loop on scopes
                for scopePath, cList in callScopes.items():
                    #Loop on calls
                    for c in set(cList):
                        foundInUse = []
                        foundElsewhere = []
                        foundInInclude = []
                        foundInContains = []
                        foundInSameScope = []

                        #We look for sub:c or interface:c
                        for kind in (canonicKind, 'interface'):
                            #Loop on each use statement in scope or in upper scopes
                            uList = [useList[filename][sc] for sc in useList[filename]
                                     if (sc == scopePath or scopePath.startswith(sc + '/'))]
                            for use in [use for l in uList for use in l]:
                                moduleScope = 'module:' + n2name(use.find('./{*}module-N/{*}N')).upper()
                                only = [n2name(n).upper() for n in use.findall('.//{*}use-N//{*}N')]
                                callScope = moduleScope + '/' + kind + ':' + c
                                if len(only) > 0:
                                    #There is a "ONLY" keyword
                                    if c in only and callScope in allScopes:
                                        foundInUse.append(callScope)
                                else:
                                    #There is no "ONLY"
                                    for _, scopes in result['scopes'].items():
                                        if callScope in scopes:
                                            foundInUse.append(callScope)

                            #Look for subroutine directly accessible
                            callScope = kind + ':' + c
                            for _, scopes in result['scopes'].items():
                                if callScope in scopes:
                                    foundElsewhere.append(callScope)

                            #Look for include files
                            callScope = kind + ':' + c
                            for incFile in incInScope[scopePath]:
                                if callScope in result['scopes'][incFile]:
                                    foundInInclude.append(callScope)

                            #Look for contained routines
                            callScope = scopePath + '/' + kind + ':' + c
                            if callScope in result['scopes'][filename]:
                                foundInContains.append(callScope)

                            #Look for routine in the same scope
                            if '/' in scopePath:
                                callScope = scopePath.rsplit('/', 1)[0] + '/' + kind + ':' + c
                            else:
                                callScope = kind + ':' + c
                            if callScope in result['scopes'][filename]:
                                foundInSameScope.append(callScope)

                        #Final selection
                        foundInUse = list(set(foundInUse)) #If a module is used several times
                        if len(foundInUse + foundInInclude + foundInContains + foundInSameScope) > 1:
                            logging.error(('Several definition of the program unit found for {callScope} ' + \
                                           'called in {scopePath}:').format(callScope=c, scopePath=scopePath))
                            logging.error('  found {i} time(s) in USE statements'.format(i=len(foundInUse)))
                            logging.error('  found {i} time(s) in include files'.format(i=len(foundInInclude)))
                            logging.error('  found {i} time(s) in CONTAINS block'.format(i=len(foundInContains)))
                            logging.error('  found {i} time(s) in the same scope'.format(i=len(foundInSameScope)))
                            result['execution_tree'][scopePath].append('??')
                        elif len(foundInUse + foundInInclude + foundInContains + foundInSameScope) == 1:
                            r = (foundInUse + foundInInclude + foundInContains + foundInSameScope)[0]
                            if canonicKind != 'func' or r in allScopes:
                                result['execution_tree'][scopePath].append(r)
                        elif len(foundElsewhere) > 1:
                            logging.warning(('Several definition of the program unit found for {callScope} ' + \
                                             'called in {scopePath}').format(callScope=c, scopePath=scopePath))
                        elif len(foundElsewhere) == 1:
                            result['execution_tree'][scopePath].append(foundElsewhere[0])
                        else:
                            if canonicKind != 'func':
                                logging.warning(('No definition of the program unit found for {callScope} ' + \
                                                 'called in {scopePath}').format(callScope=c, scopePath=scopePath))

        #execution_tree: named interface
        #We replace named interface by the list of routines declared in this interface
        #This is not perfect because only one routine is called and not all
        for _, execList in result['execution_tree'].items():
            for item in list(execList):
                itemSplt = item.split('/')[-1].split(':')
                if itemSplt[0] == 'interface' and itemSplt[1] != '--UNKNOWN--':
                    #This is a named interface
                    filenames = [k for (k, v) in result['scopes'].items() if item in v]
                    if len(filenames) == 1:
                        #We have found in which file this interface is declared
                        execList.remove(item)
                        for sub in [sub for sub in result['scopes'][filenames[0]]
                                    if sub.startswith(item + '/')]:
                            subscopeIn = sub.rsplit('/', 2)[0] + '/' + sub.split('/')[-1]
                            if subscopeIn in result['scopes'][filenames[0]]:
                                #Routine found in the same scope as the interface
                                execList.append(subscopeIn)
                            else:
                                execList.append(sub.split('/')[-1])


        #execution_tree: cleaning (uniq values)
        for scopePath, execList in result['execution_tree'].items():
            result['execution_tree'][scopePath] = list(set(execList))

        descTreToJson(result, descTree)
        return result
    else:
        return jsonToDescTree(descTree)

@debugDecor
def jsonToDescTree(descTree):
    if isinstance(descTree, str):
        with open(descTree, 'r') as f:
            return json.load(f)
    else:
        return descTree

#No @debugDecor for this low-level method
def descTreToJson(descTreeObj, descTreeFile):
    with open(descTreeFile, 'w') as f:
        json.dump(descTreeObj, f)

#No @debugDecor for this low-level method
def scopeToFiles(scopePath, descTree):
    """
    Return the name of the file defining the scope
    :param scopePath: scope path to search for
    :param descTree: tree description file (obtained by descTree) or its json equivalence
    :return: list file names in which scope is defined
    """
    return [filename for filename, scopes in descTree['scopes'].items() if scopePath in scopes]

@debugDecor
def fileToScopes(filename, descTree):
    """
    Return the scopes contained in the file
    :param filename: name of the file tn inspect
    :param descTree: tree description file (obtained by descTree) or its json equivalence
    :return: list of scopes defined in the file
    """
    return jsonToDescTree(descTree)['scopes'][filename]

def _recurList(node, descTreePart, level, down):
    """
    :param node: initial node
    :param descTreePart: 'compilation_tree' or 'execution_tree' part of a descTree object
    :param level: number of levels (0 to get only the initial node, None to get all nodes)
    :param down: True to get the nodes lower in the tree, False to get the upper ones
    :return: list of nodes lower or upper tahn initial node (recursively)
    """
    def recur(n, level, currentList):
        if down:
            result = descTreePart.get(n, [])
        else:
            result = [item for (item, l) in descTreePart.items() if n in l]
        if level is None or level > 1:
            for r in list(result):
                if r not in currentList: #for FORTRAN recursive calls
                    result.extend(recur(r, None if level is None else level - 1, result))
        return result
    return recur(node, level, [])

@debugDecor
def needsFile(filename, descTree, level=1):
    """
    :param filename: initial file name
    :param descTree: tree description file (obtained by descTree) or its json equivalence
    :param level: number of levels (0 to get only the initial file, None to get all files)
    :return: list of file names needed by the initial file (recursively)
    """
    return _recurList(filename, jsonToDescTree(descTree)['compilation_tree'], level, True)

@debugDecor
def neededByFile(filename, descTree, level=1):
    """
    :param filename: initial file name
    :param descTree: tree description file (obtained by descTree) or its json equivalence
    :param level: number of levels (0 to get only the initial file, None to get all files)
    :return: list of file names that needs the initial file (recursively)
    """
    return _recurList(filename, jsonToDescTree(descTree)['compilation_tree'], level, False)

@debugDecor
def callsScopes(scopePath, descTree, level=1):
    """
    :param scopePath: initial scope path
    :param descTree: tree description file (obtained by descTree) or its json equivalence
    :param level: number of levels (0 to get only the initial scope path, None to get all scopes)
    :return: list of scopes called by the initial scope path (recursively)
    """
    return _recurList(scopePath, jsonToDescTree(descTree)['execution_tree'], level, True)

@debugDecor
def calledByScope(scopePath, descTree, level=1):
    """
    :param scopePath: initial scope path
    :param descTree: tree description file (obtained by descTree) or its json equivalence
    :param level: number of levels (0 to get only the initial scope path, None to get all scopes)
    :return: list of scopes that calls the initial scope path (recursively)
    """
    return _recurList(scopePath, jsonToDescTree(descTree)['execution_tree'], level, False)

@debugDecor
def isUnderStopScopes(scopePath, descTree, stopScopes, includeInterfaces=False, includeStopScopes=False):
    """
    :param scopePath: scope path to test
    :param descTree: tree description file (obtained by descTree) or its json equivalence
    :param stopScopes: list of scopes
    :param includeInterfaces: if True, interfaces of positive scopes are also positive
    :param includeInterfaces: if True, scopes that are in stopScopes return True
    :return: True if the scope path is called directly or indirectly by one of the scope paths listed in stopScopes
    """
    scopeSplt = scopePath.split('/')
    if includeInterfaces and len(scopeSplt) >= 2 and scopeSplt[-2].split(':')[0] == 'interface':
        #This scope declares an interface, we look for the scope corresponding to this interface
        scopeI = scopeSplt[-1]
        if scopeI in descTree['execution_tree']:
            #The actual code for the routine exists
            return isUnderStopScopes(scopeI, descTree, stopScopes, includeStopScopes=includeStopScopes)
        else:
            #No code found for this interface
            return False
    upperScopes = calledByScope(scopePath, descTree, None)
    return any([scp in upperScopes for scp in stopScopes]) or (includeStopScopes and scopePath in stopScopes)

@debugDecor
def plotTree(centralNodeList, descTree, output, plotMaxUpper, plotMaxLower, kind, frame=False):
    """
    Compute a dependency graph
    :param centralNodeList: file, scope path, list of files or list of scope paths
    :param descTree: tree description file (obtained by descTree)
    :param output: output file name (.dot or .png extension)
    :param plotMaxUpper: Maximum number of elements to plot, upper than the central element
    :param plotMaxLower: Maximum number of elements to plot, lower than the central element
    :param kind: must be 'compilation_tree' or 'execution_tree'
    :param frame: True to plot a frame grouping the central nodes
    """
    assert kind in ('compilation_tree', 'execution_tree')
    def h(obj):
        result = str(hash(obj))
        if result[0] == '-':
            result = 'M' + result[1:] #to minus sign
        return result
    def createNode(node, label=None):
        result = ""
        if label is not None:
            result += "subgraph cluster_" + h(node) + " {\n"
            result += 'label="{label}"\n'.format(label=label)
        if kind == 'execution_tree':
            color = 'blue' if node.split('/')[-1].split(':')[0] == 'func' else 'green'
        else:
            color = 'black'
        result += h(node) + ' [label="{node}" color="{color}"]\n'.format(node=node, color=color)
        if label is not None:
            result += "}\n"
        return result
    def createLink(file1, file2):
        return h(file1) + ' -> ' + h(file2) + '\n'
    def createCluster(nodes, label=None):
        result = "subgraph cluster_R {\n"
        result += "{rank=same " + (' '.join([h(node) for node in nodes])) + "}\n"
        if label is not None:
            result += 'label="{label}"\n'.format(label=label)
        result += "}\n"
        return result
    def add(item):
        if item not in dot: dot.append(item)
    def filename(scopePath):
        if kind == 'compilation_tree':
            return None
        else:
            return [f for f, l in descTree['scopes'].items() if scopePath in l][0]
    def recur(node, level, down):
        if level is None or level > 0:
            if down:
                result = descTree[kind].get(node, [])
            else:
                result = [f for f, l in descTree[kind].items()
                          if node in l]
            for r in result:
                add(createNode(r, filename(r)))
                add(createLink(node, r) if down else createLink(r, node))
                if level is None or level > 1:
                    recur(r, None if level is None else level - 1, down)

    # Read the tree description file
    descTree = jsonToDescTree(descTree)

    # Are all the central scopes in the same file
    printInFrame = False
    if kind == 'execution_tree':
        centralScopeFilenames = []
        for scopePath in centralNodeList:
            centralScopeFilenames.append(filename(scopePath))
        centralScopeFilenames = list(set(centralScopeFilenames))
        if len(centralScopeFilenames) == 1:
            frame = True
            printInFrame = True
        else:
            printInFrame = False

    dot = ["digraph D {\n"]
    if not isinstance(centralNodeList, list):
        centralNodeList = [centralNodeList]
    for centralNode in centralNodeList:
        add(createNode(centralNode, None if printInFrame else filename(centralNode)))
        recur(centralNode, plotMaxLower, True)
        recur(centralNode, plotMaxUpper, False)
    if frame:
        if kind == 'compilation_tree':
            frameText = None
        else:
            frameText = centralScopeFilenames[0] if printInFrame else None
        add(createCluster(centralNodeList, frameText))
    add("}\n")
    dot = ''.join(dot)
    fmt = os.path.splitext(output)[1].lower()[1:]
    if fmt == 'dot':
        with open(output, 'w') as f:
            f.write(dot)
    else:
        dotCommand = ['dot', '-T' + fmt, '-o', output]
        logging.info('Dot command: ' + ' '.join(dotCommand))
        subprocess.run(dotCommand, input=dot.encode('utf8'), check=True)

@debugDecor
def plotCompilTreeFromFile(filename, descTree, output, plotMaxUpper, plotMaxLower):
    """
    Compute the compilation dependency graph
    :param filename: central file
    :param descTree: tree description file (obtained by descTree)
    :param output: output file name (.dot or .png extension)
    :param plotMaxUpper: Maximum number of elements to plot, upper than the central element
    :param plotMaxLower: Maximum number of elements to plot, lower than the central element
    """
    return plotTree(filename, descTree, output, plotMaxUpper, plotMaxLower, 'compilation_tree', True)

@debugDecor
def plotExecTreeFromScope(scopePath, descTree, output, plotMaxUpper, plotMaxLower):
    """
    Compute the execution dependency graph
    :param scopePath: central scope path
    :param descTree: tree description file (obtained by descTree)
    :param output: output file name (.dot or .png extension)
    :param plotMaxUpper: Maximum number of elements to plot, upper than the central element
    :param plotMaxLower: Maximum number of elements to plot, lower than the central element
    """
    return plotTree(scopePath, descTree, output, plotMaxUpper, plotMaxLower, 'execution_tree')

@debugDecor
def plotCompilTreeFromScope(scopePath, descTree, output, plotMaxUpper, plotMaxLower):
    """
    Compute the compilation dependency graph
    :param scopePath: central scope path
    :param descTree: tree description file (obtained by descTree)
    :param output: output file name (.dot or .png extension)
    :param plotMaxUpper: Maximum number of elements to plot, upper than the central element
    :param plotMaxLower: Maximum number of elements to plot, lower than the central element
    """
    descTree = jsonToDescTree(descTree)
    return plotTree(scopeToFiles(scopePath, descTree), descTree, output, plotMaxUpper, plotMaxLower,
                    'compilation_tree')

@debugDecor
def plotExecTreeFromFile(filename, descTree, output, plotMaxUpper, plotMaxLower):
    """
    Compute the execution dependency graph
    :param filename: central filename
    :param descTree: tree description file (obtained by descTree)
    :param output: output file name (.dot or .png extension)
    :param plotMaxUpper: Maximum number of elements to plot, upper than the central element
    :param plotMaxLower: Maximum number of elements to plot, lower than the central element
    """
    descTree = jsonToDescTree(descTree)
    return plotTree(fileToScopes(filename, descTree), descTree, output, plotMaxUpper, plotMaxLower,
                    'execution_tree', True)

@debugDecor
def findScopeInterface(descTree, scopePath):
    """
    Return the file name containing an interface for the scope path
    :param descTree: descTree file
    :param scopePath: scope path for which an interface is searched
    :return: (file name, interface scope) or (None, None) if not found
    """
    for filename, scopes in descTree['scopes'].items():
        for scopeInterface in scopes:
            if re.search(r'interface:[a-zA-Z0-9_-]*/' + scopePath, scopeInterface):
                return filename, scopeInterface
    return None, None


class Tree():
    @copy_doc(getDirs)
    def getDirs(self):
        return getDirs(tree=self.tree)

    @copy_doc(descTree)
    def descTree(self, *args, **kwargs):
        return descTree(*args, **kwargs)

    @copy_doc(plotCompilTreeFromFile)
    def plotCompilTreeFromFile(self, *args, **kwargs):
        return plotCompilTreeFromFile(*args, **kwargs)

    @copy_doc(plotExecTreeFromFile)
    def plotExecTreeFromFile(self, *args, **kwargs):
        return plotExecTreeFromFile(*args, **kwargs)

    @copy_doc(plotCompilTreeFromScope)
    def plotCompilTreeFromScope(self, *args, **kwargs):
        return plotCompilTreeFromFile(*args, **kwargs)

    @copy_doc(plotExecTreeFromScope)
    def plotExecTreeFromScope(self, *args, **kwargs):
        return plotExecTreeFromFile(*args, **kwargs)
