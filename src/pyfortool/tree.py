"""
This module contains the Tree class to browse the tree
"""

import glob
import os
import logging
import json
import subprocess
import re
from functools import wraps

from pyfortool.util import debugDecor, n2name
import pyfortool.scope
import pyfortool.pyfortool


def updateTree(method='file'):
    """
    Decorator factory to update the tree after having executed a PYFTscope method
    :param method: method to use for updating
                   - 'file': analyze current file (default)
                   - 'scan': analyse new files and suppress tree information
                             for suppressed files
                   - 'signal': analyse files (if any) signaled using
                               the signal method of the tree object
    """
    assert method in ('file', 'scan', 'signal')

    def decorator(func):
        @wraps(func)
        def wrapper(self, *args, **kwargs):
            result = func(self, *args, **kwargs)
            if self.tree.isValid:
                if method == 'file':
                    self.tree.update(self)
                elif method == 'scan':
                    current = set(self.tree.getFiles())
                    old = set(self.tree.knownFiles())
                    self.tree.update(current.symmetric_difference(old))
                elif method == 'signal':
                    self.tree.update(self.tree.popSignaled())
            return result
        return wrapper
    return decorator


class Tree():
    """
    Class to browse the Tree
    """
    def __init__(self, tree=None, descTreeFile=None,
                 parser=None, parserOptions=None, wrapH=False,
                 verbosity=None):
        """
        :param tree: list of directories composing the tree or None
        :param descTreeFile: filename where the description of the tree will be stored
        :param parser, parserOptions, wrapH: see the PYFT class
        :param verbosity: if not None, sets the verbosity level
        """
        # Options
        self._tree = [] if tree is None else tree
        self._descTreeFile = descTreeFile
        self._parser = parser
        self._parserOptions = parserOptions
        self._wrapH = wrapH
        self._verbosity = verbosity

        # Files signaled for update
        self._signaled = set()

        # File analysis
        self._cwd = os.getcwd()
        self._emptyCache()
        self._scopes = {}
        self._useList = {}
        self._includeList = {}
        self._callList = {}
        self._funcList = {}
        self._cacheCompilationTree = None
        self._cacheExecutionTree = None
        self._cacheIncInScope = None
        if descTreeFile is not None and os.path.exists(descTreeFile):
            self.fromJson(descTreeFile)
        elif tree is not None:
            self._build()
            if descTreeFile is not None:
                self.toJson(descTreeFile)

    def getFullContent(self):
        """
        :return: a dict containing the full content of the instance
        """
        return {'tree': self._tree,
                'descTreeFile': self._descTreeFile,
                'parser': self._parser,
                'parserOptions': self._parserOptions,
                'wrapH': self._wrapH,
                'verbosity': self._verbosity,
                'cwd': self._cwd,
                'scopes': self._scopes,
                'useList': self._useList,
                'includeList': self._includeList,
                'callList': self._callList,
                'funcList': self._funcList,
                'signaled': self._signaled,
                'cache_compilationTree': self._cacheCompilationTree,
                'cacheExecutionTree': self._cacheExecutionTree,
                'cacheIncScope': self._cacheIncInScope}

    def setFullContent(self, content):
        """
        :param content: Fill the current instance with this dict
        """
        self._tree = content['tree']
        self._descTreeFile = content['descTreeFile']
        self._parser = content['parser']
        self._parserOptions = content['parserOptions']
        self._wrapH = content['wrapH']
        self._verbosity = content['verbosity']
        self._cwd = content['cwd']
        self._scopes = content['scopes']
        self._useList = content['useList']
        self._includeList = content['includeList']
        self._callList = content['callList']
        self._funcList = content['funcList']
        self._signaled = content['signaled']
        self._cacheCompilationTree = content['cache_compilationTree']
        self._cacheExecutionTree = content['cacheExecutionTree']
        self._cacheIncInScope = content['cacheIncScope']

    def copyFromOtherTree(self, other):
        """
        Sets self to be a copy of other
        """
        self.setFullContent(other.getFullContent())

    def copyToOtherTree(self, other):
        """
        Sets other to be a copy of self
        """
        other.setFullContent(self.getFullContent())

    def signal(self, file):
        """
        Method used for signaling a modified file which needs to be analized
        :param filename: file name or PYFTscope object
        """
        self._signaled.add(file)

    def popSignaled(self):
        """
        :return: the list of file signaled for update and empties the list
        """
        temp = self._signaled
        self._signaled = set()
        return temp

    def knownFiles(self):
        """
        :return: the list of analysez file names
        """
        return list(self._scopes.keys())

    @property
    def isValid(self):
        """Is the Tree object valid"""
        return len(self._scopes) != 0

    @property
    def tree(self):
        """List of directories"""
        return self._tree

    @debugDecor
    def getDirs(self):
        """
        :param tree: list of directories composing the tree or None
        :return: list of directories and subdirectories
        """
        result = []
        if self.tree is not None:
            for tDir in self.tree:
                result += glob.glob(tDir + '/**/', recursive=True)
        return result

    @debugDecor
    def getFiles(self):
        """
        :param tree: list of directories composing the tree or None
        :return: list of directories and subdirectories
        """
        filenames = []
        for tDir in self.tree:
            for filename in glob.glob(tDir + '/**/*', recursive=True):
                if os.path.splitext(filename)[1] not in ('', '.json', '.fypp', '.txt'):
                    # We only keep files with extension
                    filenames.append(filename)
        return filenames

    @debugDecor
    def _build(self):
        """
        Builds the self._* variable
        """
        # Loop on directory and files
        for filename in self.getFiles():
            self._analyseFile(filename)

    @debugDecor
    def update(self, file):
        """
        Updates the object when a file has changed
        :param file: name of the file (or list of names) with updated content
                     or PYFTscope object
        """
        if self.isValid:
            if not isinstance(file, (list, set)):
                file = [file]
            if len(file) != 0:
                for onefile in file:
                    self._analyseFile(onefile)
                self._emptyCache()

    def _emptyCache(self):
        """Empties cached values"""
        self._cacheCompilationTree = None
        self._cacheExecutionTree = None
        self._cacheIncInScope = None

    @property
    def _incInScope(self):
        """Fill and return the self._cacheIncInScope cached value"""
        if self.isValid and self._cacheIncInScope is None:
            # pylint: disable-next=pointless-statement
            self._compilationTree  # self._cacheIncInScope computed at the same time
        return self._cacheIncInScope

    @property
    @debugDecor
    def _compilationTree(self):
        """Fill and return the self._cacheCompilationTree cached value"""
        if self.isValid and self._cacheCompilationTree is None:
            self._cacheCompilationTree = {f: [] for f in self._scopes}
            self._cacheIncInScope = {}
            # Compilation_tree computation: include
            for filename, incScopePaths in self._includeList.items():
                # Loop on scopes
                for scopePath, incList in incScopePaths.items():
                    # Loop on each included file
                    self._cacheIncInScope[scopePath] = []
                    for inc in incList:
                        # Try to guess the right file
                        same = []
                        subdir = []
                        basename = []
                        # Loop on each file found in the source tree
                        for file in self._cacheCompilationTree:
                            if os.path.normpath(inc) == os.path.normpath(file):
                                # Exactly the same file name (including directories)
                                same.append(file)
                            elif ((not os.path.isabs(file)) and
                                  os.path.realpath(inc) == os.path.realpath(os.path.join(
                                      os.path.dirname(inc), file))):
                                # The include statement refers to a file contained in the
                                # directory where inc is
                                subdir.append(file)
                            elif os.path.basename(inc) == os.path.basename(file):
                                # Same name excluding the directories
                                basename.append(file)
                        if len(same) > 1:
                            same = subdir = basename = []
                        if len(subdir) > 1:
                            subdir = basename = []
                        if len(basename) > 1:
                            basename = []
                        found = True
                        if len(same) > 0:
                            incFilename = same[0]
                        elif len(subdir) > 0:
                            incFilename = subdir[0]
                        elif len(basename) > 0:
                            incFilename = basename[0]
                        else:
                            # We haven't found the file in the tree, we keep the inc untouched
                            found = False
                            incFilename = inc
                        self._cacheCompilationTree[filename].append(incFilename)
                        if found:
                            self._cacheIncInScope[scopePath].append(incFilename)

            # Compilation_tree computation: use
            for filename, uList in self._useList.items():
                # Loop on each use statement
                for modName, _ in [use for li in uList.values() for use in li]:
                    moduleScopePath = 'module:' + modName
                    # Loop on scopes to find the module
                    found = []
                    for file, scopes in self._scopes.items():
                        if moduleScopePath in scopes:
                            found.append(file)
                    if len(found) == 1:
                        self._cacheCompilationTree[filename].append(found[0])
                    else:
                        logging.info('Several or none file containing the scope path ' +
                                     '%s have been found for file %s',
                                     moduleScopePath, filename)

            # Compilation_tree: cleaning (uniq values)
            for filename, depList in self._cacheCompilationTree.items():
                self._cacheCompilationTree[filename] = list(set(depList))

        return self._cacheCompilationTree

    @property
    @debugDecor
    def _executionTree(self):
        """Fill and return the self._cacheCompilationTree cached value"""
        if self.isValid and self._cacheExecutionTree is None:
            self._cacheExecutionTree = {}
            # Execution_tree: call statements
            allScopes = [scopePath for _, l in self._scopes.items() for scopePath in l]
            self._cacheExecutionTree = {scopePath: [] for scopePath in allScopes}
            for canonicKind, progList in (('sub', self._callList), ('func', self._funcList)):
                for filename, callScopes in progList.items():
                    # Loop on scopes
                    for scopePath, cList in callScopes.items():
                        # Loop on calls
                        for call in set(cList):
                            foundInUse = []
                            foundElsewhere = []
                            foundInInclude = []
                            foundInContains = []
                            foundInSameScope = []

                            # We look for sub:c or interface:c
                            for kind in (canonicKind, 'interface'):
                                # Loop on each use statement in scope or in upper scopes
                                uList = [self._useList[filename][sc]
                                         for sc in self._useList[filename]
                                         if (sc == scopePath or scopePath.startswith(sc + '/'))]
                                for modName, only in [use for li in uList for use in li]:
                                    moduleScope = 'module:' + modName
                                    callScope = moduleScope + '/' + kind + ':' + call
                                    if len(only) > 0:
                                        # There is a "ONLY" keyword
                                        if call in only and callScope in allScopes:
                                            foundInUse.append(callScope)
                                    else:
                                        # There is no "ONLY"
                                        for _, scopes in self._scopes.items():
                                            if callScope in scopes:
                                                foundInUse.append(callScope)

                                # Look for subroutine directly accessible
                                callScope = kind + ':' + call
                                for _, scopes in self._scopes.items():
                                    if callScope in scopes:
                                        foundElsewhere.append(callScope)

                                # Look for include files
                                callScope = kind + ':' + call
                                for incFile in self._incInScope[scopePath]:
                                    if callScope in self._scopes[incFile]:
                                        foundInInclude.append(callScope)

                                # Look for contained routines
                                callScope = scopePath + '/' + kind + ':' + call
                                if callScope in self._scopes[filename]:
                                    foundInContains.append(callScope)

                                # Look for routine in the same scope
                                if '/' in scopePath:
                                    callScope = scopePath.rsplit('/', 1)[0] + '/' + kind + \
                                                ':' + call
                                else:
                                    callScope = kind + ':' + call
                                if callScope in self._scopes[filename]:
                                    foundInSameScope.append(callScope)

                            # Final selection
                            foundInUse = list(set(foundInUse))  # If a module is used several times
                            if len(foundInUse + foundInInclude +
                                   foundInContains + foundInSameScope) > 1:
                                logging.error('Several definition of the program unit found for '
                                              '%s called in %s:', call, scopePath)
                                logging.error('  found %i time(s) in USE statements',
                                              len(foundInUse))
                                logging.error('  found %i time(s) in include files',
                                              len(foundInInclude))
                                logging.error('  found %i time(s) in CONTAINS block',
                                              len(foundInContains))
                                logging.error('  found %i time(s) in the same scope',
                                              len(foundInSameScope))
                                self._cacheExecutionTree[scopePath].append('??')
                            elif len(foundInUse + foundInInclude +
                                     foundInContains + foundInSameScope) == 1:
                                rr = (foundInUse + foundInInclude +
                                      foundInContains + foundInSameScope)[0]
                                if canonicKind != 'func' or rr in allScopes:
                                    self._cacheExecutionTree[scopePath].append(rr)
                            elif len(foundElsewhere) > 1:
                                logging.info('Several definition of the program unit found for '
                                             '%s called in %s', call, scopePath)
                            elif len(foundElsewhere) == 1:
                                self._cacheExecutionTree[scopePath].append(foundElsewhere[0])
                            else:
                                if canonicKind != 'func':
                                    logging.info('No definition of the program unit found for '
                                                 '%s called in %s', call, scopePath)

            # Execution_tree: named interface
            # We replace named interface by the list of routines declared in this interface
            # This is not perfect because only one routine is called and not all
            for _, execList in self._cacheExecutionTree.items():
                for item in list(execList):
                    itemSplt = item.split('/')[-1].split(':')
                    if itemSplt[0] == 'interface' and itemSplt[1] != '--UNKNOWN--':
                        # This is a named interface
                        filenames = [k for (k, v) in self._scopes.items() if item in v]
                        if len(filenames) == 1:
                            # We have found in which file this interface is declared
                            execList.remove(item)
                            for sub in [sub for sub in self._scopes[filenames[0]]
                                        if sub.startswith(item + '/')]:
                                subscopeIn = sub.rsplit('/', 2)[0] + '/' + sub.split('/')[-1]
                                if subscopeIn in self._scopes[filenames[0]]:
                                    # Routine found in the same scope as the interface
                                    execList.append(subscopeIn)
                                else:
                                    execList.append(sub.split('/')[-1])

            # Execution_tree: cleaning (uniq values)
            for scopePath, execList in self._cacheExecutionTree.items():
                self._cacheExecutionTree[scopePath] = list(set(execList))

        return self._cacheExecutionTree

    @debugDecor
    def _analyseFile(self, file):
        """
        :param file: Name of the file to explore, or PYFTscope object
        :return: dict of use, include, call, function and scope list
        """
        def extractString(text):
            text = text.strip()
            if text[0] in ('"', "'"):
                assert text[-1] == text[0]
                text = text[1, -1]
            return text

        # Loop on directory and files
        if isinstance(file, pyfortool.scope.PYFTscope) or os.path.isfile(file):
            if isinstance(file, pyfortool.scope.PYFTscope):
                pft = file.mainScope
                filename = pft.getFileName()
                mustClose = False
            else:
                pft = pyfortool.pyfortool.conservativePYFT(file, self._parser, self._parserOptions,
                                                           self._wrapH, verbosity=self._verbosity)
                filename = file
                mustClose = True
            filename = filename[2:] if filename.startswith('./') else filename

            # Loop on scopes
            self._scopes[filename] = []
            self._includeList[filename] = {}
            self._useList[filename] = {}
            self._callList[filename] = {}
            self._funcList[filename] = {}
            scopes = pft.getScopes()
            for scope in scopes:
                # Scope found in file
                self._scopes[filename].append(scope.path)
                # We add, to this list, the "MODULE PROCEDURE" declared in INTERFACE statements
                if scope.path.split('/')[-1].split(':')[0] == 'interface':
                    for name in [n2name(nodeN).upper()
                                 for moduleproc in scope.findall('./{*}procedure-stmt')
                                 for nodeN in moduleproc.findall('./{*}module-procedure-N-LT/' +
                                                                 '{*}N')]:
                        for sc in scopes:
                            if re.search(scope.path.rsplit('/', 1)[0] + '/[a-zA-Z]*:' + name,
                                         sc.path):
                                self._scopes[filename].append(scope.path + '/' +
                                                              sc.path.split('/')[-1])

                # include, use, call and functions
                # Fill compilation_tree
                # Includes give directly the name of the source file but possibly without
                # the directory
                self._includeList[filename][scope.path] = \
                    [file.text for file in scope.findall('.//{*}include/{*}filename')]  # cpp
                self._includeList[filename][scope.path].extend(
                    [extractString(file.text)
                     for file in scope.findall('.//{*}include/{*}filename/{*}S')])  # FORTRAN

                # For use statements, we need to scan all the files to know which one
                # contains the module
                self._useList[filename][scope.path] = []
                for use in scope.findall('.//{*}use-stmt'):
                    modName = n2name(use.find('./{*}module-N/{*}N')).upper()
                    only = [n2name(n).upper() for n in use.findall('.//{*}use-N//{*}N')]
                    self._useList[filename][scope.path].append((modName, only))

                # Fill execution tree
                # We need to scan all the files to find which one contains the subroutine/function
                self._callList[filename][scope.path] = \
                    list(set(n2name(call.find('./{*}procedure-designator/{*}named-E/{*}N')).upper()
                             for call in scope.findall('.//{*}call-stmt')))
                # We cannot distinguish function from arrays
                self._funcList[filename][scope.path] = set()
                for name in [n2name(call.find('./{*}N')).upper()
                             for call in scope.findall('.//{*}named-E/{*}R-LT/{*}parens-R/../..')]:
                    # But we can exclude some names if they are declared as arrays
                    var = scope.varList.findVar(name)
                    if var is None or var['as'] is None:
                        self._funcList[filename][scope.path].add(name)
                self._funcList[filename][scope.path] = list(self._funcList[filename][scope.path])
            if mustClose:
                pft.close()
        else:
            if filename in self._scopes:
                del self._scopes[filename], self._includeList[filename], \
                    self._useList[filename], self._callList[filename], \
                    self._funcList[filename]

    @debugDecor
    def fromJson(self, filename):
        """read from json"""
        with open(filename, 'r', encoding='utf-8') as file:
            descTree = json.load(file)
        self._cwd = descTree['cwd']
        self._scopes = descTree['scopes']
        self._useList = descTree['useList']
        self._includeList = descTree['includeList']
        self._callList = descTree['callList']
        self._funcList = descTree['funcList']

    @debugDecor
    def toJson(self, filename):
        """save to json"""
        descTree = {'cwd': self._cwd,
                    'scopes': self._scopes,
                    'useList': self._useList,
                    'includeList': self._includeList,
                    'callList': self._callList,
                    'funcList': self._funcList,
                    }
        # Order dict keys and list values
        descTree['scopes'] = {k: sorted(descTree['scopes'][k]) for k in sorted(descTree['scopes'])}
        for cat in ('useList', 'includeList', 'callList', 'funcList'):
            descTree[cat] = {file: {scope: sorted(descTree[cat][file][scope])
                                    for scope in sorted(descTree[cat][file])}
                             for file in sorted(descTree[cat])}
        # Write json on disk with indentation
        with open(filename, 'w', encoding='utf-8') as file:
            json.dump(descTree, file, indent=2)

    # No @debugDecor for this low-level method
    def scopeToFiles(self, scopePath):
        """
        Return the name of the file defining the scope
        :param scopePath: scope path to search for
        :return: list file names in which scope is defined
        """
        return [filename for filename, scopes in self._scopes.items() if scopePath in scopes]

    @debugDecor
    def fileToScopes(self, filename):
        """
        Return the scopes contained in the file
        :param filename: name of the file tn inspect
        :return: list of scopes defined in the file
        """
        return self._scopes[filename]

    @staticmethod
    def _recurList(node, descTreePart, level, down):
        """
        :param node: initial node
        :param descTreePart: 'compilation_tree' or 'execution_tree' part of a descTree object
        :param level: number of levels (0 to get only the initial node, None to get all nodes)
        :param down: True to get the nodes lower in the tree, False to get the upper ones
        :return: list of nodes lower or upper tahn initial node (recursively)
        """
        def recur(nnn, level, currentList):
            if down:
                result = descTreePart.get(nnn, [])
            else:
                result = [item for (item, l) in descTreePart.items() if nnn in l]
            if level is None or level > 1:
                for res in list(result):
                    if res not in currentList:  # for FORTRAN recursive calls
                        result.extend(recur(res, None if level is None else level - 1, result))
            return result
        return recur(node, level, [])

    @debugDecor
    def needsFile(self, filename, level=1):
        """
        :param filename: initial file name
        :param level: number of levels (0 to get only the initial file, None to get all files)
        :return: list of file names needed by the initial file (recursively)
        """
        return self._recurList(filename, self._compilationTree, level, True)

    @debugDecor
    def neededByFile(self, filename, level=1):
        """
        :param filename: initial file name
        :param level: number of levels (0 to get only the initial file, None to get all files)
        :return: list of file names that needs the initial file (recursively)
        """
        return self._recurList(filename, self._compilationTree, level, False)

    @debugDecor
    def callsScopes(self, scopePath, level=1):
        """
        :param scopePath: initial scope path
        :param level: number of levels (0 to get only the initial scope path,
                                        None to get all scopes)
        :return: list of scopes called by the initial scope path (recursively)
        """
        return self._recurList(scopePath, self._executionTree, level, True)

    @debugDecor
    def calledByScope(self, scopePath, level=1):
        """
        :param scopePath: initial scope path
        :param level: number of levels (0 to get only the initial scope path,
                                        None to get all scopes)
        :return: list of scopes that calls the initial scope path (recursively)
        """
        return self._recurList(scopePath, self._executionTree, level, False)

    @debugDecor
    def isUnderStopScopes(self, scopePath, stopScopes,
                          includeInterfaces=False, includeStopScopes=False):
        """
        :param scopePath: scope path to test
        :param stopScopes: list of scopes
        :param includeInterfaces: if True, interfaces of positive scopes are also positive
        :param includeInterfaces: if True, scopes that are in stopScopes return True
        :return: True if the scope path is called directly or indirectly by one of the scope
                 paths listed in stopScopes
        """
        scopeSplt = scopePath.split('/')
        if includeInterfaces and len(scopeSplt) >= 2 and scopeSplt[-2].split(':')[0] == 'interface':
            # This scope declares an interface, we look for the scope corresponding
            # to this interface
            scopeI = scopeSplt[-1]
            if scopeI in self._executionTree:
                # The actual code for the routine exists
                return self.isUnderStopScopes(scopeI, stopScopes,
                                              includeStopScopes=includeStopScopes)
            # No code found for this interface
            return False
        upperScopes = self.calledByScope(scopePath, None)
        return (any(scp in upperScopes for scp in stopScopes) or
                (includeStopScopes and scopePath in stopScopes))

    @debugDecor
    def plotTree(self, centralNodeList, output, plotMaxUpper, plotMaxLower, kind, frame=False):
        """
        Compute a dependency graph
        :param centralNodeList: file, scope path, list of files or list of scope paths
        :param output: output file name (.dot or .png extension)
        :param plotMaxUpper: Maximum number of elements to plot, upper than the central element
        :param plotMaxLower: Maximum number of elements to plot, lower than the central element
        :param kind: must be 'compilation_tree' or 'execution_tree'
        :param frame: True to plot a frame grouping the central nodes
        """
        assert kind in ('compilation_tree', 'execution_tree')

        hashValues = {0: 1}

        def myHash(obj):
            objHash = hash(obj)
            if objHash not in hashValues:
                hashValues[0] += 1
                hashValues[objHash] = hashValues[0]
            return str(hashValues[objHash])

        def createNode(node, label=None):
            result = ""
            if label is not None:
                result += "subgraph cluster_" + myHash(node) + " {\n"
                result += f'label="{label}"\n'
            if kind == 'execution_tree':
                color = 'blue' if node.split('/')[-1].split(':')[0] == 'func' else 'green'
            else:
                color = 'black'
            result += myHash(node) + f' [label="{node}" color="{color}"]\n'
            if label is not None:
                result += "}\n"
            return result

        def createLink(file1, file2):
            return myHash(file1) + ' -> ' + myHash(file2) + '\n'

        def createCluster(nodes, label=None):
            result = "subgraph cluster_R {\n"
            result += "{rank=same " + (' '.join([myHash(node) for node in nodes])) + "}\n"
            if label is not None:
                result += f'label="{label}"\n'
            result += "}\n"
            return result

        def add(item):
            if item not in dot:
                dot.append(item)

        def filename(scopePath):
            if kind == 'compilation_tree':
                return None
            return [f for f, l in self._scopes.items() if scopePath in l][0]

        def recur(node, level, down, var):
            if level is None or level > 0:
                if down:
                    result = var.get(node, [])
                else:
                    result = [f for f, l in var.items() if node in l]
                for res in result:
                    add(createNode(res, filename(res)))
                    add(createLink(node, res) if down else createLink(res, node))
                    if level is None or level > 1:
                        recur(res, None if level is None else level - 1, down, var)

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

        # Order the tree to obtain deterministic graphs
        var = self._executionTree if kind == 'execution_tree' else self._compilationTree
        var = {k: sorted(var[k]) for k in sorted(var)}

        dot = ["digraph D {\n"]
        if not isinstance(centralNodeList, list):
            centralNodeList = [centralNodeList]
        for centralNode in centralNodeList:
            add(createNode(centralNode, None if printInFrame else filename(centralNode)))
            recur(centralNode, plotMaxLower, True, var)
            recur(centralNode, plotMaxUpper, False, var)
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
            with open(output, 'w', encoding='utf-8') as file:
                file.write(dot)
        else:
            dotCommand = ['dot', '-T' + fmt, '-o', output]
            logging.info('Dot command: %s', ' '.join(dotCommand))
            subprocess.run(dotCommand, input=dot.encode('utf8'), check=True)

    @debugDecor
    def plotCompilTreeFromFile(self, filename, output, plotMaxUpper, plotMaxLower):
        """
        Compute the compilation dependency graph
        :param filename: central file
        :param output: output file name (.dot or .png extension)
        :param plotMaxUpper: Maximum number of elements to plot, upper than the central element
        :param plotMaxLower: Maximum number of elements to plot, lower than the central element
        """
        return self.plotTree(filename, output, plotMaxUpper, plotMaxLower, 'compilation_tree', True)

    @debugDecor
    def plotExecTreeFromScope(self, scopePath, output, plotMaxUpper, plotMaxLower):
        """
        Compute the execution dependency graph
        :param scopePath: central scope path
        :param output: output file name (.dot or .png extension)
        :param plotMaxUpper: Maximum number of elements to plot, upper than the central element
        :param plotMaxLower: Maximum number of elements to plot, lower than the central element
        """
        return self.plotTree(scopePath, output, plotMaxUpper, plotMaxLower, 'execution_tree')

    @debugDecor
    def plotCompilTreeFromScope(self, scopePath, output, plotMaxUpper, plotMaxLower):
        """
        Compute the compilation dependency graph
        :param scopePath: central scope path
        :param output: output file name (.dot or .png extension)
        :param plotMaxUpper: Maximum number of elements to plot, upper than the central element
        :param plotMaxLower: Maximum number of elements to plot, lower than the central element
        """
        return self.plotTree(self.scopeToFiles(scopePath), output, plotMaxUpper, plotMaxLower,
                             'compilation_tree')

    @debugDecor
    def plotExecTreeFromFile(self, filename, output, plotMaxUpper, plotMaxLower):
        """
        Compute the execution dependency graph
        :param filename: central filename
        :param output: output file name (.dot or .png extension)
        :param plotMaxUpper: Maximum number of elements to plot, upper than the central element
        :param plotMaxLower: Maximum number of elements to plot, lower than the central element
        """
        return self.plotTree(self.fileToScopes(filename), output, plotMaxUpper, plotMaxLower,
                             'execution_tree', True)

    @debugDecor
    def findScopeInterface(self, scopePath):
        """
        Return the file name containing an interface for the scope path
        :param scopePath: scope path for which an interface is searched
        :return: (file name, interface scope) or (None, None) if not found
        """
        for filename, scopes in self._scopes.items():
            for scopeInterface in scopes:
                if re.search(r'interface:[a-zA-Z0-9_-]*/' + scopePath, scopeInterface):
                    return filename, scopeInterface
        return None, None
