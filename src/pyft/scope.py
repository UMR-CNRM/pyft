#!/usr/bin/env python3

"""
This module implements the scope stuff
"""

import copy

from pyft.variables import Variables, updateVarList
from pyft.cosmetics import Cosmetics
from pyft.applications import Applications
from pyft.statements import Statements
from pyft.cpp import Cpp
from pyft.openacc import Openacc
from pyft.util import PYFTError, debugDecor, n2name, tag
from pyft.tree import Tree
from pyft.expressions import createElem


class PYFTscope(Variables, Cosmetics, Applications, Statements, Cpp, Openacc):
    """
    This class wrapps the xml node representing a FORTRAN scope
    """
    SCOPE_STMT = {'module': 'module-stmt',
                  'func': 'function-stmt',
                  'sub': 'subroutine-stmt',
                  'type': 'T-stmt',
                  'prog': 'program-stmt',
                  'interface': 'interface-stmt'}
    SCOPE_CONSTRUCT = {'module': 'program-unit',
                       'func': 'program-unit',
                       'sub': 'program-unit',
                       'type': 'T-construct',
                       'prog': 'program-unit',
                       'interface': 'interface-construct'}

    def __init__(self, xml, scopePath='/', parentScope=None,
                 enableCache=False, tree=None):
        """
        :param xml: xml corresponding to this PYFTscope
        :param ns: name space
        :param scopePath: scope path ('/' separated string) of this node
        :param parentScope: parent PYFTscope instance
        :param enableCache: True to cache node parents
        :param tree: an optional Tree instance
        """
        super().__init__()
        self._xml = xml
        self._mainScope = self if parentScope is None else parentScope._mainScope
        self._path = scopePath
        self._parentScope = parentScope
        self.tree = Tree() if tree is None else tree
        self.__cacheParent = {}
        if enableCache:
            for node in self.iter():
                for subNode in node:
                    self.__cacheParent[id(subNode)] = node

    def __copy__(self):
        cls = self.__class__
        result = cls.__new__(cls)
        result.__dict__.update(self.__dict__)
        return result

    def __deepcopy__(self, memo):
        cls = self.__class__
        result = cls.__new__(cls)
        memo[id(self)] = result
        for key, val in self.__dict__.items():
            setattr(result, key, copy.deepcopy(val, memo))
        return result

    def __getattr__(self, method):
        """
        Apply find, findall, iter... on the xml
        """
        return getattr(self._xml, method)

    def __getitem__(self, *args, **kwargs):
        return self._xml.__getitem__(*args, **kwargs)

    def __setitem__(self, *args, **kwargs):
        return self._xml.__setitem__(*args, **kwargs)

    def __delitem__(self, *args, **kwargs):
        return self._xml.__delitem__(*args, **kwargs)

    def __len__(self, *args, **kwargs):
        return self._xml.__len__(*args, **kwargs)

    @updateVarList
    def remove(self, node):
        """
        Remove node from the xml
        """
        if isinstance(node, PYFTscope):
            node = node._xml
        self.getParent(node).remove(node)

    @property
    def node(self):
        """
        Return the xml node
        """
        return self._xml

    @property
    def path(self):
        """
        Return the current path
        """
        return self._path

    @property
    def mainScope(self):
        """
        Return the main scope (the upper scope in the file)
        """
        return self._mainScope

    @property
    def parentScope(self):
        """
        Return the parent of the current scope
        """
        return self._parentScope

    # No @debugDecor for this low-level method
    def getParent(self, item, level=1):
        """
        :param item: item whose parent is to be searched
        :param level: number of degrees (1 to get the parent, 2 to get
                      the parent of the parent...)
        """
        def check(node):
            # We check if the registered parent is still the right one
            # node must be in its parent, and the parent chain must go to the root node
            return node in self.__cacheParent.get(id(node), []) and \
                   (self.__cacheParent[id(node)] == self.mainScope.node or
                    check(self.__cacheParent[id(node)]))

        assert level >= 1
        parent = None
        if check(item):
            parent = self.__cacheParent[id(item)]
        else:
            for node in self.mainScope.node.iter():
                if item in list(node):
                    parent = node
                    if self.__cacheParent:
                        # __cacheParent not empty means that we want to use the caching system
                        self.__cacheParent[id(item)] = node
                    break
        return parent if level == 1 else self.getParent(parent, level - 1)

    def getSiblings(self, item, before=True, after=True):
        """
        :param item: item whose siblings are to be searched
        :param before: returns siblings before
        :param after: returns siblings after
        By default before and after are True so that all siblings are returned
        """

        siblings = self.getParent(item).findall('./{*}*')
        if not after:
            siblings = siblings[:siblings.index(item)]
        if not before:
            siblings = siblings[siblings.index(item) + 1:]
        return [s for s in siblings if s != item]

    # No @debugDecor for this low-level method
    @staticmethod
    def _getNodeName(node):
        """
        Internal methode to compute the name of a scope
        :param node: program-unit node
        :return: name
        """
        # If there was no interface bloc, code could be n2name(node[0].find('.//{*}N'))
        # But (as of 7 Jul 2023), interface have two nested N
        nodeN = node.find('.//{*}N')
        if nodeN is not None and nodeN.find('.//{*}N') is not None:
            # As of 7 Jul 2023, this is the case for interface statements
            nodeN = nodeN.find('.//{*}N')
        if nodeN is not None:
            name = n2name(nodeN).upper()
        else:
            name = '--UNKNOWN--'
        return name

    # No @debugDecor for this low-level method
    def _getNodePath(self, node):
        """
        Internal methode to compute a path part from a node
        :param node: program-unit node
        :return: path part (e.g. module:MODU)
        """
        stmt = tag(node[0])
        name = self._getNodeName(node[0])
        return {v: k for (k, v) in self.SCOPE_STMT.items()}[stmt] + ':' + name

    # No @debugDecor for this low-level method
    @staticmethod
    def normalizeScope(scopePath):
        """
        Method to normalize a scope path
        """
        return '/'.join([(k.lower() + ':' + w.upper())
                         for (k, w) in [component.split(':')
                         for component in scopePath.split('/')]])

    @debugDecor
    def showScopesList(self):
        """
        Shows the list of scopes found in the source code
        """
        print("These scopes have been found in the source code:")
        print("\n".join(['  - ' + scope.path for scope in self.getScopes()]))

    @debugDecor
    def getScopes(self, level=-1, excludeContains=False, excludeKinds=None):
        """
        :param level: -1 to get all child scopes
                      1 to get only direct child scopes
                      2 to get direct of direct ...
        :param excludeContains: if True, each PYFTscope which is a module, function or subroutine
                                that contain (after a 'CONTAINS' statement) other subroutines or
                                functions, those contained subroutines or functions are excluded
                                from the result; but the PYFTscope contains the 'END' statement
                                of the module/subroutine or function.
        :param excludeKinds: if not None, is a list of scope kinds to exclude
        :return: list of PYFTscope found in the current scope
        """
        assert level == -1 or level > 0, 'level must be -1 or a positive int'

        def _getRecur(node, level, basePath=''):
            # If node is the entire xml
            if tag(node) == 'object':
                usenode = node.find('./{*}file')
            else:
                usenode = node
            results = []
            for child in [child for child in usenode
                          if tag(child) in self.SCOPE_CONSTRUCT.values()]:
                nodePath = self._getNodePath(child)
                if excludeKinds is None or nodePath.split(':')[0] not in excludeKinds:
                    scopePath = self._getNodePath(child) if basePath in ('', '/') \
                                else basePath + '/' + nodePath
                    if excludeContains:
                        childNode = createElem('virtual')
                        breakOnCOntains = False
                        for node in child:  # pylint: disable=redefined-argument-from-local
                            if tag(node) == 'contains-stmt':
                                breakOnCOntains = True
                                break  # we are outside of the targeted bloc
                            childNode.append(node)
                        if breakOnCOntains:
                            childNode.append(child[-1])
                        else:
                            childNode = child
                    else:
                        childNode = child
                    results.append(PYFTscope(childNode,
                                             scopePath=scopePath, parentScope=self,
                                             enableCache=False, tree=self.tree))
                    if level != 1:
                        results.extend(_getRecur(child, level - 1, scopePath))
            return results

        return _getRecur(self.node, level, self.path)

    @debugDecor
    def getScopeNode(self, scopePath, excludeContains=False):
        """
        :param scopePath: scope path to search for
        :param excludeContains: see getScopes
        :return: PYFTscope whose path is the path asked for
        """
        scope = [scope for scope in self.getScopes(excludeContains=excludeContains)
                 if scope.path == scopePath]
        if len(scope) != 1:
            raise PYFTError(f'{scopePath} not found (or found several times')
        return scope[0]

    @debugDecor
    def getScopeNodes(self, scopePath, excludeContains=False, excludeKinds=None):
        """
        :param scopePath: scope path, or list of scope paths, to search for
                          None to return all scopes
        :param excludeContains: see getScopes
        :param excludeKinds: see getScopes
        :return: PYFTscope whose path is the path asked for
        """
        scopes = self.getScopes(excludeContains=excludeContains,
                                excludeKinds=excludeKinds)
        if scopePath is not None:
            if not isinstance(scopePath, list):
                scopePath = [scopePath]
            scopes = [scope for scope in scopes if scope.path in scopePath]
        return scopes

    @debugDecor
    def isScopeNode(self, node):
        """
        :param node: node to test
        :return: True if node is a scope node (construct node around a
                 module, subroutine, function or type declaration)
        """
        return tag(node) in self.SCOPE_CONSTRUCT.values()

    @debugDecor
    def getParentScopeNode(self, item, mustRaise=True):
        """
        :param item: item whose scope parent is to be searched
        :param mustRaise: True to raise an exception if parent is not found
        :return: the scope parent node of item
        Example: if item is a call statement, result is the program-unit node
                 in which the call statement is
        """
        result = self.getParent(item)
        while result is not None and not self.isScopeNode(result):
            result = self.getParent(result)
        if result is None and mustRaise:
            raise PYFTError("The scope parent has not been found.")
        return result

    @debugDecor
    def getScopePath(self, item, includeItself=True):
        """
        :param item: item whose path must be determined
        :param includeItself: include the item if it is a scope node
        :return: the full path of the structure containing item
        """
        if includeItself and self.isScopeNode(item):
            result = [self._getNodePath(item)]
        else:
            result = []
        item = self.getParentScopeNode(item, mustRaise=False)
        while item is not None:
            result = [self._getNodePath(item)] + result
            item = self.getParentScopeNode(item, mustRaise=False)
        return '/'.join(result)

    def getFileName(self):
        """
        :return: the name of the input file name or 'unknown' if not available
                 in the xml fragment provided
        """
        return self.mainScope.node.find('.//{*}file').attrib['name']

    @updateVarList
    def empty(self, addStmt=None, simplify=False):
        """
        Empties the scope by removing all statements except dummy arguments declaration
        and USE statements (because they can be useful for the dummy argument declarations).
        :param addStmt: add this statement in the body of the emptied scopes
        :param simplify: try to simplify code
        """
        scopes = []  # list of scopes to empty
        for scope in self.getScopes(level=1):
            if scope.path.split('/')[-1].split(':')[0] == 'module':
                scopes.extend(scope.getScopes(level=1))
            else:
                scopes.append(scope)
        tagExcluded = (list(self.SCOPE_STMT.values()) +
                       ['end-' + decl for decl in self.SCOPE_STMT.values()] +
                       ['T-decl-stmt', 'use-stmt', 'C'])
        for scope in scopes:
            for node in list(scope):
                if tag(node) not in tagExcluded:
                    self.remove(node)
        self.removeUnusedLocalVar(simplify=simplify)
        if simplify:
            self.removeComments()
            self.removeEmptyLines()
        if addStmt is not None:
            if isinstance(addStmt, str):
                addStmt = self.createExpr(addStmt)
            elif not isinstance(addStmt, list):
                addStmt = [addStmt]
            for stmt in addStmt:
                scope.insertStatement(scope.path, stmt, False)
