#!/usr/bin/env python3

"""
This module implements the scope stuff
"""

import copy
import os

from pyfortool.variables import Variables, updateVarList
from pyfortool.cosmetics import Cosmetics
from pyfortool.applications import Applications
from pyfortool.statements import Statements
from pyfortool.cpp import Cpp
from pyfortool.openacc import Openacc
from pyfortool.util import PYFTError, debugDecor, n2name, tag
from pyfortool.tree import Tree, updateTree
from pyfortool.expressions import createElem, createExpr


class ElementView():
    """
    This class acts as a view of an ElementTree exposing only a part of the subelements
    """

    def __init__(self, xml, excludeContains=False):
        """
        :param xml: xml corresponding to a scope
        :param excludeContains: do not take into account the CONTAINS part
        """
        super().__init__()
        self._excludeContains = excludeContains
        self._xml = xml

    @property
    def _virtual(self):
        """
        :param xml: xml corresponding to a scope
        :return: a node (possibly the xml node) containing only the relevant subelements
        """
        if self._excludeContains:
            contains = self._xml.find('./{*}contains-stmt')
            if contains is None:
                return self._xml
            indexContains = list(self._xml).index(contains)
            childNode = createElem('virtual')
            childNode.extend(self._xml[:indexContains] + [self._xml[-1]])
            return childNode
        return self._xml

    # PROPERTIES

    @property
    def tag(self):
        """
        https://docs.python.org/3/library/xml.etree.elementtree.html#xml.etree.ElementTree.Element.tag
        """
        return self._xml.tag

    @property
    def tail(self):
        """
        https://docs.python.org/3/library/xml.etree.elementtree.html#xml.etree.ElementTree.Element.tail
        """
        return self._xml.tail

    @property
    def text(self):
        """
        https://docs.python.org/3/library/xml.etree.elementtree.html#xml.etree.ElementTree.Element.text
        """
        return self._xml.text

    # READ-ONLY METHODS, they can always use the virtual approach

    def findtext(self, *args, **kwargs):
        """
        https://docs.python.org/3/library/xml.etree.elementtree.html#xml.etree.ElementTree.Element.findtext
        """
        return self._virtual.findtext(*args, **kwargs)

    def iterfind(self, *args, **kwargs):
        """
        https://docs.python.org/3/library/xml.etree.elementtree.html#xml.etree.ElementTree.Element.iterfind
        """
        return self._virtual.iterfind(*args, **kwargs)

    def itertext(self, *args, **kwargs):
        """
        https://docs.python.org/3/library/xml.etree.elementtree.html#xml.etree.ElementTree.Element.itertext
        """
        return self._virtual.itertext(*args, **kwargs)

    def __getitem__(self, *args, **kwargs):
        return self._virtual.__getitem__(*args, **kwargs)

    def __len__(self, *args, **kwargs):
        return self._virtual.__len__(*args, **kwargs)

    def __iter__(self):
        return list(self._virtual).__iter__()

    def find(self, *args, **kwargs):
        """
        https://docs.python.org/3/library/xml.etree.elementtree.html#xml.etree.ElementTree.Element.find
        """
        return self._virtual.find(*args, **kwargs)

    def findall(self, *args, **kwargs):
        """
        https://docs.python.org/3/library/xml.etree.elementtree.html#xml.etree.ElementTree.Element.findall
        """
        return self._virtual.findall(*args, **kwargs)

    def iter(self, *args, **kwargs):
        """
        https://docs.python.org/3/library/xml.etree.elementtree.html#xml.etree.ElementTree.Element.iter
        """
        return self._virtual.iter(*args, **kwargs)

    def items(self, *args, **kwargs):
        """
        https://docs.python.org/3/library/xml.etree.elementtree.html#xml.etree.ElementTree.Element.items
        """
        return self._virtual.items(*args, **kwargs)

    # WRITE METHODS

    @updateVarList
    def clear(self):
        """
        https://docs.python.org/3/library/xml.etree.elementtree.html#xml.etree.ElementTree.Element.clear
        """
        for item in self:
            self.remove(item)
        self.text = None
        self.tail = None

    def append(self, *args, **kwargs):
        """
        https://docs.python.org/3/library/xml.etree.elementtree.html#xml.etree.ElementTree.Element.append
        """
        # Append after the 'END SUBROUTINE' statement
        return self._xml.append(*args, **kwargs)

    def extend(self, *args, **kwargs):
        """
        https://docs.python.org/3/library/xml.etree.elementtree.html#xml.etree.ElementTree.Element.extend
        """
        # Extend after the 'END SUBROUTINE' statement
        return self._xml.extend(*args, **kwargs)

    def _getIndex(self, index):
        """
        :param index: index in the virtual node
        :return: index in the _xml node
        """
        if not self._excludeContains:
            return index
        contains = self._xml.find('./{*}contains-stmt')
        if contains is None:
            return index
        indexContains = list(self._xml).index(contains)
        # Checks
        if index > indexContains or index < -indexContains - 1:
            raise IndexError('list index out of range')
        # Converts negative index into positive index
        if index == -1:
            # END SUBROUTINE
            return index
        if index < -1:
            index = indexContains + index + 1

        return len(self._xml) if index == indexContains else index

    def __setitem__(self, index, item):
        return self._xml.__setitem__(self._getIndex(index), item)

    @updateVarList
    def __delitem__(self, index):
        return self._xml.__delitem__(self._getIndex(index))

    def insert(self, index, item):
        """
        https://docs.python.org/3/library/xml.etree.elementtree.html#xml.etree.ElementTree.Element.insert
        """
        return self._xml.insert(0 if index == 0 else (self._getIndex(index - 1) + 1), item)

    @updateVarList
    def remove(self, node):
        """
        Remove node from the xml
        """
        if isinstance(node, ElementView):
            node = node._xml  # pylint: disable=protected-access
        self.getParent(node).remove(node)


class PYFTscope(ElementView, Variables, Cosmetics, Applications, Statements, Cpp, Openacc):
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
                 enableCache=False, tree=None, excludeContains=False):
        """
        :param xml: xml corresponding to this PYFTscope
        :param scopePath: scope path ('/' separated string) of this node
        :param parentScope: parent PYFTscope instance
        :param enableCache: True to cache node parents
        :param tree: an optional Tree instance
        :param excludeContains: do not take into account the CONTAINS part
        """
        super().__init__(xml=xml, excludeContains=excludeContains)
        self._mainScope = self if parentScope is None else parentScope._mainScope
        self._path = scopePath
        self._parentScope = parentScope
        self.tree = Tree() if tree is None else tree
        self._cacheParent = {}

        if enableCache and parentScope is None:
            # parent cache associated to the main scope
            for node in self.iter():
                for subNode in node:
                    self._cacheParent[id(subNode)] = node

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

    def __getattr__(self, attr):
        """
        Get some attributes defined in the PYFT class
        """
        if attr in ('SHARED_TREE', 'NO_PARALLEL_LOCK', 'PARALLEL_FILE_LOCKS '):
            return getattr(self._parentScope, attr)
        raise AttributeError(f"{attr} doesn't exist")

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
        # pylint: disable=protected-access
        def check(node):
            # We check if the registered parent is still the right one
            # node must be in its parent, and the parent chain must go to the root node
            return node in self.mainScope._cacheParent.get(id(node), []) and \
                   (self.mainScope._cacheParent[id(node)] == self.mainScope._xml or
                    check(self.mainScope._cacheParent[id(node)]))

        assert level >= 1
        parent = None
        if check(item):
            parent = self.mainScope._cacheParent[id(item)]
        else:
            for node in self.mainScope.iter():
                if item in list(node):
                    parent = node
                    if self.mainScope._cacheParent:
                        # _cacheParent not empty means that we want to use the caching system
                        self.mainScope._cacheParent[id(item)] = node
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
    def showScopesList(self, includeItself=False):
        """
        Shows the list of scopes found in the source code
        :param includeItself: include itself if self represent a "valid" scope (not a file)
        """
        print("These scopes have been found in the source code:")
        print("\n".join(['  - ' + scope.path
                         for scope in self.getScopes(includeItself=includeItself)]))

    @debugDecor
    def getScopes(self, level=-1, excludeContains=True, excludeKinds=None, includeItself=True):
        """
        :param level: -1 to get all child scopes
                      1 to get only direct child scopes
                      2 to get direct and direct of direct ...
        :param excludeContains: if True, each PYFTscope which is a module, function or subroutine
                                that contain (after a 'CONTAINS' statement) other subroutines or
                                functions, those contained subroutines or functions are excluded
                                from the result; but the PYFTscope contains the 'END' statement
                                of the module/subroutine or function.
        :param excludeKinds: if not None, is a list of scope kinds to exclude
        :param includeItself: include itself if self represent a "valid" scope (not a file)
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
                    scopePath = nodePath if basePath in ('', '/') \
                                else basePath + '/' + nodePath
                    results.append(PYFTscope(child,
                                             scopePath=scopePath, parentScope=self,
                                             enableCache=False, tree=self.tree,
                                             excludeContains=excludeContains))
                    if level != 1:
                        results.extend(_getRecur(child, level - 1, scopePath))
            return results

        if includeItself and tag(self) in self.SCOPE_CONSTRUCT.values():
            itself = [self]
        else:
            itself = []

        return _getRecur(self, level, self.path) + itself

    @debugDecor
    def getScopeNode(self, scopePath, excludeContains=True, includeItself=True):
        """
        :param scopePath: scope path to search for
        :param excludeContains: see getScopes
        :param includeItself: include itself if self represent a "valid" scope (not a file)
        :return: PYFTscope whose path is the path asked for
        """
        scope = [scope for scope in self.getScopes(excludeContains=excludeContains,
                                                   includeItself=includeItself)
                 if scope.path == scopePath]
        if len(scope) == 0:
            raise PYFTError(f'{scopePath} not found')
        if len(scope) > 1:
            raise PYFTError(f'{scopePath} found several times')
        return scope[0]

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
        return os.path.normpath(self.mainScope.find('.//{*}file').attrib['name'])

    @updateTree()
    @updateVarList
    def empty(self, addStmt=None, simplify=False):
        """
        Empties the scope by removing all statements except dummy arguments declaration
        and USE statements (because they can be useful for the dummy argument declarations).
        :param addStmt: add this statement in the body of the emptied scopes
        :param simplify: try to simplify code
        """
        scopes = []  # list of scopes to empty
        for scope in self.getScopes(level=1, excludeContains=False):
            if scope.path.split('/')[-1].split(':')[0] == 'module':
                scopes.extend(scope.getScopes(level=1, excludeContains=False, includeItself=False))
            else:
                scopes.append(scope)
        tagExcluded = (list(self.SCOPE_STMT.values()) +
                       ['end-' + decl for decl in self.SCOPE_STMT.values()] +
                       ['T-decl-stmt', 'use-stmt', 'C'])
        for scope in scopes:
            for node in list(scope):
                if tag(node) not in tagExcluded:
                    scope.remove(node)
            scope.removeUnusedLocalVar(simplify=simplify)
            if addStmt is not None:
                if isinstance(addStmt, str):
                    addStmt = createExpr(addStmt)
                elif not isinstance(addStmt, list):
                    addStmt = [addStmt]
                for stmt in addStmt:
                    scope.insertStatement(stmt, False)
        if simplify:
            # Apllied on self (and not scope) to remove lines before the first scope
            # in case self represents an entire file
            self.removeComments()
            self.removeEmptyLines()
