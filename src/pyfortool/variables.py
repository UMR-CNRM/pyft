"""
This module implements the VarList and Variables classes to deal with variables
"""

import logging
import copy
import re
from functools import wraps
import os

from pyfortool.util import PYFTError, debugDecor, alltext, isExecutable, n2name, tag, noParallel
from pyfortool.expressions import (createArrayBounds, simplifyExpr, createExprPart,
                                   createExpr, createElem)
from pyfortool.tree import updateTree
import pyfortool.pyfortool


# No @debugDecor for this low-level method
def _getDeclStmtTag(scopePath):
    """
    Internal function
    :param scopePath: a scope path
    :return: the declaration statement we can find in this scope path
    """
    if scopePath.split('/')[-1].split(':')[0] == 'type':
        declStmt = 'component-decl-stmt'
    else:
        declStmt = 'T-decl-stmt'
    return declStmt


def updateVarList(func):
    """
    Decorator to signal that a varList update is needed
    """
    @wraps(func)
    def wrapper(self, *args, **kwargs):
        result = func(self, *args, **kwargs)
        self.mainScope._varList = None  # pylint: disable=protected-access
        return result
    return wrapper


class VarList():
    """
    The VarList class stores the characterisitcs of all variables contained in a source code
    """
    def __init__(self, mainScope, _preCompute=None):
        """
        :param mainScope: a PYFT object
        :param _preCompute: pre-computed values (_varList, _fullVarList, _scopePath)

        Notes: - variables are found in modules only if the 'ONLY' attribute is used
               - array specification and type is unknown for module variables
               - 'ASSOCIATE' statements are not followed
        """
        # _varList is the effective list of variables for the current scope
        # _fullVarList is the list of variables of the whole file
        #     We need it because findVar must be able to return a variable declaration
        #     that exists in an upper scope
        # _scopePath is the path of the current scope

        if _preCompute is None:
            self._varList = self._fromScope(mainScope)
            self._scopePath = mainScope.path
            self._fullVarList = self._varList
        else:
            assert mainScope is None
            self._varList, self._fullVarList, self._scopePath = _preCompute

    def __getitem__(self, *args, **kwargs):
        return self._varList.__getitem__(*args, **kwargs)

    def __setitem__(self, *args, **kwargs):
        return self._varList.__setitem__(*args, **kwargs)

    def __delitem__(self, *args, **kwargs):
        return self._varList.__delitem__(*args, **kwargs)

    def __len__(self, *args, **kwargs):
        return self._varList.__len__(*args, **kwargs)

    @staticmethod
    def _fromScope(mainScope):
        """
        :param mainScope: a PYFT object
        """
        def decodeArraySpecs(arraySpecs):
            asList = []
            asxList = []
            for arraySpec in arraySpecs:
                lb = arraySpec.find('.//{*}lower-bound')
                ub = arraySpec.find('.//{*}upper-bound')
                asList.append([alltext(lb) if lb is not None else None,
                               alltext(ub) if ub is not None else None])
                asxList.append([lb if lb is not None else None, ub if ub is not None else None])
            return asList, asxList

        result = []
        for scope in mainScope.getScopes():
            # In case scope is a function, we determine the name of the result
            if tag(scope[0]) == 'function-stmt':
                rSpec = scope[0].find('./{*}result-spec/{*}N')
                funcResultName = rSpec if rSpec is not None else \
                    scope[0].find('./{*}function-N/{*}N')
                funcResultName = n2name(funcResultName).upper()
            else:
                funcResultName = ''

            # Find dummy arguments
            dummyArgs = [n2name(e).upper() for stmt in scope
                         for e in stmt.findall('.//{*}dummy-arg-LT/{*}arg-N/{*}N')]

            # Loop on each declaration statement
            for declStmt in [stmt for stmt in scope
                             if tag(stmt) in ('T-decl-stmt' 'component-decl-stmt')]:
                tSpec = alltext(declStmt.find('.//{*}_T-spec_'))
                iSpec = declStmt.find('.//{*}intent-spec')
                if iSpec is not None:
                    iSpec = iSpec.text
                optSpec = False
                allocatable = False
                parameter = False
                pointer = False
                allattributes = declStmt.findall('.//{*}attribute/{*}attribute-N')
                for attribute in allattributes:
                    if alltext(attribute).upper() == 'OPTIONAL':
                        optSpec = True
                    if alltext(attribute).upper() == 'ALLOCATABLE':
                        allocatable = True
                    if alltext(attribute).upper() == 'PARAMETER':
                        parameter = True
                    if alltext(attribute).upper() == 'POINTER':
                        pointer = True
                # Dimensions declared with the DIMENSION attribute
                arraySpecs = declStmt.findall('.//{*}attribute//{*}array-spec//{*}shape-spec')
                as0List, asx0List = decodeArraySpecs(arraySpecs)

                # Loop on each declared variables
                for enDecl in declStmt.findall('.//{*}EN-decl'):
                    varName = n2name(enDecl.find('.//{*}N')).upper()
                    # Dimensions declared after the variable name
                    arraySpecs = enDecl.findall('.//{*}array-spec//{*}shape-spec')
                    asList, asxList = decodeArraySpecs(arraySpecs)
                    # Initial value (parameter or not)
                    init = enDecl.find('./{*}init-E')
                    if init is not None:
                        init = alltext(init)

                    result.append({'as': asList if len(as0List) == 0 else as0List,
                                   'asx': asxList if len(asx0List) == 0 else asx0List,
                                   'n': varName, 'i': iSpec, 't': tSpec,
                                   'arg': varName in dummyArgs,
                                   'use': False, 'opt': optSpec, 'allocatable': allocatable,
                                   'parameter': parameter, 'pointer': pointer,
                                   'result': funcResultName == varName,
                                   'init': init, 'scopePath': scope.path})

            # Loop on each use statement
            for useStmt in [stmt for stmt in scope if tag(stmt) == 'use-stmt']:
                module = n2name(useStmt.find('.//{*}module-N').find('.//{*}N'))
                for var in useStmt.findall('.//{*}use-N'):
                    varName = n2name(var.find('.//{*}N'))
                    result.append({'as': None, 'asx': None,
                                   'n': varName, 'i': None, 't': None, 'arg': False,
                                   'use': module, 'opt': None, 'allocatable': None,
                                   'parameter': None, 'pointer': None, 'result': None,
                                   'init': None, 'scopePath': scope.path})
        return result

    def restrict(self, scopePath, excludeContains):
        """
        :param scopePath: return a varList restricted to this scope path
        :param excludeContains: exclude variables declared in contained parts
        """
        scopePath = '' if scopePath == '/' else scopePath
        root = scopePath + '/' if scopePath == '/' else scopePath
        varList = [item for item in self._varList
                   if item['scopePath'] == scopePath or
                   (item['scopePath'].startswith(root) and
                    not excludeContains)]
        return VarList(None, _preCompute=(varList, self._fullVarList, scopePath))

    @debugDecor
    def findVar(self, varName, array=None, exactScope=False, extraVarList=None):
        """
        Search for a variable in a list of declared variables
        :param varName: variable name
        :param array: True to limit search to arrays,
                      False to limit search to non array,
                      None to return anything
        :param exactScope: True to limit search to variables declared in the scopePath
        :param extraVarList: None or list of variables (such as those contained in a VarList object)
                             defined but not yet available in the self.varList object.
        :return: None if not found or the description of the variable

        The function is designed to return the declaration of a given variable.
        If we know that the variable is (is not) an array, the last declaration statement
        must (must not) be an array declaration. If the last declaration statement found doesn't
        correspond to what is expected, we don't return it.
        In case array is None, we return the last declaration statement without checking its kind.
        """
        extraVarList = extraVarList if extraVarList is not None else []
        # Select all the variables declared in the current scope or upper,
        # then select the last declared
        candidates = {v['scopePath']: v for v in self._fullVarList + extraVarList
                      if v['n'].upper() == varName.upper() and
                      (self._scopePath == v['scopePath'] or
                       (self._scopePath.startswith(v['scopePath'] + '/') and
                        not exactScope))}
        if len(candidates) > 0:
            last = candidates[max(candidates, key=len)]
            if array is True and last.get('as', None) is not None and len(last['as']) > 0:
                return last
            if array is False and len(last.get('as', [])) == 0:
                return last
            if array is None:
                return last
        return None

    @debugDecor
    def showVarList(self):
        """
        Display on stdout a nice view of all the variables
        """
        for sc in set(v['scopePath'] for v in self._varList):
            print(f'List of variables declared in {sc}:')
            for var in [var for var in self._varList if var['scopePath'] == sc]:
                print(f"  Variable {var['n']}:")
                if var['use']:
                    print(f"    is a variable taken in the {var['use']} module")
                else:
                    isscalar = len(var['as']) == 0
                    if isscalar:
                        print('    is scalar')
                    else:
                        print('    is of rank {}, with dimensions {}'.format(len(var['as']),
                              ', '.join([(':'.join([('' if s is None else s)
                                                    for s in var['as'][i]]))
                                         for i in range(len(var['as']))])))
                    if var['arg']:
                        intent = 'without intent' if var['i'] is None else \
                                 f"with intent {var['i']}"
                        print(f'    is a dummy argument {intent}')
                    else:
                        print('    is a local variable')
                print()


class Variables():
    """
    Methos to deal with variables
    """
    def __init__(self, **kwargs):  # pylint: disable=unused-argument
        """
        **kwargs is used to enable the use of super().__init__
        """
        self._varList = None

    @property
    def varList(self):
        """
        Returns the varList object corresponding to the node
        """
        # Evaluate the varList object if not already done
        if self.mainScope._varList is None:  # pylint: disable=protected-access
            self.mainScope._varList = VarList(self.mainScope)  # pylint: disable=protected-access

        # Restrict the object to the current node
        # pylint: disable-next=protected-access
        return self.mainScope._varList.restrict(self.path, self._excludeContains)

    # No @debugDecor for this low-level method
    def _normalizeScopeVar(self, scopeVarList):
        """
        Internal method to normalize scopeVarList
        (list of tuples made of scope path, variable name, and optional other values)
        """
        return [(self.normalizeScope(scopePath), var.upper(), *other)
                for (scopePath, var, *other) in scopeVarList]

    # No @debugDecor for this low-level method
    def _normalizeUniqVar(self, scopeVarList):
        """
        Internal method to suppress duplicates in scopeVarList
        (list of tuples made of scope path, variable name, and optional other values)
        """
        # We could use list(set(self._normalizeScopeVar(scopeVarList)))
        # but order differs from one execution to the other
        result = []
        for scopeVar in self._normalizeScopeVar(scopeVarList):
            if scopeVar not in result:
                result.append(scopeVar)
        return result

    @debugDecor
    def attachArraySpecToEntity(self):
        """
        Find all T-decl-stmt elements that have a child element 'attribute' with
        attribute-N="DIMENSION" and move the attribute into EN-N elements
        E.g., before :
        REAL, DIMENSION(D%NIJT,D%NKT) :: ZTLK, ZRT
        INTEGER, PARAMETER, DIMENSION(1,1) :: IBUEXTRAIND=(/18, 30/)
        after :
        REAL :: ZTLK(D%NIJT,D%NKT), ZRT(D%NIJT,D%NKT)
        INTEGER, PARAMETER  :: IBUEXTRAIND(1,1)=(/18, 30/)
        Limitations : "DIMENSION" must be in upper case in attribute.text
        """
        # Find all T-decl-stmt elements that have a child element 'attribute'
        # with attribute-N="DIMENSION"
        for decl in self.findall('.//{*}T-decl-stmt'):
            arraySpec = decl.find('./{*}attribute[{*}attribute-N="DIMENSION"]/{*}array-spec')
            attrElem = decl.find('./{*}attribute[{*}attribute-N="DIMENSION"]/{*}array-spec/...')
            # Discard allocatable (':' in arraySpec)
            if arraySpec is not None and ':' not in alltext(arraySpec):
                # Check if EN-decl elements don't already have an array-spec child element
                # or an intial value
                if decl.find('./{*}EN-decl-LT/{*}EN-decl/{*}array-spec') is None and \
                   decl.find('./{*}EN-decl-LT/{*}EN-decl/{*}init-E') is None:
                    # Attach the array-spec element after the EN-N element
                    for elem in decl.findall('./{*}EN-decl-LT/{*}EN-decl'):
                        elem.append(copy.deepcopy(arraySpec))
                    # Remove the dimension and array-spec elements
                    self.removeFromList(attrElem, decl)

    @debugDecor
    def checkImplicitNone(self, mustRaise=False):
        """
        :param mustRaise: True to raise
        Issue a logging.warning if the "IMPLICIT NONE" statment is missing
        If mustRaise is True, issue a logging.error instead and raise an error
        """
        for scope in self.getScopes():
            # The IMPLICIT NONE statement is inherited from the top unit, control at top
            # unit is enough apart for INTERFACE blocs
            if (scope.path.count('/') == 0 or
                (scope.path.count('/') >= 2 and
                 scope.path.split('/')[-2].startswith('interface:'))):
                if scope.find('./{*}implicit-none-stmt') is None:
                    message = "The 'IMPLICIT NONE' statment is missing in file " + \
                              "'{file}' for {scopePath}.".format(file=scope.getFileName(),
                                                                 scopePath=scope.path)
                    if mustRaise:
                        logging.error(message)
                        raise PYFTError(message)
                    logging.warning(message)

    @debugDecor
    def checkIntent(self, mustRaise=False):
        """
        :param mustRaise: True to raise
        Issue a logging.warning if some "INTENT" attributes are missing
        If mustRaise is True, issue a logging.error instead and raise an error
        """
        ok = True
        log = logging.error if mustRaise else logging.warning
        for var in self.varList:
            if var['arg'] and var['i'] is None:
                log(("The dummy argument {} as no INTENT attribute, in " +
                    "file '{}'").format(var['n'], self.getFileName()))
                ok = False
        if not ok and mustRaise:
            raise PYFTError("There are dummy arguments without INTENT attribute in " +
                            "file '{}'".format(self.getFileName()))

    @debugDecor
    @noParallel
    @updateVarList
    @updateTree('signal')
    def removeVar(self, varList, simplify=False):
        """
        :param varList: list of variables to remove. Each item is a list or tuple of two elements.
                        The first one describes where the variable is used, the second one is
                        the name of the variable. The first element is a '/'-separated path with
                        each element having the form 'module:<name of the module>',
                        'sub:<name of the subroutine>', 'func:<name of the function>' or
                        'type:<name of the type>'
        :param simplify: try to simplify code (if we delete a declaration statement that used a
                         variable as kind selector, and if this variable is not used else where,
                         we also delete it)
        Remove the variable from declaration, and from the argument list if needed
        """
        varList = self._normalizeUniqVar(varList)

        # Sort scopes by depth
        sortedVarList = {}
        for scopePath, varName in varList:
            nb = scopePath.count('/')
            sortedVarList[nb] = sortedVarList.get(nb, []) + [(scopePath, varName.upper())]

        varToRemoveIfUnused = []
        # Loop on varList starting by inner most variables
        nbList = [] if len(sortedVarList.keys()) == 0 else \
            range(max(sortedVarList.keys()) + 1)[::-1]
        for nb in nbList:
            sortedVarList[nb] = sortedVarList.get(nb, [])
            # Loop on scopes
            for scopePath in list(set(scopePath for scopePath, _ in sortedVarList[nb])):
                # use of mainScope because variable can be declared upper than self
                scope = self.mainScope.getScopeNode(scopePath)
                # Variables searched in this scope
                varNames = list(set(v for (w, v) in sortedVarList[nb] if w == scopePath))
                declStmt = _getDeclStmtTag(scopePath)
                # If scopePath is "module:XX/sub:YY", getScopeNode returns a node
                # containing the subroutine declaration statements and
                # excluding the subroutine and functions potentially included
                # after a "contains" statement
                previous = None
                # list() to allow removing during the iteration
                for node in list(scope):
                    deleted = False

                    # Checks if variable is a dummy argument
                    dummyList = node.find('{*}dummy-arg-LT')  # This is the list of the dummies
                    if dummyList is not None:
                        # Loop over all dummy arguments
                        for arg in dummyList.findall('.//{*}arg-N'):
                            name = n2name(arg.find('.//{*}N')).upper()
                            for varName in [v for v in varNames if v == name]:
                                # This dummy arg is a searched variable, we remove it from the list
                                scope.removeFromList(arg, dummyList)

                    # In case the variable is declared
                    if tag(node) == declStmt:
                        # We are in a declaration statement
                        # list of declaration in the current statment
                        declList = node.find('./{*}EN-decl-LT')
                        for enDecl in declList.findall('.//{*}EN-decl'):
                            name = n2name(enDecl.find('.//{*}N')).upper()
                            for varName in [v for v in varNames if v == name]:
                                # The argument is declared here,
                                # we suppress it from the declaration list
                                varNames.remove(varName)
                                scope.removeFromList(enDecl, declList)
                        # In all the variables are suppressed from the declaration statement
                        if len(list(declList.findall('./{*}EN-decl'))) == 0:
                            if simplify:
                                varToRemoveIfUnused.extend([[scopePath, n2name(nodeN)]
                                                            for nodeN in node.findall('.//{*}N')])
                            # We will delete the current node but we don't want to lose
                            # any text. So, we put the node's text in the tail of the previous node
                            if previous is not None and node.tail is not None:
                                if previous.tail is None:
                                    previous.tail = ''
                                previous.tail += node.tail
                            deleted = True
                            scope.getParent(node).remove(node)

                    # In case the variable is a module variable
                    if tag(node) == 'use-stmt':
                        # We are in a use statement
                        useList = node.find('./{*}rename-LT')
                        if useList is not None:
                            for rename in useList.findall('.//{*}rename'):
                                name = n2name(rename.find('.//{*}N')).upper()
                                for varName in [v for v in varNames if v == name]:
                                    varNames.remove(varName)
                                    # The variable is declared here, we remove it from the list
                                    scope.removeFromList(rename, useList)
                                    # In case the variable was alone
                                    attribute = node.find('{*}module-N').tail
                                    if attribute is None:
                                        attribute = ''
                                    attribute = attribute.replace(' ', '').replace('\n', '')
                                    attribute = attribute.replace('&', '').upper()
                                    useList = node.find('./{*}rename-LT')
                                    if len(useList) == 0 and attribute[0] == ',' and \
                                       attribute[1:] == 'ONLY:':
                                        # If there is a 'ONLY' attribute,
                                        # we suppress the use statement entirely
                                        if previous is not None and node.tail is not None:
                                            if previous.tail is None:
                                                previous.tail = ''
                                            previous.tail += node.tail
                                        deleted = True
                                        scope.getParent(node).remove(node)
                                        scope.tree.signal(scope)  # Tree must be updated
                                    elif len(useList) == 0:
                                        # there is no 'ONLY' attribute
                                        moduleName = scope.getSiblings(useList, before=True,
                                                                       after=False)[-1]
                                        previousTail = moduleName.tail
                                        if previousTail is not None:
                                            moduleName.tail = previousTail.replace(',', '')
                                        scope.getParent(useList).remove(useList)
                    # end loop if all variables have been found
                    if len(varNames) == 0:
                        break
                    # Store node for the following iteration
                    if not deleted:
                        previous = node

                # If some variables have not been found, they are certainly declared one level upper
                if len(varNames) != 0:
                    newWhere = '/'.join(scopePath.split('/')[:-1])
                    sortedVarList[nb - 1] = sortedVarList.get(nb - 1, []) + \
                        [(newWhere, varName) for varName in varNames]

        if simplify and len(varToRemoveIfUnused) > 0:
            self.removeVarIfUnused(varToRemoveIfUnused, excludeDummy=True, simplify=True)

    @debugDecor
    @updateVarList
    def addVar(self, varList):
        """
        :param varList: list of variable specification to insert in the xml code
                        a variable specification is a list of four elements:
                        - variable scope path (path to module, subroutine, function or type
                          declaration)
                        - variable name
                        - declarative statment
                        - position of the variable in the list of dummy argument,
                          None for a local variable
        """
        varList = self._normalizeUniqVar(varList)

        for (scopePath, name, declStmt, pos) in varList:
            scope = self.getScopeNode(scopePath)

            # Add variable to the argument list
            if pos is not None:
                argN = createElem('arg-N')
                nodeN = createElem('N')
                nodeN.append(createElem('n', text=name))
                argN.append(nodeN)
                # search for a potential node, within the scope, with a list of dummy arguments
                argLst = scope.find('.//{*}dummy-arg-LT')
                if argLst is None:
                    # This was a subroutine or function without dummy arguments
                    scope[0][0].tail = '('
                    argLst = createElem('dummy-arg-LT', tail=')')
                    scope[0].insert(1, argLst)
                scope.insertInList(pos, argN, argLst)

            # Declare the variable
            # The following test is needed in case several variables are added in the argument list
            # but the declaration statement is given only once for all the variables
            if declStmt is not None and declStmt != '':
                # Declaration statement tag according to path (member of type declaration or not)
                declStmtTag = _getDeclStmtTag(scopePath)

                if scopePath.split('/')[-1].split(':')[0] == 'type':
                    # Add declaration statement in type declaration
                    # Statement building
                    ds = createExpr(declStmt)[0]
                    previousTail = '\n' + declStmt[:re.search(r'\S', declStmt).start()]

                    # node insertion
                    # scope[0] is the T-stmt node, scope[-1] is the end-T-stmt node
                    # scope[-2] is the last node before the end-T-stmt node (last component,
                    # comment or the T-stmt node)
                    ds.tail = scope[-2].tail
                    scope[-2].tail = previousTail
                    scope.insert(-1, ds)  # insert before last one

                else:
                    # Add declaration statement (not type declaration case)
                    # Statement building
                    ds = createExpr(declStmt)[0]
                    previousTail = '\n' + declStmt[:re.search(r'\S', declStmt).start()]

                    # node insertion index
                    declLst = [node for node in scope if tag(node) == declStmtTag]
                    if len(declLst) != 0:
                        # There already are declaration statements
                        # We look for the last position in the declaration list which do not use
                        # the variable we add
                        for decl in declLst:
                            index = list(scope).index(decl)
                            if name in [n2name(nodeN) for nodeN in decl.findall('.//{*}N')]:
                                break
                    else:
                        # There is no declaration statement
                        # list of executable nodes
                        stmtLst = [node for node in scope if isExecutable(node)]
                        if len(stmtLst) == 0:
                            # There is no executable statement, we insert the declaration at the end
                            # Last node is the ending node (e.g. end-subroutine-stmt)
                            index = len(scope) - 1
                        else:
                            # We insert the declaration just before the first executable statement
                            index = list(scope).index(stmtLst[0])

                    # node insertion
                    if index != 0:
                        ds.tail = scope[index - 1].tail
                        scope[index - 1].tail = previousTail
                    scope.insert(index, ds)

    @debugDecor
    @noParallel
    @updateVarList
    @updateTree('signal')
    def addModuleVar(self, moduleVarList):
        """
        :param moduleVarList: list of module variable specification to insert in the xml code
                              a module variable specification is a list of three elements:
                              - scope path (path to module, subroutine, function or type
                                declaration)
                              - module name
                              - variable name or or list of variable names
                                or None to add a USE statement without the ONLY attribute
        For example addModuleVar('sub:FOO', 'MODD_XX', 'Y') will add the following line in
        subroutine FOO:
        USE MODD_XX, ONLY: Y
        """
        moduleVarList = self._normalizeScopeVar(moduleVarList)

        for (scopePath, moduleName, varName) in moduleVarList:
            if varName is None:
                varName = []
            elif not isinstance(varName, list):
                varName = [varName]
            scope = self.getScopeNode(scopePath)

            # USE statement already present
            useLst = [node for node in scope if tag(node) == 'use-stmt']

            # Check if we need to add a USE
            insertUse = True
            for us in useLst:
                usName = n2name(us.find('.//{*}module-N//{*}N'))
                usVar = [n2name(v.find('.//{*}N')).upper() for v in us.findall('.//{*}use-N')]
                if len(varName) == 0 and len(usVar) == 0 and usName.upper() == moduleName.upper():
                    # There aleardy is a 'USE MODULE' and we wanted to insert a 'USE MODULE'
                    insertUse = False
                elif len(varName) > 0 and len(usVar) > 0 and usName.upper() == moduleName.upper():
                    # There already is a 'USE MODULE, ONLY:' and we want to insert another
                    # 'USE MODULE, ONLY:'
                    # We suppress from the varName list, all the variables already defined
                    varName = [var for var in varName if var.upper() not in usVar]
                    if len(varName) == 0:
                        # All the variables we wanted to import are already defined
                        insertUse = False

            if insertUse:
                # Statement building
                stmt = f'USE {moduleName}'
                if len(varName) > 0:
                    stmt += ', ONLY:{}'.format(', '.join(varName))
                us = createExpr(stmt)[0]

                # node insertion index
                if len(useLst) != 0:
                    # There already have use statements, we add the new one after them
                    index = list(scope).index(useLst[-1]) + 1
                else:
                    # There is no use statement, we add the new node just after the first node
                    index = 1

                us.tail = scope[index - 1].tail
                scope[index - 1].tail = '\n'
                scope.insert(index, us)
                scope.tree.signal(scope)  # Tree must be updated

    @debugDecor
    def showUnusedVar(self):
        """
        Displays on stdout a list of unued variables
        """
        scopes = self.getScopes(excludeKinds=['type'])
        varUsed = self.isVarUsed([(scope.path, v['n'])
                                  for scope in scopes
                                  for v in self.varList
                                  if v['scopePath'] == scope.path])
        for scope in scopes:
            varList = [k[1].upper() for (k, v) in varUsed.items() if (not v) and k[0] == scope.path]
            if len(varList) != 0:
                print(f'Some variables declared in {scope.path} are unused:')
                print('  - ' + ('\n  - '.join(varList)))

    @debugDecor
    def removeUnusedLocalVar(self, excludeList=None, simplify=False):
        """
        Remove unused local variables (dummy and module variables are not suppressed)
        :param excludeList: list of variable names to exclude from removal (even if unused)
        :param simplify: try to simplify code (if we delete a declaration statement that used a
                         variable as kind selector, and if this variable is not used else where,
                         we also delete it)
        """
        if excludeList is None:
            excludeList = []
        else:
            excludeList = [item.upper() for item in excludeList]

        allVar = [(scope.path, v['n'])
                  for scope in self.getScopes(excludeKinds=['type'])
                  for v in scope.varList
                  if v['n'].upper() not in excludeList and v['scopePath'] == scope.path]
        self.removeVarIfUnused(allVar, excludeDummy=True, excludeModule=True, simplify=simplify)

    @debugDecor
    def addExplicitArrayBounds(self, node=None):
        """
        Replace ':' by explicit arrays bounds.
        :param node: xml node in which ':' must be replaced (None to replace everywhere)
        """
        if node is None:
            nodes = [(scope, scope) for scope in self.getScopes()]
        else:
            nodes = [(self, node)]

        for (scope, childNode) in nodes:
            for parent4 in [parent4 for parent4
                            in childNode.findall('.//{*}section-subscript/../../../..')
                            if parent4.find('./{*}R-LT/{*}component-R') is None]:
                # Shape of type members is unknown
                for parent in parent4.findall('.//{*}section-subscript/..'):
                    for sub in parent.findall('.//{*}section-subscript'):
                        lowerUsed = sub.find('./{*}lower-bound')
                        upperUsed = sub.find('./{*}upper-bound')
                        # A slice can be A(:), A(I:) or A(:I), but not A(I)
                        # With A(:) and A(:I), lowerUsed is None
                        # With A(I:) lowerUsed.tail contains a ':'
                        # With A(I) lowerUsed.tail  doesn't contain a ':'
                        if lowerUsed is None or \
                           (lowerUsed.tail is not None and ':' in lowerUsed.tail):
                            if lowerUsed is None or upperUsed is None:
                                # At least one array bound is implicit
                                varDesc = scope.varList.findVar(n2name(parent4.find('.//{*}N')))
                                if varDesc is not None and varDesc['t'] is not None and \
                                   'CHAR' not in varDesc['t']:  # module array or character
                                    lowerDecl, upperDecl = varDesc['as'][list(parent).index(sub)]
                                    if lowerDecl is None:
                                        lowerDecl = '1'
                                    # When a bound is explicit, we keep it, otherwise we
                                    # take the declared bound
                                    lowerBound = lowerDecl if lowerUsed is None \
                                        else alltext(lowerUsed)
                                    upperBound = upperDecl if upperUsed is None \
                                        else alltext(upperUsed)
                                    if upperBound is not None:  # case of implicit shape
                                        lowerXml, upperXml = createArrayBounds(lowerBound,
                                                                               upperBound,
                                                                               'ARRAY')
                                        # We remove current explicit bounds or the ':',
                                        # and replace them by the new explicit declaration
                                        for nnn in sub:
                                            if tag(nnn) in ('lower-bound', 'upper-bound'):
                                                sub.remove(nnn)
                                            else:
                                                raise PYFTError("Unexpected case, " +
                                                                "tag is {}".format(tag(nnn)))
                                        sub.text = ''  # Delete the initial ':'
                                        sub.extend([lowerXml, upperXml])

    @debugDecor
    @noParallel
    def addArrayParentheses(self):
        """
        Look for arrays and add parenthesis. A => A(:)
        """
        # Loop on scopes
        for scope in self.getScopes():
            for node in scope.iter():
                # Arrays are used in statements
                # * We must exclude allocate-stmt, deallocate-stmt, pointer-a-stmt, T-decl-stmt and
                #   associate-stmt,
                #   nullify-stmt function-stmt, subroutine-stmt, interface-stmt must be kept
                #   untouched
                #   action-stmt is discarded as it contains another statement.
                # * Array variables to modify can be found in
                #     - simple affectation (a-stmt),
                #     - if construct conditions (if-then-stmt, else-if-stmt),
                #     - where-construct mask (where-construct-stmt, else-where-stmt),
                #     - if statement condition (if-stmt/condition-E, be carefull to not take the
                #                               whole if-stmt as it contains the action-stmt which,
                #                               in turn, can contain an allocate/deallocate/pointer
                #                               assignment)
                #     - where statement mask (where-stmt/mask-E and not directly
                #                             where-stmt as for if-stmt),
                #     - select-case-stmt and case-stmt,
                #     - do-stmt or forall-construct-stmt (e.g. FOR I=LBOUND(A, 0), UBOUND(A, 0))
                #     - forall-stmt/forall-triplet-spec-LT
                nodeToTransform = None
                if tag(node) in ('allocate-stmt', 'deallocate-stmt', 'pointer-a-stmt',
                                 'T-decl-stmt', 'associate-stmt', 'function-stmt',
                                 'subroutine-stmt', 'interface-stmt', 'action-stmt',
                                 'nullify-stmt'):
                    # excluded
                    pass
                elif tag(node) in ('if-stmt', 'where-stmt', 'forall-stmt'):
                    # We must transform only a part of the node
                    part = {'if-stmt': 'condition-E', 'where-stmt': 'mask-E',
                            'forall-stmt': 'forall-triplet-spec-LT'}[tag(node)]
                    nodeToTransform = node.find('./{*}' + part)
                elif tag(node).endswith('-stmt'):
                    nodeToTransform = node
                if nodeToTransform is not None:
                    scope.addArrayParenthesesInNode(nodeToTransform)

    @debugDecor
    def addArrayParenthesesInNode(self, node):
        """
        Look for arrays and add parenthesis. A => A(:)
        :param node: xml node in which ':' must be added
        """
        # Loop on variables
        for namedE in node.findall('.//{*}named-E'):
            if not namedE.find('./{*}R-LT'):  # no parentheses
                if not self.isNodeInProcedure(namedE, ('ALLOCATED', 'ASSOCIATED', 'PRESENT')):
                    # Pointer/allocatable used in ALLOCATED/ASSOCIATED must not be modified
                    # Array in present must not be modified
                    nodeN = namedE.find('./{*}N')
                    var = self.varList.findVar(n2name(nodeN))
                    if var is not None and var['as'] is not None and len(var['as']) > 0 and \
                       not ((var['pointer'] or var['allocatable']) and self.isNodeInCall(namedE)):
                        # This is a known array variable, with no parentheses
                        # But we exclude pointer allocatable in call statement because the called
                        # subroutine can wait for a pointer/allocatable and not an array (and it
                        # is difficult to guess as it would need to find the source code of the
                        # subroutine)
                        nodeRLT = createElem('R-LT')
                        namedE.insert(list(namedE).index(nodeN) + 1, nodeRLT)
                        arrayR = createElem('array-R', text='(')
                        nodeRLT.append(arrayR)
                        sectionSubscriptLT = createElem('section-subscript-LT', tail=')')
                        arrayR.append(sectionSubscriptLT)
                        for _ in var['as']:
                            sectionSubscript = createElem('section-subscript', text=':', tail=', ')
                            sectionSubscriptLT.append(sectionSubscript)
                        sectionSubscript.tail = None  # last one

    @debugDecor
    @updateVarList
    def modifyAutomaticArrays(self, declTemplate=None, startTemplate=None, endTemplate=None):
        """
        :param declTemplate: declaration template
        :param startTemplate: template for the first executable statement
        :param: endTemplate: template for the last executable statement
        :return: number of arrays modified
        Modifies all automatic arrays declaration in subroutine and functions. The declaration is
        replaced by the declaration template, the start template is inserted as first executable
        statement and the end template as last executable statement. Each template can use the
        following place holders:
        "{doubledotshape}", "{shape}", "{lowUpList}", "{name}" and "{type}" wich are, respectively
        modified into ":, :, :", "I, I:J, 0:I", "1, I, I, J, 0, I", "A", "REAL" if the original
        declaration statement was "A(I, I:J, 0:I)". The template
        "{type}, DIMENSION({doubledotshape}), ALLOCATABLE :: {name}#
         ALLOCATE({name}({shape}))#DEALLOCATE({name})"
        replaces automatic arrays by allocatables
        """
        templates = {'decl': declTemplate if declTemplate is not None else '',
                     'start': startTemplate if startTemplate is not None else '',
                     'end': endTemplate if endTemplate is not None else ''}  # ordered dict

        number = 0
        for scope in [scope for scope in self.getScopes()
                      if scope.path.split('/')[-1].split(':')[0] in ('sub', 'func')]:
            # For all subroutine and function scopes
            # Determine the list of variables to transform
            varListToTransform = []
            for var in [var for var in scope.varList
                        if var['as'] is not None and
                        len(var['as']) > 0 and
                        not (var['arg'] or var['allocatable'] or
                             var['pointer'] or var['result'])]:
                # For all automatic arrays, which are not argument, not allocatable,
                # not pointer and not result
                if var['init'] is not None:
                    logging.warning("An array (%s) has an initial value, it can't be " +
                                    "processed by modifyAutomaticArrays.)", var['n'])
                else:
                    varListToTransform.append(var)
            # A variable can make use of the size of another variable in its declaration statement
            # We order the variables to not insert the declaration of a variable before the
            # declaration of the variables it depends on
            orderedVarListToTransform = []
            while len(varListToTransform) > 0:
                nAdded = 0
                for var in varListToTransform[:]:
                    # flatten var['asx'] excluding None
                    listN = [x for dim in var['asx'] for x in dim if x is not None]
                    listN = [n2name(nodeN).upper() for asx in listN
                             for nodeN in asx.findall('.//{*}N/{*}n/..')]
                    if len(set(listN).intersection([v['n'].upper()
                                                    for v in varListToTransform])) == 0:
                        # Variable var does not use variables still in varListToTransform
                        varListToTransform.remove(var)
                        orderedVarListToTransform.append(var)
                        nAdded += 1
                if nAdded == 0:
                    raise PYFTError('It seems that there is a circular reference in ' +
                                    'the declaration statements')
            # Loop on variable to transform
            for var in orderedVarListToTransform[::-1]:  # reverse order
                number += 1
                # Apply the template
                templ = copy.deepcopy(templates)
                for templPart in templ:
                    if '{doubledotshape}' in templ[templPart]:
                        templ[templPart] = templ[templPart].replace(
                            '{doubledotshape}', ','.join([':'] * len(var['as'])))
                    if '{shape}' in templ[templPart]:
                        result = []
                        for i in range(len(var['as'])):
                            if var['as'][i][0] is None:
                                result.append(var['as'][i][1])
                            else:
                                result.append(var['as'][i][0] + ':' + var['as'][i][1])
                        templ[templPart] = templ[templPart].replace('{shape}', ', '.join(result))
                    if '{name}' in templ[templPart]:
                        templ[templPart] = templ[templPart].replace('{name}', var['n'])
                    if '{type}' in templ[templPart]:
                        templ[templPart] = templ[templPart].replace('{type}', var['t'])
                    if '{lowUpList}' in templ[templPart]:
                        result = []
                        for i in range(len(var['as'])):
                            if var['as'][i][0] is None:
                                result.extend([1, var['as'][i][1]])
                            else:
                                result.extend([var['as'][i][0], var['as'][i][1]])
                        templ[templPart] = templ[templPart].replace(
                            '{lowUpList}', ', '.join([str(r) for r in result]))

                # Get the xml for the template
                separator = "!ABCDEFGHIJKLMNOPQRSTUVWabcdefghijklmnopqrstuvwxyz0123456789"
                part = 0
                for node in createExpr(templ['decl'] + '\n' + separator + '\n' +
                                       templ['start'] + '\n' + separator + '\n' +
                                       templ['end']):
                    templPart = list(templ.keys())[part]
                    if not isinstance(templ[templPart], list):
                        templ[templPart] = []
                    if tag(node) == 'C' and node.text == separator:
                        part += 1
                    else:
                        templ[templPart].append(node)

                # Replace declaration statement
                # We look for the last position in the declaration list which do not use the
                # variable we add
                # This algorithm will stop when the original declaration statement is encoutered
                for decl in scope.findall('./{*}T-decl-stmt'):
                    index = list(scope).index(decl)
                    if var['n'] in [n2name(nodeN) for nodeN in decl.findall('.//{*}N')]:
                        break
                scope.removeVar([(scope.path, var['n'])], simplify=False)
                for nnn in templ['decl'][::-1]:
                    scope.insert(index, nnn)

                # Insert statements
                for nnn in templ['start'][::-1]:
                    scope.insertStatement(nnn, True)
                for nnn in templ['end'][::-1]:
                    scope.insertStatement(nnn, False)
        return number

    @staticmethod
    @debugDecor
    def varSpec2stmt(varSpec):
        """
        :param varSpec: a variable description, same form as the items return by self.varList
        :return: the associated declarative statement
        """
        if varSpec['use'] is not False:
            stmt = f"USE {varSpec['use']}, ONLY: {varSpec['n']}"
        else:
            stmt = varSpec['t']
            if varSpec['as']:
                stmt += ', DIMENSION('
                dl = []
                for el in varSpec['as']:
                    if el[0] is None and el[1] is None:
                        dl.append(':')
                    elif el[0] is None:
                        dl.append(el[1])
                    else:
                        dl.append(el[0] + ':' + el[1])
                if (varSpec['allocatable'] and not all(d == ':' for d in dl)) or \
                   (any(d == ':' for d in dl) and not varSpec['allocatable']):
                    raise PYFTError('Missing dim are mandatory and allowed ' +
                                    'only for allocatable arrays')
                stmt += ', '.join(dl) + ')'
                if varSpec['allocatable']:
                    stmt += ", ALLOCATABLE"
            if varSpec['parameter']:
                stmt += ", PARAMETER"
            if varSpec['i'] is not None:
                stmt += ", INTENT(" + varSpec['i'] + ")"
            if varSpec['opt'] is True:
                stmt += ", OPTIONAL"
            stmt += " :: " + varSpec['n']
            if varSpec['init'] is not None:
                stmt += "=" + varSpec['init']
        return stmt

    @debugDecor
    def findIndexArrayBounds(self, arr, index, loopVar):
        """
        Find bounds and loop variable for a given array index
        :param arr: array node (named-E node with a array-R child)
        :param index: index of the rank of the array
        :param loopVar: None to create new variable for each added DO loop
                        or a function that return the name of the variable to use for the
                            loop control.
                        This function returns a string (name of the variable), or True to create
                        a new variable, or False to not transform this statement
                        The functions takes as arguments:
                          - lower and upper bounds as defined in the declaration statement
                          - lower and upper bounds as given in the statement
                          - name of the array
                          - index of the rank
        :return: the tuple (loopName, lowerBound, upperBound) where:
                    loopName is the name of the variable to use for the loop
                    lower and upper bounds are the bounds to use for the DO loop
        loopName can be:
            - a string
            - False to discard this index
            - True to create a new variable to loop with
        """
        name = n2name(arr.find('./{*}N'))
        ss = arr.findall('./{*}R-LT/{*}array-R/{*}section-subscript-LT/{*}section-subscript')[index]
        # We are only interested by the subscript containing ':'
        # we must not iterate over the others, eg: X(:,1)
        if ':' in alltext(ss):
            # Look for lower and upper bounds for iteration and declaration
            lowerUsed = ss.find('./{*}lower-bound')
            upperUsed = ss.find('./{*}upper-bound')
            varDesc = self.varList.findVar(name, array=True)
            if varDesc is not None:
                lowerDecl, upperDecl = varDesc['as'][index]
                if lowerDecl is None:
                    lowerDecl = '1'  # default lower index for FORTRAN arrays
            else:
                lowerDecl, upperDecl = None, None

            # name of the loop variable
            if loopVar is None:
                # loopVar is not defined, we create a new variable for the loop only if lower
                # and upper bounds have been found (easy way to discard character strings)
                varName = lowerDecl is not None and upperDecl is not None
            else:
                varName = loopVar(lowerDecl, upperDecl,
                                  None if lowerUsed is None else alltext(lowerUsed),
                                  None if upperUsed is None else alltext(upperUsed), name, index)
            return (varName,
                    lowerDecl if lowerUsed is None else alltext(lowerUsed),
                    upperDecl if upperUsed is None else alltext(upperUsed))
        return None

    @debugDecor
    def arrayR2parensR(self, namedE, table):
        """
        Transform a array-R into a parens-R node by replacing slices by variables
        In 'A(:)', the ':' is in a array-R node whereas in 'A(JL)', 'JL' is in a parens-R node.
        Both the array-R and the parens-R nodes are inside a R-LT node
        :param namedE: a named-E node
        :param table: dictionnary returned by the decode function
        :param varList: None or a VarList object in which varaibles are searched for
        """
        # Before A(:): <f:named-E>
        #                <f:N><f:n>A</f:n></f:N>
        #                <f:R-LT>
        #                  <f:array-R>(
        #                    <f:section-subscript-LT>
        #                      <f:section-subscript>:</f:section-subscript>
        #                    </f:section-subscript-LT>)
        #                  </f:array-R>
        #                </f:R-LT>
        #               </f:named-E>
        # After  A(I): <f:named-E>
        #                <f:N><f:n>A</f:n></f:N>
        #                <f:R-LT>
        #                  <f:parens-R>(
        #                    <f:element-LT>
        #                      <f:element><f:named-E><f:N><f:n>I</f:n></f:N></f:named-E></f:element>
        #                    </f:element-LT>)
        #                  </f:parens-R>
        #                </f:R-LT>
        #              </f:named-E>

        nodeRLT = namedE.find('./{*}R-LT')
        arrayR = nodeRLT.find('./{*}array-R')  # Not always in first position, eg: ICED%XRTMIN(:)
        if arrayR is not None:
            index = list(nodeRLT).index(arrayR)
            parensR = createElem('parens-R', text='(', tail=')')
            elementLT = createElem('element-LT')
            parensR.append(elementLT)
            ivar = -1
            for ss in nodeRLT[index].findall('./{*}section-subscript-LT/{*}section-subscript'):
                element = createElem('element', tail=', ')
                elementLT.append(element)
                if ':' in alltext(ss):
                    ivar += 1
                    varName = list(table.keys())[ivar]  # variable name
                    lower = ss.find('./{*}lower-bound')
                    upper = ss.find('./{*}upper-bound')
                    if lower is not None:
                        lower = alltext(lower)
                    if upper is not None:
                        upper = alltext(upper)
                    if lower is not None and ss.text is not None and ':' in ss.text:
                        # fxtran bug workaround
                        upper = lower
                        lower = None
                    if lower is None and upper is None:
                        # E.g. 'A(:)'
                        # In this case we use the DO loop bounds without checking validity with
                        # respect to the array declared bounds
                        element.append(createExprPart(varName))
                    else:
                        # E.g.:
                        # !$mnh_expand_array(JI=2:15)
                        # A(2:15) or A(:15) or A(2:)
                        if lower is None:
                            # lower bound not defined, getting lower declared bound for this array
                            lower = self.varList.findVar(n2name(namedE.find('{*}N')),
                                                         array=True)['as'][ivar][0]
                            if lower is None:
                                lower = '1'  # default fortran lower bound
                        elif upper is None:
                            # upper bound not defined, getting upper declared bound for this array
                            upper = self.varList.findVar(n2name(namedE.find('{*}N')),
                                                         array=True)['as'][ivar][1]
                        # If the DO loop starts from JI=I1 and goes to JI=I2; and array
                        # bounds are J1:J2
                        # We compute J1-I1+JI and J2-I2+JI and they should be the same
                        # E.g: array bounds could be 'I1:I2' (becoming JI:JI) or 'I1+1:I2+1"
                        # (becoming JI+1:JI+1)
                        newlower = simplifyExpr(lower, add=varName, sub=table[varName][0])
                        newupper = simplifyExpr(upper, add=varName, sub=table[varName][1])
                        if newlower != newupper:
                            raise PYFTError(("Don't know how to do with an array declared with " +
                                             "'{la}:{ua}' and a loop from '{ll}' to '{ul}'"
                                             ).format(la=lower, ua=upper,
                                                      ll=table[varName][0],
                                                      ul=table[varName][1]))
                        element.append(createExprPart(newlower))
                else:
                    element.append(ss.find('./{*}lower-bound'))
            element.tail = None  # last element
            nodeRLT.remove(nodeRLT[index])
            nodeRLT.insert(index, parensR)

    @debugDecor
    def findArrayBounds(self, arr, loopVar, extraVarList=None):
        """
        Find bounds and loop variable given an array
        :param arr: array node (named-E node with a array-R child)
        :param loopVar: None to create new variable for each added DO loop
                        or a function that return the name of the variable to use for the loop
                            control.
                        This function returns a string (name of the variable), or True to create
                        a new variable, or False to not transform this statement
                        The functions takes as arguments:
                          - lower and upper bounds as defined in the declaration statement
                          - lower and upper bounds as given in the statement
                          - name of the array
                          - index of the rank
        :param extraVarList: None or list of variables (such as those contained in a VarList object)
                             defined but not yet available in the self.varList object.
        :return: the tuple (table, newVar) where:
                    table is a dictionnary: keys are loop variable names
                                            values are tuples with lower and upper bounds
                    newVar is a list of loop variables not found in varList. This list has the same
                           format as the varList list.

        In case the loop variable cannot be defined, the function returns (None, [])
        """
        table = {}  # ordered since python 3.7
        name = n2name(arr.find('./{*}N'))
        varNew = []
        extraVarList = extraVarList if extraVarList is not None else []

        # Iteration on the different subscript
        for iss, ss in enumerate(arr.findall('./{*}R-LT/{*}array-R/{*}section-subscript-LT/' +
                                             '{*}section-subscript')):
            # We are only interested by the subscript containing ':'
            # we must not iterate over the others, eg: X(:,1)
            if ':' in alltext(ss):
                # Look for loop variable name and lower/upper bounds for iteration
                varName, lower, upper = self.findIndexArrayBounds(arr, iss, loopVar)
                # varName can be a string (name to use), True (to create a variable),
                # False (to discard the array
                if varName is not False and varName in table:
                    raise PYFTError(("The variable {var} must be used for the rank #{i1} whereas " +
                                     "it is already used for rank #{i2} (for array {name})."
                                     ).format(var=varName, i1=str(iss),
                                              i2=str(list(table.keys()).index(varName)),
                                              name=name))
                if varName is True:
                    # A new variable must be created
                    # We look for a variable name that don't already exist
                    # We can reuse a newly created varaible only if it is not used for the previous
                    # indexes of the same statement
                    j = 0
                    found = False
                    while not found:
                        j += 1
                        varName = 'J' + str(j)
                        var = self.varList.findVar(varName, extraVarList=extraVarList + varNew)
                        if (var is None or var.get('new', False)) and varName not in table:
                            found = True
                    varDesc = {'as': [], 'asx': [], 'n': varName, 'i': None,
                               't': 'INTEGER', 'arg': False, 'use': False, 'opt': False,
                               'scopePath': self.path}
                    if varDesc not in varNew:
                        varNew.append(varDesc)

                elif (varName is not False and
                      self.varList.findVar(varName, array=False, exactScope=True) is None):
                    # We must declare the variable
                    varDesc = {'as': [], 'asx': [], 'n': varName, 'i': None,
                               't': 'INTEGER', 'arg': False, 'use': False, 'opt': False,
                               'scopePath': self.path}
                    varNew.append(varDesc)

                # fill table
                table[varName] = (lower, upper)

        return (None, []) if False in table else (table, varNew)

    @debugDecor
    @updateVarList
    def renameVar(self, oldName, newName):
        """
        :param oldName: old name of the variable
        :param newName: new name of the variable
        """
        for node in self.findall('.//{*}N'):
            if n2name(node).upper() == oldName.upper():
                # Remove all n tag but one
                for nnn in node.findall('./{*}n')[1:]:
                    node.remove(nnn)
                # Fill the first n with the new name
                node.find('./{*}n').text = newName

    @debugDecor
    def removeVarIfUnused(self, varList, excludeDummy=False, excludeModule=False, simplify=False):
        """
        :param varList: list of variables to remove if unused. Each item is a list or tuple of two
                        elements.
                        The first one describes where the variable is used, the second one is
                        the name of the variable. The first element is a '/'-separated path with
                        each element having the form 'module:<name of the module>',
                        'sub:<name of the subroutine>' or 'func:<name of the function>'
        :param excludeDummy: if True, dummy arguments are always kept untouched
        :param excludeModule: if True, module variables are always kept untouched
        :param simplify: try to simplify code (if we delete a declaration statement that used a
                         variable as kind selector, and if this variable is not used else where,
                         we also delete it)
        :return: the varList without the unremovable variables
        If possible, remove the variable from declaration, and from the argument list if needed
        """
        varList = self._normalizeUniqVar(varList)
        if excludeModule:
            varList = [v for v in varList if v[0].split('/')[-1].split(':')[0] != 'module']

        varUsed = self.isVarUsed(varList, dummyAreAlwaysUsed=excludeDummy)
        varListToRemove = []
        for scopePath, varName in varList:
            assert scopePath.split('/')[-1].split(':')[0] != 'type', \
              "The removeVarIfUnused cannot be used with type members"
            if not varUsed[(scopePath, varName)]:
                varListToRemove.append([scopePath, varName])
        self.removeVar(varListToRemove, simplify=simplify)
        return varListToRemove

    @debugDecor
    def isVarUsed(self, varList, exactScope=False, dummyAreAlwaysUsed=False):
        """
        :param varList: list of variables to test. Each item is a list or tuple of two elements.
                        The first one describes where the variable is declared, the second one is
                        the name of the variable. The first element is a '/'-separated path with
                        each element having the form 'module:<name of the module>',
                        'sub:<name of the subroutine>' or 'func:<name of the function>'
        :param exactScope: True to search strictly in scope
        :param dummyAreAlwaysUsed: Returns True if variable is a dummy argument
        :return: a dict whose keys are the elements of varList, and values are True when the
                 variable is used, False otherwise

        If exactScope is True, the function will search for variable usage
        only in this scope. But this feature has a limited interest.

        If exactScope is False:
          - if scopePath is a subroutine/function in a contains section,
            and if the variable is not declared in this scope, usages are
            searched in the module/subroutine/function upper that declared
            the variable and in all subroutines/functions in the contains section
          - if scopePath is a module/subroutine/function that has a
            contains sections, usages are searched in all subroutines/functions
            in the contains section

        To know if a variable can be removed, you must use exactScope=False
        """
        varList = self._normalizeUniqVar(varList)
        # We must not limit to self.getScopes because var can be used upper than self
        allScopes = {scope.path: scope for scope in self.mainScope.getScopes()}

        # Computes in which scopes variable must be searched
        if exactScope:
            locsVar = {(scopePath, varName): [scopePath]
                       for scopePath, varName in varList}
        else:
            locsVar = {}
            for scopePath, varName in varList:
                # We search everywhere if var declaration is not found
                # Otherwise, we search from the scope where the variable is declared
                var = allScopes[scopePath].varList.findVar(varName)
                path = scopePath.split('/')[0] if var is None else var['scopePath']

                # We start search from here but we must include all routines in contains
                # that do not declare again the same variable name
                testScopes = [path]  # we must search in the current scope
                for scPath, sc in allScopes.items():
                    if scPath.startswith(path + '/') and \
                       scPath.split('/')[-1].split(':')[0] != 'type':
                        # sc is a scope path contained inside path and is not a type declaration
                        if sc.varList.findVar(varName, exactScope=True) is None:
                            # There is not another variable with same name declared inside
                            testScopes.append(scPath)  # if variable is used here, it is used
                locsVar[(scopePath, varName)] = testScopes

        # For each scope to search, list all the variables used
        usedVar = {}
        for scopePath in list(set(item for sublist in locsVar.values() for item in sublist)):
            usedVar[scopePath] = []
            # Loop on all child in the scope
            for node in allScopes[scopePath]:
                # we don't want use statement, it could be where the variable is declared,
                # not a usage place
                if not tag(node) == 'use-stmt':
                    if tag(node) == 'T-decl-stmt':
                        # We don't want the part with the list of declared variables, we only want
                        # to capture variables used in the kind selector or in the shape
                        # specification
                        nodesN = node.findall('.//{*}_T-spec_//{*}N') + \
                                 node.findall('.//{*}shape-spec//{*}N')
                    else:
                        nodesN = node.findall('.//{*}N')

                    # We look for the variable name in these 'N' nodes.
                    for nodeN in nodesN:
                        if dummyAreAlwaysUsed:
                            # No need to check if the variable is a dummy argument; because if it is
                            # one it will be found in the argument list of the subroutine/function
                            # and will be considered as used
                            usedVar[scopePath].append(n2name(nodeN).upper())
                        else:
                            parPar = allScopes[scopePath].getParent(nodeN, 2)  # parent of parent
                            # We exclude dummy argument list to really check if the variable is used
                            # and do not only appear as an argument of the subroutine/function
                            if parPar is None or not tag(parPar) == 'dummy-arg-LT':
                                usedVar[scopePath].append(n2name(nodeN).upper())

        result = {}
        for scopePath, varName in varList:
            assert scopePath.split('/')[-1].split(':')[0] != 'type', \
                'We cannot check type component usage'
            result[(scopePath, varName)] = any(varName.upper() in usedVar[scopePath]
                                               for scopePath in locsVar[(scopePath, varName)])

        return result

    @debugDecor
    @updateVarList
    def addArgInTree(self, varName, declStmt, pos, stopScopes, moduleVarList=None,
                     otherNames=None,
                     parser=None, parserOptions=None, wrapH=False):
        """
        Adds an argument to the routine and propagates it upward until we encounter a scope
        where the variable exists or a scope in stopScopes
        :param varName: variable name
        :param declStmt: declarative statment (will be used by addVar)
        :param pos: position of the variable in the list of dummy argument
        :param stopScopes: list of scopes to reach
        :param moduleVarList: list of module variable specification to insert in the xml code
                              a module variable specification is a list of two elements:
                              - module name
                              - variable name or or list of variable names
                                or None to add a USE statement without the ONLY attribute
                              use moduleVarList to not add module variables
        :param otherNames: None or list of other variable names that can be used
                           These variables are used first
        :param parser, parserOptions, wrapH: see the PYFT class

        Argument is inserted only on paths leading to one of scopes listed in stopScopes
        """
        def insertInArgList(varName, varNameToUse, pos, callFuncStmt):
            """
            Insert varName in the list of arguments to the subroutine or function call
            :param varName: name of the dummy argument
            :param varNameToUse: name of the variable
            :param pos: inclusion position
            :param callFuncStmt: call statement or function call
            """
            argList = callFuncStmt.find('./{*}R-LT/{*}parens-R/{*}element-LT')
            if argList is not None:
                container = createElem('element')
            else:
                argList = callFuncStmt.find('./{*}arg-spec')
                container = createElem('arg')
                if argList is None:
                    # Call without argument
                    callFuncStmt.find('./{*}procedure-designator').tail = '('
                    argList = createElem('arg-spec', tail=')')
                    callFuncStmt.append(argList)
            item = createExprPart(varNameToUse)
            previous = pos - 1 if pos >= 0 else len(argList) + pos  # convert negative pos
            while previous >= 0 and tag(argList[previous]) in ('C', 'cnt'):
                previous -= 1
            following = pos if pos > 0 else len(argList) + pos + 1  # convert negative pos
            while following <= len(argList) - 1 and tag(argList[following]) in ('C', 'cnt'):
                following += 1
            if (previous >= 0 and argList[previous].find('./{*}arg-N/{*}k') is not None) or \
               (following <= len(argList) - 1 and
                argList[following].find('./{*}arg-N/{*}k') is not None) or \
               following == len(argList):
                # We use the key=val syntax whereever it is possible because it's safer in case of
                # optional arguments
                # If previous arg, or following arg is already with a key=val syntax, we can (must)
                # use it
                # If the inserted argument is the last one of the list, it also can use this syntax
                k = createElem('k', text=varName)
                argN = createElem('arg-N', tail='=')
                argN.append(k)
                argN.set('n', varName)
                container.append(argN)
            container.append(item)
            self.insertInList(pos, container, argList)

        if self.path in stopScopes or self.tree.isUnderStopScopes(self.path, stopScopes):
            # We are on the path to a scope in the stopScopes list, or scopeUp is one of the
            # stopScopes
            var = self.varList.findVar(varName, exactScope=True)
            if otherNames is not None:
                vOther = [self.varList.findVar(v, exactScope=True) for v in otherNames]
                vOther = [v for v in vOther if v is not None]
                if len(vOther) > 0:
                    var = vOther[-1]

            if var is None:
                # The variable doesn't exist in this scope, we add it
                self.addVar([[self.path, varName, declStmt, pos]])
                if moduleVarList is not None:
                    # Module variables must be added when var is added
                    self.addModuleVar([(self.path, moduleName, moduleVarNames)
                                       for (moduleName, moduleVarNames) in moduleVarList])

                # We look for interface declaration if subroutine is directly accessible
                if len(self.path.split('/')) == 1:
                    filename, scopePathInterface = self.tree.findScopeInterface(self.path)
                    if filename is not None:
                        pft = None
                        try:
                            if self.getFileName() == os.path.normpath(filename):
                                # interface declared in same file
                                xml = self.mainScope
                                pft = None
                            else:
                                pft = pyfortool.pyfortool.conservativePYFT(
                                          filename, parser, parserOptions, wrapH, tree=self.tree,
                                          clsPYFT=self._mainScope.__class__)
                                xml = pft
                            scopeInterface = xml.getScopeNode(scopePathInterface)
                            varInterface = scopeInterface.varList.findVar(varName, exactScope=True)
                            if varInterface is None:
                                scopeInterface.addVar([[scopePathInterface, varName,
                                                        declStmt, pos]])
                                if moduleVarList is not None:
                                    # Module variables must be added when var is added
                                    xml.addModuleVar(
                                        [(scopePathInterface, moduleName, moduleVarNames)
                                         for (moduleName, moduleVarNames) in moduleVarList])
                            if pft is not None:
                                pft.write()
                        finally:
                            if pft is not None:
                                pft.close()

            if var is None and self.path not in stopScopes:
                # We must propagates upward
                # scopes calling the current scope
                for scopePathUp in self.tree.calledByScope(self.path):
                    if scopePathUp in stopScopes or self.tree.isUnderStopScopes(scopePathUp,
                                                                                stopScopes):
                        # We are on the path to a scope in the stopScopes list, or scopePathUp is
                        # one of the stopScopes
                        # can be defined several times?
                        for filename in self.tree.scopeToFiles(scopePathUp):
                            pft = None
                            try:
                                if self.getFileName() == os.path.normpath(filename):
                                    # Upper scope is in the same file
                                    xml = self.mainScope
                                    pft = None
                                else:
                                    pft = pyfortool.pyfortool.conservativePYFT(
                                              filename, parser, parserOptions, wrapH,
                                              tree=self.tree,
                                              clsPYFT=self._mainScope.__class__)
                                    xml = pft
                                scopeUp = xml.getScopeNode(scopePathUp)
                                # Add the argument and propagate upward
                                scopeUp.addArgInTree(
                                    varName, declStmt, pos,
                                    stopScopes, moduleVarList, otherNames,
                                    parser=parser, parserOptions=parserOptions,
                                    wrapH=wrapH)
                                # Add the argument to calls (subroutine or function)
                                name = self.path.split('/')[-1].split(':')[1].upper()
                                isCalled = False
                                varNameToUse = varName
                                if otherNames is not None:
                                    vOther = [scopeUp.varList.findVar(v, exactScope=True)
                                              for v in otherNames]
                                    vOther = [v for v in vOther if v is not None]
                                    if len(vOther) > 0:
                                        varNameToUse = vOther[-1]['n']
                                if self.path.split('/')[-1].split(':')[0] == 'sub':
                                    # We look for call statements
                                    for callStmt in scopeUp.findall('.//{*}call-stmt'):
                                        callName = n2name(callStmt.find(
                                            './{*}procedure-designator/{*}named-E/{*}N')).upper()
                                        if callName == name:
                                            insertInArgList(varName, varNameToUse, pos, callStmt)
                                            isCalled = True
                                else:
                                    # We look for function use
                                    for funcCall in scopeUp.findall(
                                                    './/{*}named-E/{*}R-LT/{*}parens-R/' +
                                                    '{*}element-LT/../../..'):
                                        funcName = n2name(funcCall.find('./{*}N')).upper()
                                        if funcName == name:
                                            insertInArgList(varName, varNameToUse, pos, funcCall)
                                            isCalled = True
                                if pft is not None:
                                    pft.write()
                            finally:
                                if pft is not None:
                                    pft.close()

                            if isCalled:
                                # We must check in the scope (or upper scopes) if an interface
                                # block declares the routine
                                for interface in self.findall('.//{*}interface-construct/{*}' +
                                                              'program-unit/{*}subroutine-stmt/' +
                                                              '{*}subroutine-N/{*}N/../../../'):
                                    if n2name(interface.find('./{*}subroutine-stmt/' +
                                                             '{*}subroutine-N/' +
                                                             '{*}N')).upper() == name:
                                        # We must add the argument to the interface
                                        raise PYFTError('This case is not yet implemented')
