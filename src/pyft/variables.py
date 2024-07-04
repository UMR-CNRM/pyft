"""
This module implements functions to deal with variables
"""

import xml.etree.ElementTree as ET
from xml.etree.ElementTree import Element
import logging
import copy
import re

from pyft.util import PYFTError, debugDecor, alltext, fortran2xml, isExecutable, n2name
from pyft.expressions import createArrayBounds, simplifyExpr, createExprPart, createExpr
from pyft.tree import updateTree
import pyft.pyft


#No @debugDecor for this low-level method
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


class Variables():
    @debugDecor
    def getVarList(self, scopePath=None):
        """
        :param scopePath: restrict list to this scope path or scope path list (None to get
                             variables from all the scopes)
        :return: a list of dictionaries. Each one is for a different variable
                 and has the following keys:
                  - as: list of array specifications, [] for a scalar, None if unknown
                  - asx: same but encoded in xml, [] for a scalar, None if unknown
                  - n: name of the variable as written
                  - i: intent
                  - t: type specification, or None if unknown
                  - arg: False if variable is a not dummy argument
                         argument position otherwise
                  - use: false if variable is not a module variable
                         module name otherwise
                  - opt: true if variable is optional
                  - scope: its scope
                  - allocatable: boolean
                  - parameter: boolean
                  - pointer: boolean
                  - result: boolean (function result)
        Notes: - variables are found in modules only if the 'ONLY' attribute is used
               - array specification and type is unknown for module variables
               - function is not able to follow the 'ASSOCIATE' statements
        """
        def decode_array_specs(array_specs):
            as_list = []
            asx_list = []
            for array_spec in array_specs:
                lb = array_spec.find('.//{*}lower-bound')
                ub = array_spec.find('.//{*}upper-bound')
                as_list.append([alltext(lb) if lb is not None else None, alltext(ub) if ub is not None else None])
                asx_list.append([lb if lb is not None else None, ub if ub is not None else None])
            return as_list, asx_list

        result = []
        for scope in self.getScopeNodes(scopePath, excludeContains=True):
            #In case scope is a function, we determine the name of the result
            if scope[0].tag.split('}')[1] == 'function-stmt':
                rSpec = scope[0].find('./{*}result-spec/{*}N')
                funcResultName = rSpec if rSpec is not None else scope[0].find('./{*}function-N/{*}N')
                funcResultName = n2name(funcResultName).upper()
            else:
                funcResultName = ''
    
            #Find dummy arguments
            dummy_args = [n2name(e).upper() for stmt in scope for e in stmt.findall('.//{*}dummy-arg-LT/{*}arg-N/{*}N')]
    
            decl_stmts = [stmt for stmt in scope
                          if stmt.tag.endswith('}T-decl-stmt') or stmt.tag.endswith('}component-decl-stmt')]
            #Loop on each declaration statement
            for decl_stmt in decl_stmts:
                t_spec = alltext(decl_stmt.find('.//{*}_T-spec_'))
                i_spec = decl_stmt.find('.//{*}intent-spec')
                if i_spec is not None: i_spec = i_spec.text
                opt_spec = False
                allocatable = False
                parameter = False
                pointer = False
                allattributes = decl_stmt.findall('.//{*}attribute/{*}attribute-N')
                for attribute in allattributes:
                    if alltext(attribute).upper() == 'OPTIONAL': opt_spec = True
                    if alltext(attribute).upper() == 'ALLOCATABLE': allocatable = True
                    if alltext(attribute).upper() == 'PARAMETER': parameter = True
                    if alltext(attribute).upper() == 'POINTER': pointer = True
                #Dimensions declared with the DIMENSION attribute
                array_specs = decl_stmt.findall('.//{*}attribute//{*}array-spec//{*}shape-spec')
                as0_list, asx0_list = decode_array_specs(array_specs)
    
                #Loop on each declared variables
                en_decls = decl_stmt.findall('.//{*}EN-decl')
                for en_decl in en_decls:
                    n = n2name(en_decl.find('.//{*}N')).upper()
                    #Dimensions declared after the variable name
                    array_specs = en_decl.findall('.//{*}array-spec//{*}shape-spec')
                    as_list, asx_list = decode_array_specs(array_specs)
                    #Initial value (parameter or not)
                    init = en_decl.find('./{*}init-E')
                    if init is not None: init = alltext(init)
    
                    result.append({'as': as_list if len(as0_list) == 0 else as0_list,
                                   'asx': asx_list if len(asx0_list) == 0 else asx0_list,
                                   'n': n, 'i': i_spec, 't': t_spec, 'arg': n in dummy_args,
                                   'use': False, 'opt': opt_spec, 'allocatable': allocatable,
                                   'parameter': parameter, 'pointer': pointer, 'result': funcResultName == n,
                                   'init': init, 'scope': scope.path})
    
            #Loop on each use statement
            use_stmts = [stmt for stmt in scope if stmt.tag.endswith('}use-stmt')]
            for use_stmt in use_stmts:
                module = n2name(use_stmt.find('.//{*}module-N').find('.//{*}N'))
                for v in use_stmt.findall('.//{*}use-N'):
                    n = n2name(v.find('.//{*}N'))
                    result.append({'as': None, 'asx': None,
                                   'n': n, 'i': None, 't': None, 'arg': False,
                                   'use': module, 'opt': None, 'allocatable': None,
                                   'parameter': None, 'pointer': None, 'result': None,
                                   'init': None, 'scope': scope.path})
    
        return result

    @debugDecor
    def findVar(self, varName, scopePath, varList=None, array=None, exactScope=False):
            """
            Search for a variable in a list of declared variables
            :param varName: variable name
            :param scopePath: scope path in which variable is defined
            :param varList: list of declared variables such as returned by getVarList, None to build this list
            :param array: True to limit search to arrays, False to limit search to non array, None to return anything
            :param exactScope: True to limit search to variables declared in the scopePath
            :return: None if not found or the description of the variable
    
            The function is designed to return the declaration of a given variable.
            If we know that the variable is (is not) an array, the last declaration statement
            must (must not) be an array declaration. If the last declaration statement found doesn't
            correspond to what is expected, we don't return it.
            In case array is None, we return the last declaration statement without checking its kind.
            """
            if varList is None:
                varList = self.getVarList()
            # Select all the variables declared in the current scope or upper, then select the last declared
            candidates = {v['scope']:v for v in varList
                          if v['n'].upper() == varName.upper() and \
                             (((not exactScope) and scopePath.startswith(v['scope'])) or \
                              (     exactScope  and scopePath == v['scope']         ))}
            if len(candidates) > 0:
                last = candidates[max(candidates, key=len)]
                if array is True and last.get('as', None) is not None and len(last['as']) > 0:
                    return last
                elif array is False and len(last.get('as', [])) == 0:
                    return last
                elif array is None:
                    return last
                else:
                    return None
            else:
                return None

    @debugDecor
    def showVarList(self, scopePath=None):
        """
        Display on stdout a nice view of all the variables
        :param scopePath: restrict list to this scope path (None to loop
                             over all scopes)
        """
        for scope in self.getScopeNodes(scopePath, excludeContains=True):
            print('List of variables declared in {}:'.format(scope.path))
            for v in self.getVarList(scope.path):
                print('  Variable {}:'.format(v['n']))
                if v['use']:
                    print('    is a variable taken in the {} module'.format(v['use']))
                else:
                    isscalar = len(v['as']) == 0
                    if isscalar:
                        print('    is scalar')
                    else:
                        print('    is of rank {}, with dimensions {}'.format(len(v['as']),
                                            ', '.join([(':'.join([('' if s is None else s)
                                                                  for s in v['as'][i]]))
                                                       for i in range(len(v['as']))])))
                    if v['arg']:
                        intent = 'without intent' if v['i'] is None else 'with intent {}'.format(v['i'])
                        print('    is a dummy argument {}'.format(intent))
                    else:
                        print('    is a local variable')
                print()

    #No @debugDecor for this low-level method
    def _normalizeUniqVar(self, scopeVarList):
        """
        Internal method to suppress duplicates in scopeVarList
        (list of tuples made of scope path and variable name)
        """
        return list(set((self.normalizeScope(scopePath), var.upper())
                        for (scopePath, var) in scopeVarList))

    @debugDecor
    def attachArraySpecToEntity(self):
        """
        Find all T-decl-stmt elements that have a child element 'attribute' with attribute-N="DIMENSION" and
        move the attribute into EN-N elements
        E.g., before :
        REAL, DIMENSION(D%NIJT,D%NKT) :: ZTLK, ZRT
        INTEGER, PARAMETER, DIMENSION(1,1) :: IBUEXTRAIND=(/18, 30/)
        after :
        REAL :: ZTLK(D%NIJT,D%NKT), ZRT(D%NIJT,D%NKT)
        INTEGER, PARAMETER  :: IBUEXTRAIND(1,1)=(/18, 30/)
        Limitations : "DIMENSION" must be in upper case in attribute.text
        """
        # Find all T-decl-stmt elements that have a child element 'attribute' with attribute-N="DIMENSION"
        for decl in self.findall('.//{*}T-decl-stmt'):
            array_spec = decl.find('./{*}attribute[{*}attribute-N="DIMENSION"]/{*}array-spec')
            attr_elem = decl.find('./{*}attribute[{*}attribute-N="DIMENSION"]/{*}array-spec/...')
            #Discard allocatable (':' in array_spec)
            if array_spec is not None and not ':' in alltext(array_spec):
                # Check if EN-decl elements don't already have an array-spec child element
                # or an intial value
                if decl.find('./{*}EN-decl-LT/{*}EN-decl/{*}array-spec') is None and \
                   decl.find('./{*}EN-decl-LT/{*}EN-decl/{*}init-E') is None:
                    n = decl.findall('./{*}EN-decl-LT/{*}EN-decl')
                    # Attach the array-spec element after the EN-N element
                    for elem in n:
                        elem.append(copy.deepcopy(array_spec))
                    # Remove the dimension and array-spec elements
                    self.removeFromList(attr_elem, decl)

    @debugDecor
    def getImplicitNoneText(self, scope):
        """
        :param scope: PYFTscope (or scope path) to search for the implicit none statement
        :return: the "IMPLICIT NONE" text
        """
        if isinstance(scope, str):
            scope = self.getScopeNode(scope)
        for node in scope:
            if node.tag.endswith('}implicit-none-stmt'):
                return node.text
        return None
    
    @debugDecor
    def checkImplicitNone(self, mustRaise=False):
        """
        :param mustRaise: True to raise
        Issue a logging.warning if the "IMPLICIT NONE" statment is missing
        If mustRaise is True, issue a logging.error instead and raise an error
        """
        for scope in self.getScopes():
            #The IMPLICIT NONE statement is inherited from the top unit, control at top unit is enough
            #apart for INTERFACE blocs
            if scope.path.count('/') == 0 or \
               (scope.path.count('/') >= 2 and \
                scope.path.split('/')[-2].startswith('interface:')):
              if self.getImplicitNoneText(scope) is None:
                  message = "The 'IMPLICIT NONE' statment is missing in file " + \
                            "'{file}' for {scopePath}.".format(file=self.getFileName(),
                                                               scopePath=scope.path)
                  if mustRaise:
                      logging.error(message)
                      raise PYFTError(message)
                  else:
                      logging.warning(message)
    
    @debugDecor
    def checkIntent(self, mustRaise=False):
        """
        :param mustRaise: True to raise
        Issue a logging.warning if some "INTENT" attributes are missing
        If mustRaise is True, issue a logging.error instead and raise an error
        """
        ok = True
        l = logging.error if mustRaise else logging.warn
        for v in self.getVarList():
            if v['arg'] and v['i'] is None:
              l(("The dummy argument {} as no INTENT attribute, in " + \
                 "file '{}'").format(v['n'], self.getFileName()))
              ok = False
        if not ok and mustRaise:
            raise PYFTError("There are dummy arguments without INTENT attribute in " + \
                            "file '{}'".format(self.getFileName()))

    @debugDecor
    @updateTree('signal')
    def removeVar(self, varList, simplify=False):
        """
        :param varList: list of variables to remove. Each item is a list or tuple of two elements.
                        The first one describes where the variable is declared, the second one is the name
                        of the variable. The first element is a '/'-separated path with each element
                        having the form 'module:<name of the module>', 'sub:<name of the subroutine>',
                        'func:<name of the function>' or 'type:<name of the type>'
        :param simplify: try to simplify code (if we delete a declaration statement that used a
                         variable as kind selector, and if this variable is not used else where,
                         we also delete it)
        Remove the variable from declaration, and from the argument list if needed
        """
        varList = self._normalizeUniqVar(varList)
    
        #Sort scopes by depth
        sortedVarList = {}
        for scopePath, varName in varList:
            nb = scopePath.count('/')
            sortedVarList[nb] = sortedVarList.get(nb, []) + [(scopePath, varName.upper())]
    
        varToRemoveIfUnused = []
        #Loop on varList starting by inner most variables
        nbList = [] if len(sortedVarList.keys()) == 0 else range(max(sortedVarList.keys()) + 1)[::-1]
        for nb in nbList:
            sortedVarList[nb] = sortedVarList.get(nb, [])
            #Loop on scopes
            for scopePath in list(set([scopePath for scopePath, _ in sortedVarList[nb]])):
                #Variables searched in this scope
                varNames = list(set([v for (w, v) in sortedVarList[nb] if w == scopePath]))
                declStmt = _getDeclStmtTag(scopePath)
                # If scopePath is "module:XX/sub:YY", getScopeNode returns a node
                # containing the subroutine declaration statements and
                # excluding the subroutine and functions potentially included
                # after a "contains" statement
                previous = None
                # list() to allow removing during the iteration
                for node in list(self.getScopeNode(scopePath, excludeContains=True)):
                    deleted = False
    
                    #Checks if variable is a dummy argument
                    dummy_lst = node.find('{*}dummy-arg-LT') #This is the list of the dummy arguments
                    if dummy_lst is not None:
                        #Loop over all dummy arguments
                        for arg in dummy_lst.findall('.//{*}arg-N'):
                            name = n2name(arg.find('.//{*}N')).upper()
                            for varName in [v for v in varNames if v == name]:
                                #This dummy arg is a searched variable, we remove it from the list
                                self.removeFromList(arg, dummy_lst)
    
                    #In case the variable is declared
                    if node.tag.endswith('}' + declStmt):
                        #We are in a declaration statement
                        decl_lst = node.find('./{*}EN-decl-LT') #list of declaration in the current statment
                        for en_decl in decl_lst.findall('.//{*}EN-decl'):
                            name = n2name(en_decl.find('.//{*}N')).upper()
                            for varName in [v for v in varNames if v == name]:
                                #The argument is declared here, we suppress it from the declaration list
                                varNames.remove(varName)
                                self.removeFromList(en_decl, decl_lst)
                        #In all the variables are suppressed from the declaration statement
                        if len(list(decl_lst.findall('./{*}EN-decl'))) == 0:
                            if simplify:
                                varToRemoveIfUnused.extend([[scopePath, n2name(N)] for N in node.findall('.//{*}N')])
                            #We will delete the current node but we don't want to lose
                            #any text. So, we put the node's text in the tail of the previous node
                            if previous is not None and node.tail is not None:
                                if previous.tail is None: previous.tail = ''
                                previous.tail += node.tail
                            deleted = True
                            self.getParent(node).remove(node)
    
                    #In case the variable is a module variable
                    if node.tag.endswith('}use-stmt'):
                        #We are in a use statement
                        use_lst = node.find('./{*}rename-LT')
                        if use_lst is not None:
                            for rename in use_lst.findall('.//{*}rename'):
                                name = n2name(rename.find('.//{*}N')).upper()
                                for varName in [v for v in varNames if v == name]:
                                    varNames.remove(varName)
                                    #The variable is declared here, we remove it from the list
                                    self.removeFromList(rename, use_lst)
                                    #In case the variable was alone
                                    attribute = node.find('{*}module-N').tail
                                    if attribute is None: attribute = ''
                                    attribute = attribute.replace(' ', '').replace('\n', '').replace('&', '').upper()
                                    use_lst = node.find('./{*}rename-LT')
                                    if len(use_lst) == 0 and attribute[0] == ',' and attribute[1:] == 'ONLY:':
                                        #If there is a 'ONLY' attribute, we suppress the use statement entirely
                                        if previous is not None and node.tail is not None:
                                            if previous.tail is None: previous.tail = ''
                                            previous.tail += node.tail
                                        deleted = True
                                        self.getParent(node).remove(node)
                                        self.tree.signal(self)  # Tree must be updated
                                    elif len(use_lst) == 0:
                                        #there is no 'ONLY' attribute
                                        moduleName = self.getSiblings(use_lst, before=True, after=False)[-1]
                                        previousTail = moduleName.tail
                                        if previousTail is not None:
                                            moduleName.tail = previousTail.replace(',', '')
                                        self.getParent(use_lst).remove(use_lst)
                    #end loop if all variables have been found
                    if len(varNames) == 0:
                        break
                    #Store node for the following iteration
                    if not deleted:
                        previous = node
    
                #If some variables have not been found, they are certainly declared one level upper
                if len(varNames) != 0:
                     newWhere = '/'.join(scopePath.split('/')[:-1])
                     sortedVarList[nb - 1] = sortedVarList.get(nb - 1, []) + [(newWhere, varName) for varName in varNames]
    
        if simplify and len(varToRemoveIfUnused) > 0:
            self.removeVarIfUnused(varToRemoveIfUnused, excludeDummy=True, simplify=True)

    @debugDecor
    def addVar(self, varList):
        """
        :param varList: list of variable specification to insert in the xml code
                        a variable specification is a list of four elements:
                        - variable scope path (path to module, subroutine, function or type declaration)
                        - variable name
                        - declarative statment
                        - position of the variable in the list of dummy argument,
                          None for a local variable
        """
        for (scopePath, name, declStmt, pos) in varList:
            locNode = self.getScopeNode(scopePath)
    
            #Add variable to the argument list
            if pos is not None:
                argN = Element('{http://fxtran.net/#syntax}:arg-N')
                N = Element('{http://fxtran.net/#syntax}:N')
                n = Element('{http://fxtran.net/#syntax}:n')
                n.text = name
                N.append(n)
                argN.append(N)
                #search for a potential node, within the scope, with a list of dummy arguments
                argLst = locNode.find('.//{*}dummy-arg-LT')
                if argLst is None:
                   #This was a subroutine or function without dummy arguments
                   locNode[0][0].tail = '('
                   argLst = Element('{http://fxtran.net/#syntax}:dummy-arg-LT')
                   argLst.tail = ')'
                   locNode[0].insert(1, argLst)
                self.insertInList(pos, argN, argLst)
    
            #Declare the variable
            #The following test is needed in case several variables are added in the argument list
            #but the declaration statement is given only once for all the variables
            if declStmt is not None and declStmt != '':
                #Declaration statement tag according to path (member of type declaration or not)
                declStmtTag = _getDeclStmtTag(scopePath)
    
                if scopePath.split('/')[-1].split(':')[0] == 'type':
                    #Add declaration statement in type declaration
                    #Statement building
                    fortranSource = "MODULE MODU_{var}\nTYPE TYP_{var}\n{decl}\nEND TYPE\nEND MODULE".format(var=name, decl=declStmt)
                    _, _, xml = fortran2xml(fortranSource)
                    ds = xml.find('.//{*}' + declStmtTag)
                    previousTail = '\n' + declStmt[:re.search('\S', declStmt).start()]

                    #node insertion
                    #locNode[0] is the T-stmt node, locNode[-1] is the end-T-stmt node
                    #locNode[-2] is the last node before the end-T-stmt node (last component, comment or the T-stmt node)
                    ds.tail = locNode[-2].tail
                    locNode[-2].tail = previousTail
                    locNode.insert(-1, ds) #insert before last one
    
                else:
                    #Add declaration statement (not type declaration case)
                    #Statement building
                    fortranSource = "SUBROUTINE SUB_{var}\n{decl}\nEND SUBROUTINE".format(var=name, decl=declStmt)
                    _, _, xml = fortran2xml(fortranSource)
                    ds = xml.find('.//{*}' + declStmtTag)
                    previousTail = '\n' + declStmt[:re.search('\S', declStmt).start()]

                    #node insertion index
                    declLst = [node for node in locNode if node.tag.endswith('}' + declStmtTag)]
                    if len(declLst) != 0:
                        #There already are declaration statements
                        #We look for the last position in the declaration list which do not use the variable we add
                        for decl in declLst:
                            index = list(locNode).index(decl)
                            if name in [n2name(N) for N in decl.findall('.//{*}N')]:
                                break
                    else:
                        #There is no declaration statement
                        stmtLst = [node for node in locNode if isExecutable(node)] #list of executable nodes
                        if len(stmtLst) == 0:
                            #There is no executable statement, we insert the declaration at the end
                            index = len(locNode) - 1 #Last node is the ending node (e.g. end-subroutine-stmt)
                        else:
                            #We insert the declaration just before the first executable statement
                            index = list(locNode).index(stmtLst[0])
    
                    #node insertion
                    if index != 0:
                        ds.tail = locNode[index - 1].tail
                        locNode[index - 1].tail = previousTail
                    locNode.insert(index, ds)

    @debugDecor
    @updateTree('signal')
    def addModuleVar(self, moduleVarList):
        """
        :param moduleVarList: list of module variable specification to insert in the xml code
                              a module variable specification is a list of three elements:
                              - scope path (path to module, subroutine, function or type declaration)
                              - module name
                              - variable name or or list of variable names
                                or None to add a USE statement without the ONLY attribute
        For example addModuleVar('sub:FOO', 'MODD_XX', 'Y') will add the following line in subroutine FOO:
        USE MODD_XX, ONLY: Y
        """
        for (scopePath, moduleName, varName) in moduleVarList:
            if varName is None:
                varName = []
            elif not isinstance(varName, list):
                varName = [varName]
            locNode = self.getScopeNode(scopePath)
    
            #USE statement already present
            useLst = [node for node in locNode if node.tag.endswith('}use-stmt')]
    
            #Check if we need to add a USE
            insertUse = True
            for us in useLst:
                us_name = n2name(us.find('.//{*}module-N//{*}N'))
                us_var = [n2name(v.find('.//{*}N')).upper() for v in us.findall('.//{*}use-N')]
                if len(varName) == 0 and len(us_var) == 0 and us_name.upper() == moduleName.upper():
                    #There aleardy is a 'USE MODULE' and we wanted to insert a 'USE MODULE' statement
                    insertUse = False
                elif len(varName) > 0 and len(us_var) > 0 and us_name.upper() == moduleName.upper():
                    #There already is a 'USE MODULE, ONLY:' and we want to insert another 'USE MODULE, ONLY:'
                    #We suppress from the varName list, all the variables already defined
                    varName = [var for var in varName if var.upper() not in us_var]
                    if len(varName) == 0:
                        #All the variables we wanted to import are already defined
                        insertUse = False
    
            if insertUse:
                #Statement building
                fortranSource = "SUBROUTINE FOO598756\nUSE {}".format(moduleName)
                if len(varName) > 0:
                    fortranSource += ', ONLY:{}'.format(', '.join(varName))
                fortranSource += "\nEND SUBROUTINE"
                _, _, xml = fortran2xml(fortranSource)
                us = xml.find('.//{*}use-stmt')
    
                #node insertion index
                if len(useLst) != 0:
                    #There already have use statements, we add the new one after them
                    index = list(locNode).index(useLst[-1]) + 1
                else:
                    #There is no use statement, we add the new node just after the first node
                    index = 1
    
                us.tail = locNode[index - 1].tail
                locNode[index - 1].tail = '\n'
                locNode.insert(index, us)
                self.tree.signal(self)  # Tree must be updated

    @debugDecor
    def showUnusedVar(self, scopePath=None):
        """
        Displays on stdout a list of unued variables
        :param scopePath: scope path (or list of) to explore (None for all)
        """
        scopes = self.getScopeNodes(scopePath, excludeKinds=['type'])
        varUsed = self.isVarUsed([(scope.path, v['n'])
                                  for scope in scopes
                                  for v in self.getVarList(scope.path)])
        for scope in scopes:
            varList = [k[1].upper() for (k, v) in varUsed.items() if (not v) and k[0] == scope.path]
            if len(varList) != 0:
                print('Some variables declared in {} are unused:'.format(scope.path))
                print('  - ' + ('\n  - '.join(varList)))

    @debugDecor
    def removeUnusedLocalVar(self, scopePath=None, excludeList=None, simplify=False):
        """
        Remove unused local variables (dummy and module variables are not suppressed)
        :param scopePath: scope path (or list of) to explore (None for all)
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
                  for scope in self.getScopeNodes(scopePath, excludeKinds=['type'])
                  for v in self.getVarList(scope.path) if v['n'].upper() not in excludeList]
        self.removeVarIfUnused(allVar, excludeDummy=True, excludeModule=True, simplify=simplify)

    @debugDecor
    def addExplicitArrayBounds(self, node=None, varList=None, scopePath=None):
        """
        Replace ':' by explicit arrays bounds.
        :param node: xml node in which ':' must be replaced (None to replace everywhere)
        :param varList: var list or None to compute it
        :param scopePath: scope path.
                          If node is not None, scopePath can be None (and the scope path
                                               will be guessed) or must correspond to the node.
                          If node is None, scopePath can be None to search everywhere or be defined
                                           to restrain search to this scope or list of scopes.
        """
        #List of variables
        if varList is None:
            varList = self.getVarList()
    
        if node is None:
            nodes = [(scope.path, scope)
                     for scope in self.getScopeNodes(scopePath, excludeContains=True)]
        else:
            nodes = [(scopePath, node) if scopePath is not None else self.getScopePath(node)]

        for (scopePath, childNode) in nodes:
            for parent4 in childNode.findall('.//{*}section-subscript/../../../..'): #named-E
                if parent4.find('./{*}R-LT/{*}component-R') is None:
                    #Shape of type members is unknown
                    for parent in parent4.findall('.//{*}section-subscript/..'):
                        for sub in parent.findall('.//{*}section-subscript'):
                            lower_used = sub.find('./{*}lower-bound')
                            upper_used = sub.find('./{*}upper-bound')
                            #A slice can be A(:), A(I:) or A(:I), but not A(I)
                            #With A(:) and A(:I), lower_used is None
                            #With A(I:) lower_used.tail contains a ':'
                            #With A(I) lower_used.tail  doesn't contain a ':'
                            if lower_used is None or \
                               (lower_used.tail is not None and ':' in lower_used.tail):
                                if lower_used is None or upper_used is None:
                                    #At least one array bound is implicit
                                    varDesc = self.findVar(n2name(parent4.find('.//{*}N')),
                                                           scopePath, varList=varList)
                                    if varDesc is not None and varDesc['t'] is not None and \
                                       not 'CHAR' in varDesc['t']: #module array or character
                                        lower_decl, upper_decl = varDesc['as'][list(parent).index(sub)]
                                        if lower_decl is None: lower_decl = '1'
                                        #When a bound is explicit, we keep it, otherwise we
                                        #take the declared bound
                                        lowerBound = lower_decl if lower_used is None \
                                                     else alltext(lower_used)
                                        upperBound = upper_decl if upper_used is None \
                                                     else alltext(upper_used)
                                        if upperBound is not None: #case of implicit shape
                                            lowerXml, upperXml = createArrayBounds(lowerBound,
                                                                                   upperBound,
                                                                                   'ARRAY')
                                            #We remove current explicit bounds or the ':',
                                            #and replace them by the new explicit declaration
                                            for n in sub:
                                                if n.tag.split('}')[1] in ('lower-bound', 'upper-bound'):
                                                    sub.remove(n)
                                                else:
                                                    raise PYFTError("Unexpected case, tag is {}".format(n.tag.split('}')[1]))
                                            sub.text = '' # Delete the initial ':'
                                            sub.extend([lowerXml, upperXml])

    @debugDecor
    def addArrayParentheses(self, varList=None, scopePath=None):
        """
        Look for arrays and add parenthesis. A => A(:)
        :param varList: var list or None to compute it
        :param scopePath: scope path (or list of scope paths) to deal with,
                          None to transform all found scopes
        """
        #List of variables
        if varList is None:
            varList = self.getVarList()
    
        #Loop on scopes
        for scope in self.getScopeNodes(scopePath, excludeContains=True):
            for node in scope.iter():
                #Arrays are used in statements
                #* We must exclude allocate-stmt, deallocate-stmt, pointer-a-stmt, T-decl-stmt and associate-stmt,
                #  nullify-stmt
                #  function-stmt, subroutine-stmt, interface-stmt must be kept untouched
                #  action-stmt is discarded as it contains another statement.
                #* Array variables to modify can be found in
                #    - simple affectation (a-stmt),
                #    - if construct conditions (if-then-stmt, else-if-stmt),
                #    - where-construct mask (where-construct-stmt, else-where-stmt),
                #    - if statement condition (if-stmt/condition-E, be carefull to not take the whole if-stmt as it
                #                            contains the action-stmt which, in turn, can contain an
                #                            allocate/deallocate/pointer assignment)
                #    - where statement mask (where-stmt/mask-E and not directly where-stmt as for if-stmt),
                #    - select-case-stmt and case-stmt,
                #    - do-stmt or forall-construct-stmt (e.g. FOR I=LBOUND(A, 0), UBOUND(A, 0))
                #    - forall-stmt/forall-triplet-spec-LT
                tag = node.tag.split('}')[1]
                nodeToTransform = None
                if tag in ('allocate-stmt', 'deallocate-stmt', 'pointer-a-stmt', 'T-decl-stmt',
                           'associate-stmt', 'function-stmt', 'subroutine-stmt', 'interface-stmt',
                           'action-stmt', 'nullify-stmt'):
                    #excluded
                    pass
                elif tag in ('if-stmt', 'where-stmt', 'forall-stmt'):
                    #We must transform only a part of the node
                    part = {'if-stmt': 'condition-E', 'where-stmt': 'mask-E',
                            'forall-stmt': 'forall-triplet-spec-LT'}[tag]
                    nodeToTransform = node.find('./{*}' + part)
                elif tag.endswith('-stmt'):
                    nodeToTransform = node
                if nodeToTransform is not None:
                    self.addArrayParenthesesInNode(nodeToTransform, varList, scope.path)

    @debugDecor
    def addArrayParenthesesInNode(self, node, varList=None, scopePath=None):
        """
        Look for arrays and add parenthesis. A => A(:)
        :param node: xml node in which ':' must be added
        :param varList: var list or None to compute it
        :param scopePath: scope path corresponding to the node (None to compute it)
        """
        #List of variables
        if varList is None:
            varList = self.getVarList()
    
        #Scope (as a string path)
        if scopePath is None:
            scopePath = self.getScopePath(node)
    
        #Loop on variables
        for namedE in node.findall('.//{*}named-E'):
            if not namedE.find('./{*}R-LT'): #no parentheses
                if not self.isNodeInProcedure(namedE, ('ALLOCATED', 'ASSOCIATED', 'PRESENT')):
                    #Pointer/allocatable used in ALLOCATED/ASSOCIATED must not be modified
                    #Array in present must not be modified
                    N = namedE.find('./{*}N')
                    var = self.findVar(n2name(N), scopePath, varList)
                    if var is not None and var['as'] is not None and len(var['as']) > 0 and \
                       not ((var['pointer'] or var['allocatable']) and self.isNodeInCall(namedE)):
                        #This is a known array variable, with no parentheses
                        #But we exclude pointer allocatable in call statement because the called subroutine
                        #can wait for a pointer/allocatable and not an array (and it is difficult to guess as it
                        #would need to find the source code of the subroutine)
                        RLT = ET.Element('{http://fxtran.net/#syntax}R-LT')
                        namedE.insert(list(namedE).index(N) + 1, RLT)
                        arrayR = ET.Element('{http://fxtran.net/#syntax}array-R')
                        arrayR.text = '('
                        RLT.append(arrayR)
                        sectionSubscriptLT = ET.Element('{http://fxtran.net/#syntax}section-subscript-LT')
                        sectionSubscriptLT.tail = ')'
                        arrayR.append(sectionSubscriptLT)
                        for _ in var['as']:
                            sectionSubscript = ET.Element('{http://fxtran.net/#syntax}section-subscript')
                            sectionSubscript.text = ':'
                            sectionSubscript.tail = ', '
                            sectionSubscriptLT.append(sectionSubscript)
                        sectionSubscript.tail = None #last one

    @debugDecor
    def modifyAutomaticArrays(self, declTemplate=None, startTemplate=None, endTemplate=None,
                              scopePath=None, varList=None):
        """
        :param declTemplate: declaration template
        :param startTemplate: template for the first executable statement
        :param: endTemplate: template for the last executable statement
        :param scopePath: scope path or list of scope paths in which to apply the transformation
                          (None to apply everywhere)
        :param varList: varList to use (None to compute it)
        :return: number of arrays modified
        Modifies all automatic arrays declaration in subroutine and functions. The declaration is replaced
        by the declaration template, the start template is inserted as first executable statement and
        the end template as last executable statement. Each template can use the following place holders:
        "{doubledotshape}", "{shape}", "{lowUpList}", "{name}" and "{type}" wich are, respectively modified
        into ":, :, :", "I, I:J, 0:I", "1, I, I, J, 0, I", "A", "REAL" if the original declaration
        statement was "A(I, I:J, 0:I)". The template
        "{type}, DIMENSION({doubledotshape}), ALLOCATABLE :: {name}#ALLOCATE({name}({shape}))#DEALLOCATE({name})"
        replaces automatic arrays by allocatables
        """
        if varList is None:
            varList = self.getVarList()

        templates = {'decl': declTemplate if declTemplate is not None else '',
                     'start': startTemplate if startTemplate is not None else '',
                     'end': endTemplate if endTemplate is not None else ''} #ordered dict

        number = 0
        scopes = self.getScopeNodes(scopePath)
        for scope in [scope for scope in scopes if scope.path.split('/')[-1].split(':')[0] in ('sub', 'func')]:
            #For all subroutine and function scopes
            #Determine the list of variables to transform
            varListToTransform = []
            for var in [var for var in varList
                        if var['scope'] == scope.path and var['as'] is not None and \
                           len(var['as']) > 0 and \
                           not (var['arg'] or var['allocatable'] or \
                                var['pointer'] or var['result'])]:
                #For all automatic arrays, which are not argument, not allocatable, not pointer and not result
                if var['init'] is not None:
                    logging.warning(("An array ({name}) has an initial value, it can't be processed " + \
                                     "by modifyAutomaticArrays.)").format(name=var['n']))
                else:
                    varListToTransform.append(var)
            #A variable can make use of the size of another variable in its declaration statement
            #We order the variables to not insert the declaration of a variable before the declaration of
            #the variables it depends on
            orderedVarListToTransform = []
            while len(varListToTransform) > 0:
                nAdded = 0
                for var in varListToTransform[:]:
                    Nlist = [x for l in var['asx'] for x in l if x is not None] #flatten var['asx'] excluding None
                    Nlist = [n2name(N).upper() for asx in Nlist for N in asx.findall('.//{*}N/{*}n/..')]
                    if len(set(Nlist).intersection([v['n'].upper() for v in varListToTransform])) == 0:
                        #Variable var does not use variables still in varListToTransform
                        varListToTransform.remove(var)
                        orderedVarListToTransform.append(var)
                        nAdded += 1
                if nAdded == 0:
                    raise PYFTError('It seems that there is a circular reference in the declaration statements')
            #Loop on variable to transform
            for var in orderedVarListToTransform[::-1]: #reverse order
                number += 1
                #Apply the template
                templ = copy.deepcopy(templates)
                for t in templ:
                    if '{doubledotshape}' in templ[t]:
                        templ[t] = templ[t].replace('{doubledotshape}', ','.join([':'] * len(var['as'])))
                    if '{shape}' in templ[t]:
                        result = []
                        for i in range(len(var['as'])):
                            if var['as'][i][0] is None:
                                result.append(var['as'][i][1])
                            else:
                                result.append(var['as'][i][0] + ':' + var['as'][i][1])
                        templ[t] = templ[t].replace('{shape}', ', '.join(result))
                    if '{name}' in templ[t]:
                        templ[t] = templ[t].replace('{name}', var['n'])
                    if '{type}' in templ[t]:
                        templ[t] = templ[t].replace('{type}', var['t'])
                    if '{lowUpList}' in templ[t]:
                        result = []
                        for i in range(len(var['as'])):
                            if var['as'][i][0] is None:
                                result.extend([1, var['as'][i][1]])
                            else:
                                result.extend([var['as'][i][0], var['as'][i][1]])
                        templ[t] = templ[t].replace('{lowUpList}', ', '.join([str(r) for r in result]))

                #Get the xml for the template
                separator = "!ABCDEFGHIJKLMNOPQRSTUVWabcdefghijklmnopqrstuvwxyz0123456789"
                part = 0
                for node in createExpr(templ['decl'] + '\n' + separator + '\n' + \
                                       templ['start'] + '\n' + separator + '\n' + \
                                       templ['end']):
                    t = list(templ.keys())[part]
                    if not isinstance(templ[t], list):
                        templ[t] = []
                    if node.tag.split('}')[1] == 'C' and node.text == separator:
                        part += 1
                    else:
                        templ[t].append(node)

                #Replace declaration statement
                #We look for the last position in the declaration list which do not use the variable we add
                #This algorithm will stop when the original declaration statement is encoutered
                for decl in scope.findall('./{*}T-decl-stmt'):
                    index = list(scope).index(decl)
                    if var['n'] in [n2name(N) for N in decl.findall('.//{*}N')]:
                        break
                self.removeVar([(scope.path, var['n'])], simplify=False)
                for n in templ['decl'][::-1]:
                    scope.insert(index, n)

                #Insert statements
                for n in templ['start'][::-1]:
                    self.insertStatement(scope, n, True)
                for n in templ['end'][::-1]:
                    self.insertStatement(scope, n, False)
        return number

    @staticmethod
    @debugDecor
    def varSpec2stmt(varSpec):
        """
        :param varSpec: a variable description, same form as the items return by getVarList
        :return: the associated declarative statement
        """
        if varSpec['use'] is not False:
            stmt = 'USE {module}, ONLY: {var}'.format(module=varSpec['use'], var=varSpec['n'])
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
                if (varSpec['allocatable'] and not all([d == ':' for d in dl])) or \
                   (any([d == ':' for d in dl]) and not varSpec['allocatable']):
                    raise PYFTError('Missing dim are mandatory and allowed only for allocatable arrays')
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
    def findIndexArrayBounds(self, arr, index, varList, scopePath, loopVar):
        """
        Find bounds and loop variable for a given array index
        :param arr: array node (named-E node with a array-R child)
        :param index: index of the rank of the array
        :param varList: list of currently declared variables obtained by getVarList
        :param scopePath: scope path where the array is used
        :param loopVar: None to create new variable for each added DO loop
                        or a function that return the name of the variable to use for the loop control.
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
        #We are only interested by the subscript containing ':'
        #we must not iterate over the others, eg: X(:,1)
        if ':' in alltext(ss):
            #Look for lower and upper bounds for iteration and declaration
            lower_used = ss.find('./{*}lower-bound')
            upper_used = ss.find('./{*}upper-bound')
            varDesc = self.findVar(name, scopePath, varList, array=True)
            if varDesc is not None:
                lower_decl, upper_decl = varDesc['as'][index]
                if lower_decl is None: lower_decl = '1' #default lower index for FORTRAN arrays
            else:
                lower_decl, upper_decl = None, None

            #name of the loop variable
            if loopVar is None:
                #loopVar is not defined, we create a new variable for the loop
                #only if lower and upper bounds have been found (easy way to discard character strings)
                varName = lower_decl is not None and upper_decl is not None
            else:
                varName = loopVar(lower_decl, upper_decl,
                                  None if lower_used is None else alltext(lower_used),
                                  None if upper_used is None else alltext(upper_used), name, index)
            return (varName,
                    lower_decl if lower_used is None else alltext(lower_used),
                    upper_decl if upper_used is None else alltext(upper_used))

    @debugDecor
    def arrayR2parensR(self, namedE, table, varList, scopePath):
        """
        Transform a array-R into a parens-R node by replacing slices by variables
        In 'A(:)', the ':' is in a array-R node whereas in 'A(JL)', 'JL' is in a parens-R node.
        Both the array-R and the parens-R nodes are inside a R-LT node
        :param namedE: a named-E node
        :param table: dictionnary returned by the decode function
        :param varList: description of declared variables
        :param scopePath: scope path in which nameE is
        """
        #Before A(:): <f:named-E>
        #               <f:N><f:n>A</f:n></f:N>
        #               <f:R-LT>
        #                 <f:array-R>(
        #                   <f:section-subscript-LT>
        #                     <f:section-subscript>:</f:section-subscript>
        #                   </f:section-subscript-LT>)
        #                 </f:array-R>
        #               </f:R-LT>
        #              </f:named-E>
        #After  A(I): <f:named-E>
        #               <f:N><f:n>A</f:n></f:N>
        #               <f:R-LT>
        #                 <f:parens-R>(
        #                   <f:element-LT>
        #                     <f:element><f:named-E><f:N><f:n>I</f:n></f:N></f:named-E></f:element>
        #                   </f:element-LT>)
        #                 </f:parens-R>
        #               </f:R-LT>
        #             </f:named-E>

        RLT = namedE.find('./{*}R-LT')
        arrayR = RLT.find('./{*}array-R') #Not always in first position, eg: ICED%XRTMIN(:)
        if arrayR is not None:
            index = list(RLT).index(arrayR)
            parensR = ET.Element('{http://fxtran.net/#syntax}parens-R')
            parensR.text = '('
            parensR.tail = ')'
            elementLT = ET.Element('{http://fxtran.net/#syntax}element-LT')
            parensR.append(elementLT)
            ivar = -1
            for ss in RLT[index].findall('./{*}section-subscript-LT/{*}section-subscript'):
                element = ET.Element('{http://fxtran.net/#syntax}element')
                element.tail = ', '
                elementLT.append(element)
                if ':' in alltext(ss):
                    ivar += 1
                    v = list(table.keys())[ivar] #variable name
                    lower = ss.find('./{*}lower-bound')
                    upper = ss.find('./{*}upper-bound')
                    if lower is not None: lower = alltext(lower)
                    if upper is not None: upper = alltext(upper)
                    if lower is not None and ss.text is not None and ':' in ss.text:
                        #fxtran bug workaround
                        upper = lower
                        lower = None
                    if lower is None and upper is None:
                        #E.g. 'A(:)'
                        #In this case we use the DO loop bounds without checking validity with
                        #respect to the array declared bounds
                        element.append(createExprPart(v))
                    else:
                        #E.g.:
                        #!$mnh_expand_array(JI=2:15)
                        #A(2:15) or A(:15) or A(2:)
                        if lower is None:
                            #lower bound not defined, getting lower declared bound for this array
                            lower = self.findVar(n2name(namedE.find('{*}N')),
                                                 scopePath, varList, array=True)['as'][ivar][0]
                            if lower is None: lower = '1' #default fortran lower bound
                        elif upper is None:
                            #upper bound not defined, getting lower declared bound for this array
                            upper = self.findVar(n2name(namedE.find('{*}N')),
                                                 scopePath, varList, array=True)['as'][ivar][1]
                        #If the DO loop starts from JI=I1 and goes to JI=I2; and array bounds are J1:J2
                        #We compute J1-I1+JI and J2-I2+JI and they should be the same
                        #E.g: array bounds could be 'I1:I2' (becoming JI:JI) or 'I1+1:I2+1" (becoming JI+1:JI+1)
                        newlower = simplifyExpr(lower, add=v, sub=table[v][0])
                        newupper = simplifyExpr(upper, add=v, sub=table[v][1])
                        if newlower != newupper:
                            raise PYFTError(("Don't know how to do with an array declared with '{la}:{ua}' " + \
                                             "and a loop from '{ll}' to '{ul}'").format(la=lower, ua=upper,
                                                                                        ll=table[v][0],
                                                                                        ul=table[v][1]))
                        element.append(createExprPart(newlower))
                else:
                    element.append(ss.find('./{*}lower-bound'))
            element.tail = None #last element
            RLT.remove(RLT[index])
            RLT.insert(index, parensR)

    @debugDecor
    def findArrayBounds(self, arr, varList, scopePath, loopVar):
        """
        Find bounds and loop variable given an array
        :param arr: array node (named-E node with a array-R child)
        :param varList: list of currently declared variables obtained by getVarList
        :param scopePath: scope path where the array is used
        :param loopVar: None to create new variable for each added DO loop
                        or a function that return the name of the variable to use for the loop control.
                        This function returns a string (name of the variable), or True to create
                        a new variable, or False to not transform this statement
                        The functions takes as arguments:
                          - lower and upper bounds as defined in the declaration statement
                          - lower and upper bounds as given in the statement
                          - name of the array
                          - index of the rank
        :return: the tuple (table, newVar) where:
                    table is a dictionnary: keys are loop variable names
                                            values are tuples with lower and upper bounds
                    newVar is a list of loop variables not found in varList. This list has the same
                           format as the varList list.

        In case the loop variable cannot be defined, the function returns (None, [])
        """
        table = {} #ordered since python 3.7
        name = n2name(arr.find('./{*}N'))
        varNew = []

        #Iteration on the different subscript
        for iss, ss in enumerate(arr.findall('./{*}R-LT/{*}array-R/{*}section-subscript-LT/{*}section-subscript')):
            #We are only interested by the subscript containing ':'
            #we must not iterate over the others, eg: X(:,1)
            if ':' in alltext(ss):
                #Look for loop variable name and lower/upper bounds for iteration
                varName, lower, upper = self.findIndexArrayBounds(arr, iss, varList, scopePath, loopVar)
                #varName can be a string (name to use), True (to create a variable), False (to discard the array
                if varName is not False and varName in table.keys():
                    raise PYFTError(("The variable {var} must be used for the rank #{i1} whereas it " + \
                                     "is already used for rank #{i2} (for array {name}).").format(
                                       var=varName, i1=str(iss), i2=str(list(table.keys()).index(varName)),
                                       name=name))
                if varName is True:
                    #A new variable must be created
                    j = 1
                    #We look for a variable name that don't already exist
                    #We can reuse a newly created varaible only if it is not used for the previous indexes
                    #of the same statement
                    while any([v['n'] for v in (varList + varNew)
                               if ((v['n'].upper() == 'J' + str(j) and not v.get('new', False)) or
                                   'J' + str(j) in table.keys())]):
                        j += 1
                    varName = 'J' + str(j)
                    varDesc = {'as': [], 'asx': [], 'n': varName, 'i': None,
                               't': 'INTEGER', 'arg': False, 'use': False, 'opt': False,
                               'scope': scopePath}
                    if varDesc not in varNew:
                        varNew.append(varDesc)

                elif varName is not False and self.findVar(varName, scopePath,
                                                           varList, array=False, exactScope=True) is None:
                    #We must declare the variable
                    varDesc = {'as': [], 'asx': [], 'n': varName, 'i': None,
                               't': 'INTEGER', 'arg': False, 'use': False, 'opt': False,
                               'scope': scopePath}
                    varNew.append(varDesc)

                #fill table
                table[varName] = (lower, upper)

        return (None, []) if False in table.keys() else (table, varNew)

    @debugDecor
    def renameVar(self, oldName, newName):
        """
        :param oldName: old name of the variable
        :param newName: new name of the variable
        """
        for node in self.findall('.//{*}N'):
            if n2name(node).upper() == oldName.upper():
                #Remove all n tag but one
                for n in node.findall('./{*}n')[1:]:
                    node.remove(n)
                #Fill the first n with the new name
                node.find('./{*}n').text = newName

    @debugDecor
    def removeVarIfUnused(self, varList, excludeDummy=False, excludeModule=False, simplify=False):
        """
        :param varList: list of variables to remove if unused. Each item is a list or tuple of two elements.
                        The first one describes where the variable is declared, the second one is the name
                        of the variable. The first element is a '/'-separated path with each element
                        having the form 'module:<name of the module>', 'sub:<name of the subroutine>' or
                        'func:<name of the function>'
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
    def isVarUsed(self, varList, strictScope=False, dummyAreAlwaysUsed=False):
        """
        :param varList: list of variables to remove if unused. Each item is a list or tuple of two elements.
                        The first one describes where the variable is declared, the second one is the name
                        of the variable. The first element is a '/'-separated path with each element
                        having the form 'module:<name of the module>', 'sub:<name of the subroutine>' or
                        'func:<name of the function>'
        :param strictScope: True to search strictly in scope
        :param dummyAreAlwaysUsed: Returns True if variable is a dummy argument
        :return: a dict whose keys are the elements of varList, and values are True when the variable is
                 used, False otherwise
    
        If strictScope is True, the function will search for variable usage
        only in this scope. But this feature has a limited interest.
    
        If strictScope is False:
          - if scopePath is a subroutine/function in a contains section,
            and if the variable is not declared in this scope, usages are
            searched in the module/subroutine/function upper that declared
            the variable and in all subroutines/functions in the contains section
          - if scopePath is a module/subroutine/function that has a
            contains sections, usages are searched in all subroutines/functions
            in the contains section

        To know if a variable can be removed, you must use strictScope=False
        """
        varList = self._normalizeUniqVar(varList)

        #Computes in which scopes variable must be searched
        if strictScope:
            locsVar = [([scopePath], varName) for scopePath, varName in varList]
        else:
            #Function to determine if var is declared in this scope, with cache
            allVar = {}
            allScopes = {scope.path: scope for scope in self.getScopes(excludeContains=True)}
            def _varInLoc(var, scopePath):
                #Is the variable declared in this scope
                if not scopePath in allVar:
                    allVar[scopePath] = allScopes[scopePath].getVarList()
                return var.upper() in [v['n'].upper() for v in allVar[scopePath]]

            locsVar = {}
            for scopePath, varName in varList:
                path = scopePath

                #Should we search in upper levels
                while('/' in path and not _varInLoc(varName, path)):
                    #Declared upper, we must start the search one level upper
                    path = '/'.join(path.split('/')[:-1])

                #We start search from here but we must include all routines in contains
                #that do not declare again the same variable name
                testScopes = [path] #we must search in the current scope
                for l in allScopes:
                    if l.startswith(path + '/') and \
                       l.split('/')[-1].split(':')[0] != 'type':
                        #l is a scope path contained inside path and is not a type declaration
                        if not _varInLoc(varName, l): #there is not another variable with same name declared inside
                            testScopes.append(l) #if variable is used here, it is used
                locsVar[(scopePath, varName)] = testScopes

        #For each scope to search, list all the variables used
        usedVar = {}
        for scopePath in list(set([item for sublist in locsVar.values() for item in sublist])):
            usedVar[scopePath] = []
            #Loop on all child in the scope
            for node in allScopes[scopePath]:
                #we don't want use statement, it could be where the variable is declared, not a usage place
                if not node.tag.endswith('}use-stmt'):
                    if node.tag.endswith('}T-decl-stmt'):
                        #We don't want the part with the list of declared variables, we only want
                        #to capture variables used in the kind selector or in the shape specification
                        Nnodes = node.findall('.//{*}_T-spec_//{*}N') + node.findall('.//{*}shape-spec//{*}N')
                    else:
                        Nnodes = node.findall('.//{*}N')

                    #We look for the variable name in these 'N' nodes.
                    for N in Nnodes:
                        if dummyAreAlwaysUsed:
                            #No need to check if the variable is a dummy argument; because if it is one
                            #it will be found in the argument list of the subroutine/function and will
                            #be considered as used
                            usedVar[scopePath].append(n2name(N).upper())
                        else:
                            parPar = self.getParent(N, 2) #parent of parent
                            #We exclude dummy argument list to really check if the variable is used
                            #and do not only appear as an argument of the subroutine/function
                            if parPar is None or not parPar.tag.endswith('}dummy-arg-LT'):
                                usedVar[scopePath].append(n2name(N).upper())

        result = {}
        for scopePath, varName in varList:
            assert scopePath.split('/')[-1].split(':')[0] != 'type', 'We cannot check type component usage'
            result[(scopePath, varName)] = any(varName.upper() in usedVar[scopePath]
                                               for scopePath in locsVar[(scopePath, varName)])
    
        return result

    @debugDecor
    def addArgInTree(self, scopePath, varName, declStmt, pos, stopScopes, moduleVarList=None,
                     otherNames=None,
                     parser=None, parserOptions=None, wrapH=False):
        """
        Adds an argument to the routine and propagates it upward until we encounter a scope
        where the variable exists or a scope in stopScopes
        :param scopePath: scope to start with (if None, try to guess it from the scopes defined in
                                               the object)
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
        :param parser, parserOptions, wrapH: see the pyft class

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
                container = ET.Element('{http://fxtran.net/#syntax}element')
            else:
                argList = callFuncStmt.find('./{*}arg-spec')
                container = ET.Element('{http://fxtran.net/#syntax}arg')
                if argList is None:
                    #Call without argument
                    callFuncStmt.find('./{*}procedure-designator').tail = '('
                    argList = ET.Element('{http://fxtran.net/#syntax}arg-spec')
                    argList.tail = ')'
                    callFuncStmt.append(argList)
            item = createExprPart(varNameToUse)
            previous = pos - 1 if pos >= 0 else len(argList) + pos #convert negative pos using length
            while previous >= 0 and argList[previous].tag.split('}')[1] in ('C', 'cnt'):
                previous -= 1
            following = pos if pos > 0 else len(argList) + pos + 1 #convert negative pos using length
            while following <= len(argList) - 1 and argList[following].tag.split('}')[1] in ('C', 'cnt'):
                following += 1
            if (previous >= 0 and argList[previous].find('./{*}arg-N/{*}k') is not None) or \
               (following <= len(argList) - 1 and argList[following].find('./{*}arg-N/{*}k') is not None) or \
               following == len(argList):
                #We use the key=val syntax whereever it is possible because it's safer in case of optional arguments
                #If previous arg, or following arg is already with a key=val syntax, we can (must) use it
                #If the inserted argument is the last one of the list, it also can use this syntax
                k = ET.Element('{http://fxtran.net/#syntax}k')
                k.text = varName
                argN = ET.Element('{http://fxtran.net/#syntax}arg-N')
                argN.append(k)
                argN.tail = '='
                argN.set('n', varName)
                container.append(argN)
            container.append(item)
            self.insertInList(pos, container, argList)

        if scopePath is None:
            allScopes = [scope.path for scope in self.getScopes()
                         if scope.path.split('/')[-1].split(':')[0] in ('sub', 'func')]
            if len(allScopes) == 1:
                scopePath = allScopes[0]
            else:
                raise PYFTError('Unable to guess the scope path to deal with')

        if scopePath in stopScopes or self.tree.isUnderStopScopes(scopePath, stopScopes):
            #We are on the path to a scope in the stopScopes list, or scopeUp is one of the stopScopes
            varList = self.getVarList()
            var = self.findVar(varName, scopePath, varList, exactScope=True)
            if otherNames is not None:
                vOther = [self.findVar(v, scopePath, varList, exactScope=True) for v in otherNames]
                vOther = [v for v in vOther if v is not None]
                if len(vOther) > 0:
                    var = vOther[-1]

            if var is None:
               #The variable doesn't exist in this scope, we add it
               self.addVar([[scopePath, varName, declStmt, pos]])
               if moduleVarList is not None:
                   #Module variables must be added when var is added
                   self.addModuleVar([(scopePath, moduleName, moduleVarNames)
                                      for (moduleName, moduleVarNames) in moduleVarList])
               #We look for interface declaration if subroutine is directly accessible
               if len(scopePath.split('/')) == 1:
                   filename, scopeInterface = self.tree.findScopeInterface(scopePath)
                   if filename is not None:
                       if self.getFileName() == filename:
                           #interface declared in same file
                           xml = self
                           pft = None
                       else:
                           pft = pyft.pyft.conservativePYFT(filename, parser, parserOptions,
                                                            wrapH, tree=self.tree)
                           xml = pft
                       varInterface = xml.findVar(varName, scopeInterface, exactScope=True)
                       if varInterface is None:
                           xml.addVar([[scopeInterface, varName, declStmt, pos]])
                           if moduleVarList is not None:
                               #Module variables must be added when var is added
                               xml.addModuleVar([(scopeInterface, moduleName, moduleVarNames)
                                                  for (moduleName, moduleVarNames) in moduleVarList])
                       if pft is not None: pft.write()

            if var is None and scopePath not in stopScopes:
                #We must propagates upward
                for scopeUp in self.tree.calledByScope(scopePath): #scopes calling the current scope
                    if scopeUp in stopScopes or self.tree.isUnderStopScopes(scopeUp, stopScopes):
                        #We are on the path to a scope in the stopScopes list, or scopeUp is one of the stopScopes
                        for filename in self.tree.scopeToFiles(scopeUp): #can be defined several times?
                            if self.getFileName() == filename:
                                #Upper scope is in the same file
                                xml = self
                                pft = None
                            else:
                                pft = pyft.pyft.conservativePYFT(filename, parser, parserOptions,
                                                                 wrapH, tree=self.tree)
                                xml = pft
                            #Add the argument and propagate upward
                            xml.addArgInTree(scopeUp, varName, declStmt, pos,
                                             stopScopes, moduleVarList, otherNames,
                                             parser=parser, parserOptions=parserOptions, wrapH=wrapH)
                            #Add the argument to calls (subroutine or function)
                            scopeUpNode = ET.Element('virtual')
                            scopeUpNode.extend(xml.getScopeNode(scopeUp, excludeContains=True))
                            name = scopePath.split('/')[-1].split(':')[1].upper()
                            isCalled = False
                            varNameToUse = varName
                            if otherNames is not None:
                                varListUp = xml.getVarList()
                                vOther = [xml.findVar(v, scopeUp, varListUp, exactScope=True)
                                          for v in otherNames]
                                vOther = [v for v in vOther if v is not None]
                                if len(vOther) > 0:
                                    varNameToUse = vOther[-1]['n']
                            if scopePath.split('/')[-1].split(':')[0] == 'sub':
                                #We look for call statements
                                for callStmt in scopeUpNode.findall('.//{*}call-stmt'):
                                    callName = n2name(callStmt.find('./{*}procedure-designator/{*}named-E/{*}N')).upper()
                                    if callName == name:
                                        insertInArgList(varName, varNameToUse, pos, callStmt)
                                        isCalled = True
                            else:
                                #We look for function use
                                for funcCall in scopeUpNode.findall('.//{*}named-E/{*}R-LT/{*}parens-R/{*}element-LT/../../..'):
                                    funcName = n2name(funcCall.find('./{*}N')).upper()
                                    if funcName == name:
                                        insertInArgList(varName, varNameToUse, pos, funcCall)
                                        isCalled = True
                            if pft is not None: pft.write()

                            if isCalled:
                                #We must check in the scope (or upper scopes) if an interface block declares the routine
                                for interface in self.findall('.//{*}interface-construct/{*}' + \
                                                              'program-unit/{*}subroutine-stmt/' + \
                                                              '{*}subroutine-N/{*}N/../../../'):
                                    if n2name(interface.find('./{*}subroutine-stmt/{*}subroutine-N/' + \
                                                             '{*}N')).upper() == name:
                                        #We must add the argument to the interface
                                        raise PYFTError('This case is not yet implemented')
