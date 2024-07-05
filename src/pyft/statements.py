"""
This module includes functions to act on statements
"""

import re
import logging
import copy
from pyft.util import n2name, non_code, debugDecor, alltext, PYFTError, tag
from pyft.expressions import createExprPart, createArrayBounds, createElem
from pyft.tree import updateTree
from pyft import NAMESPACE

def _nodesInIf(ifNode):
    """
    Internal method to return nodes in if structure
    """
    nodes = []
    for block in ifNode.findall('./{*}if-block'):
        for item in [i for i in block
                     if tag(i) not in ('if-then-stmt', 'else-if-stmt',
                                       'else-stmt', 'end-if-stmt')]:
            if not non_code(item): nodes.append(item)
    return nodes

def _nodesInWhere(whereNode):
    """
    Internal method to return nodes in where structure
    """
    nodes = []
    for block in whereNode.findall('./{*}where-block'):
        for item in [i for i in block
                     if tag(i) not in ('where-construct-stmt', 'else-where-stmt',
                                       'end-where-stmt')]:
            if not non_code(item): nodes.append(item)
    return nodes

def _nodesInDo(doNode):
    """
    Internal method to return nodes in do structure
    """
    nodes = []
    for item in [i for i in doNode if tag(i) not in ('do-stmt', 'end-do-stmt')]:
        if not non_code(item): nodes.append(item)
    return nodes

def _nodesInCase(caseNode):
    """
    Internal method to return nodes in do structure
    """
    nodes = []
    for block in caseNode.findall('./{*}selectcase-block'):
        for item in [i for i in block
                     if tag(i) not in ('select-case-stmt', 'case-stmt',
                                       'end-select-case-stmt')]:
            if not non_code(item): nodes.append(item)
    return nodes

class Statements():
    #No @debugDecor for this low-level method
    def isNodeInProcedure(self, node, procList):
        """
        Return True if node (named-E) is an argument of a procedure listed in procList
        :param node: node to test
        :param procList: list of procedure names
        """
        #E.g. The xml for "ASSOCIATED(A)" is
        #<f:named-E>
        #  <f:N><f:n>ASSOCIATED</f:n></f:N>
        #  <f:R-LT><f:parens-R>(
        #    <f:element-LT><f:element><f:named-E><f:N><f:n>A</f:n></f:N></f:named-E></f:element></f:element-LT>)
        #  </f:parens-R></f:R-LT>
        #</f:named-E>
        inside = False
        par = self.getParent(node)
        if tag(par) == 'element':
            par = self.getParent(par)
            if tag(par) == 'element-LT':
                par = self.getParent(par)
                if tag(par) == 'parens-R':
                    par = self.getParent(par)
                    if tag(par) == 'R-LT':
                        previous = self.getSiblings(par, before=True, after=False)
                        if len(previous) > 0 and tag(previous[-1]) == 'N' and \
                           n2name(previous[-1]).upper() in [p.upper() for p in procList]:
                            inside = True
        return inside

    #No @debugDecor for this low-level method
    def isNodeInCall(self, node):
        """
        Return True if node (named-E) is an argument of a called procedure
        :param node: node to test
        """
        #E.g. The xml for "CALL FOO(A)" is
        #<f:call-stmt>CALL
        #  <f:procedure-designator><f:named-E><f:N><f:n>FOO</f:n></f:N></f:named-E></f:procedure-designator>(
        #  <f:arg-spec><f:arg><f:named-E><f:N><f:n>A</f:n></f:N></f:named-E></f:arg></f:arg-spec>)
        #</f:call-stmt>
        inside = False
        par = self.getParent(node)
        if tag(par) == 'arg':
            par = self.getParent(par)
            if tag(par) == 'arg-spec':
                par = self.getParent(par)
                if tag(par) == 'call-stmt':
                    inside = True
        return inside

    @debugDecor
    def removeCall(self, callName, scopePath, simplify=False):
        """
        :param callName: name of the subprogram calls to remove.
        :param scopePath: scope path, or list of, to explore (None for all). A path is a
                          '/'-separated string with each element having the form
                          'module:<name of the module>', 'sub:<name of the subroutine>' or
                          'func:<name of the function>'
        :param simplify: try to simplify code (if we delete "CALL FOO(X)" and if X not used else where,
                         we also delete it; or if the call was alone inside a if-then-endif construct,
                         the construct is also removed, and variables used in the if condition are also
                         checked...)
        :return: number of calls suppressed
        """
        callName = callName.upper()
        callNodes = []
        for scope in self.getScopeNodes(scopePath, excludeContains=True):
            callNodes += scope.findall('.//{*}call-stmt')
        callNodes = [cn for cn in callNodes
                     if n2name(cn.find('.//{*}named-E/{*}N')).upper() == callName] #filter by name
        self.removeStmtNode(callNodes, simplify, simplify)
        return len(callNodes)

    @debugDecor
    def removePrints(self, scopePath, simplify=False):
        """
        Removes all print statements
        :param scopePath: scope path, or list of, to explore (None for all). A path is a
                          '/'-separated string with each element having the form
                          'module:<name of the module>', 'sub:<name of the subroutine>' or
                          'func:<name of the function>'
        :param simplify: try to simplify code (if we delete "print*, X" and if X is not used else where,
                         we also delete it; or if the print was alone inside a if-then-endif construct,
                         the construct is also removed, and variables used in the if condition are also
                         checked...)
        """
        printNodes = []
        for scope in self.getScopeNodes(scopePath, excludeContains=True):
            printNodes += scope.findall('.//{*}print-stmt')
        self.removeStmtNode(printNodes, simplify, simplify)

    @debugDecor
    def removeArraySyntax(self, concurrent=False, useMnhExpand=True, everywhere=True,
                          loopVar=None, reuseLoop=True, funcList=None,
                          updateMemSet=False, updateCopy=False):
        """
        Transform array syntax into DO loops
        :param concurrent: use 'DO CONCURRENT' instead of simple 'DO' loops
        :param useMnhExpand: use the mnh directives to transform the entire bloc in a single loop
        :param everywhere: transform all array syntax in DO loops
        :param loopVar: None to create new variable for each added DO loop
                        or a function that return the name of the variable to use for the loop control.
                        This function returns a string (name of the variable), or True to create
                        a new variable, or False to not transform this statement
                        The functions takes as arguments:
                          - lower and upper bounds as defined in the declaration statement
                          - lower and upper bounds as given in the statement
                          - name of the array
                          - index of the rank
        :param reuseLoop: if True, try to reuse loop created whith everywhere=True
        :param funcList: list of entity names that must be recognized as array functions
                         (in addition to the intrisic ones) to discard from transformation
                         statements that make use of them. None is equivalent to an empty list.
        :param updateMemSet: True to put affectation to constante in DO loops
        :param updateCopy: True to put array copy in DO loops

        Notes: * With useMnhExpand, the function checks if the coding is conform to what is needed
                 for the filepp/mnh_expand tool (to not breack compatibility with this tool)
               * Arrays are transformed only if ':' are used.
                 A=A(:) is not transformed at all (or raises an exception if found in a WHERE block)
                 A(:)=A is wrongly transformed into "DO...; A(J1)=A; ENDDO" and will produce a compilation error
                 WHERE(L) X(:)=0. is not transformed at all (unknown behaviour in case of nested WHERE)
               * This function is not compatible with functions that return arrays:
                 X(1:5)=FUNC(1) will be transformed into "DO J1=1,5; X(J1)=FUNC(1); ENDDO"
                 x(1:5)=FUNC(X(1:5)) will be transformed into "DO J1=1,5; X(J1)=FUNC(X(J1)); ENDDO"
                 But intrinsic functions (COUNT, ANY...) are recognised and corresponding statements
                 are not transformed.
                 The list of intrinsic array functions can be extended by user functions with the funcList
                 argument.
        """

        #Developer notes:
        #We use recursivity to avoid the use of the 'getParent' function.
        #We start from the top node and call 'recur'.
        #
        #The 'recur' function loops over the different nodes and:
        # - search for mnh directives (if 'useMnhExpand' is True):
        #     - when it is an opening directive:
        #         - decode the directive to identify bounds and variables to use ('decode' function)
        #         - introduce the DO loops (with 'createDoConstruct')
        #         - activate the 'in_mnh' flag
        #     - when it is a closing directive:
        #         - deactivate the 'in_mnh' flag
        # - while the 'in_mnh' flag is activated:
        #     - update ('updateStmt' function, that uses 'arrayR2parensR') and put all statements in the DO loops
        # - in case (if 'everywhere' is True) statement is expressed using array-syntax:
        #     - find the bounds and guess a set of variables to use ('findArrayBounds' function)
        #     - introduce the DO loops (with 'createDoConstruct') if we cannot reuse the previous one
        #     - update ('updateStmt' function, that uses 'arrayR2parensR') and put all statements in the DO loops
        # - in case the statement contains other statements (SUBROUTINE, DO loop...), call 'recur' on it
        #
        #Because we iterate on the statements, the tree structure cannot be modified during the iteration.
        #All the modifications to apply are, instead, stored in objetcs ('toinsert', 'toremove' and 'varList') and
        #are applied afterwards.
        #
        #In addition, a number of instructions are needed to preserve and/or modify the indentation and can
        #somewhat obfuscate the source code.

        def decode(directive):
            """
            Decode mnh_expand directive
            :param directive: mnh directive text
            :return: (table, kind) where
                     table is a dictionnary: keys are variable names, values are tuples with first and last index
                     kind is 'array' or 'where'
            """
            #E.g. !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
            #We expect that the indexes are declared in the same order as the one they appear in arrays
            #For the example given, arrays are addressed with (JIJ, JK)
            #For this example, return would be ('array', {'JIJ':('IIJB', 'IIJE'), 'JK':('1', 'IKT')})
            table = directive.split('(')[1].split(')')[0].split(',')
            table = {c.split('=')[0]:c.split('=')[1].split(':') for c in table} #ordered since python 3.7
            if directive.lstrip(' ').startswith('!$mnh_expand'):
                kind = directive[13:].lstrip(' ').split('(')[0].strip()
            else:
                kind = directive[17:].lstrip(' ').split('(')[0].strip()
            return table, kind

        def updateStmt(e, table, kind, extraindent, parent, varList, scopePath):
            """
            Updates the statement given the table dictionnary '(:, :)' is replaced by '(JI, JK)' if
            table.keys() is ['JI', 'JK']
            :param e: statement to update
            :param table: dictionnary retruned by the decode function
            :param kind: kind of mnh directives: 'array' or 'where'
                                                 or None if transformation is not governed by mnh directive
            :param varList: description of declared variables
            :param scopePath: current scope path
            """

            def add_extra(node, extra):
                """Helper function to add indentation spaces"""
                if extra != 0 and (not node.tail is None) and '\n' in node.tail:
                    #We add indentation after new line only
                    #- if tail already contains a '\n' to discard
                    #  a-stmt followed by a comment
                    #  or '&' immediatly followed by something at the beginning of a line
                    #- if not folowed by another new line (with optional space in between)
                    node.tail = re.sub(r"(\n[ ]*)(\Z|[^\n ]+)", r"\1" + extra * ' ' + r"\2", node.tail)

            add_extra(e, extraindent) #Set indentation for the *next* node
            if tag(e) == 'C':
                pass
            elif tag(e) == 'cpp':
                i = list(parent).index(e)
                if i == 0:
                    #In this case, it would be a solution to add an empty comment before the
                    #node e to easilty control the indentation contained in the tail
                    raise PYFTError("How is it possible?")
                parent[i - 1].tail = parent[i - 1].tail.rstrip(' ') #Remove the indentation
            elif tag(e) == 'a-stmt':
                sss = e.findall('./{*}E-1/{*}named-E/{*}R-LT/{*}array-R/' + \
                                '{*}section-subscript-LT/{*}section-subscript')
                if len([ss for ss in sss if ':' in alltext(ss)]) != len(table):
                    raise PYFTError("Inside code sections to transform in DO loops, all affectations must use ':'.\n" + \
                                    "This is not the case in:\n{stmt}".format(stmt=alltext(e)))
                if e.find('./{*}E-1/{*}named-E/{*}N').tail is not None and kind is not None:
                    raise PYFTError("To keep the compatibility with the filepp version of loop " + \
                                    "expansion, nothing must appear between array names and opening " + \
                                    "parethesis inside mnh directive sections.")
                #We loop on named-E nodes (and not directly on array-R nodes to prevent using the costly getParent)
                for namedE in e.findall('.//{*}R-LT/..'):
                    self.arrayR2parensR(namedE, table, varList, scopePath) #Replace slices by variable
                for cnt in e.findall('.//{*}cnt'):
                    add_extra(cnt, extraindent) #Add indentation spaces after continuation characters
            elif tag(e) == 'if-stmt':
                logging.warning("An if statement is inside a code " + \
                                "section transformed in DO loop in {f}".format(f=self.getFileName()))
                #Update the statement contained in the action node
                updateStmt(e.find('./{*}action-stmt')[0], table, kind, 0, e, varList, scopePath)
            elif tag(e) == 'if-construct':
                logging.warning("An if construct is inside a code " + \
                                "section transformed in DO loop in {f}".format(f=self.getFileName()))
                for ifBlock in e.findall('./{*}if-block'): #Loop over the blocks (if, elseif, else)
                    for child in ifBlock: #Loop over each statement inside the block
                        if tag(child) not in ('if-then-stmt', 'else-if-stmt',
                                              'else-stmt', 'end-if-stmt'):
                            updateStmt(child, table, kind, extraindent, ifBlock, varList, scopePath)
                        else:
                            add_extra(child, extraindent) #Update indentation because the loop is here and not in recur
                            for cnt in child.findall('.//{*}cnt'):
                                add_extra(cnt, extraindent) #Add indentation spaces after continuation characters
            elif tag(e) == 'where-stmt':
                #Where statement becomes if statement
                e.tag = f'{{{NAMESPACE}}}if-stmt'
                e.text = 'IF (' + e.text.split('(', 1)[1]
                updateStmt(e.find('./{*}action-stmt')[0], table, kind, extraindent, e, varList, scopePath) #Update the action part
                mask = e.find('./{*}mask-E')
                mask.tag = f'{{{NAMESPACE}}}condition-E' #rename the condition tag
                for namedE in mask.findall('.//{*}R-LT/..'):
                    self.arrayR2parensR(namedE, table, varList, scopePath) #Replace slices by variable
                for cnt in e.findall('.//{*}cnt'):
                    add_extra(cnt, extraindent) #Add indentation spaces after continuation characters
            elif tag(e) == 'where-construct':
                if kind != 'where' and kind is not None:
                    raise PYFTError('To keep the compatibility with the filepp version of loop " + \
                                    "expansion, no where construct must appear in mnh_expand_array blocks.')
                #Where construct becomes if construct
                e.tag = f'{{{NAMESPACE}}}if-construct'
                for whereBlock in e.findall('./{*}where-block'): #Loop over the blocks (where, elsewhere)
                    whereBlock.tag = f'{{{NAMESPACE}}}if-block'
                    for child in whereBlock: #Loop over each statement inside the block
                        if tag(child) == 'end-where-stmt':
                            #rename ENDWHERE into ENDIF
                            child.tag = f'{{{NAMESPACE}}}end-if-stmt'
                            child.text = 'END IF'
                            add_extra(child, extraindent) #Update indentation because the loop is here and not in recur
                        elif tag(child) in ('where-construct-stmt', 'else-where-stmt'):
                            add_extra(child, extraindent) #Update indentation because the loop is here and not in recur
                            if tag(child) == 'where-construct-stmt':
                                #rename WHERE into IF (the THEN part is attached to the condition)
                                child.tag = f'{{{NAMESPACE}}}if-then-stmt'
                                child.text = 'IF (' + child.text.split('(', 1)[1]
                            else:
                                #In where construct the same ELSEWHERE keyword is used with or without mask
                                #Whereas for if structure ELSEIF is used with a condition and ELSE without condition
                                if '(' in child.text:
                                    #rename ELSEWHERE into ELSEIF
                                    child.tag = f'{{{NAMESPACE}}}else-if-stmt'
                                    child.text = 'ELSE IF (' + child.text.split('(', 1)[1]
                                else:
                                    #rename ELSEWHERE into ELSE
                                    child.tag = f'{{{NAMESPACE}}}else-stmt'
                                    child.text = 'ELSE'
                            for mask in child.findall('./{*}mask-E'): #would a find be enough?
                                #add THEN
                                mask.tag = f'{{{NAMESPACE}}}condition-E'
                                mask.tail += ' THEN'
                                for namedE in mask.findall('.//{*}R-LT/..'):
                                    self.arrayR2parensR(namedE, table, varList, scopePath) #Replace slices by variable in the condition
                            for cnt in child.findall('.//{*}cnt'):
                                add_extra(cnt, extraindent) #Add indentation spaces after continuation characters
                        else:
                            updateStmt(child, table, kind, extraindent, whereBlock, varList, scopePath)
            else:
                raise PYFTError('Unexpected tag found in mnh_expand directives: {t}'.format(t=tag(e)))
            return e

        def closeLoop(loopdesc):
            """Helper function to deal with indetation"""
            if loopdesc:
                inner, outer, indent, extraindent = loopdesc
                if inner[-2].tail is not None:
                    outer.tail = inner[-2].tail[:-extraindent] #tail of last statement in DO loop before transformation
                inner[-2].tail = '\n' + (indent + extraindent - 2) * ' ' #position of the ENDDO
            return False

        toinsert = [] #list of nodes to insert
        toremove = [] #list of nodes to remove
        varList = [] #list of variables
        def recur(elem, scopePath):
            in_mnh = False #are we in a DO loop created by a mnh directive
            in_everywhere = False #are we in a created DO loop (except loops created with mnh directive)
            tailSave = {} #Save tail content before transformation (to retrieve original indentation)
            scopePath = self.getScopePath(elem) if self.isScopeNode(elem) else scopePath
            for ie, e in enumerate(list(elem)): #we loop on elements in the natural order
                if tag(e) == 'C' and e.text.lstrip(' ').startswith('!$mnh_expand') and useMnhExpand:
                    #This is an opening mnh directive
                    if in_mnh:
                        raise PYFTError('Nested mnh_directives are not allowed')
                    in_mnh = True
                    in_everywhere = closeLoop(in_everywhere) #close other loop if needed

                    #Directive decoding
                    table, kind = decode(e.text)
                    indent = len(e.tail) - len(e.tail.rstrip(' ')) #indentation of the next statement
                    toremove.append((elem, e)) #we remove the directive itself
                    if ie != 0:
                        #We add, to the tail of the previous node, the tail of the directive (except one \n)
                        if elem[ie - 1].tail is None: elem[ie - 1].tail = ''
                        elem[ie - 1].tail += e.tail.replace('\n', '', 1).rstrip(' ')
                    #Building loop
                    inner, outer, extraindent = self.createDoConstruct(table, indent=indent, concurrent=concurrent)
                    toinsert.append((elem, outer, ie)) #Place to insert the loop

                elif tag(e) == 'C' and e.text.lstrip(' ').startswith('!$mnh_end_expand') and useMnhExpand:
                    #This is a closing mnh directive
                    if not in_mnh:
                        raise PYFTError('End mnh_directive found before begin directive in {f}'.format(f=self.getFileName()))
                    if (table, kind) != decode(e.text):
                        raise PYFTError("Opening and closing mnh directives must be conform in {f}".format(f=self.getFileName()))
                    in_mnh = False
                    toremove.append((elem, e)) #we remove the directive itself
                    #We add, to the tail of outer DO loop, the tail of the directive (except one \n)
                    outer.tail += e.tail.replace('\n', '', 1) #keep all but one new line characters
                    elem[ie - 1].tail = elem[ie - 1].tail[:-2] #previous item controls the position of ENDDO

                elif in_mnh:
                    #This statement is between the opening and closing mnh directive
                    toremove.append((elem, e)) #we remove it from its old place
                    inner.insert(-1, e) #Insert first in the DO loop
                    updateStmt(e, table, kind, extraindent, inner, varList, scopePath) #then update, providing new parent in argument

                elif everywhere and tag(e) in ('a-stmt', 'if-stmt', 'where-stmt',
                                               'where-construct'):
                    #This node could contain array-syntax

                    #Is the node written using array-syntax? Getting the first array...
                    if tag(e) == 'a-stmt':
                        #Left side of the assignment
                        arr = e.find('./{*}E-1/{*}named-E/{*}R-LT/{*}array-R/../..')
                        #Right side
                        is_memSet = False
                        is_copy = False
                        E2 = e.find('./{*}E-2')
                        num = len(E2.findall('.//{*}array-R')) #Number of arrays using array-syntax
                        if num == 0:
                            #It is an array initialisation when there is no array-syntax on the right side
                            #If array-syntax is used without explicit '(:)', it could be detected as an initialisation
                            is_memSet = True
                        elif len(E2) == 1 and tag(E2[0]) == 'named-E' and num == 1 and \
                            E2[0].find('.//{*}parens-R') is None:
                            #It is an array copy when there is only one child in the right hand side
                            #    and this child is a named-E and this child contains only
                            #    one array-R node and no parens-R
                            is_copy = True
                        #Discard?
                        if (is_memSet and not updateMemSet) or (is_copy and not updateCopy):
                            arr = None
                    elif tag(e) == 'if-stmt':
                        #We only deal with assignment in the if statement case
                        arr = e.find('./{*}action-stmt/{*}a-stmt/{*}E-1/{*}named-E/{*}R-LT/{*}array-R/../..')
                        if arr is not None:
                            #In this case we transform the if statement into an if-construct
                            self.changeIfStatementsInIfConstructs(singleItem=e, parent=elem)
                            recur(e, scopePath) #to transform the content of the if
                            arr = None #to do nothing more on this node
                    elif tag(e) == 'where-stmt':
                        arr = e.find('./{*}mask-E//{*}named-E/{*}R-LT/{*}array-R/../..')
                    elif tag(e) == 'where-construct':
                        arr = e.find('./{*}where-block/{*}where-construct-stmt/{*}mask-E//{*}named-E/{*}R-LT/{*}array-R/../..')

                    #Check if it is written using array-syntax and must not be excluded; then compute bounds
                    if arr is None:
                        #There is no array-syntax
                        newtable = None
                    elif len(set([alltext(a).count(':') for a in e.findall('.//{*}R-LT/{*}array-R')])) > 1:
                        #All the elements written using array-syntax don't have the same rank
                        #(can be due to function calls, eg: "X(:)=FUNC(Y(:,:))")
                        newtable = None
                    elif len(set(['ALL', 'ANY', 'COSHAPE', 'COUNT', 'CSHIFT', 'DIMENSION',
                                  'DOT_PRODUCT', 'EOSHIFT', 'LBOUND', 'LCOBOUND', 'MATMUL',
                                  'MAXLOC', 'MAXVAL', 'MERGE', 'MINLOC', 'MINVAL', 'PACK',
                                  'PRODUCT', 'REDUCE', 'RESHAPE', 'SHAPE', 'SIZE', 'SPREAD',
                                  'SUM', 'TRANSPOSE', 'UBOUND', 'UCOBOUND', 'UNPACK'] + \
                                  (funcList if funcList is not None else [])
                                ).intersection(set([n2name(N) for N in e.findall('.//{*}named-E/{*}N')]))) > 0:
                        #At least one intrinsic array function is used
                        newtable = None
                    else:
                        if len(varList) == 0:
                            #Get all the variables declared in the tree
                            varList.extend(self.getVarList())
                        #Guess a variable name
                        if arr is not None:
                            newtable, varNew = self.findArrayBounds(arr, varList, scopePath, loopVar)
                            for v in varNew:
                                v['new'] = True
                            varList.extend(varNew)
                        else:
                            newtable = None

                    if newtable is None:
                        #We cannot convert the statement (not in array-syntax, excluded or no variable found to loop)
                        in_everywhere = closeLoop(in_everywhere) #close previous loop if needed
                    else:
                        #we have to transform the statement
                        if not (in_everywhere and table == newtable):
                            #No opened previous loop, or not coresponding
                            in_everywhere = closeLoop(in_everywhere) #close previous loop, if needed
                            #We must create a DO loop
                            if ie != 0 and elem[ie -1].tail is not None:
                                #Indentation of the current node, attached to the previous sibling
                                tail = tailSave.get(elem[ie -1], elem[ie -1].tail) #get tail before transformation
                                indent = len(tail) - len(tail.rstrip(' '))
                            else:
                                indent = 0
                            table = newtable #save the information on the newly build loop
                            kind = None #not built from mnh directives
                            #Building loop
                            inner, outer, extraindent = self.createDoConstruct(table, indent=indent, concurrent=concurrent)
                            toinsert.append((elem, outer, ie)) #place to insert the loop
                            in_everywhere = (inner, outer, indent, extraindent) #we are now in a loop
                        tailSave[e] = e.tail #save tail for future indentation computation
                        toremove.append((elem, e)) #we remove it from its old place
                        inner.insert(-1, e) #Insert first in the DO loop
                        updateStmt(e, table, kind, extraindent, inner, varList, scopePath) #then update, providing new parent in argument
                        if not reuseLoop:
                            #Prevent from reusing this DO loop
                            in_everywhere = closeLoop(in_everywhere)

                else:
                    in_everywhere = closeLoop(in_everywhere) #close loop if needed
                    if len(e) >= 1:
                        #Iteration
                        recur(e, scopePath)
            in_everywhere = closeLoop(in_everywhere)

        recur(self.node, self.getScopePath(self.node))
        #First, element insertion by reverse order (in order to keep the insertion index correct)
        for elem, outer, ie in toinsert[::-1]:
            elem.insert(ie, outer)
        #Then, suppression
        for parent, elem in toremove:
            parent.remove(elem)
        #And variable creation
        checkNewVarList = []
        for v in varList:
            if v.get('new', False) and v not in checkNewVarList:
                checkNewVarList.append(v)
        self.addVar([(v['scope'], v['n'], 'INTEGER :: {name}'.format(name=v['n']), None)
                     for v in checkNewVarList])

    @debugDecor
    @updateTree('signal')
    def inlineContainedSubroutines(self, simplify=False, loopVar=None):
        """
        Inline all contained subroutines in the main subroutine
        Steps :
            - Identify contained subroutines
            - Look for all CALL statements, check if it is a containted routines; if yes, inline
            - Delete the containted routines
        :param simplify: try to simplify code (construct or variables becoming useless)
        :param loopVar: None to create new variable for each added DO loop (around ELEMENTAL subroutine calls)
                        or a function that return the name of the variable to use for the loop control.
                        This function returns a string (name of the variable), or True to create
                        a new variable, or False to not transform this statement
                        The functions takes as arguments:
                          - lower and upper bounds as defined in the declaration statement
                          - lower and upper bounds as given in the statement
                          - name of the array
                          - index of the rank
        """

        scopes = self.getScopes()

        # Inline contained subroutines : look for sub: / sub:
        containedRoutines = {}
        for scope in scopes:
            if scope.path.count('sub:') >= 2:
                containedRoutines[alltext(scope.find('.//{*}subroutine-N/{*}N/{*}n'))] = scope
        # Start by nested contained subroutines call, and end up with the last index = the main subroutine to treat
        scopes.reverse()
        # Loop on all subroutines (main + contained)
        for scope in [scope for scope in scopes if scope.path.count('sub:') >= 1]:
            # Loop on all CALL statements
            for callStmtNn in scope.findall('.//{*}call-stmt/{*}procedure-designator/{*}named-E/{*}N/{*}n'):
                for containedRoutine in [cr for cr in containedRoutines if alltext(callStmtNn) == cr]:
                    # name of the routine called = a contained subroutine => inline
                    self.inline(
                           containedRoutines[containedRoutine],
                           self.getParent(callStmtNn, level=4),
                           scope.path, containedRoutines[containedRoutine].path, # Scope paths
                           self.getVarList(), # Must be recomputed each time to take into account new variable
                           simplify=simplify, loopVar=loopVar)

        for scope in scopes: #loop on all subroutines
            if scope.path.count('sub:') >= 2:
                #This is a contained subroutine
                name = scope.path.split(':')[-1].upper() # Subroutine name
                nodes = [N for N in self.findall('.//{*}N') if n2name(N).upper() == name] # All nodes refering the subroutine
                if all([N in scope.iter() for N in nodes]):
                    # Subroutine name not used (apart in its definition scope), we suppress it from the CONTAINS part
                    self.remove(scope)
                    self.tree.signal(self) # Tree must be updated, only in this case

        if simplify:
            self.removeEmptyCONTAINS()

    @debugDecor
    @updateTree()
    def inline(self, subContained, callStmt, mainScopePath, subScopePath,
               varList, simplify=False, loopVar=None):
        """
        Inline a subContainted subroutine
        Steps :
            - update the main code if needed (if statement and/or elemental)
            - copy the subContained node
            - remove everything before the declarations variables and the variables declarations
            - deal with optional argument
            - from the callStmt, replace all the arguments by their names
            - inline in the main code
            - add local variables and use statements to the main code
        :param subContained: xml fragment corresponding to the sub: to inline
        :param callStmt: the call-stmt to replace
        :param mainScopePath: scope path of the main (calling) subroutine
        :param subScopePath: scope path of the contained subroutine to include
        :param varList: var list of all scopes
        :param simplify: try to simplify code (construct or variables becoming useless)
        :param loopVar: None to create new variable for each added DO loop (around ELEMENTAL subroutine calls)
                        or a function that return the name of the variable to use for the loop control.
                        This function returns a string (name of the variable), or True to create
                        a new variable, or False to not transform this statement
                        The functions takes as arguments:
                          - lower and upper bounds as defined in the declaration statement
                          - lower and upper bounds as given in the statement
                          - name of the array
                          - index of the rank
        """
        def setPRESENTby(node, var, val):
            """
            Replace PRESENT(var) by .TRUE. if val is True, by .FALSE. otherwise on node if var is found
            :param node: xml node to work on (a contained subroutine)
            :param var: string of the name of the optional variable to check
            """
            for namedE in node.findall('.//{*}named-E/{*}N/..'):
                if n2name(namedE.find('./{*}N')).upper() == 'PRESENT':
                    presentarg = n2name(namedE.find('./{*}R-LT/{*}parens-R/{*}element-LT/{*}element/{*}named-E/{*}N'))
                    if presentarg.upper() == var.upper():
                        for n in namedE[:]:
                            namedE.remove(n)
                        namedE.tag = f'{{{NAMESPACE}}}literal-E'
                        namedE.text = '.TRUE.' if val else '.FALSE.'

        # Get parent of callStmt
        parent = self.getParent(callStmt)

        # Expand the if-construct if the call-stmt is in a one-line if-construct
        if tag(parent) == 'action-stmt':
            self.changeIfStatementsInIfConstructs(self.getParent(parent))
            parent = self.getParent(callStmt) #update parent

        # Specific case for ELEMENTAL subroutines
        # Introduce DO-loops if it is called on arrays
        prefix = subContained.findall('.//{*}prefix')
        if len(prefix) > 0 and 'ELEMENTAL' in [p.text.upper() for p in prefix]:
            #Add missing parentheses
            self.addArrayParenthesesInNode(callStmt, varList=varList, scopePath=mainScopePath)

            #Add explcit bounds
            self.addExplicitArrayBounds(node=callStmt, varList=varList, scopePath=mainScopePath)

            #Detect if subroutine is called on arrays
            arrayRincallStmt = callStmt.findall('.//{*}array-R')
            if len(arrayRincallStmt) > 0: #Called on arrays
                # Look for an array affectation to guess the DO loops to put around the call
                table, newVar = self.findArrayBounds(self.getParent(arrayRincallStmt[0], 2),
                                                     varList, mainScopePath, loopVar)

                # Add declaration of loop index if missing
                for varName in table.keys():
                    if not self.findVar(varName, mainScopePath, varList):
                        v = {'as': [], 'asx': [],
                             'n': varName, 'i': None, 't': 'INTEGER', 'arg': False,
                             'use': False, 'opt': False, 'allocatable': False,
                             'parameter': False, 'init': None, 'scope': mainScopePath}
                        self.addVar([[mainScopePath, v['n'], self.varSpec2stmt(v), None]])
                        varList.append(v)

                # Create the DO loops
                inner, outer, _ = self.createDoConstruct(table)

                # Move the call statement in the DO loops
                inner.insert(-1, callStmt) #callStmt in the DO-loops
                parent.insert(list(parent).index(callStmt), outer) #DO-loops near the original call stmt
                parent.remove(callStmt) #original call stmt removed
                parent = inner #Update parent
                for namedE in callStmt.findall('./{*}arg-spec/{*}arg/{*}named-E'):
                    # Replace slices by indexes if any
                    if namedE.find('./{*}R-LT'):
                        self.arrayR2parensR(namedE, table, varList, mainScopePath)

        # Deep copy the object to possibly modify the original one multiple times
        node = copy.deepcopy(subContained)

        # Get local variables that are not present in the main routine for later addition
        localVarToAdd = []
        subst = []
        for var in [v for v in varList
                    if not v['arg'] and not v['use'] and v['scope'] == subScopePath]: #for local variables only
            if self.findVar(var['n'], mainScopePath, varList):
               # Variable is already defined in main or upper, there is a name conflict,
               # the local variable must be renamed before being declared in the main routine
               newName = re.sub(r'_\d+$', '', var['n'])
               i = 1
               while self.findVar(newName + '_' + str(i), subScopePath, varList):
                   i += 1
               newName += '_' + str(i)
               node.renameVar(var['n'], newName)
               subst.append((var['n'], newName))
               var['n'] = newName
            var['scope'] = mainScopePath #important for findVar(.., mainScopePath,...) to find it
            localVarToAdd.append(var)

        #In case a substituted variable is used in the declaration of another variable
        for oldName, newName in subst:
            for v in localVarToAdd + varList:
                if v['as'] is not None:
                   v['as'] = [[re.sub(r'\b' +oldName + r'\b', newName, dim[i])
                               if dim[i] is not None else None
                               for i in (0, 1)]
                              for dim in v['as']]

        # Remove all objects that is implicit none, comment or else until reach something interesting
        # USE statements are stored for later user
        # subroutine-stmt and end-subroutine-stmt are kept to ensure consistency (for removeStmtNode with simplify)
        localUseToAdd = node.findall('./{*}use-stmt')
        for n in node.findall('./{*}T-decl-stmt') + localUseToAdd + node.findall('./{*}implicit-none-stmt'):
            node.remove(n)
        while tag(node[1]) == 'C':
            node.remove(node[1])

        # Variable correspondance
        # For each dummy argument, we look for the calling arg name and shape
        # CALL FOO(Z(:))
        # SUBROUTINE FOO(P)
        # vartable = {'P':{'name': 'Z', dim=[':']}}
        vartable = {} #ordered dict
        for argN in subContained.findall('.//{*}subroutine-stmt/{*}dummy-arg-LT/{*}arg-N'):
            vartable[alltext(argN).upper()] = None #Not present by default
        for iarg, arg in enumerate(callStmt.findall('.//{*}arg')):
            key = arg.find('.//{*}arg-N')
            if key is not None:
                #arg is VAR=value
                dummyName = alltext(key).upper()
                argnode = arg[1]
            else:
                dummyName = list(vartable.keys())[iarg]
                argnode = arg[0]
            RLTarray = argnode.findall('.//{*}R-LT/{*}array-R')
            if len(RLTarray) > 0:
                #array
                if len(RLTarray) > 1 or \
                   argnode.find('./{*}R-LT/{*}array-R') is None or \
                   tag(argnode) != 'named-E':
                    #Only simple cases are treated
                    raise PYFTError('Argument to complicated: ' + str(alltext(argnode)))
                dim = RLTarray[0].find('./{*}section-subscript-LT')[:]
            else:
                dim = None
            if dim is None:
                #A%B => argname = 'A%B'
                #Z(1) => argname = 'Z(1)'
                argname = "".join(argnode.itertext())
            else:
                #Z(:) => argname = 'Z'
                tmp = copy.deepcopy(argnode)
                RLT = tmp.find('./{*}R-LT')
                RLT.remove(RLT.find('./{*}array-R'))
                argname = "".join(tmp.itertext())
            vartable[dummyName] = {'node': argnode, 'name': argname, 'dim': dim}

        # Look for PRESENT(var) and replace it by True when variable is present, by False otherwise
        for dummyName in [dummyName for (dummyName, value) in vartable.items() if value is not None]:
            setPRESENTby(node, dummyName, True)
        for dummyName in [dummyName for (dummyName, value) in vartable.items() if value is None]:
            setPRESENTby(node, dummyName, False)

        # Look for usage of variable not present and delete corresponding code
        for dummyName in [dummyName for (dummyName, value) in vartable.items() if value is None]:
            for N in node.findall('.//{*}named-E/{*}N'):
                if n2name(N).upper() == dummyName:
                    removed = False
                    # Parent is the  named-E node, we need at least the upper level
                    par = node.getParent(N, level=2)
                    allreadySuppressed = []
                    while par and not removed and par not in allreadySuppressed:
                        toSuppress = None
                        tagName = tag(par)
                        if tagName in ('a-stmt', 'print-stmt'):
                            # Context 1: an a-stmt of type E1 = E2
                            toSuppress = par
                        elif tagName == 'call-stmt':
                            # We should rewrite the call statement without this optional argument
                            # But it is not easy: we must check that the argument is really optional
                            # for the called routine and we must add (if not already present)
                            # keywords for following arguments
                            raise NotImplementedError('call-stmt not (yet?) implemented')
                        elif tagName in ('if-stmt', 'where-stmt'):
                            # Context 2: an if-stmt of type  : IF(variable) ...
                            toSuppress = par
                        elif tagName in ('if-then-stmt', 'else-if-stmt', 'where-construct-stmt',
                                     'else-where-stmt'):
                            # Context 3: an if-block of type  : IF(variable) THEN...
                            # We delete the entire construct
                            toSuppress = node.getParent(par, 2)
                        elif tagName in ('select-case-stmt', 'case-stmt'):
                            # Context 4: SELECT CASE (variable)... or CASE (variable)
                            # We deleted the entire construct
                            toSuppress = node.getParent(par, 2)
                        elif tagName.endswith('-block') or tagName.endswith('-stmt') or \
                             tagName.endswith('-construct'):
                            # action-stmt, do-stmt, forall-construct-stmt, forall-stmt,
                            # if-block, where-block, selectcase-block,
                            #  must not contain directly
                            # the variable but must contain other statements using the variable. We target the inner
                            # statement in the previous cases.
                            # Some cases may have been overlooked and should be added above.
                            raise PYFTError(("We shouldn't be here. A case may have been " + \
                                             "overlooked (tag={tag}).".format(tag=tagName)))
                        if toSuppress is not None:
                            removed = True
                            if toSuppress not in allreadySuppressed:
                                # We do not simplify variables to prevent side-effect with the variable renaming
                                node.removeStmtNode(toSuppress, False, simplify)
                                allreadySuppressed.extend(list(toSuppress.iter()))
                        else:
                            par = node.getParent(par)

        # Loop on the dummy argument
        for name in vartable:
            dummy = vartable[name] #This is the dummy argument information
            # Loop on all variables in the contained routine
            # It is important to build again the list of nodes, because it may have changed during the previous
            # dummy argument substitution
            for namedE in node.findall('.//{*}named-E/{*}N/{*}n/../..'):
                if n2name(namedE.find('{*}N')).upper() == name:
                    #0 Concatenation of n nodes (a name could be split over several n nodes)
                    N = namedE.find('./{*}N')
                    ns = N.findall('./{*}n')
                    ns[0].text = n2name(N)
                    for n in ns[1:]:
                        N.remove(n)

                    #1 We get info about variables (such as declared in the main or in the contained routines)
                    desc_main = self.findVar(dummy['name'], mainScopePath, varList)
                    desc_sub = self.findVar(name, subScopePath, varList)
                    #In case variable is used for the declaration of another variable, we must update desc_sub
                    for n in range(len(varList)):
                        if varList[n]['as'] is not None:
                            varList[n]['as'] = [[re.sub(r'\b' + name + r'\b', dummy['name'], dim[i])
                                                 if dim[i] is not None else None
                                                 for i in (0, 1)]
                                                for dim in varList[n]['as']]

                    #3 We select the indexes (only for array argument and not structure argument containing an array)
                    #  using the occurrence to replace inside the subcontained routine body
                    RLT = namedE.find('./{*}R-LT')
                    if RLT is not None and tag(RLT[0]) != 'component-R':
                        #The variable name is immediately followed by a parenthesis
                        assert tag(RLT[0]) in ('array-R', 'parens-R'), 'Internal error'
                        slices = RLT[0].findall('./{*}section-subscript-LT/{*}section-subscript')
                        slices += RLT[0].findall('./{*}element-LT/{*}element')
                    else:
                        #No parenthesis
                        if (desc_main is not None and len(desc_main['as']) > 0) or \
                           len(desc_sub['as']) > 0 or dummy['dim'] is not None:
                            #No parenthesis, but this is an array, we add as many ':' as needed
                            if len(desc_sub['as']) > 0:
                                ndim = len(desc_sub['as'])
                            else:
                                #ELEMENTAL routine, dummy arg is scalar
                                if dummy['dim'] is not None:
                                    #We use the variable passed as argument because parenthesis were used
                                    ndim = len([d for d in dummy['dim'] if ':' in alltext(d)])
                                else:
                                    #We use the declared version in main
                                    ndim = len(desc_main['as'])
                            ns[0].text += '(' + (', '.join([':'] * ndim)) + ')'
                            updatedNamedE = createExprPart(alltext(namedE))
                            namedE.tag = updatedNamedE.tag
                            namedE.text = updatedNamedE.text
                            for n in namedE[:]:
                                namedE.remove(n)
                            namedE.extend(updatedNamedE[:])
                            slices = namedE.find('./{*}R-LT')[0].findall('./{*}section-subscript-LT/{*}section-subscript')
                        else:
                            #This is not an array
                            slices = []

                    #4 New name (the resultig xml is not necessarily a valid fxtran xml)
                    namedE.find('./{*}N')[0].text = dummy['name']

                    #5 We update the indexes to take into account a different declaration (lower bound especially)
                    #  in the main and in the contained routines.
                    #  Moreover, we could need to add indexes
                    if len(slices) > 0:
                        #This is an array
                        for isl, sl in enumerate(slices):
                            #0 Compute bounds for array
                            if len(desc_sub['as']) == 0 or desc_sub['as'][isl][1] is None:
                                #ELEMENTAL or array with implicit shape
                                for i in (0, 1): #0 for lower bound and 1 for upper bound
                                    if dummy['dim'] is not None:
                                        #Parenthesis in the call statement
                                        tagName = './{*}lower-bound' if i == 0 else './{*}upper-bound'
                                        desc_sub[i] = dummy['dim'][isl].find(tagName)
                                    else:
                                        desc_sub[i] = None
                                    if desc_sub[i] is not None:
                                        #lower/upper limit was given in the call statement
                                        desc_sub[i] = alltext(desc_sub[i])
                                    else:
                                        #if available we take lower/upper limit set in the declaration
                                        if desc_main is not None and desc_main['as'][isl][1] is not None:
                                            #Declaration found in main, and not using implicit shape
                                            desc_sub[i] = desc_main['as'][isl][i]
                                            if i == 0 and desc_sub[i] is None: desc_sub[i] = '1' #Default FORTRAN value
                                        else:
                                            desc_sub[i] = "L" if i == 0 else "U"
                                            desc_sub[i] += "BOUND({name}, {isl})".format(name=dummy['name'], isl=isl + 1)
                            else:
                                desc_sub[0] = desc_sub['as'][isl][0]
                                if desc_sub[0] is None: desc_sub[0] = '1' #Default FORTRAN value
                                desc_sub[1] = desc_sub['as'][isl][1]

                            #1 Offset computation
                            #  if only a subset is passed
                            #  and/or if lower bound of array is different in main and in sub contained routine
                            #REAL, DIMENSION(M1:M2):: Z; CALL FOO(N1:N2)
                            #SUBROUTINE FOO(P); REAL, DIMENSION(K1:K2):: P; P(I1:I2)
                            #If M1 or K1 is not set, they defaults to 1; if not set, N1 defaults to M1 and I1 to K1
                            #P(I1:I2)=Z(I1-K1+N1:I2-K1+N1)
                            offset = 0
                            if dummy['dim'] is not None and not alltext(dummy['dim'][isl]).strip().startswith(':'):
                                offset = alltext(dummy['dim'][isl].find('./{*}lower-bound'))
                            else:
                                if desc_main is not None:
                                    offset = desc_main['as'][isl][0]
                                    if offset is None:
                                        offset = '1' #Default FORTRAN value
                                    elif offset.strip().startswith('-'):
                                        offset = '(' + offset + ')'
                                else:
                                    offset = "LBOUND({name}, {isl})".format(name=dummy['name'], isl=isl + 1)
                            if offset.upper() == desc_sub[0].upper():
                                offset = 0
                            else:
                                if desc_sub[0].strip().startswith('-'):
                                    offset += '- (' + desc_sub[0] + ')'
                                else:
                                    offset += '-' + desc_sub[0]

                            #2 Update index with the offset and add indexes instead of ':'
                            if offset != 0:
                                if tag(sl) == 'element' or \
                                   (tag(sl) == 'section-subscript' and not ':' in alltext(sl)):
                                    #Z(I) or last index of Z(:, I)
                                    bounds = sl
                                else:
                                    low = sl.find('./{*}lower-bound')
                                    if low is None:
                                        low = createElem('lower-bound')
                                        low.append(createExprPart(desc_sub[0]))
                                        low.tail = sl.text #':'
                                        sl.text = None
                                        sl.insert(0, low)
                                    up = sl.find('./{*}upper-bound')
                                    if up is None:
                                        up = createElem('upper-bound')
                                        up.append(createExprPart(desc_sub[1]))
                                        sl.append(up)
                                    bounds = [low, up]
                                for bound in bounds:
                                    #bound[-1] is a named-E, literal-E, op-E...
                                    if bound[-1].tail is None: bound[-1].tail = ''
                                    bound[-1].tail += '+' + offset #Not valid fxtran xml

                        # We must add extra indexes
                        # CALL FOO(Z(:,1))
                        # SUBROUTINE FOO(P); REAL, DIMENSION(K1:K2):: P
                        # P(I) => Z(I, 1)
                        if dummy['dim'] is not None and len(dummy['dim']) > len(slices):
                            slices[-1].tail = ', '
                            par = node.getParent(slices[-1])
                            par.extend(dummy['dim'][len(slices):])

                    #6 Convert (wrong) xml into text and into xml again (to obtain a valid fxtran xml)
                    #  This double conversion is not sufficient in some case.
                    #  E.g. variable (N/n tag) replaced by real value
                    updatedNamedE = createExprPart(alltext(namedE))
                    namedE.tag = updatedNamedE.tag
                    namedE.text = updatedNamedE.text
                    for n in namedE[:]:
                        namedE.remove(n)
                    namedE.extend(updatedNamedE[:])

        node.remove(node.find('./{*}subroutine-stmt'))
        node.remove(node.find('./{*}end-subroutine-stmt'))

        # Add local var and use to main routine
        self.addVar([[mainScopePath, var['n'], self.varSpec2stmt(var), None] for var in localVarToAdd])
        self.addModuleVar([[mainScopePath, n2name(use_stmt.find('.//{*}module-N//{*}N')),
                            [n2name(v.find('.//{*}N')) for v in use_stmt.findall('.//{*}use-N')]]
                           for use_stmt in localUseToAdd])

        # Remove call statement of the contained routines
        index = list(parent).index(callStmt)
        parent.remove(callStmt)
        if callStmt.tail is not None:
            if node[-1].tail is None:
                node[-1].tail = callStmt.tail
            else:
                node[-1].tail = node[-1].tail + callStmt.tail
        for node in node[::-1]:
            #node is a program-unit, we must insert the subelements
            parent.insert(index, node)

    @debugDecor
    def setFalseIfStmt(self, flags, scopePath=None, simplify=False):
        """
        Set to .FALSE. a given boolean fortran flag before removing the node if simplify is True
        :param flags: list of strings of flags to set to .FALSE.
        :param scopePath: scope path, or list of, to explore (None for all). A path is a
                          '/'-separated string with each element having the form
                          'module:<name of the module>', 'sub:<name of the subroutine>' or
                          'func:<name of the function>'
        :param simplify: try to simplify code (if the .FALSE. was alone inside a if-then-endif construct,
                         the construct is removed, and variables used in the if condition are also
                         checked)
        """
        if isinstance(flags, str):
            flags = [flags]
        flags = [flag.upper() for flag in flags]
        singleFalseBlock, multipleFalseBlock = [], []
        for scope in self.getScopeNodes(scopePath, excludeContains=True, excludeKinds=['type']):
            #Loop on nodes composing the scope
            for node in scope:
                #Loop on condition nodes
                for cond in node.findall('.//{*}condition-E'):
                    found = False
                    for namedE in node.findall('.//{*}condition-E//{*}named-E'):
                        if alltext(namedE).upper() in flags:
                            #This named-E must be replaced by .FALSE.
                            found = True
                            namedE.tag = '{{{NAMESPACE}}}literal-E'
                            namedE.text = '.FALSE.'
                            for item in list(namedE):
                                namedE.remove(item)
                    if found:
                        node_opE = cond.find('./{*}op-E')
                        if node_opE is not None:
                            #Multiple flags conditions
                            multipleFalseBlock.append(node_opE)
                        else:
                            #Solo condition
                            # <if-block><if-then-stmt>
                            singleFalseBlock.append(self.getParent(cond, level=2))
        if simplify:
            self.removeStmtNode(singleFalseBlock, simplify, simplify)
            self.evalFalseIfStmt(multipleFalseBlock, simplify)

    @debugDecor
    def evalFalseIfStmt(self, nodes, simplify=False):
        """
        Evaluate if-stmt with multiple op-E and remove the nodes if only .FALSE. are present
        :param nodes: list of nodes of type op-E to evaluate (containing .FALSE.)
        :param simplify: try to simplify code (if if-block is removed, variables used in the if condition are also
                         checked)
        """
        nodesTorm = []
        for node in nodes:
            toRemove = True
            for n in node:
                if tag(n) == 'op' or (tag(n) == 'literal-E' and '.FALSE.' in alltext(n)):
                    pass
                else:
                    toRemove = False
                    break
            if toRemove:
                #<if-block><if-then-stmt><condition-E>
                nodesTorm.append(self.getParent(level=3))
        self.removeStmtNode(nodesTorm, simplify, simplify)

    @debugDecor
    def checkOpInCall(self, mustRaise=False):
        """
        :param mustRaise: True to raise
        Issue a logging.warning if some call arguments are operations
        If mustRaise is True, issue a logging.error instead and raise an error
        """
        ok = True
        l = logging.error if mustRaise else logging.warn
        for arg in self.findall('.//{*}call-stmt/{*}arg-spec/{*}arg/{*}op-E'):
            l(("The call argument {} is an operation, in file '{}'"
              ).format(alltext(arg).replace('\n', ' \\n '), self.getFileName()))
            ok = False
        if not ok and mustRaise:
            raise PYFTError(("There are call arguments which are operations in file '{}'"
                            ).format(self.getFileName()))

    @debugDecor
    def insertStatement(self, scope, stmt, first):
        """
        Insert a statement to be executed first (or last)
        :param scope: scope (path or node) in which the statement must be inserted
        :param stmt: statement to insert
        :param first: True to insert it in first position, False to insert it in last position
        """
        if isinstance(scope, str):
            scope = self.getScopeNode(scope)
        if first:
            #Statement must be inserted after all use, T-decl, implicit-non-stmt, interface and cray pointers
            nodes = scope.findall('./{*}T-decl-stmt') + scope.findall('./{*}use-stmt') + \
                    scope.findall('./{*}implicit-none-stmt') + scope.findall('./{*}interface-construct') + \
                    scope.findall('./{*}pointer-stmt')
            if len(nodes) > 0:
                #Insertion after the last node
                index = max([list(scope).index(n) for n in nodes]) + 1
            else:
                #Insertion after the subroutine or function node
                index = 2
            #If an include statements follows, it certainly contains an interface
            while tag(scope[index]) in ('C', 'include', 'include-stmt') or \
                  (tag(scope[index]) == 'cpp' and scope[index].text.startswith('#include')):
                index += 1
        else:
            #Statement must be inserted before the contains statement
            contains = scope.find('./{*}contains-stmt')
            if contains is not None:
                #Insertion before the contains statement
                index = list(scope).index(contains)
            else:
                #Insertion before the end subroutine or function statement
                index = -1
        if scope[index - 1].tail is None:
            scope[index - 1].tail = '\n'
        elif not '\n' in scope[index - 1].tail:
            scope[index - 1].tail += '\n'
        scope.insert(index, stmt)

    @debugDecor
    def removeStmtNode(self, nodes, simplifyVar, simplifyStruct):
        """
        This function removes a statement node and:
          - suppress variable that became useless (if simplifyVar is True)
          - suppress outer loop/if if useless (if simplifyStruct is True)
        :param nodes: node (or list of nodes) to remove
        :param simplifyVar: try to simplify code (if we delete "CALL FOO(X)" and if X not used else where,
                            we also delete it; or if the call was alone inside a if-then-endif construct,
                            with simplifyStruct=True, the construct is also removed, and variables used
                            in the if condition are also checked...)
        :param simplifyStruct: try to simplify code (if we delete "CALL FOO(X)" and if the call was
                               alone inside a if-then-endif construct, the construct is also removed,
                               and variables used in the if condition (with simplifyVar=True) aro
                               also checked...)
        """

        #In case the suppression of an if-stmt or where-stmt is asked,
        #we must start by the inner statement
        nodesToSuppress = []
        if not isinstance(nodes, list): nodes = [nodes]
        for node in nodes:
            if tag(node) in ('if-stmt', 'where-stmt'):
                action = node.find('./{*}action-stmt')
                if action is not None and len(action) != 0:
                    nodesToSuppress.append(action[0])
                else:
                    nodesToSuppress.append(node)
            elif tag(node) == '}action-stmt':
                if len(node) != 0:
                    nodesToSuppress.append(node[0])
                else:
                    nodesToSuppress.append(node)
            else:
                nodesToSuppress.append(node)

        varToCheck = [] #List of variables to check for suppression
        if simplifyVar:
            #Loop to identify all the potential variables to remove
            for node in nodesToSuppress:
                scopePath = self.getScopePath(node)
                if tag(node) == 'do-construct':
                    #Try to remove variables used in the loop
                    varToCheck.extend([(scopePath, n2name(arg))
                                       for arg in node.find('./{*}do-stmt').findall('.//{*}N')])
                elif tag(node) in ('if-construct', 'if-stmt'):
                    #Try to remove variables used in the conditions
                    varToCheck.extend([(scopePath, n2name(arg))
                                       for arg in node.findall('.//{*}condition-E//{*}N')])
                elif tag(node) in ('where-construct', 'where-stmt'):
                    #Try to remove variables used in the conditions
                    varToCheck.extend([(scopePath, n2name(arg))
                                       for arg in node.findall('.//{*}mask-E//{*}N')])
                elif tag(node) == 'call-stmt':
                    #We must check if we can suppress the variables used to call the subprogram
                    varToCheck.extend([(scopePath, n2name(arg))
                                       for arg in node.findall('./{*}arg-spec//{*}N')])
                    #And maybe, the subprogram comes from a module
                    varToCheck.append((scopePath,
                                       n2name(node.find('./{*}procedure-designator//{*}N'))))
                elif tag(node) in ('a-stmt', 'print-stmt'):
                    varToCheck.extend([(scopePath, n2name(arg)) for arg in node.findall('.//{*}N')])
                elif tag(node) == 'selectcase-construct':
                    #Try to remove variables used in the selector and in conditions
                    varToCheck.extend([(scopePath, n2name(arg))
                                       for arg in node.findall('.//{*}case-E//{*}N')])
                    varToCheck.extend([(scopePath, n2name(arg))
                                       for arg in node.findall('.//{*}case-value//{*}N')])

        #Node suppression
        parents = {} #cache
        for node in nodesToSuppress:
            parent = self.getParent(node)
            parents[id(node)] = parent
            newlines = '\n' * (alltext(node).count('\n') if tag(node).endswith('-construct') else 0)
            if node.tail is not None or len(newlines) > 0:
                previous = self.getSiblings(node, after=False)
                if len(previous) == 0:
                    previous = parent
                else:
                    previous = previous[-1]
                if previous.tail is None: previous.tail = ''
                previous.tail = previous.tail.replace('\n','') + (node.tail if node.tail is not None else '')
            parent.remove(node)

        #Variable simplification
        self.removeVarIfUnused(varToCheck, excludeDummy=True, excludeModule=True, simplify=simplifyVar)

        #List the new nodes to suppress
        newNodesToSuppress = []
        for node in nodesToSuppress:
            parent = parents[id(node)]
            #If we have suppressed the statement in a if statement (one-line if) or where statement
            #we must suppress the entire if/where statement even when simplifyStruct is False
            if tag(parent) == 'action-stmt':
                newNodesToSuppress.append(self.getParent(parent))

            elif simplifyStruct:
                if tag(parent) == 'do-construct' and len(_nodesInDo(parent)) == 0:
                    newNodesToSuppress.append(parent)
                elif tag(parent) == 'if-block':
                    parPar = self.getParent(parent)
                    if len(_nodesInIf(parPar)) == 0:
                        newNodesToSuppress.append(parPar)
                elif tag(parent) == 'where-block':
                    parPar = self.getParent(parent)
                    if len(_nodesInWhere(parPar)) == 0:
                        newNodesToSuppress.append(parPar)
                elif tag(parent) == 'selectcase-block':
                    parPar = self.getParent(parent)
                    if len(_nodesInCase(parPar)) == 0:
                        newNodesToSuppress.append(parPar)

        constructNodes, otherNodes = [], []
        for n in newNodesToSuppress:
            if tag(n).endswith('-construct'):
                if n not in constructNodes: constructNodes.append(n)
            else:
                if n not in otherNodes: otherNodes.append(n)
        #suppress all statements at once
        if len(otherNodes) > 0:
            self.removeStmtNode(otherNodes, simplifyVar, simplifyStruct)
        #suppress construct nodes one by one (recursive call)
        for n in constructNodes:
            self.removeConstructNode(n, simplifyVar, simplifyStruct)

    @debugDecor
    def removeConstructNode(self, node, simplifyVar, simplifyStruct):
        """
        This function removes a construct node and:
          - suppress variable that became useless (if simplifyVar is True)
          - suppress outer loop/if if useless (if simplifyStruct is True)
        :param node: node representing the statement to remove
        :param simplifyVar: try to simplify code (if we delete "CALL FOO(X)" and if X not used else where,
                            we also delete it; or if the call was alone inside a if-then-endif construct,
                            with simplifyStruct=True, the construct is also removed, and variables used
                            in the if condition are also checked...)
        :param simplifyStruct: try to simplify code (if we delete "CALL FOO(X)" and if the call was
                               alone inside a if-then-endif construct, the construct is also removed,
                               and variables used in the if condition (with simplifyVar=True) aro
                               also checked...)
    
        If a statement is passed, it is suppressed by removeStmtNode
        """
        assert tag(node).endswith('-stmt') or tag(node).endswith('-construct'), \
          "Don't know how to suppress only a part of a structure or of a statement"
    
        #This function removes inner statement to give a chance to identify and suppress unused variables
        #During this step, nodes are suppressed with simplifyStruct=False to prevent infinite loops
        #then the actual node is removed using removeStmtNode
    
        if tag(node).endswith('-construct'):
            #inner nodes
            nodes = {'do-construct': _nodesInDo,
                     'if-construct': _nodesInIf,
                     'where-construct': _nodesInWhere,
                     'selectcase-construct': _nodesInCase}[tag(node)](node)
            #sort nodes by type
            constructNodes, otherNodes = [], []
            for n in nodes:
                if tag(n).endswith('-construct'):
                    constructNodes.append(n)
                else:
                    otherNodes.append(n)
            #suppress all statements at once
            self.removeStmtNode(otherNodes, simplifyVar, False)
            #suppress construct nodes one by one (recursive call)
            for n in constructNodes:
                self.removeConstructNode( n, simplifyVar, False)
            #suppress current node
            self.removeStmtNode(node, simplifyVar, simplifyStruct)
        else:
            #At least a-stmt, print-stmt
            self.removeStmtNode(node, simplifyVar, simplifyStruct)

    @staticmethod
    @debugDecor
    def createDoConstruct(loopVariables, indent=0, concurrent=False):
        """
        :param loopVariables: ordered dictionnary with loop variables as key and bounds as values.
                              Bounds are expressed with a 2-tuple.
                              Keys must be in the same order as the order used when addressing an element:
                                if loopVariables.keys is [JI, JK], arrays are addressed with (JI, JK)
        :param indent: current indentation
        :param concurrent: if False, output is made of nested 'DO' loops
                           if True, output is made of a single 'DO CONCURRENT' loop
        :return: (inner, outer, extraindent) with
                  - inner the inner do-construct where statements must be added
                  - outer the outer do-construct to be inserted somewhere
                  - extraindent the number of added indentation (2 if concurrent else 2*len(loopVariables))
        """
        if concurrent:
            #<f:do-construct>
            #  <f:do-stmt>DO CONCURRENT (
            #    <f:forall-triplet-spec-LT>
            #      <f:forall-triplet-spec>
            #        <f:V><f:named-E><f:N><f:n>JIJ</f:n></f:N></f:named-E></f:V>=
            #        <f:lower-bound><f:named-E><f:N><f:n>IIJB</f:n></f:N></f:named-E></f:lower-bound>:
            #        <f:upper-bound><f:named-E><f:N><f:n>IIJE</f:n></f:N></f:named-E></f:upper-bound>
            #      </f:forall-triplet-spec>,
            #      <f:forall-triplet-spec>
            #        <f:V><f:named-E><f:N><f:n>JK</f:n></f:N></f:named-E></f:V>=
            #        <f:lower-bound><f:literal-E><f:l>1</f:l></f:literal-E></f:lower-bound>:
            #        <f:upper-bound><f:named-E><f:N><f:n>IKT</f:n></f:N></f:named-E></f:upper-bound>
            #      </f:forall-triplet-spec>
            #    </f:forall-triplet-spec-LT>)
            #  </f:do-stmt>
            #  statements
            #  <f:end-do-stmt>END DO</f:end-do-stmt>
            #</f:do-construct>
            triplets = []
            for v, (l, u) in list(loopVariables.items())[::-1]: #Better for vectorisation with some compilers
                V = createElem('V')
                V.append(createExprPart(v))
                V.tail = '='
                lower, upper = createArrayBounds(l, u, 'DOCONCURRENT')

                triplet = createElem('forall-triplet-spec')
                triplet.extend([V, lower, upper])

                triplets.append(triplet)

            tripletLT = createElem('forall-triplet-spec-LT')
            tripletLT.tail = ')'
            for triplet in triplets[:-1]:
                triplet.tail = ', '
            tripletLT.extend(triplets)

            dostmt = createElem('do-stmt')
            dostmt.text = 'DO CONCURRENT ('
            dostmt.tail = '\n'
            dostmt.append(tripletLT)
            enddostmt = createElem('end-do-stmt')
            enddostmt.text = 'END DO'

            doconstruct = createElem('do-construct')
            doconstruct.extend([dostmt, enddostmt])
            doconstruct.tail = '\n'
            inner = outer = doconstruct
            doconstruct[0].tail += (indent + 2) * ' ' #Indentation for the statement after DO
        else:
            #<f:do-construct>
            #  <f:do-stmt>DO
            #    <f:do-V><f:named-E><f:N><f:n>JRR</f:n></f:N></f:named-E></f:do-V> =
            #    <f:lower-bound><f:literal-E><f:l>1</f:l></f:literal-E></f:lower-bound>:
            #    <f:upper-bound><f:named-E><f:N><f:n>IKT</f:n></f:N></f:named-E></f:upper-bound>
            #  </f:do-stmt>\n
            #  statements \n
            #  <f:end-do-stmt>END DO</f:end-do-stmt>
            #</f:do-construct>\n
            def make_do(v, l, u):
                doV = createElem('do-V')
                doV.append(createExprPart(v))
                doV.tail = '='
                lower, upper = createArrayBounds(l, u, 'DO')

                dostmt = createElem('do-stmt')
                dostmt.text = 'DO '
                dostmt.tail = '\n'
                dostmt.extend([doV, lower, upper])

                enddostmt = createElem('end-do-stmt')
                enddostmt.text = 'END DO'

                doconstruct = createElem('do-construct')
                doconstruct.extend([dostmt, enddostmt])
                doconstruct.tail = '\n'
                return doconstruct

            outer = None
            inner = None
            for i, (v, (l, u)) in enumerate(list(loopVariables.items())[::-1]):
                doconstruct = make_do(v, l, u)
                doconstruct[0].tail += (indent + 2 * i + 2) * ' ' #Indentation for the statement after DO
                if outer is None:
                    outer = doconstruct
                    inner = doconstruct
                else:
                    inner.insert(1, doconstruct)
                    inner = doconstruct
                    doconstruct.tail += (indent + 2 * i - 2) * ' ' #Indentation for the ENDDO statement
        return inner, outer, 2 if concurrent else 2 * len(loopVariables)

    @staticmethod
    @debugDecor
    def insertInList(pos, item, l):
        """
        :param pos: insertion position
        :param item: item to add to the list
        :param l: the parent of item (the list)
        """
        #insertion
        if pos < 0: pos = len(l) + 1 + pos
        l.insert(pos, item)
        if len(l) > 1:
            i = list(l).index(item) #effective position
            if i == len(l) - 1:
                #The item is the last one
                l[i - 1].tail = ', '
            else:
                l[i].tail = ', '

    @debugDecor
    def removeFromList(self, item, l):
        """
        :param item: item to remove from list
        :param l: the parent of item (the list)
        """
    
        nodesToSuppress = [item]
    
        #Suppression of the comma
        i = list(l).index(item)
        if item.tail is not None and ',' in item.tail:
            #There's a comma just after the node
            tail = item.tail
            item.tail = tail.replace(',', '')
        elif i != 0 and ',' in l[i - 1].tail:
            #There's a comma just before the node
            tail = l[i - 1].tail
            l[i - 1].tail = tail.replace(',', '')
        else:
            found = False
            #We look for a comma in the first node after the current node that
            #is not after another item of the list
            j = i + 1
            while j < len(l) and not found:
                if non_code(l[j]):
                    #This is a candidate
                    if l[j].tail is not None and ',' in l[j].tail:
                        #Comma found and suppressed
                        found = True
                        tail = l[j].tail
                        l[j].tail = tail.replace(',', '')
                    else:
                        j += 1
                else:
                    #This is another item
                    break
    
            #We look for a comma in the last node before the current node that
            #is not a comment or a contiuation character
            j = i - 1
            while j >= 0 and not found:
                if l[j].tail is not None and ',' in l[j].tail:
                    #Comma found and suppressed
                    found = True
                    tail = l[j].tail
                    l[j].tail = tail.replace(',', '')
                else:
                    if non_code(l[j]):
                        #We can search before
                        j -= 1
                    else:
                        #This is another item
                        break
    
            if not found and \
               len([e for e in l if not non_code(e)]) != 1:
                raise RuntimeError("Something went wrong here....")
    
        #Suppression of continuation characters
        if i + 1 < len(l) and tag(l[i + 1]) == 'cnt':
            #Node is followed by a continuation character
            reason = 'lastOnLine'
            #If the node is followed by a continuation character and is just after another
            #continuation character, we must supress the continuation character which is after.
            #    USE MODD, ONLY: X, &
            #                    Y, & !Variable to suppress, with its '&' character
            #                    Z
            #In addition, if the character found before is at the begining of the line,
            #it must also be removed. To know if it is at the begining of the line,
            #we can check if another continuation character is before.
            #    USE MODD, ONLY: X, &
            #                  & Y, & !Variable to suppress, with both '&' characters
            #                  & Z
        elif len([l[j] for j in range(i + 1, len(l)) if not non_code(l[j])]) == 0:
            #Node is the last of the list
            reason = 'last'
            #If the removed node is the last of the list and a continuation character is just before
            #We must suppress it.
            #    USE MODD, ONLY: X, &
            #                    Y !Variable to suppress, with the preceding '&'
            #In addition, if the character found before is at the begining of the line,
            #the preceding one must also be removed.
            #    USE MODD, ONLY: X, &
            #                  & Y !Variable to suppress, with 2 '&'
        else:
            #We must not suppress '&' characters
            reason = None
        if reason is not None:
            def _getPrecedingCnt(l, i):
                """
                Return the index of the preceding node which is a continuation character
                :param l: the list containig the node to suppress
                :param i: the index of the current node, i-1 is the starting index for the search
                :return: a tuple with three elements:
                          - the node containing the preceding '&' character
                          - the parent of the node containing the preceding '&' character
                          - index of the preceding '&' in the parent (previsous element of the tuple)
                Note:
                  - In the general case the preceding '&' belongs to the same list:
                      USE MODD, ONLY: X, &
                                      Y
                  - But it exists a special case, where the preceding '&' don't belong to
                    the same list (in the following example, both '&' are attached to the parent):
                      USE MODD, ONLY: &
                                    & X
                """
                j = i - 1
                while j >= 0 and tag(l[j]) == 'C':
                    j -= 1
                if j >= 0 and tag(l[j]) == 'cnt':
                    return l[j], l, j
                elif j == -1:
                    #In the following special case, the '&' don't belong to the list but are siblings
                    #of the list.
                    #    USE MODD, ONLY: &
                    #                  & X
                    siblings = self.getSiblings(l, before=True, after=False)
                    j2 = len(siblings) - 1
                    while j2 >= 0 and tag(siblings[j2]) == 'C':
                        j2 -= 1
                    if j2 >= 0 and tag(siblings[j2]) == 'cnt':
                        return siblings[j2], siblings, j2
                    else:
                        return None, None, None
                else:
                    return None, None, None
            #We test if the preceding node (excluding comments) is a continuation character
            precCnt, newl, j = _getPrecedingCnt(l, i)
            if precCnt is not None:
                #Preceding node is a continuation character
                nodesToSuppress.append(precCnt if reason == 'last' else l[i + 1])
                if j is not None:
                    precCnt2, _, j2 = _getPrecedingCnt(newl, j)
                    if precCnt2 is not None:
                        #There is another continuation character before
                        nodesToSuppress.append(precCnt2 if reason == 'last' else precCnt)
    
        #Suppression of nodes
        for e in nodesToSuppress:
            #Get the tail of the previous children of the list and append the item's tail to be removed
            if e in l:
                parent = l
            else:
                #parent must be recomputed because previsous removal may have change it
                parent = self.getParent(l)
            i = list(parent).index(e)
            if i != 0 and e.tail is not None:
                if parent[i - 1].tail is None:
                    parent[i - 1].tail = ''
                parent[i - 1].tail = parent[i - 1].tail + e.tail
            parent.remove(e)
