"""
This module includes the Statements class containing methods to act on statements
"""

import re
import logging
import copy
from pyfortool.util import n2name, nonCode, debugDecor, alltext, PYFTError, tag, noParallel
from pyfortool.expressions import createExprPart, createArrayBounds, createElem
from pyfortool.tree import updateTree
from pyfortool.variables import updateVarList
from pyfortool import NAMESPACE


def _nodesInIf(ifNode):
    """
    Internal method to return nodes in if structure
    """
    nodes = []
    for block in ifNode.findall('./{*}if-block'):
        for item in [i for i in block
                     if tag(i) not in ('if-then-stmt', 'else-if-stmt',
                                       'else-stmt', 'end-if-stmt')]:
            if not nonCode(item):
                nodes.append(item)
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
            if not nonCode(item):
                nodes.append(item)
    return nodes


def _nodesInDo(doNode):
    """
    Internal method to return nodes in do structure
    """
    nodes = []
    for item in [i for i in doNode if tag(i) not in ('do-stmt', 'end-do-stmt')]:
        if not nonCode(item):
            nodes.append(item)
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
            if not nonCode(item):
                nodes.append(item)
    return nodes


class Statements():
    """
    Methods to act on statements
    """

    # No @debugDecor for this low-level method
    def isNodeInProcedure(self, node, procList):
        """
        Return True if node (named-E) is an argument of a procedure listed in procList
        :param node: node to test
        :param procList: list of procedure names
        """
        # E.g. The xml for "ASSOCIATED(A)" is
        # <f:named-E>
        #   <f:N><f:n>ASSOCIATED</f:n></f:N>
        #   <f:R-LT><f:parens-R>(
        #     <f:element-LT><f:element><f:named-E><f:N><f:n>A</f:n></f:N>
        #            </f:named-E></f:element></f:element-LT>)
        #   </f:parens-R></f:R-LT>
        # </f:named-E>
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

    # No @debugDecor for this low-level method
    def isNodeInCall(self, node):
        """
        Return True if node (named-E) is an argument of a called procedure
        :param node: node to test
        """
        # E.g. The xml for "CALL FOO(A)" is
        # <f:call-stmt>CALL
        #   <f:procedure-designator><f:named-E><f:N><f:n>FOO</f:n></f:N>
        #          </f:named-E></f:procedure-designator>(
        #   <f:arg-spec><f:arg><f:named-E><f:N><f:n>A</f:n></f:N></f:named-E></f:arg></f:arg-spec>)
        # </f:call-stmt>
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
    def removeCall(self, callName, simplify=False):
        """
        :param callName: name of the subprogram calls to remove.
        :param simplify: try to simplify code (if we delete "CALL FOO(X)" and if X not
                         used else where, we also delete it; or if the call was alone inside
                         a if-then-endif construct, the construct is also removed, and
                         variables used in the if condition are also checked...)
        :return: number of calls suppressed
        """
        # Select all call-stmt and filter by name
        callNodes = [cn for cn in self.findall('.//{*}call-stmt')
                     if n2name(cn.find('.//{*}named-E/{*}N')).upper() == callName.upper()]
        self.removeStmtNode(callNodes, simplify, simplify)
        return len(callNodes)

    @debugDecor
    def removePrints(self, simplify=False):
        """
        Removes all print statements
        :param simplify: try to simplify code (if we delete "print*, X" and if X is not
                         used else where, we also delete it; or if the print was alone inside
                         a if-then-endif construct, the construct is also removed, and
                         variables used in the if condition are also checked...)
        """
        self.removeStmtNode(self.findall('.//{*}print-stmt'), simplify, simplify)

    @debugDecor
    def removeArraySyntax(self, concurrent=False, useMnhExpand=True, everywhere=True,
                          loopVar=None, reuseLoop=True, funcList=None,
                          updateMemSet=False, updateCopy=False, addAccIndependentCollapse=True):
        """
        Transform array syntax into DO loops
        :param concurrent: use 'DO CONCURRENT' instead of simple 'DO' loops
        :param useMnhExpand: use the mnh directives to transform the entire bloc in a single loop
        :param everywhere: transform all array syntax in DO loops
        :param loopVar: None to create new variable for each added DO loop, or
                        a function that return the name of the variable to use for the loop control.
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
        :param addAccIndependentCollapse: True to add !$acc loop independent collapse(X) before
                                          the DO construct

        Notes: * With useMnhExpand, the function checks if the coding is conform to what is needed
                 for the filepp/mnh_expand tool (to not breack compatibility with this tool)
               * Arrays are transformed only if ':' are used.
                 A=A(:) is not transformed at all (or raises an exception if found in a WHERE block)
                 A(:)=A is wrongly transformed into "DO...; A(J1)=A; ENDDO" and will produce a
                     compilation error
                 WHERE(L) X(:)=0. is not transformed at all (unknown behaviour in case
                     of nested WHERE)
               * This function is not compatible with functions that return arrays:
                 X(1:5)=FUNC(1) will be transformed into "DO J1=1,5; X(J1)=FUNC(1); ENDDO"
                 x(1:5)=FUNC(X(1:5)) will be transformed into "DO J1=1,5; X(J1)=FUNC(X(J1)); ENDDO"
                 But intrinsic functions (COUNT, ANY...) are recognised and corresponding statements
                 are not transformed.
                 The list of intrinsic array functions can be extended by user functions with the
                 funcList argument.
        """

        # Developer notes:
        # We use recursivity to avoid the use of the 'getParent' function.
        # We start from the top node and call 'recur'.
        #
        # The 'recur' function loops over the different nodes and:
        #  - search for mnh directives (if 'useMnhExpand' is True):
        #      - when it is an opening directive:
        #          - decode the directive to identify bounds and variables to use
        #            ('decode' function)
        #          - introduce the DO loops (with 'createDoConstruct')
        #          - activate the 'inMnh' flag
        #      - when it is a closing directive:
        #          - deactivate the 'inMnh' flag
        #  - while the 'inMnh' flag is activated:
        #      - update ('updateStmt' function, that uses 'arrayR2parensR') and put all statements
        #        in the DO loops
        #  - in case (if 'everywhere' is True) statement is expressed using array-syntax:
        #      - find the bounds and guess a set of variables to use ('findArrayBounds' function)
        #      - introduce the DO loops (with 'createDoConstruct') if we cannot reuse the
        #        previous one
        #      - update ('updateStmt' function, that uses 'arrayR2parensR') and put all statements
        #        in the DO loops
        #  - in case the statement contains other statements (SUBROUTINE, DO loop...), call 'recur'
        #    on it
        #
        # Because we iterate on the statements, the tree structure cannot be modified during the
        # iteration.
        # All the modifications to apply are, instead, stored in objetcs ('toinsert', 'toremove'
        # and 'varList') are applied afterwards.
        #
        # In addition, a number of instructions are needed to preserve and/or modify the indentation
        # and can somewhat obfuscate the source code.

        def decode(directive):
            """
            Decode mnh_expand directive
            :param directive: mnh directive text
            :return: (table, kind) where
                     table is a dictionnary: keys are variable names, values are tuples with first
                         and last index
                     kind is 'array' or 'where'
            """
            # E.g. !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
            # We expect that the indexes are declared in the same order as the one they appear
            # in arrays
            # For the example given, arrays are addressed with (JIJ, JK)
            # For this example, return would be
            #    ('array', {'JIJ':('IIJB', 'IIJE'), 'JK':('1', 'IKT')})
            table = directive.split('(')[1].split(')')[0].split(',')
            table = {c.split('=')[0]: c.split('=')[1].split(':')
                     for c in table}  # ordered since python 3.7
            if directive.lstrip(' ').startswith('!$mnh_expand'):
                kind = directive[13:].lstrip(' ').split('(')[0].strip()
            else:
                kind = directive[17:].lstrip(' ').split('(')[0].strip()
            return table, kind

        def updateStmt(stmt, table, kind, extraindent, parent, scope):
            """
            Updates the statement given the table dictionnary '(:, :)' is replaced by '(JI, JK)' if
            table.keys() is ['JI', 'JK']
            :param stmt: statement to update
            :param table: dictionnary retruned by the decode function
            :param kind: kind of mnh directives: 'array' or 'where'
                                                 or None if transformation is not governed by
                                                 mnh directive
            :param scope: current scope
            """

            def addExtra(node, extra):
                """Helper function to add indentation spaces"""
                if extra != 0 and (node.tail is not None) and '\n' in node.tail:
                    # We add indentation after new line only
                    # - if tail already contains a '\n' to discard
                    #   a-stmt followed by a comment
                    #   or '&' immediatly followed by something at the beginning of a line
                    # - if not folowed by another new line (with optional space in between)
                    node.tail = re.sub(r"(\n[ ]*)(\Z|[^\n ]+)",
                                       r"\1" + extra * ' ' + r"\2", node.tail)

            addExtra(stmt, extraindent)  # Set indentation for the *next* node
            if tag(stmt) == 'C':
                pass
            elif tag(stmt) == 'cpp':
                i = list(parent).index(stmt)
                if i == 0:
                    # In this case, it would be a solution to add an empty comment before the
                    # node stmt to easilty control the indentation contained in the tail
                    raise PYFTError("How is it possible?")
                parent[i - 1].tail = parent[i - 1].tail.rstrip(' ')  # Remove the indentation
            elif tag(stmt) == 'a-stmt':
                sss = stmt.findall('./{*}E-1/{*}named-E/{*}R-LT/{*}array-R/' +
                                   '{*}section-subscript-LT/{*}section-subscript')
                if len([ss for ss in sss if ':' in alltext(ss)]) != len(table):
                    raise PYFTError("Inside code sections to transform in DO loops, " +
                                    "all affectations must use ':'.\n" +
                                    "This is not the case in:\n{stmt}".format(stmt=alltext(stmt)))
                if stmt.find('./{*}E-1/{*}named-E/{*}N').tail is not None and kind is not None:
                    raise PYFTError("To keep the compatibility with the filepp version of loop " +
                                    "expansion, nothing must appear between array names and " +
                                    "opening parethesis inside mnh directive sections.")
                # We loop on named-E nodes (and not directly on array-R nodes to prevent using
                # the costly getParent)
                for namedE in stmt.findall('.//{*}R-LT/..'):
                    scope.arrayR2parensR(namedE, table)  # Replace slices by variable
                for cnt in stmt.findall('.//{*}cnt'):
                    addExtra(cnt, extraindent)  # Add indentation after continuation characters
            elif tag(stmt) == 'if-stmt':
                logging.warning(
                    "An if statement is inside a code section transformed in DO loop in %s",
                    scope.getFileName())
                # Update the statement contained in the action node
                updateStmt(stmt.find('./{*}action-stmt')[0], table, kind, 0, stmt, scope)
            elif tag(stmt) == 'if-construct':
                logging.warning(
                    "An if construct is inside a code section transformed in DO loop in %s",
                    scope.getFileName())
                # Loop over the blocks: if, elseif, else
                for ifBlock in stmt.findall('./{*}if-block'):
                    for child in ifBlock:  # Loop over each statement inside the block
                        if tag(child) not in ('if-then-stmt', 'else-if-stmt',
                                              'else-stmt', 'end-if-stmt'):
                            updateStmt(child, table, kind, extraindent, ifBlock, scope)
                        else:
                            # Update indentation because the loop is here and not in recur
                            addExtra(child, extraindent)
                            for cnt in child.findall('.//{*}cnt'):
                                # Add indentation spaces after continuation characters
                                addExtra(cnt, extraindent)
            elif tag(stmt) == 'where-stmt':
                # Where statement becomes if statement
                stmt.tag = f'{{{NAMESPACE}}}if-stmt'
                stmt.text = 'IF (' + stmt.text.split('(', 1)[1]
                # Update the action part
                updateStmt(stmt.find('./{*}action-stmt')[0], table, kind,
                           extraindent, stmt, scope)
                mask = stmt.find('./{*}mask-E')
                mask.tag = f'{{{NAMESPACE}}}condition-E'  # rename the condition tag
                for namedE in mask.findall('.//{*}R-LT/..'):
                    scope.arrayR2parensR(namedE, table)  # Replace slices by variable
                for cnt in stmt.findall('.//{*}cnt'):
                    addExtra(cnt, extraindent)  # Add indentation after continuation characters
            elif tag(stmt) == 'where-construct':
                if kind != 'where' and kind is not None:
                    raise PYFTError('To keep the compatibility with the filepp version of loop " + \
                                    "expansion, no where construct must appear " + \
                                    "in mnh_expand_array blocks.')
                # Where construct becomes if construct
                stmt.tag = f'{{{NAMESPACE}}}if-construct'
                # Loop over the blocks (where, elsewhere)
                for whereBlock in stmt.findall('./{*}where-block'):
                    whereBlock.tag = f'{{{NAMESPACE}}}if-block'
                    for child in whereBlock:  # Loop over each statement inside the block
                        if tag(child) == 'end-where-stmt':
                            # rename ENDWHERE into ENDIF
                            child.tag = f'{{{NAMESPACE}}}end-if-stmt'
                            child.text = 'END IF'
                            # Update indentation because the loop is here and not in recur
                            addExtra(child, extraindent)
                        elif tag(child) in ('where-construct-stmt', 'else-where-stmt'):
                            # Update indentation because the loop is here and not in recur
                            addExtra(child, extraindent)
                            if tag(child) == 'where-construct-stmt':
                                # rename WHERE into IF (the THEN part is attached to the condition)
                                child.tag = f'{{{NAMESPACE}}}if-then-stmt'
                                child.text = 'IF (' + child.text.split('(', 1)[1]
                            else:
                                # In where construct the same ELSEWHERE keyword is used with or
                                # without mask. Whereas for if structure ELSEIF is used with a
                                # condition and ELSE without condition
                                if '(' in child.text:
                                    # rename ELSEWHERE into ELSEIF
                                    child.tag = f'{{{NAMESPACE}}}else-if-stmt'
                                    child.text = 'ELSE IF (' + child.text.split('(', 1)[1]
                                else:
                                    # rename ELSEWHERE into ELSE
                                    child.tag = f'{{{NAMESPACE}}}else-stmt'
                                    child.text = 'ELSE'
                            for mask in child.findall('./{*}mask-E'):  # would a find be enough?
                                # add THEN
                                mask.tag = f'{{{NAMESPACE}}}condition-E'
                                mask.tail += ' THEN'
                                for namedE in mask.findall('.//{*}R-LT/..'):
                                    # Replace slices by variable in the condition
                                    scope.arrayR2parensR(namedE, table)
                            for cnt in child.findall('.//{*}cnt'):
                                # Add indentation spaces after continuation characters
                                addExtra(cnt, extraindent)
                        else:
                            updateStmt(child, table, kind, extraindent, whereBlock, scope)
            else:
                raise PYFTError('Unexpected tag found in mnh_expand ' +
                                'directives: {t}'.format(t=tag(stmt)))
            return stmt

        def closeLoop(loopdesc):
            """Helper function to deal with indentation"""
            if loopdesc:
                inner, outer, indent, extraindent = loopdesc
                if inner[-2].tail is not None:
                    # tail of last statement in DO loop before transformation
                    outer.tail = inner[-2].tail[:-extraindent]
                inner[-2].tail = '\n' + (indent + extraindent - 2) * ' '  # position of the ENDDO
            return False

        toinsert = []  # list of nodes to insert
        toremove = []  # list of nodes to remove
        newVarList = []  # list of new variables

        def recur(elem, scope):
            inMnh = False  # are we in a DO loop created by a mnh directive
            inEverywhere = False  # are we in a created DO loop (except if done with mnh directive)
            tailSave = {}  # Save tail before transformation (to retrieve original indentation)
            for ie, sElem in enumerate(list(elem)):  # we loop on elements in the natural order
                if tag(sElem) == 'C' and sElem.text.lstrip(' ').startswith('!$mnh_expand') and \
                   useMnhExpand:
                    # This is an opening mnh directive
                    if inMnh:
                        raise PYFTError('Nested mnh_directives are not allowed')
                    inMnh = True
                    inEverywhere = closeLoop(inEverywhere)  # close other loop if needed

                    # Directive decoding
                    table, kind = decode(sElem.text)
                    # indentation of next statement
                    indent = len(sElem.tail) - len(sElem.tail.rstrip(' '))
                    toremove.append((elem, sElem))  # we remove the directive itself
                    if ie != 0:
                        # We add, to the tail of the previous node, the tail of
                        # the directive (except one \n)
                        if elem[ie - 1].tail is None:
                            elem[ie - 1].tail = ''
                        elem[ie - 1].tail += sElem.tail.replace('\n', '', 1).rstrip(' ')

                    # Building acc loop collapse independent directive
                    if addAccIndependentCollapse:
                        accCollapse = createElem('C', text='!$acc loop independent collapse(' +
                                                 str(len(table.keys())) + ')',
                                                 tail='\n' + indent * ' ')
                        toinsert.append((elem, accCollapse, ie))

                    # Building loop
                    inner, outer, extraindent = scope.createDoConstruct(table, indent=indent,
                                                                        concurrent=concurrent)
                    toinsert.append((elem, outer, ie))  # Place to insert the loop

                elif (tag(sElem) == 'C' and
                      sElem.text.lstrip(' ').startswith('!$mnh_end_expand') and useMnhExpand):
                    # This is a closing mnh directive
                    if not inMnh:
                        raise PYFTError('End mnh_directive found before begin directive ' +
                                        'in {f}'.format(f=scope.getFileName()))
                    if (table, kind) != decode(sElem.text):
                        raise PYFTError("Opening and closing mnh directives must be conform " +
                                        "in {f}".format(f=scope.getFileName()))
                    inMnh = False
                    toremove.append((elem, sElem))  # we remove the directive itself
                    # We add, to the tail of outer DO loop, the tail of the
                    # directive (except one \n)
                    # pylint: disable-next=undefined-loop-variable
                    outer.tail += sElem.tail.replace('\n', '', 1)  # keep all but one new line char
                    # previous item controls the position of ENDDO
                    elem[ie - 1].tail = elem[ie - 1].tail[:-2]

                elif inMnh:
                    # This statement is between the opening and closing mnh directive
                    toremove.append((elem, sElem))  # we remove it from its old place
                    inner.insert(-1, sElem)  # Insert first in the DO loop
                    # then update, providing new parent in argument
                    updateStmt(sElem, table, kind, extraindent, inner, scope)

                elif everywhere and tag(sElem) in ('a-stmt', 'if-stmt', 'where-stmt',
                                                   'where-construct'):
                    # This node could contain array-syntax

                    # Is the node written using array-syntax? Getting the first array...
                    if tag(sElem) == 'a-stmt':
                        # Left side of the assignment
                        arr = sElem.find('./{*}E-1/{*}named-E/{*}R-LT/{*}array-R/../..')
                        # Right side
                        isMemSet = False
                        isCopy = False
                        nodeE2 = sElem.find('./{*}E-2')
                        # Number of arrays using array-syntax
                        num = len(nodeE2.findall('.//{*}array-R'))
                        if num == 0:
                            # It is an array initialisation when there is no array-syntax
                            # on the right side
                            # If array-syntax is used without explicit '(:)', it could be
                            # detected as an initialisation
                            isMemSet = True
                        elif (len(nodeE2) == 1 and tag(nodeE2[0]) == 'named-E' and num == 1 and
                              nodeE2[0].find('.//{*}parens-R') is None):
                            # It is an array copy when there is only one child in the right
                            # hand side and this child is a named-E and this child contains only
                            # one array-R node and no parens-R
                            isCopy = True
                        # Discard?
                        if (isMemSet and not updateMemSet) or (isCopy and not updateCopy):
                            arr = None
                    elif tag(sElem) == 'if-stmt':
                        # We only deal with assignment in the if statement case
                        arr = sElem.find('./{*}action-stmt/{*}a-stmt/{*}E-1/' +
                                         '{*}named-E/{*}R-LT/{*}array-R/../..')
                        if arr is not None:
                            # In this case we transform the if statement into an if-construct
                            scope.changeIfStatementsInIfConstructs(singleItem=sElem)
                            recur(sElem, scope)  # to transform the content of the if
                            arr = None  # to do nothing more on this node
                    elif tag(sElem) == 'where-stmt':
                        arr = sElem.find('./{*}mask-E//{*}named-E/{*}R-LT/{*}array-R/../..')
                    elif tag(sElem) == 'where-construct':
                        arr = sElem.find('./{*}where-block/{*}where-construct-stmt/' +
                                         '{*}mask-E//{*}named-E/{*}R-LT/{*}array-R/../..')

                    # Check if it is written using array-syntax and must not be excluded;
                    # then compute bounds
                    if arr is None:
                        # There is no array-syntax
                        newtable = None
                    elif len(set(alltext(a).count(':')
                                 for a in sElem.findall('.//{*}R-LT/{*}array-R'))) > 1:
                        # All the elements written using array-syntax don't have the same rank
                        # (can be due to function calls, eg: "X(:)=FUNC(Y(:,:))")
                        newtable = None
                    elif len(set(['ALL', 'ANY', 'COSHAPE', 'COUNT', 'CSHIFT', 'DIMENSION',
                                  'DOT_PRODUCT', 'EOSHIFT', 'LBOUND', 'LCOBOUND', 'MATMUL',
                                  'MAXLOC', 'MAXVAL', 'MERGE', 'MINLOC', 'MINVAL', 'PACK',
                                  'PRODUCT', 'REDUCE', 'RESHAPE', 'SHAPE', 'SIZE', 'SPREAD',
                                  'SUM', 'TRANSPOSE', 'UBOUND', 'UCOBOUND', 'UNPACK'] +
                                 (funcList if funcList is not None else [])
                                 ).intersection(set(n2name(nodeN) for nodeN
                                                    in sElem.findall('.//{*}named-E/{*}N')))) > 0:
                        # At least one intrinsic array function is used
                        newtable = None
                    else:
                        # Guess a variable name
                        if arr is not None:
                            newtable, varNew = scope.findArrayBounds(arr, loopVar, newVarList)
                            for var in varNew:
                                var['new'] = True
                                if var not in newVarList:
                                    newVarList.append(var)
                        else:
                            newtable = None

                    if newtable is None:
                        # We cannot convert the statement (not in array-syntax,
                        # excluded or no variable found to loop)
                        inEverywhere = closeLoop(inEverywhere)  # close previous loop if needed
                    else:
                        # we have to transform the statement
                        if not (inEverywhere and table == newtable):
                            # No opened previous loop, or not coresponding
                            inEverywhere = closeLoop(inEverywhere)  # close previous loop, if needed
                            # We must create a DO loop
                            if ie != 0 and elem[ie - 1].tail is not None:
                                # Indentation of the current node, attached to the previous sibling
                                # get tail before transformation
                                tail = tailSave.get(elem[ie - 1], elem[ie - 1].tail)
                                indent = len(tail) - len(tail.rstrip(' '))
                            else:
                                indent = 0
                            table = newtable  # save the information on the newly build loop
                            kind = None  # not built from mnh directives
                            # Building loop
                            inner, outer, extraindent = scope.createDoConstruct(
                                table, indent=indent, concurrent=concurrent)
                            toinsert.append((elem, outer, ie))  # place to insert the loop
                            inEverywhere = (inner, outer, indent, extraindent)  # we are now in loop
                        tailSave[sElem] = sElem.tail  # save tail for future indentation computation
                        toremove.append((elem, sElem))  # we remove it from its old place
                        inner.insert(-1, sElem)  # Insert first in the DO loop
                        # then update, providing new parent in argument
                        updateStmt(sElem, table, kind, extraindent, inner, scope)
                        if not reuseLoop:
                            # Prevent from reusing this DO loop
                            inEverywhere = closeLoop(inEverywhere)

                else:
                    inEverywhere = closeLoop(inEverywhere)  # close loop if needed
                    if len(sElem) >= 1:
                        # Iteration
                        recur(sElem, scope)
            inEverywhere = closeLoop(inEverywhere)

        for scope in self.getScopes():
            recur(scope, scope)
        # First, element insertion by reverse order (in order to keep the insertion index correct)
        for elem, outer, ie in toinsert[::-1]:
            elem.insert(ie, outer)
        # Then, suppression
        for parent, elem in toremove:
            parent.remove(elem)
        # And variable creation
        self.addVar([(v['scopePath'], v['n'], f"INTEGER :: {v['n']}", None)
                     for v in newVarList])

    @debugDecor
    @noParallel
    @updateTree('signal')
    @updateVarList
    def inlineContainedSubroutines(self, simplify=False, loopVar=None):
        """
        Inline all contained subroutines in the main subroutine
        Steps :
            - Identify contained subroutines
            - Look for all CALL statements, check if it is a containted routines; if yes, inline
            - Delete the containted routines
        :param simplify: try to simplify code (construct or variables becoming useless)
        :param loopVar: None to create new variable for each added DO loop (around ELEMENTAL
                            subroutine calls)
                        or a function that return the name of the variable to use for the
                        loop control.
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
        # Start by nested contained subroutines call, and end up with the last index = the main
        # subroutine to treat
        scopes.reverse()
        # Loop on all subroutines (main + contained)
        for scope in [scope for scope in scopes if scope.path.count('sub:') >= 1]:
            # Loop on all CALL statements
            for callStmtNn in scope.findall('.//{*}call-stmt/{*}procedure-designator/' +
                                            '{*}named-E/{*}N/{*}n'):
                for containedRoutine in [cr for cr in containedRoutines
                                         if alltext(callStmtNn) == cr]:
                    # name of the routine called = a contained subroutine => inline
                    self.inline(containedRoutines[containedRoutine],
                                self.getParent(callStmtNn, level=4),
                                scope,
                                simplify=simplify, loopVar=loopVar)

        for scope in scopes:  # loop on all subroutines
            if scope.path.count('sub:') >= 2:
                # This is a contained subroutine
                name = scope.path.split(':')[-1].upper()  # Subroutine name
                # All nodes refering the subroutine
                nodes = [nodeN for nodeN in self.findall('.//{*}N')
                         if n2name(nodeN).upper() == name]
                if all(nodeN in scope.iter() for nodeN in nodes):
                    # Subroutine name not used (apart in its definition scope),
                    # we suppress it from the CONTAINS part
                    self.remove(scope)
                    self.tree.signal(self)  # Tree must be updated, only in this case

        if simplify:
            self.removeEmptyCONTAINS()

    @debugDecor
    @noParallel
    @updateTree()
    @updateVarList
    def inline(self, subContained, callStmt, mainScope,
               simplify=False, loopVar=None):
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
        :param mainScope: scope of the main (calling) subroutine
        :param simplify: try to simplify code (construct or variables becoming useless)
        :param loopVar: None to create new variable for each added DO loop (around ELEMENTAL
                            subroutine calls)
                        or a function that return the name of the variable to use for the
                        loop control.
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
            Replace PRESENT(var) by .TRUE. if val is True, by .FALSE. otherwise on node
            if var is found
            :param node: xml node to work on (a contained subroutine)
            :param var: string of the name of the optional variable to check
            """
            for namedE in node.findall('.//{*}named-E/{*}N/..'):
                if n2name(namedE.find('./{*}N')).upper() == 'PRESENT':
                    presentarg = n2name(namedE.find('./{*}R-LT/{*}parens-R/{*}element-LT/'
                                                    '{*}element/{*}named-E/{*}N'))
                    if presentarg.upper() == var.upper():
                        for nnn in namedE[:]:
                            namedE.remove(nnn)
                        namedE.tag = f'{{{NAMESPACE}}}literal-E'
                        namedE.text = '.TRUE.' if val else '.FALSE.'

        # Get parent of callStmt
        parent = mainScope.getParent(callStmt)

        # Expand the if-construct if the call-stmt is in a one-line if-construct
        if tag(parent) == 'action-stmt':
            mainScope.changeIfStatementsInIfConstructs(mainScope.getParent(parent))
            parent = mainScope.getParent(callStmt)  # update parent

        # Specific case for ELEMENTAL subroutines
        # Introduce DO-loops if it is called on arrays
        prefix = subContained.findall('.//{*}prefix')
        if len(prefix) > 0 and 'ELEMENTAL' in [p.text.upper() for p in prefix]:
            # Add missing parentheses
            mainScope.addArrayParenthesesInNode(callStmt)

            # Add explcit bounds
            mainScope.addExplicitArrayBounds(node=callStmt)

            # Detect if subroutine is called on arrays
            arrayRincallStmt = callStmt.findall('.//{*}array-R')
            if len(arrayRincallStmt) > 0:  # Called on arrays
                # Look for an array affectation to guess the DO loops to put around the call
                table, _ = mainScope.findArrayBounds(mainScope.getParent(arrayRincallStmt[0], 2),
                                                     loopVar)

                # Add declaration of loop index if missing
                for varName in table.keys():
                    if not mainScope.varList.findVar(varName):
                        var = {'as': [], 'asx': [],
                               'n': varName, 'i': None, 't': 'INTEGER', 'arg': False,
                               'use': False, 'opt': False, 'allocatable': False,
                               'parameter': False, 'init': None, 'scopePath': mainScope.path}
                        mainScope.addVar([[mainScope.path, var['n'],
                                           mainScope.varSpec2stmt(var), None]])

                # Create the DO loops
                inner, outer, _ = mainScope.createDoConstruct(table)

                # Move the call statement in the DO loops
                inner.insert(-1, callStmt)  # callStmt in the DO-loops
                # DO-loops near the original call stmt
                parent.insert(list(parent).index(callStmt), outer)
                parent.remove(callStmt)  # original call stmt removed
                parent = inner  # Update parent
                for namedE in callStmt.findall('./{*}arg-spec/{*}arg/{*}named-E'):
                    # Replace slices by indexes if any
                    if namedE.find('./{*}R-LT'):
                        mainScope.arrayR2parensR(namedE, table)

        # Deep copy the object to possibly modify the original one multiple times
        node = copy.deepcopy(subContained)

        # Get local variables that are not present in the main routine for later addition
        localVarToAdd = []
        subst = []
        varList = copy.deepcopy(self.varList)  # Copy to be able to update it with pending changes
        for var in [var for var in varList.restrict(subContained.path, True)
                    if not var['arg'] and not var['use']]:

            if varList.restrict(mainScope.path, True).findVar(var['n']):
                # Variable is already defined in main or upper, there is a name conflict,
                # the local variable must be renamed before being declared in the main routine
                newName = re.sub(r'_\d+$', '', var['n'])
                i = 1
                while (varList.restrict(subContained.path, True).findVar(newName + '_' + str(i)) or
                       varList.restrict(mainScope.path, True).findVar(newName + '_' + str(i))):
                    i += 1
                newName += '_' + str(i)
                node.renameVar(var['n'], newName)
                subst.append((var['n'], newName))
                var['n'] = newName
            # important for varList.findVar(.., mainScope.path) to find it
            var['scopePath'] = mainScope.path
            localVarToAdd.append(var)

        # In case a substituted variable is used in the declaration of another variable
        for oldName, newName in subst:
            for var in localVarToAdd + varList[:]:
                if var['as'] is not None:
                    var['as'] = [[re.sub(r'\b' + oldName + r'\b', newName, dim[i])
                                  if dim[i] is not None else None
                                  for i in (0, 1)]
                                 for dim in var['as']]

        # Remove all objects that is implicit none, comment or else until reach
        # something interesting
        # USE statements are stored for later user
        # subroutine-stmt and end-subroutine-stmt are kept to ensure consistency
        # (for removeStmtNode with simplify)
        localUseToAdd = node.findall('./{*}use-stmt')
        for sNode in node.findall('./{*}T-decl-stmt') + localUseToAdd + \
                node.findall('./{*}implicit-none-stmt'):
            node.remove(sNode)
        while tag(node[1]) == 'C' and not node[1].text.startswith('!$acc'):
            node.remove(node[1])

        # Variable correspondance
        # For each dummy argument, we look for the calling arg name and shape
        # CALL FOO(Z(:))
        # SUBROUTINE FOO(P)
        # variable = {'P':{'name': 'Z', dim=[':']}}
        vartable = {}  # ordered dict
        for argN in subContained.findall('.//{*}subroutine-stmt/{*}dummy-arg-LT/{*}arg-N'):
            vartable[alltext(argN).upper()] = None  # Not present by default
        for iarg, arg in enumerate(callStmt.findall('.//{*}arg')):
            key = arg.find('.//{*}arg-N')
            if key is not None:
                # arg is VAR=value
                dummyName = alltext(key).upper()
                argnode = arg[1]
            else:
                dummyName = list(vartable.keys())[iarg]
                argnode = arg[0]
            nodeRLTarray = argnode.findall('.//{*}R-LT/{*}array-R')
            if len(nodeRLTarray) > 0:
                # array
                if len(nodeRLTarray) > 1 or \
                   argnode.find('./{*}R-LT/{*}array-R') is None or \
                   tag(argnode) != 'named-E':
                    # Only simple cases are treated
                    raise PYFTError('Argument to complicated: ' + str(alltext(argnode)))
                dim = nodeRLTarray[0].find('./{*}section-subscript-LT')[:]
            else:
                dim = None
            if dim is None:
                # A%B => argname = 'A%B'
                # Z(1) => argname = 'Z(1)'
                argname = "".join(argnode.itertext())
            else:
                # Z(:) => argname = 'Z'
                tmp = copy.deepcopy(argnode)
                nodeRLT = tmp.find('./{*}R-LT')
                nodeRLT.remove(nodeRLT.find('./{*}array-R'))
                argname = "".join(tmp.itertext())
            vartable[dummyName] = {'node': argnode, 'name': argname, 'dim': dim}

        # Look for PRESENT(var) and replace it by True when variable is present, by False otherwise
        for dummyName in [dummyName for (dummyName, value) in vartable.items()
                          if value is not None]:
            setPRESENTby(node, dummyName, True)
        for dummyName in [dummyName for (dummyName, value) in vartable.items()
                          if value is None]:
            setPRESENTby(node, dummyName, False)

        # Look for usage of variable not present and delete corresponding code
        for dummyName in [dummyName for (dummyName, value) in vartable.items() if value is None]:
            for nodeN in [nodeN for nodeN in node.findall('.//{*}named-E/{*}N')
                          if n2name(nodeN).upper() == dummyName]:
                removed = False
                # Parent is the  named-E node, we need at least the upper level
                par = node.getParent(nodeN, level=2)
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
                        # the variable but must contain other statements using the variable.
                        # We target the inner statement in the previous cases.
                        # Some cases may have been overlooked and should be added above.
                        raise PYFTError(("We shouldn't be here. A case may have been " +
                                         "overlooked (tag={tag}).".format(tag=tagName)))
                    if toSuppress is not None:
                        removed = True
                        if toSuppress not in allreadySuppressed:
                            # We do not simplify variables to prevent side-effect with
                            # the variable renaming
                            node.removeStmtNode(toSuppress, False, simplify)
                            allreadySuppressed.extend(list(toSuppress.iter()))
                    else:
                        par = node.getParent(par)

        # Loop on the dummy argument
        for name, dummy in vartable.items():
            # Loop on all variables in the contained routine
            # It is important to build again the list of nodes, because it may have
            # changed during the previous dummy argument substitution
            for namedE in [namedE for namedE in node.findall('.//{*}named-E/{*}N/{*}n/../..')
                           if n2name(namedE.find('{*}N')).upper() == name]:
                # 0 Concatenation of n nodes (a name could be split over several n nodes)
                nodeN = namedE.find('./{*}N')
                ns = nodeN.findall('./{*}n')
                ns[0].text = n2name(nodeN)
                for nnn in ns[1:]:
                    nodeN.remove(nnn)

                # 1 We get info about variables (such as declared in the main or in
                # the contained routines)
                descMain = varList.restrict(mainScope.path, True).findVar(dummy['name'])
                descSub = varList.restrict(subContained.path, True).findVar(name)
                # In case variable is used for the declaration of another variable,
                # we must update descSub
                for var in varList:
                    if var['as'] is not None:
                        var['as'] = [[re.sub(r'\b' + name + r'\b', dummy['name'], dim[i])
                                      if dim[i] is not None else None
                                      for i in (0, 1)]
                                     for dim in var['as']]

                # 3 We select the indexes (only for array argument and not structure argument
                # containing an array)
                #  using the occurrence to replace inside the subcontained routine body
                nodeRLT = namedE.find('./{*}R-LT')
                if nodeRLT is not None and tag(nodeRLT[0]) != 'component-R':
                    # The variable name is immediately followed by a parenthesis
                    assert tag(nodeRLT[0]) in ('array-R', 'parens-R'), 'Internal error'
                    slices = nodeRLT[0].findall('./{*}section-subscript-LT/' +
                                                '{*}section-subscript')
                    slices += nodeRLT[0].findall('./{*}element-LT/{*}element')
                else:
                    # No parenthesis
                    if (descMain is not None and len(descMain['as']) > 0) or \
                       len(descSub['as']) > 0 or dummy['dim'] is not None:
                        # No parenthesis, but this is an array, we add as many ':' as needed
                        if len(descSub['as']) > 0:
                            ndim = len(descSub['as'])
                        else:
                            # ELEMENTAL routine, dummy arg is scalar
                            if dummy['dim'] is not None:
                                # We use the variable passed as argument because
                                # parenthesis were used
                                ndim = len([d for d in dummy['dim'] if ':' in alltext(d)])
                            else:
                                # We use the declared version in main
                                ndim = len(descMain['as'])
                        ns[0].text += '(' + (', '.join([':'] * ndim)) + ')'
                        updatedNamedE = createExprPart(alltext(namedE))
                        namedE.tag = updatedNamedE.tag
                        namedE.text = updatedNamedE.text
                        for nnn in namedE[:]:
                            namedE.remove(nnn)
                        namedE.extend(updatedNamedE[:])
                        slices = namedE.find('./{*}R-LT')[0].findall(
                            './{*}section-subscript-LT/{*}section-subscript')
                    else:
                        # This is not an array
                        slices = []

                # 4 New name (the resultig xml is not necessarily a valid fxtran xml)
                namedE.find('./{*}N')[0].text = dummy['name']

                # 5 We update the indexes to take into account a different declaration
                # (lower bound especially)
                #  in the main and in the contained routines.
                #  Moreover, we could need to add indexes
                if len(slices) > 0:
                    # This is an array
                    for isl, sl in enumerate(slices):
                        # 0 Compute bounds for array
                        if len(descSub['as']) == 0 or descSub['as'][isl][1] is None:
                            # ELEMENTAL or array with implicit shape
                            for i in (0, 1):  # 0 for lower bound and 1 for upper bound
                                if dummy['dim'] is not None:
                                    # Parenthesis in the call statement
                                    tagName = './{*}lower-bound' if i == 0 else './{*}upper-bound'
                                    descSub[i] = dummy['dim'][isl].find(tagName)
                                else:
                                    descSub[i] = None
                                if descSub[i] is not None:
                                    # lower/upper limit was given in the call statement
                                    descSub[i] = alltext(descSub[i])
                                else:
                                    # if available we take lower/upper limit set in the declaration
                                    if descMain is not None and descMain['as'][isl][1] is not None:
                                        # Declaration found in main, and not using implicit shape
                                        descSub[i] = descMain['as'][isl][i]
                                        if i == 0 and descSub[i] is None:
                                            descSub[i] = '1'  # Default FORTRAN value
                                    else:
                                        descSub[i] = "L" if i == 0 else "U"
                                        descSub[i] += "BOUND({name}, {isl})".format(
                                            name=dummy['name'], isl=isl + 1)
                        else:
                            descSub[0] = descSub['as'][isl][0]
                            if descSub[0] is None:
                                descSub[0] = '1'  # Default FORTRAN value
                            descSub[1] = descSub['as'][isl][1]

                        # 1 Offset computation
                        #   if only a subset is passed
                        #   and/or if lower bound of array is different in main and in sub
                        #       contained routine
                        # REAL, DIMENSION(M1:M2):: Z; CALL FOO(N1:N2)
                        # SUBROUTINE FOO(P); REAL, DIMENSION(K1:K2):: P; P(I1:I2)
                        # If M1 or K1 is not set, they defaults to 1; if not set, N1 defaults to
                        #     M1 and I1 to K1
                        # P(I1:I2)=Z(I1-K1+N1:I2-K1+N1)
                        offset = 0
                        if dummy['dim'] is not None and \
                           not alltext(dummy['dim'][isl]).strip().startswith(':'):
                            offset = alltext(dummy['dim'][isl].find('./{*}lower-bound'))
                        else:
                            if descMain is not None:
                                offset = descMain['as'][isl][0]
                                if offset is None:
                                    offset = '1'  # Default FORTRAN value
                                elif offset.strip().startswith('-'):
                                    offset = '(' + offset + ')'
                            else:
                                offset = "LBOUND({name}, {isl})".format(
                                    name=dummy['name'], isl=isl + 1)
                        if offset.upper() == descSub[0].upper():
                            offset = 0
                        else:
                            if descSub[0].strip().startswith('-'):
                                offset += '- (' + descSub[0] + ')'
                            else:
                                offset += '-' + descSub[0]

                        # 2 Update index with the offset and add indexes instead of ':'
                        if offset != 0:
                            if tag(sl) == 'element' or \
                               (tag(sl) == 'section-subscript' and ':' not in alltext(sl)):
                                # Z(I) or last index of Z(:, I)
                                bounds = sl
                            else:
                                low = sl.find('./{*}lower-bound')
                                if low is None:
                                    low = createElem('lower-bound', tail=sl.text)
                                    low.append(createExprPart(descSub[0]))
                                    sl.text = None
                                    sl.insert(0, low)
                                up = sl.find('./{*}upper-bound')
                                if up is None:
                                    up = createElem('upper-bound')
                                    up.append(createExprPart(descSub[1]))
                                    sl.append(up)
                                bounds = [low, up]
                            for bound in bounds:
                                # bound[-1] is a named-E, literal-E, op-E...
                                if bound[-1].tail is None:
                                    bound[-1].tail = ''
                                bound[-1].tail += '+' + offset  # Not valid fxtran xml

                    # We must add extra indexes
                    # CALL FOO(Z(:,1))
                    # SUBROUTINE FOO(P); REAL, DIMENSION(K1:K2):: P
                    # P(I) => Z(I, 1)
                    if dummy['dim'] is not None and len(dummy['dim']) > len(slices):
                        slices[-1].tail = ', '
                        par = node.getParent(slices[-1])
                        par.extend(dummy['dim'][len(slices):])

                # 6 Convert (wrong) xml into text and into xml again (to obtain a valid fxtran xml)
                #  This double conversion is not sufficient in some case.
                #  E.g. variable (N/n tag) replaced by real value
                updatedNamedE = createExprPart(alltext(namedE))
                namedE.tag = updatedNamedE.tag
                namedE.text = updatedNamedE.text
                for nnn in namedE[:]:
                    namedE.remove(nnn)
                namedE.extend(updatedNamedE[:])

        node.remove(node.find('./{*}subroutine-stmt'))
        node.remove(node.find('./{*}end-subroutine-stmt'))

        # Add local var and use to main routine
        mainScope.addVar([[mainScope.path, var['n'], mainScope.varSpec2stmt(var), None]
                          for var in localVarToAdd])
        mainScope.addModuleVar([[mainScope.path, n2name(useStmt.find('.//{*}module-N//{*}N')),
                                 [n2name(v.find('.//{*}N'))
                                  for v in useStmt.findall('.//{*}use-N')]]
                                for useStmt in localUseToAdd])

        # Remove call statement of the contained routines
        index = list(parent).index(callStmt)
        parent.remove(callStmt)
        if callStmt.tail is not None:
            if node[-1].tail is None:
                node[-1].tail = callStmt.tail
            else:
                node[-1].tail = node[-1].tail + callStmt.tail
        for node in node[::-1]:
            # node is a program-unit, we must insert the subelements
            parent.insert(index, node)

    @debugDecor
    def setFalseIfStmt(self, flags, simplify=False):
        """
        Set to .FALSE. a given boolean fortran flag before removing the node if simplify is True
        :param flags: list of strings of flags to set to .FALSE.
        :param simplify: try to simplify code (if the .FALSE. was alone inside a if-then-endif
                         construct, the construct is removed, and variables used in the if
                         condition are also checked)
        """
        if isinstance(flags, str):
            flags = [flags]
        flags = [flag.upper() for flag in flags]
        for scope in self.getScopes(excludeKinds=['type']):
            singleFalseBlock, multipleFalseBlock = [], []
            # Loop on nodes composing the scope
            for node in scope:
                # Loop on condition nodes
                for cond in node.findall('.//{*}condition-E'):
                    found = False
                    for namedE in [namedE for namedE
                                   in node.findall('.//{*}condition-E//{*}named-E')
                                   if alltext(namedE).upper() in flags]:
                        # This named-E must be replaced by .FALSE.
                        found = True
                        namedE.tag = '{{{NAMESPACE}}}literal-E'
                        namedE.text = '.FALSE.'
                        for item in list(namedE):
                            namedE.remove(item)
                    if found:
                        nodeOpE = cond.find('./{*}op-E')
                        if nodeOpE is not None:
                            # Multiple flags conditions
                            multipleFalseBlock.append(nodeOpE)
                        else:
                            # Solo condition
                            # <if-block><if-then-stmt>
                            if tag(scope.getParent(cond)).startswith('if-stmt'):
                                scope.changeIfStatementsInIfConstructs(scope.getParent(cond))
                            singleFalseBlock.append(scope.getParent(cond, level=2))
            if simplify:
                scope.removeStmtNode(singleFalseBlock, simplify, simplify)
                scope.evalFalseIfStmt(multipleFalseBlock, simplify)

    @debugDecor
    def evalFalseIfStmt(self, nodes, simplify=False):
        """
        Evaluate if-stmt with multiple op-E and remove the nodes if only .FALSE. are present
        :param nodes: list of nodes of type op-E to evaluate (containing .FALSE.)
        :param simplify: try to simplify code (if if-block is removed, variables used in the
                         if condition are also checked)
        """
        nodesTorm = []
        for node in nodes:
            toRemove = True
            for nnn in node:
                if tag(nnn) == 'op' or (tag(nnn) == 'literal-E' and '.FALSE.' in alltext(nnn)):
                    pass
                else:
                    toRemove = False
                    break
            if toRemove:
                # <if-block><if-then-stmt><condition-E>
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
        log = logging.error if mustRaise else logging.warning
        for arg in self.findall('.//{*}call-stmt/{*}arg-spec/{*}arg/{*}op-E'):
            log(("The call argument {} is an operation, in file '{}'"
                 ).format(alltext(arg).replace('\n', ' \\n '), self.getFileName()))
            ok = False
        if not ok and mustRaise:
            raise PYFTError(("There are call arguments which are operations in file '{}'"
                             ).format(self.getFileName()))

    @debugDecor
    def insertStatement(self, stmt, first):
        """
        Insert a statement to be executed first (or last)
        :param stmt: statement to insert
        :param first: True to insert it in first position, False to insert it in last position
        :return: the index of the stmt inserted in scope
        """
        # pylint: disable=unsubscriptable-object
        if first:
            # Statement must be inserted after all use, T-decl, implicit-non-stmt,
            # interface and cray pointers
            nodes = self.findall('./{*}T-decl-stmt') + self.findall('./{*}use-stmt') + \
                    self.findall('./{*}implicit-none-stmt') + \
                    self.findall('./{*}interface-construct') + \
                    self.findall('./{*}pointer-stmt')
            if len(nodes) > 0:
                # Insertion after the last node
                index = max([list(self).index(n) for n in nodes]) + 1
            else:
                # Insertion after the subroutine or function node
                index = 2
            # If an include statements follows, it certainly contains an interface
            while (tag(self[index]) in ('C', 'include', 'include-stmt') or
                   (tag(self[index]) == 'cpp' and self[index].text.startswith('#include'))):
                if not self[index].text.startswith('!$acc'):
                    index += 1
                else:
                    break
        else:
            # Statement must be inserted before the contains statement
            contains = self.find('./{*}contains-stmt')
            if contains is not None:
                # Insertion before the contains statement
                index = list(self).index(contains)
            else:
                # Insertion before the end subroutine or function statement
                index = -1
        if self[index - 1].tail is None:
            self[index - 1].tail = '\n'
        elif '\n' not in self[index - 1].tail:
            self[index - 1].tail += '\n'
        self.insert(index, stmt)
        return index

    @debugDecor
    def removeStmtNode(self, nodes, simplifyVar, simplifyStruct):
        """
        This function removes a statement node and:
          - suppress variable that became useless (if simplifyVar is True)
          - suppress outer loop/if if useless (if simplifyStruct is True)
        :param nodes: node (or list of nodes) to remove
        :param simplifyVar: try to simplify code (if we delete "CALL FOO(X)" and if X not used
                            else where, we also delete it; or if the call was alone inside a
                            if-then-endif construct, with simplifyStruct=True, the construct is also
                            removed, and variables used in the if condition are also checked...)
        :param simplifyStruct: try to simplify code (if we delete "CALL FOO(X)" and if the call was
                               alone inside a if-then-endif construct, the construct is also
                               removed and variables used in the if condition
                               (with simplifyVar=True) are also checked...)
        """

        # In case the suppression of an if-stmt or where-stmt is asked,
        # we must start by the inner statement
        nodesToSuppress = []
        if not isinstance(nodes, list):
            nodes = [nodes]
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

        varToCheck = []  # List of variables to check for suppression
        if simplifyVar:
            # Loop to identify all the potential variables to remove
            for node in nodesToSuppress:
                scopePath = self.getScopePath(node)
                if tag(node) == 'do-construct':
                    # Try to remove variables used in the loop
                    varToCheck.extend([(scopePath, n2name(arg))
                                       for arg in node.find('./{*}do-stmt').findall('.//{*}N')])
                elif tag(node) in ('if-construct', 'if-stmt'):
                    # Try to remove variables used in the conditions
                    varToCheck.extend([(scopePath, n2name(arg))
                                       for arg in node.findall('.//{*}condition-E//{*}N')])
                elif tag(node) in ('where-construct', 'where-stmt'):
                    # Try to remove variables used in the conditions
                    varToCheck.extend([(scopePath, n2name(arg))
                                       for arg in node.findall('.//{*}mask-E//{*}N')])
                elif tag(node) == 'call-stmt':
                    # We must check if we can suppress the variables used to call the subprogram
                    varToCheck.extend([(scopePath, n2name(arg))
                                       for arg in node.findall('./{*}arg-spec//{*}N')])
                    # And maybe, the subprogram comes from a module
                    varToCheck.append((scopePath,
                                       n2name(node.find('./{*}procedure-designator//{*}N'))))
                elif tag(node) in ('a-stmt', 'print-stmt'):
                    varToCheck.extend([(scopePath, n2name(arg)) for arg in node.findall('.//{*}N')])
                elif tag(node) == 'selectcase-construct':
                    # Try to remove variables used in the selector and in conditions
                    varToCheck.extend([(scopePath, n2name(arg))
                                       for arg in node.findall('.//{*}case-E//{*}N')])
                    varToCheck.extend([(scopePath, n2name(arg))
                                       for arg in node.findall('.//{*}case-value//{*}N')])

        # Node suppression
        parents = {}  # cache
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
                if previous.tail is None:
                    previous.tail = ''
                previous.tail = (previous.tail.replace('\n', '') +
                                 (node.tail if node.tail is not None else ''))
            parent.remove(node)

        # Variable simplification
        self.removeVarIfUnused(varToCheck, excludeDummy=True,
                               excludeModule=True, simplify=simplifyVar)

        # List the new nodes to suppress
        newNodesToSuppress = []
        for node in nodesToSuppress:
            parent = parents[id(node)]
            # If we have suppressed the statement in a if statement (one-line if) or where statement
            # we must suppress the entire if/where statement even when simplifyStruct is False
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
        for nnn in newNodesToSuppress:
            if tag(nnn).endswith('-construct'):
                if nnn not in constructNodes:
                    constructNodes.append(nnn)
            else:
                if nnn not in otherNodes:
                    otherNodes.append(nnn)
        # suppress all statements at once
        if len(otherNodes) > 0:
            self.removeStmtNode(otherNodes, simplifyVar, simplifyStruct)
        # suppress construct nodes one by one (recursive call)
        for nnn in constructNodes:
            self.removeConstructNode(nnn, simplifyVar, simplifyStruct)

    @debugDecor
    def removeConstructNode(self, node, simplifyVar, simplifyStruct):
        """
        This function removes a construct node and:
          - suppress variable that became useless (if simplifyVar is True)
          - suppress outer loop/if if useless (if simplifyStruct is True)
        :param node: node representing the statement to remove
        :param simplifyVar: try to simplify code (if we delete "CALL FOO(X)" and if X not used
                            else where, we also delete it; or if the call was alone inside a
                            if-then-endif construct, with simplifyStruct=True, the construct is also
                            removed, and variables used in the if condition are also checked...)
        :param simplifyStruct: try to simplify code (if we delete "CALL FOO(X)" and if the call was
                               alone inside a if-then-endif construct, the construct is also
                               removed, and variables used in the if condition
                               (with simplifyVar=True) are also checked...)

        If a statement is passed, it is suppressed by removeStmtNode
        """
        assert tag(node).endswith('-stmt') or tag(node).endswith('-construct'), \
            "Don't know how to suppress only a part of a structure or of a statement"

        # This function removes inner statement to give a chance to identify and suppress unused
        # variables
        # During this step, nodes are suppressed with simplifyStruct=False to prevent infinite loops
        # then the actual node is removed using removeStmtNode

        if tag(node).endswith('-construct'):
            # inner nodes
            nodes = {'do-construct': _nodesInDo,
                     'if-construct': _nodesInIf,
                     'where-construct': _nodesInWhere,
                     'selectcase-construct': _nodesInCase}[tag(node)](node)
            # sort nodes by type
            constructNodes, otherNodes = [], []
            for nnn in nodes:
                if tag(nnn).endswith('-construct'):
                    constructNodes.append(nnn)
                else:
                    otherNodes.append(nnn)
            # suppress all statements at once
            self.removeStmtNode(otherNodes, simplifyVar, False)
            # suppress construct nodes one by one (recursive call)
            for nnn in constructNodes:
                self.removeConstructNode(nnn, simplifyVar, False)
            # suppress current node
            self.removeStmtNode(node, simplifyVar, simplifyStruct)
        else:
            # At least a-stmt, print-stmt
            self.removeStmtNode(node, simplifyVar, simplifyStruct)

    @staticmethod
    @debugDecor
    def createDoConstruct(loopVariables, indent=0, concurrent=False):
        """
        :param loopVariables: ordered dictionnary with loop variables as key and bounds as values.
                              Bounds are expressed with a 2-tuple.
                              Keys must be in the same order as the order used when addressing an
                                element: if loopVariables.keys is [JI, JK], arrays are
                                addressed with (JI, JK)
        :param indent: current indentation
        :param concurrent: if False, output is made of nested 'DO' loops
                           if True, output is made of a single 'DO CONCURRENT' loop
        :return: (inner, outer, extraindent) with
                  - inner the inner do-construct where statements must be added
                  - outer the outer do-construct to be inserted somewhere
                  - extraindent the number of added indentation
                    (2 if concurrent else 2*len(loopVariables))
        """
        if concurrent:
            # <f:do-construct>
            #   <f:do-stmt>DO CONCURRENT (
            #     <f:forall-triplet-spec-LT>
            #       <f:forall-triplet-spec>
            #         <f:V><f:named-E><f:N><f:n>JIJ</f:n></f:N></f:named-E></f:V>=
            #         <f:lower-bound><f:named-E><f:N><f:n>IIJB</f:n>
            #             </f:N></f:named-E></f:lower-bound>:
            #         <f:upper-bound><f:named-E><f:N><f:n>IIJE</f:n>
            #             </f:N></f:named-E></f:upper-bound>
            #       </f:forall-triplet-spec>,
            #       <f:forall-triplet-spec>
            #         <f:V><f:named-E><f:N><f:n>JK</f:n></f:N></f:named-E></f:V>=
            #         <f:lower-bound><f:literal-E><f:l>1</f:l></f:literal-E></f:lower-bound>:
            #         <f:upper-bound><f:named-E><f:N><f:n>IKT</f:n>
            #             </f:N></f:named-E></f:upper-bound>
            #       </f:forall-triplet-spec>
            #     </f:forall-triplet-spec-LT>)
            #   </f:do-stmt>
            #   statements
            #   <f:end-do-stmt>END DO</f:end-do-stmt>
            # </f:do-construct>
            triplets = []
            # Better for vectorisation with some compilers
            for var, (lo, up) in list(loopVariables.items())[::-1]:
                nodeV = createElem('V', tail='=')
                nodeV.append(createExprPart(var))
                lower, upper = createArrayBounds(lo, up, 'DOCONCURRENT')

                triplet = createElem('forall-triplet-spec')
                triplet.extend([nodeV, lower, upper])

                triplets.append(triplet)

            tripletLT = createElem('forall-triplet-spec-LT', tail=')')
            for triplet in triplets[:-1]:
                triplet.tail = ', '
            tripletLT.extend(triplets)

            dostmt = createElem('do-stmt', text='DO CONCURRENT (', tail='\n')
            dostmt.append(tripletLT)
            enddostmt = createElem('end-do-stmt', text='END DO')

            doconstruct = createElem('do-construct', tail='\n')
            doconstruct.extend([dostmt, enddostmt])
            inner = outer = doconstruct
            doconstruct[0].tail += (indent + 2) * ' '  # Indentation for the statement after DO
        else:
            # <f:do-construct>
            #   <f:do-stmt>DO
            #     <f:do-V><f:named-E><f:N><f:n>JRR</f:n></f:N></f:named-E></f:do-V> =
            #     <f:lower-bound><f:literal-E><f:l>1</f:l></f:literal-E></f:lower-bound>:
            #     <f:upper-bound><f:named-E><f:N><f:n>IKT</f:n></f:N></f:named-E></f:upper-bound>
            #   </f:do-stmt>\n
            #   statements \n
            #   <f:end-do-stmt>END DO</f:end-do-stmt>
            # </f:do-construct>\n
            def makeDo(var, lo, up):
                doV = createElem('do-V', tail='=')
                doV.append(createExprPart(var))
                lower, upper = createArrayBounds(lo, up, 'DO')

                dostmt = createElem('do-stmt', text='DO ', tail='\n')
                dostmt.extend([doV, lower, upper])

                enddostmt = createElem('end-do-stmt', text='END DO')

                doconstruct = createElem('do-construct', tail='\n')
                doconstruct.extend([dostmt, enddostmt])
                return doconstruct

            outer = None
            inner = None
            for i, (var, (lo, up)) in enumerate(list(loopVariables.items())[::-1]):
                doconstruct = makeDo(var, lo, up)
                # Indentation for the statement after DO
                doconstruct[0].tail += (indent + 2 * i + 2) * ' '
                if outer is None:
                    outer = doconstruct
                    inner = doconstruct
                else:
                    inner.insert(1, doconstruct)
                    inner = doconstruct
                    # Indentation for the ENDDO statement
                    doconstruct.tail += (indent + 2 * i - 2) * ' '
        return inner, outer, 2 if concurrent else 2 * len(loopVariables)

    @staticmethod
    @debugDecor
    def insertInList(pos, item, parent):
        """
        :param pos: insertion position
        :param item: item to add to the list
        :param parent: the parent of item (the list)
        """
        # insertion
        if pos < 0:
            pos = len(parent) + 1 + pos
        parent.insert(pos, item)
        if len(parent) > 1:
            i = list(parent).index(item)  # effective position
            if i == len(parent) - 1:
                # The item is the last one
                parent[i - 1].tail = ', '
            else:
                parent[i].tail = ', '

    @debugDecor
    def removeFromList(self, item, itemPar):
        """
        :param item: item to remove from list
        :param itemPar: the parent of item (the list)
        """

        nodesToSuppress = [item]

        # Suppression of the comma
        i = list(itemPar).index(item)
        if item.tail is not None and ',' in item.tail:
            # There's a comma just after the node
            tail = item.tail
            item.tail = tail.replace(',', '')
        elif i != 0 and ',' in itemPar[i - 1].tail:
            # There's a comma just before the node
            tail = itemPar[i - 1].tail
            itemPar[i - 1].tail = tail.replace(',', '')
        else:
            found = False
            # We look for a comma in the first node after the current node that
            # is not after another item of the list
            j = i + 1
            while j < len(itemPar) and not found:
                if nonCode(itemPar[j]):
                    # This is a candidate
                    if itemPar[j].tail is not None and ',' in itemPar[j].tail:
                        # Comma found and suppressed
                        found = True
                        tail = itemPar[j].tail
                        itemPar[j].tail = tail.replace(',', '')
                    else:
                        j += 1
                else:
                    # This is another item
                    break

            # We look for a comma in the last node before the current node that
            # is not a comment or a contiuation character
            j = i - 1
            while j >= 0 and not found:
                if itemPar[j].tail is not None and ',' in itemPar[j].tail:
                    # Comma found and suppressed
                    found = True
                    tail = itemPar[j].tail
                    itemPar[j].tail = tail.replace(',', '')
                else:
                    if nonCode(itemPar[j]):
                        # We can search before
                        j -= 1
                    else:
                        # This is another item
                        break

            if not found and \
               len([e for e in itemPar if not nonCode(e)]) != 1:
                raise RuntimeError("Something went wrong here....")

        # Suppression of continuation characters
        if i + 1 < len(itemPar) and tag(itemPar[i + 1]) == 'cnt':
            # Node is followed by a continuation character
            reason = 'lastOnLine'
            # If the node is followed by a continuation character and is just after another
            # continuation character, we must supress the continuation character which is after.
            #    USE MODD, ONLY: X, &
            #                    Y, & !Variable to suppress, with its '&' character
            #                    Z
            # In addition, if the character found before is at the begining of the line,
            # it must also be removed. To know if it is at the begining of the line,
            # we can check if another continuation character is before.
            #    USE MODD, ONLY: X, &
            #                  & Y, & !Variable to suppress, with both '&' characters
            #                  & Z
        elif len([itemPar[j] for j in range(i + 1, len(itemPar)) if not nonCode(itemPar[j])]) == 0:
            # Node is the last of the list
            reason = 'last'
            # If the removed node is the last of the list and a continuation character
            # is just before. We must suppress it.
            #    USE MODD, ONLY: X, &
            #                    Y !Variable to suppress, with the preceding '&'
            # In addition, if the character found before is at the begining of the line,
            # the preceding one must also be removed.
            #    USE MODD, ONLY: X, &
            #                  & Y !Variable to suppress, with 2 '&'
        else:
            # We must not suppress '&' characters
            reason = None
        if reason is not None:
            def _getPrecedingCnt(itemPar, i):
                """
                Return the index of the preceding node which is a continuation character
                :param itemPar: the list containig the node to suppress
                :param i: the index of the current node, i-1 is the starting index for the search
                :return: a tuple with three elements:
                          - the node containing the preceding '&' character
                          - the parent of the node containing the preceding '&' character
                          - index of the preceding '&' in the parent (previsous element
                            of the tuple)
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
                while j >= 0 and tag(itemPar[j]) == 'C':
                    j -= 1
                if j >= 0 and tag(itemPar[j]) == 'cnt':
                    return itemPar[j], itemPar, j
                if j == -1:
                    # In the following special case, the '&' don't belong to the list but are
                    # siblings of the list.
                    #    USE MODD, ONLY: &
                    #                  & X
                    siblings = self.getSiblings(itemPar, before=True, after=False)
                    j2 = len(siblings) - 1
                    while j2 >= 0 and tag(siblings[j2]) == 'C':
                        j2 -= 1
                    if j2 >= 0 and tag(siblings[j2]) == 'cnt':
                        return siblings[j2], siblings, j2
                return None, None, None
            # We test if the preceding node (excluding comments) is a continuation character
            precCnt, newl, j = _getPrecedingCnt(itemPar, i)
            if precCnt is not None:
                # Preceding node is a continuation character
                nodesToSuppress.append(precCnt if reason == 'last' else itemPar[i + 1])
                if j is not None:
                    precCnt2, _, _ = _getPrecedingCnt(newl, j)
                    if precCnt2 is not None:
                        # There is another continuation character before
                        nodesToSuppress.append(precCnt2 if reason == 'last' else precCnt)

        # Suppression of nodes
        for node in nodesToSuppress:
            # Get the tail of the previous children of the list and append the item's tail
            # to be removed
            if node in itemPar:
                parent = itemPar
            else:
                # parent must be recomputed because previsous removal may have change it
                parent = self.getParent(itemPar)
            i = list(parent).index(node)
            if i != 0 and node.tail is not None:
                if parent[i - 1].tail is None:
                    parent[i - 1].tail = ''
                parent[i - 1].tail = parent[i - 1].tail + node.tail
            parent.remove(node)
