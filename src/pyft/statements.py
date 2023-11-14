"""
This module includes functions to act on statements
"""
import xml.etree.ElementTree as ET
from pyft.util import (copy_doc, n2name, getParent, non_code, getSiblings, debugDecor, 
                       alltext, tostring, getIndexLoop, moveInGrandParent,
                       PYFTError)
from pyft.scope import (getScopeChildNodes, getScopeNode, getScopesList, getScopePath)
from pyft.variables import removeVarIfUnused
from pyft.expressions import createExprPart
        
def convertColonArrayinDim(sub, locNode, varArrayNamesList, varArray, varName):
    """
    Convert ':' in full array dimensions. Example if SIZE(A)=D%NKT, A(:) is converted to A(1:D%NKT) 
    :param sub: section-subscript node from the working node
    :param locNode: scope of the working node
    :param varArrayNamesList: list of all variable arrays names (list of string)
    :param varArray: list of all variables arrays (list of Dictionnaray returned from getVarList)
    :param varName : string of the variable in reading fortran ('A' in the example)
    """  
    # Get the i sub-index of the current object of E-2
    subPar = getParent(locNode,sub)
    for j,el in enumerate(subPar):
        if el == sub:
            indextoTransform=j
            break
    # Get the variable object to find its declaration dimension
    ind=varArrayNamesList.index(varName)
    lowerBound = '1'
    upperBound = str(varArray[ind]['as'][indextoTransform][1]) #e.g. D%NIJT; D%NKT; KSIZE; KPROMA etc
    lowerXml,upperXml = createArrayBounds(lowerBound, upperBound, 'ARRAY')
    sub.insert(0,lowerXml)
    sub.insert(1,upperXml)
    sub.text = '' # Delete the initial ':'    

@debugDecor
def E2StmtToDoStmt(node_astmt):
    """
    Conversion function to remove the array syntax on the first arrayR of E-2 node for elemental subroutine
    For now, this function is used only to adapt array-syntax in CALL of an ELEMENTAL SUBROUTINE: 
    a lot of the functionnality taken from aStmtToDoStmt is not useful (such as onlyNumbers and the if + 2 elif cases within for loop on subsE2)
    :param node_astmt: a-stmt node to work on
    """
    doToBuild = []
    arrayR = node_astmt.findall('.//{*}E-2//{*}array-R')
    if len(arrayR) > 0:
        subsE2=arrayR[0].findall('.//{*}section-subscript')
        for i,sub in enumerate(subsE2):
            if len(sub.findall('.//{*}upper-bound')) == 0:
                if ':' in alltext(sub): # INDEX: e.g. (IKTB:)
                    # Creation of the array-R object needed to be converted futher in parens-R
#                    ind=varArrayNamesList.index(varName)
#                    lowerBound = alltext(sub.findall('.//{*}lower-bound/{*}*/{*}*/{*}n')[0])
#                    upperBound = str(varArray[ind]['as'][i][1])
#                    lowerXml,upperXml = createArrayBounds(lowerBound, upperBound, 'ARRAY')
#                    sub.insert(0,lowerXml)
#                    sub.insert(1,upperXml)
#                    sub.text = '' # Delete the initial ':'
#                    doToBuild.append(createDoConstruct({getIndexLoop(lowerBound, upperBound):(lowerBound, upperBound)})[1])
                    pass # This case does not exit yet in the form of PHYEX 0.5.0 : case of calling an elemental subroutine with calling arg such as A(IKTB:)
                else:  # single literal-E : copy the object (e.g. IKA alone or operation such as IKE+1)
                    pass
            else:
                lowerBounds=sub.findall('.//{*}lower-bound')
                upperBounds=sub.findall('.//{*}upper-bound')
                lowerBound=alltext(lowerBounds[0])
                upperBound=alltext(upperBounds[0])
                if len(sub.findall('.//{*}literal-E')) == 2: #lower and upper Bounds
                    break
                else:
                    doToBuild.append(createDoConstruct({getIndexLoop(lowerBound, upperBound):(lowerBound, upperBound)})[1])
    return doToBuild

@debugDecor
def placeArrayRtoparensR(doc, locNode, node_opE):
    """
    Convert ArrayR to parensR (remove the array-R and add parensR)
    :param doc: etree to use for parent retrieval
    :param node_opE: working node
    :param locNode: scope of the working node
    """
    def arrayRtoparensR(doc,arrayR):
        """
        Return a parensR node from an array-R
        :param loopIndex: string for the fortran loop index 
        :param lowerBound: string for the fortran lower bound of the do loop
        :param upperBound: string for the fortran upper bound of the do loop
        """
        
        subs=arrayR.findall('.//{*}section-subscript')
        parensR = ET.Element('{http://fxtran.net/#syntax}parens-R')
        parensR.text = '('
        parensR.tail = ')'
        elementLT = ET.Element('{http://fxtran.net/#syntax}element-LT')
        for i,sub in enumerate(subs): 
            element = ET.Element('{http://fxtran.net/#syntax}element')
            if alltext(sub) == ':': # (:) only
                pass
                #try to guess from the dimension declaration (need to first find the element from varList)
            elif sub.text is not None and sub.text.strip(' ') == ':': # :INDEX e.g. (:IKTE); transform it to 1:IKTE
                u = sub.find('.//{*}upper-bound/{*}*/{*}*/{*}n')
                if u is None:
                    #fxtran bug workaround
                    u = sub.find('.//{*}lower-bound/{*}*/{*}*/{*}n')
                upperBound = alltext(u)
                element.insert(0, createExprPart(getIndexLoop('1',upperBound)))      
            elif len(sub.findall('.//{*}upper-bound')) == 0:
                if ':' in alltext(sub): # INDEX: e.g. (IKTB:)
                    pass
                else:  # single literal-E : copy the object (e.g. IKA alone or operation such as IKE+1)
                    element.insert(0,sub.findall('.//{*}lower-bound')[0]) 
            else:
                lowerBounds=sub.findall('.//{*}lower-bound')
                upperBounds=sub.findall('.//{*}upper-bound')
                lowerBound=alltext(lowerBounds[0])
                upperBound=alltext(upperBounds[0])
                element.insert(0, createExprPart(getIndexLoop(lowerBound,upperBound)))
            elementLT.append(element)
    
        for i in range(len(elementLT)-1):
            elementLT[i].tail = ','
        parensR.insert(0,elementLT)
        return parensR

    # Replace the array-like index selection by index loop on all variables (array-R)
    arrayR = node_opE.findall('.//{*}array-R')
    for node in arrayR:
        parensR=arrayRtoparensR(locNode,node)
        par = getParent(node_opE,node)
        par.insert(1,parensR)
        par.remove(node)
        
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
            V = ET.Element('{http://fxtran.net/#syntax}V')
            V.append(createExprPart(v))
            V.tail = '='
            lower, upper = createArrayBounds(l, u, 'DOCONCURRENT')

            triplet = ET.Element('{http://fxtran.net/#syntax}forall-triplet-spec')
            triplet.extend([V, lower, upper])

            triplets.append(triplet)

        tripletLT = ET.Element('{http://fxtran.net/#syntax}forall-triplet-spec-LT')
        tripletLT.tail = ')'
        for triplet in triplets[:-1]:
            triplet.tail = ', '
        tripletLT.extend(triplets)

        dostmt = ET.Element('{http://fxtran.net/#syntax}do-stmt')
        dostmt.text = 'DO CONCURRENT ('
        dostmt.tail = '\n'
        dostmt.append(tripletLT)
        enddostmt = ET.Element('{http://fxtran.net/#syntax}end-do-stmt')
        enddostmt.text = 'END DO'

        doconstruct = ET.Element('{http://fxtran.net/#syntax}do-construct')
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
            doV = ET.Element('{http://fxtran.net/#syntax}do-V')
            doV.append(createExprPart(v))
            doV.tail = '='
            lower, upper = createArrayBounds(l, u, 'DO')

            dostmt = ET.Element('{http://fxtran.net/#syntax}do-stmt')
            dostmt.text = 'DO '
            dostmt.tail = '\n'
            dostmt.extend([doV, lower, upper])

            enddostmt = ET.Element('{http://fxtran.net/#syntax}end-do-stmt')
            enddostmt.text = 'END DO'

            doconstruct = ET.Element('{http://fxtran.net/#syntax}do-construct')
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

@debugDecor
def createArrayBounds(lowerBoundstr, upperBoundstr, context):
    """
    Return a lower-bound and upper-bound node
    :param lowerBoundstr: string for the fortran lower bound of an array
    :param upperBoundstr: string for the fortran upper bound of an array
    :param context: 'DO' for DO loops
                    'DOCONCURRENT' for DO CONCURRENT loops
                    'ARRAY' for arrays
    """
    lowerBound = ET.Element('{http://fxtran.net/#syntax}lower-bound')
    lowerBound.insert(0, createExprPart(lowerBoundstr))
    upperBound = ET.Element('{http://fxtran.net/#syntax}upper-bound')
    upperBound.insert(0, createExprPart(upperBoundstr))
    if context == 'DO':
        lowerBound.tail = ', '
    elif context in ('DOCONCURRENT', 'ARRAY'):
        lowerBound.tail = ':'
    else:
        raise PYFTError('Context unknown in createArrayBounds: {c}'.format(c=str(context)))
    return lowerBound, upperBound    
    
@debugDecor
def setFalseIfStmt(doc, flags, scopePath, simplify=False):
    """
    Set to .FALSE. a given boolean fortran flag before removing the node if simplify is True
    :param doc: xml fragment to use
    :param flags: list of strings of flags to set to .FALSE.
    :param scopePath: scope to explore (None for all). This is a '/'-separated path with each element
                         having the form 'module:<name of the module>', 'sub:<name of the subroutine>' or
                         'func:<name of the function>'
    :param simplify: try to simplify code (if the .FALSE. was alone inside a if-then-endif construct,
                     the construct is removed, and variables used in the if condition are also
                     checked)
    """
    FalseNode = ET.Element('{http://fxtran.net/#syntax}literal-E')
    FalseNode.text = ' .FALSE. '
    for flag in flags:
        flag = flag.upper()
    if scopePath is None:
        scopePath = [loc for loc in getScopesList(doc) if loc.split('/')[-1].split(':')[0] != 'type']
    else:
        if isinstance(scopePath, str): scopePath = [scopePath]
    singleFalseBlock,multipleFalseBlock = [], []
    for loc in scopePath:
        #Loop on nodes composing the scope
        for node in getScopeChildNodes(doc, getScopeNode(doc, loc)):
            #Multiple flags conditions
            Node_opE = node.findall('.//{*}condition-E/{*}op-E')
            for node_opE in Node_opE:
                found = False
                for i,n in enumerate(node_opE):
                    if n.tag.endswith('}named-E') and alltext(n).upper() in flags:
                        found = True
                        if i == 0: # Append at first object of parent
                            node_opE.insert(0,FalseNode)
                        else:
                            node_opE.append(FalseNode)
                        getParent(node_opE,n).remove(n)
                if found:
                    multipleFalseBlock.append(node_opE)
            #Solo condition
            Node_namedE = node.findall('.//{*}condition-E/{*}named-E')
            for n in Node_namedE:
                if alltext(n).upper() in flags:
                    par = getParent(node,n)
                    par.append(FalseNode)
                    par.remove(n)
                    singleFalseBlock.append(getParent(node,getParent(node,par))) # <if-block><if-then-stmt>
    if simplify:
        removeStmtNode(doc, singleFalseBlock, simplify, simplify)
        evalFalseIfStmt(doc, multipleFalseBlock, simplify)
    
def evalFalseIfStmt(doc, nodes, simplify=False):
    """
    Evaluate if-stmt with multiple op-E and remove the nodes if only .FALSE. are present
    :param doc: xml fragment to use
    :param nodes: list of nodes of type op-E to evaluate (containing .FALSE.)
    :param simplify: try to simplify code (if if-block is removed, variables used in the if condition are also
                     checked)
    """
    nodes_torm = []
    for node in nodes:
        toRemove = True
        for n in node:
            if n.tag.endswith('}op') or (n.tag.endswith('}literal-E') and '.FALSE.' in alltext(n)):
                pass
            else:
                toRemove = False
                break
        if toRemove: 
            nodes_torm.append(getParent(doc,getParent(doc,getParent(doc,node)))) #<if-block><if-then-stmt><condition-E>
    removeStmtNode(doc, nodes_torm, simplify, simplify)

@debugDecor
def removeCall(doc, callName, scopePath, simplify=False):
    """
    :param doc: xml fragment to use
    :param callName: name of the subprogram calls to remove.
    :param scopePath: scope to explore (None for all). This is a '/'-separated path with each element
                         having the form 'module:<name of the module>', 'sub:<name of the subroutine>' or
                         'func:<name of the function>'
    :param simplify: try to simplify code (if we delete "CALL FOO(X)" and if X not used else where,
                     we also delete it; or if the call was alone inside a if-then-endif construct,
                     the construct is also removed, and variables used in the if condition are also
                     checked...)
    """
    callName = callName.upper()
    if scopePath is None:
        scopePath = [loc for loc in getScopesList(doc) if loc.split('/')[-1].split(':')[0] != 'type']
    else:
        if isinstance(scopePath, str): scopePath = [scopePath]
    callNodes = []
    for loc in scopePath:
        #Loop on nodes composing the scope
        for node in getScopeChildNodes(doc, getScopeNode(doc, loc)):
            callNodes += [node] if node.tag.endswith('}call-stmt') else [] #In case node is a call statement
            callNodes += [cn for cn in node.findall('.//{*}call-stmt')] #If node is a construct with call statements
    callNodes = [cn for cn in callNodes
                 if n2name(cn.find('.//{*}named-E/{*}N')).upper() == callName] #filter by name
    removeStmtNode(doc, callNodes, simplify, simplify)

@debugDecor
def removePrints(doc, scopePath, simplify=False):
    """
    Removes all print statements
    :param doc: xml fragment to use
    :param scopePath: scope to explore (None for all). This is a '/'-separated path with each element
                         having the form 'module:<name of the module>', 'sub:<name of the subroutine>' or
                         'func:<name of the function>'
    :param simplify: try to simplify code (if we delete "print*, X" and if X is not used else where,
                     we also delete it; or if the print was alone inside a if-then-endif construct,
                     the construct is also removed, and variables used in the if condition are also
                     checked...)
    """
    if scopePath is None:
        scopePath = [loc for loc in getScopesList(doc) if loc.split('/')[-1].split(':')[0] != 'type']
    else:
        if isinstance(scopePath, str): scopePath = [scopePath]
    printNodes = []
    for loc in scopePath:
        #Loop on nodes composing the scope
        for node in getScopeChildNodes(doc, getScopeNode(doc, loc)):
            printNodes += [node] if node.tag.endswith('}print-stmt') else [] #In case node is a print statement
            printNodes += [cn for cn in node.findall('.//{*}print-stmt')] #If node is a construct with print statements
    removeStmtNode(doc, printNodes, simplify, simplify)

@debugDecor
def removeConstructNode(doc, node, simplifyVar, simplifyStruct):
    """
    This function removes a construct node and:
      - suppress variable that became useless (if simplifyVar is True)
      - suppress outer loop/if if useless (if simplifyStruct is True)
    :param doc: xml fragment to use
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
    assert node.tag.endswith('-stmt') or node.tag.endswith('-construct'), \
      "Don't know how to suppress only a part of a structure or of a statement"

    #This function removes inner statement to give a chance to identify and suppress unused variables
    #During this step, nodes are suppressed with simplifyStruct=False to prevent infinite loops
    #then the actual node is removed using removeStmtNode

    if node.tag.endswith('-construct'):
        #inner nodes
        nodes = {'do-construct': _nodesInDo,
                 'if-construct': _nodesInIf,
                 'where-construct': _nodesInWhere,
                 'selectcase-construct': _nodesInCase}[node.tag.split('}')[1]](node)
        #sort nodes by type
        constructNodes, otherNodes = [], []
        for n in nodes:
            if n.tag.endswith('-construct'):
                constructNodes.append(n)
            else:
                otherNodes.append(n)
        #suppress all statements at once
        removeStmtNode(doc, otherNodes, simplifyVar, False)
        #suppress construct nodes one by one (recursive call)
        for n in constructNodes:
            removeConstructNode(doc, n, simplifyVar, False)
        #suppress current node
        removeStmtNode(doc, node, simplifyVar, simplifyStruct)
    else:
        #At least a-stmt, print-stmt
        removeStmtNode(doc, node, simplifyVar, simplifyStruct)

def _nodesInIf(ifNode):
    """
    Internal method to return nodes in if structure
    """
    nodes = []
    for block in ifNode.findall('./{*}if-block'):
        for item in [i for i in block if not (i.tag.endswith('}if-then-stmt') or \
                                              i.tag.endswith('}else-if-stmt') or \
                                              i.tag.endswith('}else-stmt') or \
                                              i.tag.endswith('}end-if-stmt'))]:
            if not non_code(item): nodes.append(item)
    return nodes

def _nodesInWhere(whereNode):
    """
    Internal method to return nodes in where structure
    """
    nodes = []
    for block in whereNode.findall('./{*}where-block'):
        for item in [i for i in block if not (i.tag.endswith('}where-construct-stmt') or \
                                              i.tag.endswith('}else-where-stmt') or \
                                              i.tag.endswith('}end-where-stmt'))]:
            if not non_code(item): nodes.append(item)
    return nodes

def _nodesInDo(doNode):
    """
    Internal method to return nodes in do structure
    """
    nodes = []
    for item in [i for i in doNode if not (i.tag.endswith('}do-stmt') or \
                                           i.tag.endswith('}end-do-stmt'))]:
        if not non_code(item): nodes.append(item)
    return nodes

def _nodesInCase(caseNode):
    """
    Internal method to return nodes in do structure
    """
    nodes = []
    for block in caseNode.findall('./{*}selectcase-block'):
        for item in [i for i in block if not (i.tag.endswith('}select-case-stmt') or \
                                              i.tag.endswith('}case-stmt') or \
                                              i.tag.endswith('}end-select-case-stmt'))]:
            if not non_code(item): nodes.append(item)
    return nodes

@debugDecor
def removeStmtNode(doc, nodes, simplifyVar, simplifyStruct):
    """
    This function removes a statement node and:
      - suppress variable that became useless (if simplifyVar is True)
      - suppress outer loop/if if useless (if simplifyStruct is True)
    :param doc: xml fragment to use
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
        if node.tag.endswith('}if-stmt') or node.tag.endswith('}where-stmt'):
            action = node.find('./{*}action-stmt')
            if action is not None and len(action) != 0:
                nodesToSuppress.append(action[0])
            else:
                nodesToSuppress.append(node)
        elif node.tag.endswith('}action-stmt'):
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
            loc = getScopePath(doc, node)
            if node.tag.endswith('}do-construct'):
                #Try to remove variables used in the loop
                varToCheck.extend([(loc, n2name(arg)) for arg in node.find('./{*}do-stmt').findall('.//{*}N')])
            elif node.tag.endswith('}if-construct') or node.tag.endswith('}if-stmt'):
                #Try to remove variables used in the conditions
                varToCheck.extend([(loc, n2name(arg)) for arg in node.findall('.//{*}condition-E//{*}N')])
            elif node.tag.endswith('}where-construct') or node.tag.endswith('}where-stmt'):
                #Try to remove variables used in the conditions
                varToCheck.extend([(loc, n2name(arg)) for arg in node.findall('.//{*}mask-E//{*}N')])
            elif node.tag.endswith('}call-stmt'):
                #We must check if we can suppress the variables used to call the subprogram
                varToCheck.extend([(loc, n2name(arg)) for arg in node.findall('./{*}arg-spec//{*}N')])
                #And maybe, the subprogram comes from a module
                varToCheck.append((loc, n2name(node.find('./{*}procedure-designator//{*}N'))))
            elif node.tag.endswith('}a-stmt') or node.tag.endswith('}print-stmt'):
                varToCheck.extend([(loc, n2name(arg)) for arg in node.findall('.//{*}N')])
            elif node.tag.endswith('}selectcase-construct'):
                #Try to remove variables used in the selector and in conditions
                varToCheck.extend([(loc, n2name(arg)) for arg in node.findall('.//{*}case-E//{*}N')])
                varToCheck.extend([(loc, n2name(arg)) for arg in node.findall('.//{*}case-value//{*}N')])
    
    #Node suppression
    parents = {} #cache
    for node in nodesToSuppress:
        parent = getParent(doc, node)
        parents[id(node)] = parent
        newlines = '\n' * (alltext(node).count('\n') if node.tag.endswith('-construct') else 0)
        if node.tail is not None or len(newlines) > 0:
            previous = getSiblings(doc, node, after=False)
            if len(previous) == 0:
                previous = parent
            else:
                previous = previous[-1]
            if previous.tail is None: previous.tail = ''
            previous.tail = previous.tail.replace('\n','') + (node.tail if node.tail is not None else '')
        parent.remove(node)
    
    #Variable simplification
    removeVarIfUnused(doc, varToCheck, excludeDummy=True, excludeModule=True, simplify=simplifyVar)
    
    #List the new nodes to suppress
    newNodesToSuppress = []
    for node in nodesToSuppress:
        parent = parents[id(node)]
        #If we have suppressed the statement in a if statement (one-line if) or where statement
        #we must suppress the entire if/where statement even when simplifyStruct is False
        if parent.tag.endswith('}action-stmt'):
            newNodesToSuppress.append(getParent(doc, parent))
    
        elif simplifyStruct:
            if parent.tag.endswith('}do-construct') and len(_nodesInDo(parent)) == 0:
                newNodesToSuppress.append(parent)
            elif parent.tag.endswith('}if-block'):
                parPar = getParent(doc, parent)
                if len(_nodesInIf(parPar)) == 0:
                    newNodesToSuppress.append(parPar)
            elif parent.tag.endswith('}where-block'):
                parPar = getParent(doc, parent)
                if len(_nodesInWhere(parPar)) == 0:
                    newNodesToSuppress.append(parPar)
            elif parent.tag.endswith('}selectcase-block'):
                parPar = getParent(doc, parent)
                if len(_nodesInCase(parPar)) == 0:
                    newNodesToSuppress.append(parPar)

    constructNodes, otherNodes = [], []
    for n in newNodesToSuppress:
        if n.tag.endswith('-construct'):
            if n not in constructNodes: constructNodes.append(n)
        else:
            if n not in otherNodes: otherNodes.append(n)
    #suppress all statements at once
    if len(otherNodes) > 0:
        removeStmtNode(doc, otherNodes, simplifyVar, False)
    #suppress construct nodes one by one (recursive call)
    for n in constructNodes:
        removeConstructNode(doc, n, simplifyVar, False)

class Statements():
    @copy_doc(removeCall)
    def removeCall(self, *args, **kwargs):
        return removeCall(self._xml, *args, **kwargs)

    @copy_doc(removePrints)
    def removePrints(self, *args, **kwargs):
        return removePrints(self._xml, *args, **kwargs)

