"""
This module implements the functions relative to openacc
"""

import re
from pyft.util import debugDecor, n2name, alltext, tag
from pyft.expressions import createElem, createExpr

class Openacc():
    @debugDecor
    def removeACC(self):
        """
        Remove openACC directives
        """
        self.removeComments(excl_directives=[],
                            pattern=re.compile(r'^\!\$ACC ', re.IGNORECASE))

    @debugDecor
    def craybyPassDOCONCURRENT(self):
        """
        By pass a bug of the CRAY compiler in which the vectorisation is not done with BR_ fonctions use or locally.
        On all expanded compute kernels with !$acc loop independent collapse(X) placed :
            - if BR_ fonction is used : !$acc loop independent collapse(X) is removed and the nested DO loops are factorised into DO CONCURRENT
            - if a mnh_undef(OPENACC) macro is in place, !$acc loop collapse independant(X) is removed 
            - if a mnh_undef(LOOP) macro is in place the nested DO loops are factorised into DO CONCURRENT
        """
        def checkPresenceofBR(node):
            """Return True if a BR_ (math BIT-REPRODUCTIBILITY) function is present in the node"""
            mathBR_list = ['ALOG','LOG', 'EXP', 'COS', 'SIN', 'ASIN', 'ATAN', 'ATAN2', 'P2','P3','P4']
            namedE = node.findall('.//{*}named-E/{*}N/{*}n')
            for el in namedE:
                if alltext(el) in ['BR_' + e for e in mathBR_list]:
                    return True
                    break
            return False
        def getStatementsInDoConstruct(node,savelist):
            for n in node:
                if 'do-construct' in n.tag:
                    getStatementsInDoConstruct(n,savelist)
                elif 'do-stmt' not in n.tag:
                    savelist.append(n)
                else:
                    pass
        
        useNestedLoops = True # Cray compiler needs nested loops by default
        useAccLoopIndependent = True # Cray compiler needs nested !$acc loop independent collapse(X) by default
        toremove = [] #list of nodes to remove
        comments = self.findall('.//{*}C')
        
        for comment in comments:
            if comment.text.startswith('!$mnh_undef(LOOP)'):
                useNestedLoops = False # Use DO CONCURRENT
            if comment.text.startswith('!$mnh_undef(OPENACC)'):
                useAccLoopIndependent = False #
                
            if comment.text.startswith('!$mnh_define(LOOP)'):
                useNestedLoops = True # Use DO CONCURRENT
            if comment.text.startswith('!$mnh_define(OPENACC)'):
                useAccLoopIndependent = True #
                
            if comment.text.startswith('!$acc loop independent collapse('):
                # Get the statements content in the DO-construct
                par = self.getParent(comment)
                ind = list(par).index(comment)
                nestedLoop = par[ind + 1]
                statements = []
                getStatementsInDoConstruct(nestedLoop,statements)
                    
                # Check presence of BR_ within the statements
                isBR_present = False
                for stmt in statements:
                    if checkPresenceofBR(stmt): 
                        isBR_present = True
                        break
                    
                # Remove !$acc loop independent collapse if BR_ is present or if !$mnh_undef(OPENACC)
                if not useAccLoopIndependent or isBR_present:
                    toremove.append((self, comment))
                
                # Use DO CONCURRENT instead of nested-loop if BR_ is present or if !$mnh_undef(LOOP)
                if not useNestedLoops or isBR_present:
                    # Determine the table of indices
                    doStmt = nestedLoop.findall('.//{*}do-stmt')
                    table = {}
                    for do in reversed(doStmt):
                        table[do.find('.//{*}do-V/{*}named-E/{*}N/{*}n').text] = [alltext(do.find('.//{*}lower-bound')),
                                                                                  alltext(do.find('.//{*}upper-bound'))]
                    
                    # Create the do-construct
                    inner, outer, extraindent = self.createDoConstruct(table, indent=len(nestedLoop.tail), concurrent=True)
                    
                    # Insert the statements in the new do-construct
                    for stmt in statements:
                        inner.insert(-1, stmt)
                    
                    # Insert the new do-construct and delete all the old do-construct
                    par.insert(ind,outer)
                    toremove.append((par, nestedLoop))
                    
        #Suppression of nodes
        for parent, elem in toremove:
            parent.remove(elem)                 
                            
    @debugDecor
    def addACC_data(self):
        """
        1) Add after declaration:
        !$acc data present ( list of intent arrays)
        2) Add at the end of the routine
        !$acc end data
        """
        scopes = self.getScopes()
        mod_type = scopes[0].path.split('/')[-1].split(':')[1][:4]
        if mod_type == 'MODD':
            pass
        else:
            for scope in scopes:
                # Do not add !$acc data directives to :
                # - MODULE or FUNCTION object,
                # - interface subroutine from a MODI
                # but only to SUBROUTINES
                if 'sub:' in scope.path and 'func' not in scope.path and 'interface' not in scope.path:
                    # Look for all intent arrays only
                    arraysIntent = []
                    for var in scope.varList:
                        # intent arrays, not of type TYPE (only REAL, INTEGER, CHARACTER)
                        if var['arg'] and var['as'] and 'TYPE' not in var['t'] and \
                           var['scopePath'] == scope.path:
                            arraysIntent.append(var['n'])
                    # Check if there is any intent variables
                    if len(arraysIntent) == 0:
                        break

                    #1) !$acc data present()
                    list_var = "!$acc data present ( "
                    count = 0
                    for var in arraysIntent:
                        if count > 6:
                            list_var = list_var + '\n!$acc &              '
                            count = 0
                        list_var = list_var + var + ", &"
                        count += 1
                    list_var_end = list_var[:-3] # remove last comma and &

                    for com in createExpr(list_var_end + ')'):
                        self.insertStatement(scope.path, self.indent(com), first=True)

                    #2) !$acc end data
                    comment = createElem('C', text='!$acc end data', tail='\n')
                    self.insertStatement(scope.path, self.indent(comment), first=False)

    @debugDecor
    def addACC_routine_seq(self, stopScopes):
        """
        Adds the '!$acc routine (<name>) seq' directive
        :param stopScopes: scope paths where we stop to add the directive
        """
        for scope in self.getScopes():
            if self.tree.isUnderStopScopes(scope.path, stopScopes,
                                           includeInterfaces=True,
                                           includeStopScopes=True):
                name = n2name(scope[0].find('.//{*}N')).upper()
                acc = createElem('C')
                acc.text = '!$acc routine ({routine}) seq'.format(routine=name)
                acc.tail = scope[0].tail
                scope[0].tail = '\n'
                scope.insert(1, acc)

