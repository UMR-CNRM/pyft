"""
This module implements the functions relative to openacc
"""

import xml.etree.ElementTree as ET
from pyft.cosmetics import indent 
from pyft.util import copy_doc, debugDecor, fortran2xml, n2name
from pyft.statements import insertStatement
from pyft.variables import getVarList
from pyft.scope import getScopesList, getScopePath
from pyft.tree import isUnderStopScopes

@debugDecor
def addACC_data(doc):
    """
    1) Add after declaration:
    !$acc data present ( list of intent arrays)
    2) Add at the end of the routine
    !$acc end data
    :param doc: etree to use
    """
    locations  = getScopesList(doc,withNodes='tuple')
    mod_type = locations[0][0].split('/')[-1].split(':')[1][:4]
    if mod_type == 'MODD':
        pass
    else:
        for loc in locations:
            # Do not add !$acc data directives to :
            # - MODULE or FUNCTION object, 
            # - interface subroutine from a MODI
            # but only to SUBROUTINES
            if 'sub:' in loc[0] and 'func' not in loc[0] and 'interface' not in loc[0]:
                scopepath = getScopePath(doc,loc[1])
                varList = getVarList(doc,scopepath)
                
                # Look for all intent arrays only
                arraysIntent = []
                for var in varList:
                     if var['arg'] and var['as'] and 'TYPE' not in var['t']: #intent arrays, not of type TYPE (only REAL, INTEGER, CHARACTER)
                         arraysIntent.append(var['n'])
                # Check if there is any intent variables
                if len(arraysIntent)== 0: 
                    break
                
                #1) !$acc data present()
                list_var = "!$acc data present ( "
                count=0
                for var in arraysIntent:
                    if count>6:
                        list_var = list_var + '\n!$acc &              '
                        count=0
                    list_var = list_var + var + ", "
                    count+=1
                list_var_end = list_var[:-2] # remove last comma
                
                fortranSource = "SUBROUTINE FOO598756\n"+ list_var_end + ")\nEND SUBROUTINE"
                _, cfxtran = fortran2xml(fortranSource)
                comment = cfxtran.findall('.//{*}C')
                for com in comment:
                    insertStatement(doc,loc[0],indent(com),first=True) 
                
                #2) !$acc end data
                fortranSource = "SUBROUTINE FOO598756\n !$acc end data \nEND SUBROUTINE"
                _, cfxtran = fortran2xml(fortranSource)
                comment = cfxtran.find('.//{*}C')
                insertStatement(doc,loc[0],indent(comment),first=False)

@debugDecor
def addACC_routine_seq(doc, descTree, stopScopes):
    """
    Adds the '!$acc routine (<name>) seq' directive
    :param doc: element tree
    :param descTree: descTree file
    :param stopScopes: scope where we stop to add the directive
    """
    for scopeName, scopeNode in getScopesList(doc, withNodes='tuple'):
        if isUnderStopScopes(scopeName, descTree, stopScopes, includeInterfaces=True, includeStopScopes=True):
            name = n2name(scopeNode[0].find('.//{*}N')).upper()
            acc = ET.Element('{http://fxtran.net/#syntax}C')
            acc.text = '!$acc routine ({routine}) seq'.format(routine=name)
            acc.tail = scopeNode[0].tail
            scopeNode[0].tail = '\n'
            scopeNode.insert(1, acc)

class Openacc():
    @copy_doc(addACC_data)
    def addACC_data(self, *args, **kwargs):
        return addACC_data(self._xml, *args, **kwargs)
        
    @copy_doc(addACC_routine_seq)
    def addACC_routine_seq(self, *args, **kwargs):
        return addACC_routine_seq(self._xml, *args, **kwargs)
