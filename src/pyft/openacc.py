"""
This module implements the functions relative to openacc
"""

from pyft.util import debugDecor, fortran2xml, n2name
from pyft.expressions import createElem

class Openacc():
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
                    varList = self.getVarList(scope.path)

                    # Look for all intent arrays only
                    arraysIntent = []
                    for var in varList:
                         if var['arg'] and var['as'] and 'TYPE' not in var['t']: #intent arrays, not of type TYPE (only REAL, INTEGER, CHARACTER)
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

                    fortranSource = "SUBROUTINE FOO598756\n"+ list_var_end + ")\nEND SUBROUTINE"
                    _, cfxtran = fortran2xml(fortranSource)
                    comment = cfxtran.findall('.//{*}C')
                    for com in comment:
                        self.insertStatement(scope.path, self.indent(com), first=True)

                    #2) !$acc end data
                    fortranSource = "SUBROUTINE FOO598756\n !$acc end data \nEND SUBROUTINE"
                    _, cfxtran = fortran2xml(fortranSource)
                    comment = cfxtran.find('.//{*}C')
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

