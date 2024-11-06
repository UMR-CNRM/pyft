"""
This module implements the Cosmetics class containg methods to deal with cosmetics
"""

import re
from pyfortool.util import debugDecor, nonCode, tag
from pyfortool.expressions import createElem
from pyfortool import NAMESPACE


class Cosmetics():
    """
    Methods to deal with cosmetics
    """
    @debugDecor
    def upperCase(self):
        """
        :return: same object but with upper case letters for FORTRAN code
        """
        for elem in self.iter():
            if (not nonCode(elem)) and elem is not None and elem.text is not None:
                elem.text = elem.text.upper()

    @debugDecor
    def lowerCase(self):
        """
        :return: same objetc but with lower case letters for FORTRAN code
        """
        for elem in self.iter():
            if (not nonCode(elem)) and elem is not None and elem.text is not None:
                elem.text = elem.text.lower()

    @debugDecor
    def indent(self, nodeToUpdate=None, indentProgramunit=0, indentBranch=2,
               exclDirectives=None):
        """
        :param nodeToUpdate: if None, the entire object is indented
        :param indentProgramunit: number of space characters inside program unit
        :param indentBranch: number of space characters fr other branches (do, if...)
        :param exclDirectives: some lines are directives and must stay unindented. The cpp
                               directives are automatically recognized by fxtran but others
                               appear as FORTRAN comments and must be indentified here. This
                               option can take the following values:
                                - None: to recognize as directives the lines begining
                                        with '!$OMP' (default)
                                - []: to suppress the exclusion
                                - [...]: to give another list of line beginings to consider
        :return: same object but with indentation corrected
        """

        if nodeToUpdate is None:
            nodeToUpdate = self

        if exclDirectives is None:
            exclDirectives = ['!$OMP']

        def setLevel(elem, level, nextElem):
            """
            :param elem: element whose tail must be modifies
            :param level: level of indentation
            :param nextElem: next element
            """
            if elem.tail is not None:
                elem.tail = elem.tail.replace('\t', '  ')
                excl = (nextElem is not None and
                        (tag(nextElem) == 'cpp' or
                         tag(nextElem) == 'C' and
                         any(nextElem.text.startswith(d) for d in exclDirectives)))
                if not excl:
                    elem.tail = re.sub('\n[ ]*', '\n' + ' ' * level, elem.tail)

        def indentRecur(elem, level, inConstruct):
            """
            :param elem: dom element
            :param level: current level for elem
            :param inConstruct: True if we are inside a construct
            """
            blocs = ['file', 'program-unit', 'if-block', 'where-block', 'selectcase-block']
            progstmt = ['subroutine-stmt', 'program-stmt', 'module-stmt', 'function-stmt',
                        'submodule-stmt', 'procedure-stmt', 'interface-stmt']
            endprogstmt = ['end-' + s for s in progstmt]
            interbranchstmt = ['else-stmt', 'else-if-stmt', 'else-where-stmt']
            branchstmt = ['if-then-stmt', 'where-construct-stmt'] + interbranchstmt
            endbranchstmt = ['end-if-stmt', 'end-where-stmt']

            currlevel = level
            laste = None
            firstnumselect = True
            for ie, sElem in enumerate(elem):
                # Indentation does not apply to these lines (eg SUBROUTINE statement, DO construct)
                # but apply to the lines inside
                if tag(sElem) in progstmt:
                    currlevel += indentProgramunit
                elif tag(sElem) in branchstmt + [inConstruct + '-stmt']:
                    currlevel += indentBranch

                # Add indentation *to the tail*, thus for the next line
                setLevel(sElem, currlevel, elem[ie + 1] if ie + 1 < len(elem) else None)

                if tag(elem) == 'selectcase-construct':
                    # Structure is:
                    # <selectcase-construct>
                    # <selectcase-block><select-case-stmt>SELECT
                    #     CASE (...)</select-case-stmt>   \n +2
                    # </selectcase-block>
                    # <selectcase-block><case-stmt>CASE<case-selector>(...)
                    #     </case-selector></case-stmt>  \n +4
                    # statement  \n +4
                    # statement  \n +2
                    # </selectcase-block>
                    # <selectcase-block><case-stmt>CASE<case-selector>(...)
                    #     </case-selector></case-stmt>  \n +4
                    # statement  \n +4
                    # statement  \n +0
                    # <end-select-case-stmt>END SELECT</end-select-case-stmt>
                    #     </selectcase-block></selectcase-construct>
                    if firstnumselect:
                        firstnumselect = False
                    else:
                        # previous line was a CASE line, we must indent it only once
                        # pylint: disable-next=unsubscriptable-object
                        setLevel(laste[-1], level + indentBranch, sElem)
                    # statements are indented twice
                    indentRecur(sElem, level + indentBranch * 2, "")
                    if tag(sElem[-1]) == 'end-select-case-stmt':
                        setLevel(sElem[-2], level, sElem[-1])

                elif tag(sElem) in blocs or tag(sElem).endswith('-construct'):
                    # This xml tag contains other tags, we iterate on them
                    if tag(sElem[0]) in interbranchstmt:
                        # Structure is <if-construct><if-block><if-then-stmt>IF...
                        #                  ...THEN</if-then-stmt>
                        #              statement (the identation of the ELSE line is
                        #                         in the tail of this stetement)
                        #              </if-block><if-block><else-stmt>ELSE</else-stmt>
                        #              statement
                        #              <end-if-stmt>ENDIF</end-if-stmt></if-block></if-construct>
                        # pylint: disable-next=unsubscriptable-object
                        setLevel(laste[-1], level, sElem)
                    construct = tag(sElem)[:-10] if tag(sElem).endswith('-construct') else ""
                    indentRecur(sElem, currlevel, construct)

                # This line contains the end statement, we must remove the indentation contained
                # in the tail of the previous item
                if tag(sElem) in endprogstmt + endbranchstmt + ['end-' + inConstruct + '-stmt']:
                    setLevel(laste, level, sElem)
                laste = sElem

        indentRecur(nodeToUpdate, 0, "")
        return nodeToUpdate

    @debugDecor
    def removeEmptyLines(self):
        """
        Remove empty lines
        """
        elem = self.find('{*}file')
        if elem is not None and elem.text is not None:
            elem.text = elem.text.replace('\n', '')
        for elem in self.iter():
            if elem.tail is not None and '\n' in elem.tail:
                elem.tail = elem.tail.replace('\t', '  ')
                elem.tail = re.sub(r"\n[  \n]*\n", r"\n", elem.tail)

    @debugDecor
    def removeComments(self, exclDirectives=None, pattern=None):
        """
        :param exclDirectives: some lines are directives and must stay unindented. The cpp
                               directives are automatically recognized by fxtran but others
                               appear as FORTRAN comments and must be indentified here. This
                               option can take the following values:
                                - None: to recognize as directives the lines begining
                                        with '!$OMP', '!$mnh', '!$acc' or '!$ACC' (default)
                                - []: to suppress the exclusion
                                - [...]: to give another list of line beginings to consider
        :param pattern: remove only comments matching the pattern (string or re.compile output)
        """
        if exclDirectives is None:
            exclDirectives = ['!$OMP', '!$mnh', '!$ACC', '!$acc']

        if isinstance(pattern, str):
            pattern = re.compile(pattern)

        def recur(elem):
            tailUpper = None
            for ie in range(len(elem))[::-1]:  # Loop from the end to the begining
                sElem = elem[ie]
                if tag(sElem) == 'C' and \
                   not any(sElem.text.startswith(d) for d in exclDirectives) and \
                   (pattern is None or pattern.match(sElem.text)):
                    # Don't loose the tail (containing new line character and indentation)
                    if ie != 0:
                        # It exists an element before,
                        # we add the current tail to this previsous element
                        if elem[ie - 1].tail is None:
                            elem[ie - 1].tail = sElem.tail
                        elif sElem.tail is not None:
                            elem[ie - 1].tail += sElem.tail
                    else:
                        # The's no previsous element, tail is givent back the container element
                        tailUpper = sElem.tail
                    elem.remove(sElem)
                if len(sElem) >= 1:
                    tail = recur(sElem)  # recursive call to inner elements
                    if tail is not None:
                        # The first element was a comment,
                        # its tail must be added to the text attribute
                        if sElem.text is None:
                            sElem.text = tail
                        else:
                            sElem.text += tail
            return tailUpper
        recur(self)

    @debugDecor
    def updateContinuation(self, nodeToUpdate=None, align=True,
                           removeALL=False, addBegin=True, removeBegin=False):
        """
        :param nodeToUpdate: if None, the entire xml is updated
        :param align: True to align begin of continued lines
        :param removeALL: True to suppress all the continuation line characters ('&')
        :param addBegin: True to add missing continuation line characters ('&')
                         at the begining of lines
        :param removeBegin: True to suppress continuation line characters ('&')
                            at the begining of lines

        When suppressed, the '&' are replaced by a space character
        Comments after a '&' are lost
        """

        assert not (align and removeALL), "We cannot remove and align at the same time"
        assert not (addBegin and (removeALL or removeBegin)), \
               "We cannot remove and add, at the same time, continuation characters"

        if nodeToUpdate is None:
            nodeToUpdate = self

        parents = {}  # cache to be used in recurDirect

        def recurReverse(elem, tail):
            for ie in range(len(elem))[::-1]:  # Loop from the end to the begining
                sElem = elem[ie]
                parents[sElem] = elem
                if tag(sElem) == 'cnt':
                    # Search for comments or cpp after the cnt node
                    commentsAfter = []
                    j = ie + 1
                    while j < len(elem) and tag(elem[j]) in ('C', 'cpp'):
                        commentsAfter.append(elem[j])
                        j += 1
                    nextNode = elem[j] if j < len(elem) else None

                    # Is it a '&' at the end of a line (or at the begining)?
                    isend = ((sElem.tail is not None and '\n' in sElem.tail) or
                             len(commentsAfter) > 0)

                    # Add missing continuation character at the begining of line
                    if isend and addBegin:
                        if sElem.tail is not None and \
                           sElem.tail.replace('\n', '').replace('\t', '').lstrip(' ') != '':
                            # tail contains text, probably an endding ')', after a carriage return
                            # Thus, there is no '&' to begin line
                            new = createElem('cnt', text='&')
                            # '&' must be put before any text on the following line containing code
                            i = 0
                            while sElem.tail[i] in (' ', '\n', '\t'):
                                i += 1
                            new.tail = ' ' + sElem.tail[i:]
                            sElem.tail = sElem.tail[:i]
                            elem.insert(ie + 1, new)
                        elif tag(nextNode) != 'cnt':
                            # There is no '&' to begin next line
                            new = createElem('cnt', text='&')
                            if len(commentsAfter) > 0:
                                # '&' must be put before any text on the following
                                # line containing code
                                i = 0
                                while i < len(commentsAfter[-1].tail) and \
                                        commentsAfter[-1].tail[i] in (' ', '\n', '\t'):
                                    i += 1
                                new.tail = ' ' + commentsAfter[-1].tail[i:]
                                commentsAfter[-1].tail = commentsAfter[-1].tail[:i]
                            else:
                                new.tail = ' '
                            elem.insert(ie + 1 + len(commentsAfter), new)

                    # Suppression
                    if removeALL or (removeBegin and not isend):
                        cpp = False
                        for com in commentsAfter[::-1]:
                            if tag(com) != 'cpp':
                                elem.remove(com)
                            else:
                                cpp = True
                        if not cpp:
                            # We cannot remove a continuation line followed by a cpp
                            elem.remove(sElem)  # OK because we loop in reverse order
                            if sElem.tail is not None:
                                txt = sElem.tail.strip() + ' '
                            else:
                                txt = ' '
                            if ie != 0:
                                if elem[ie - 1].tail is None:
                                    elem[ie - 1].tail = txt
                                else:
                                    elem[ie - 1].tail += txt

                # Recursively enter blocs
                if len(sElem) >= 1:
                    recurReverse(sElem, tail)

        def recurDirect(elem, ct, inCnt):
            """
            :param ct: current text
            :param inCnt: -1 if we are not in a statement spanning several lines
                          elswhere contains the number of spaces to add
            """
            ignoreComment = False
            if align:
                for ie, sElem in enumerate(list(elem)):
                    # It is a '&' character marking the end of the line
                    isendcnt = tag(sElem) == 'cnt' and \
                               ((sElem.tail is not None and '\n' in sElem.tail) or
                                (ie + 1 < len(elem) and tag(elem[ie + 1]) == 'C'))
                    ignoreComment = (ignoreComment or
                                     (isendcnt and
                                      (ie + 1 < len(elem) and tag(elem[ie + 1]) == 'C')))

                    # REAL :: X1, & !comment 1
                    #               !comment 2
                    #         X2, &
                    # #ifdef XXX
                    #         X3, &
                    # #endif
                    #         X4
                    if isendcnt or ignoreComment or (inCnt != -1 and tag(sElem) == 'cpp'):
                        # Number of spaces for alignment not already determined
                        # (first line of the continuation)
                        if isendcnt and inCnt == -1:
                            # Search for the container statement
                            topstmt = elem
                            while not tag(topstmt).endswith('-stmt'):
                                topstmt = parents[topstmt]

                            # Character to align on
                            if tag(topstmt) == 'a-stmt':
                                patList = ('=>', '=', r'\(')
                            elif tag(topstmt) == 'call-stmt':
                                patList = (r'\(', r'call[ ]+\w', 'call ', 'call')
                            elif tag(topstmt) == 'if-stmt':
                                patList = (r'\(', r'\)', 'if ', 'if')
                            elif tag(topstmt) == 'where-stmt':
                                patList = (r'\(', r'\)', 'where ', 'where')
                            elif tag(topstmt) == 'forall-stmt':
                                patList = (r'\(', r'\)', 'forall ', 'forall')
                            elif tag(topstmt) == 'namelist-stmt':
                                patList = ('/.*/', '/', 'namelist')
                            elif tag(topstmt) == 'subroutine-stmt':
                                patList = (r'\(', r'subroutine[ ]+\w', 'subroutine ', 'subroutine')
                            elif tag(topstmt) == 'use-stmt':
                                patList = (':', r'use[ ]+\w', 'use ', 'use')
                            elif tag(topstmt) == 'T-decl-stmt':
                                patList = ('::', r'\w,', r'\w ', r'\w')
                            elif tag(topstmt) == 'print-stmt':
                                patList = ('print', )
                            elif tag(topstmt) == 'write-stmt':
                                patList = (r'\)', r'write[ ]*\(', 'write[ ]*', 'write')
                            elif tag(topstmt) == 'procedure-stmt':
                                patList = ('module[ ]+procedure[ ]*', 'module[ ]*', 'module')
                            else:
                                patList = ('::', ':', r'\(', '=>', '=', '[', ':', '/')

                            # Compute indentation value
                            inCnt = None
                            for pat in patList:
                                if inCnt is None:
                                    mat = re.search(pat, ct, flags=re.IGNORECASE)
                                    if mat is not None:
                                        if ie + 1 < len(elem) and tag(elem[ie + 1]) != 'cnt':
                                            # If there is no continuation character at the begining,
                                            # align the text with the position after the delimiter
                                            # found
                                            inCnt = mat.end()
                                        else:
                                            inCnt = mat.end() - 1
                            if inCnt is None:
                                inCnt = 4

                        # Align the next line exept if it is a cpp line
                        if not (ie + 1 < len(elem) and tag(elem[ie + 1]) == 'cpp'):
                            if sElem.tail is not None:
                                sElem.tail = re.sub('\n[ ]*', '\n' + ' ' * inCnt, sElem.tail)
                            else:
                                sElem.tail = '\n' + ' ' * inCnt

                    if tag(sElem) not in ('C', 'cnt'):
                        ct += (sElem.text if sElem.text is not None else '')
                        ignoreComment = False

                    # Recursively enter the inner blocks
                    if len(sElem) >= 1:
                        ct, inCnt = recurDirect(sElem, ct, inCnt)

                    # Text after the end of block
                    ct += (sElem.tail if sElem.tail is not None else '')
                    if '\n' in ct:
                        ct = ct.split('\n')[-1]
                        if tag(sElem) not in ('cnt', 'C', 'cpp'):
                            inCnt = -1

            return ct, inCnt

        recurReverse(nodeToUpdate, 0)
        recurDirect(nodeToUpdate, "", -1)
        return nodeToUpdate

    __NO_VALUE__ = '__NO_VALUE__'

    @debugDecor
    def updateSpaces(self, beforeOp=1, afterOp=1, inOperator=True,
                     beforeComma=0, afterComma=1,
                     beforeParenthesis=0, afterParenthesis=0,
                     beforeAffectation=1, afterAffectation=1, inAffectation=True,
                     beforeRangeDelim=0, afterRangeDelim=0,
                     beforeUseDelim=0, afterUseDelim=1,
                     beforeDeclDelim=1, afterDeclDelim=1,
                     inDeclDelim=True, afterTypeDecl=1,
                     beforeEqDo=0, afterEqDo=0,
                     beforeEqCall=0, afterEqCall=0,
                     beforeEqInit=0, afterEqInit=0,
                     beforeEndcnt=1, afterBegincnt=1,
                     afterIfwherecase=1, beforeThen=1, beforeIfaction=1,
                     afterProgunit=1,
                     endOfLine=True, afterName=0, inName=True,
                     beforeCmdsep=0, afterCmdsep=1,
                     adjacentKeywords=__NO_VALUE__, afterKeywords=__NO_VALUE__):
        """
        :param beforeOp, afterOp: number of spaces before and after operators
        :param inOperator: True to suppress spaces in operators
        :param beforeComma, afterComma: number of spaces before and after commas
        :param beforeParenthesis, afterParenthesis: number of spaces before and after parenthesis
        :param beforeAffectation, afterAffectation: number of spaces before and after
                                                    affectations or associations
        :param inAffectation: True to suppress spaces in affectations and in association ('= >')
        :param beforeRangeDelim, afterRangeDelim: number of spaces before and after range delimiters
        :param beforeUseDelim, afterUseDelim: number of spaces before and after use delimiters (':')
        :param beforeDeclDelim, afterDeclDelim: number of spaces before and after declaration and
                                                enumerator delimiter ('::')
        :param inDeclDelim: True to suppress spaces in declaration and enumerator delimiter (': :')
        :param afterTypeDecl: number of spaces after the type in a declaration w/o '::'
                                (e.g. 'INTEGER I'); also for enumerators (minimum 1)
        :param beforeEqDo, afterEqDo: number of spaces before and after '=' sign in DO and
                                      FORALL statements
        :param beforeEqCall, afterEqCall: number of spaces before and after '=' sign
                                          in CALL statement
        :param beforeEqInit, afterEqInit: number of spaces before and after '=' sign for init values
        :param beforeEndcnt, afterBegincnt: number of spaces before a continuation chararcter at the
                                            end of the line and after a continuation character
                                            at the begining of a line
        :param afterIfwherecase: number of spaces after the IF, ELSEIF, WHERE, ELSEWHERE,
                                 SELECTCASE, CASE and FORALL keywords
        :param beforeThen: number of spaces before the THEN keyword
        :param beforeIfaction: number of spaces
                               between IF condition and action in one-line IF statement and
                               between FORALL specification and affectation in one-line FORALL
                                       statement and
                               between WHERE mask and action in one-line WHERE statement
        :param afterProgunit: between the program unit type (e.g. SUBROUTINE) and its name
        :param endOfLine: True to suppress spaces at the end of the line
        :param afterName: number of spaces after an indentifier, type or attribute name
        :param inName: True to suppress spaces in identifier names
        :param beforeCmdsep, afterCmdsep: number of spaces before and after command separator (';')
        :param adjacentKeywords: describes the number of spaces to introduce between adjancent
                                 keywords when this is legal (the list comes from the table
                                 "6.2 Adjacent keywords where separating blanks are optional" of the
                                 F2008 norm and has been complemented by "end select",
                                 "implicit none" and "module procedure"; for the last two,
                                 a minimum of 1 is required).
                                 The allowed dictionnary keys are:
                                      - block_data
                                      - double_precision
                                      - else_if
                                      - else_where
                                      - end_associate
                                      - end_block
                                      - end_block_data
                                      - end_critical
                                      - end_do
                                      - end_enum
                                      - end_file
                                      - end_forall
                                      - end_function
                                      - end_if
                                      - end_interface
                                      - end_module
                                      - end_procedure
                                      - end_program
                                      - end_selec
                                      - end_select
                                      - end_submodule
                                      - end_subroutine
                                      - end_team
                                      - end_type
                                      - end_where
                                      - go_to
                                      - in_out
                                      - select_case
                                      - select_type
                                      - implicit_none
                                      - module_procedure
                                 For example, use {'end_do':1} to write 'END DO' or
                                                  {'end_do':0} to write 'ENDDO' or
                                                  {'end_do':None} to not update the writting
                                 or use adjacentKeywords=None to disable everything
        :param afterKeywords: describes the number of spaces to introduce after keywords.
                               Some keywords need a more sophisticated treatment and are controled
                               by specific keys (e.g. CASE).
                               The keys are the keyword in lowercase, some names can be tricky
                               to guess (e.g. the key for ENDFILE is 'end-file'). By default
                               only a few are defined.
                               Use afterKeywords=None to disable everything.

        To not update spaces, put None instead of an integer and False in booleans.
        For example, to not change number of spaces after a comma, use afterComma=None

        Updates are done in the following order:
        """

        adjaKeyDesc = {
          'block_data': (1, './/{*}block-data-stmt'),
          'double_precision': (1, './/{*}intrinsic-T-spec/{*}T-N'),
          'else_if': (1, './/{*}else-if-stmt'),
          'else_where': (0, './/{*}else-where-stmt'),
          'end_associate': (1, './/{*}end-associate-stmt'),
          'end_block': (1, './/{*}end-block-stmt'),
          'end_block_data': (1, './/{*}end-block-data-stmt'),
          'end_critical': (1, './/{*}end-critical-stmt'),
          'end_do': (1, './/{*}end-do-stmt'),
          'end_enum': (1, './/{*}end-enum-stmt'),
          'end_file': (1, './/{*}end-file-stmt'),
          'end_forall': (1, './/{*}end-forall-stmt'),
          'end_function': (1, './/{*}end-function-stmt'),
          'end_if': (1, './/{*}end-if-stmt'),
          'end_interface': (1, './/{*}end-interface-stmt'),
          'end_module': (1, './/{*}end-module-stmt'),
          'end_procedure': (1, './/{*}end-procedure-stmt'),
          'end_program': (1, './/{*}end-program-stmt'),
          'end_selec': (1, './/{*}end-select-case-stmt'),
          'end_select': (1, './/{*}end-select-T-stmt'),
          'end_submodule': (1, './/{*}end-submodule-stmt'),
          'end_subroutine': (1, './/{*}end-subroutine-stmt'),
          'end_team': (1, './/{*}end-change-team-stmt'),
          'end_type': (1, './/{*}end-T-stmt'),
          'end_where': (1, './/{*}end-where-stmt'),
          'go_to': (0, './/{*}goto-stmt'),
          'in_out': (0, './/{*}intent-spec'),
          'select_case': (1, './/{*}select-case-stmt'),
          'select_type': (1, './/{*}select-T-stmt'),
          'implicit_none': (1, './/{*}implicit-none-stmt'),
          'module_procedure': (1, './/{*}procedure-stmt'),
        }

        afterKey = {
          'print': 0,
          'call': 1,
          'use': 1,
          'do': 1,
          'end-file': 1,
          'save': 1,
        }

        assert adjacentKeywords is None or adjacentKeywords == self.__NO_VALUE__ or \
               all(k in adjaKeyDesc
                   for k in adjacentKeywords), "Unknown key in **adjacentKeywords"

        def getvalAdja(key):
            if adjacentKeywords is None:
                return None
            if adjacentKeywords == self.__NO_VALUE__:
                return adjaKeyDesc[key][0]
            return adjacentKeywords.get(key, adjaKeyDesc[key][0])

        def getvalAfter(key):
            key = key[:-5]
            if afterKeywords != self.__NO_VALUE__:
                num = afterKeywords.get(key, afterKey.get(key, None))
            else:
                num = afterKey.get(key, None)
            return num

        assert afterProgunit is None or afterProgunit >= 1
        assert afterTypeDecl is None or afterTypeDecl >= 1
        for k in ('implicit_none', 'module_procedure'):
            num = getvalAdja(k)
            assert num is None or num >= 1, \
                   "adjacentKeywords['" + k + "'] must be at least 1 (is " + str(num) + ")"
        for k in ('use', 'call', 'end-file', 'do'):
            num = getvalAfter(k + '-stmt')
            assert num is None or num >= 1, \
                   "afterKeywords['" + k + "'] must be at least 1 (is " + str(num) + ")"

        for elem in self.iter():
            isNotC = tag(elem) != 'C'
            # security
            if elem.tail is None:
                elem.tail = ""
            elem.tail = elem.tail.replace('\t', '  ')

            # Around parenthesis
            if beforeParenthesis is not None:
                elem.tail = re.sub(r"[  ]*\(", " " * beforeParenthesis + r"(", elem.tail)
                elem.tail = re.sub(r"[  ]*\)", " " * beforeParenthesis + r")", elem.tail)
                if elem.text is not None and isNotC:
                    elem.text = re.sub(r"[  ]*\(", " " * beforeParenthesis + r"(", elem.text)
                    elem.text = re.sub(r"[  ]*\)", " " * beforeParenthesis + r")", elem.text)
            if afterParenthesis is not None:
                elem.tail = re.sub(r"\([  ]*", "(" + " " * afterParenthesis, elem.tail)
                elem.tail = re.sub(r"\)[  ]*", ")" + " " * afterParenthesis, elem.tail)
                if elem.text is not None and isNotC:
                    elem.text = re.sub(r"\([  ]*", "(" + " " * afterParenthesis, elem.text)
                    elem.text = re.sub(r"\)[  ]*", ")" + " " * afterParenthesis, elem.text)

            # Around commas
            if beforeComma is not None:
                elem.tail = re.sub(r"[  ]*,", " " * beforeComma + r",", elem.tail)
                if elem.text is not None and isNotC:
                    elem.text = re.sub(r"[  ]*,", " " * beforeComma + r",", elem.text)
            if afterComma is not None:
                elem.tail = re.sub(r",[  ]*", "," + " " * afterComma, elem.tail)
                if elem.text is not None and isNotC:
                    elem.text = re.sub(r",[  ]*", "," + " " * afterComma, elem.text)

            # End of line
            if endOfLine:
                elem.tail = re.sub(r"[  ]*\n", r"\n", elem.tail)

            # In names or around names (identifier, type, attribute)
            if tag(elem) in ('N', 'T-N', 'attribute-N'):
                if inName:
                    for nnn in elem.findall('{*}n'):
                        if nnn.tail is not None:
                            nnn.tail = nnn.tail.strip(' ')
                if elem.tail is not None and afterName is not None:
                    elem.tail = ' ' * afterName + elem.tail.lstrip(' ')

            # Around range delimiter
            elif tag(elem) == 'lower-bound' and elem.tail is not None and ':' in elem.tail:
                if beforeRangeDelim is not None:
                    elem.tail = ' ' * beforeRangeDelim + elem.tail.lstrip(' ')
                if afterRangeDelim is not None:
                    elem.tail = elem.tail.rstrip(' ') + ' ' * beforeRangeDelim

            # Around ':' in USE statements
            elif tag(elem) == 'module-N' and elem.tail is not None and ':' in elem.tail:
                if beforeUseDelim is not None:
                    elem.tail = re.sub(r"[  ]*:", " " * beforeUseDelim + r":", elem.tail)
                if afterUseDelim is not None:
                    elem.tail = re.sub(r":[  ]*", ":" + " " * afterUseDelim, elem.tail)

            # Around and in '::' in declaration statements
            # After the type in a declaration
            elif tag(elem) in ('attribute', '_T-spec_') and elem.tail is not None:
                if inDeclDelim:
                    elem.tail = re.sub(r":[  ]*:", r"::", elem.tail)
                if beforeDeclDelim is not None:
                    elem.tail = re.sub(r"[ ]*(:[  ]*:)", ' ' * beforeDeclDelim + r"\1", elem.tail)
                if afterDeclDelim is not None:
                    elem.tail = re.sub(r"(:[  ]*:)[ ]*", r"\1" + ' ' * afterDeclDelim,  elem.tail)
                if tag(elem) == '_T-spec_' and afterTypeDecl is not None:
                    elem.tail = elem.tail.rstrip(' ') + ' ' * afterTypeDecl

            # Around and in '::' in enumerators
            # After the enumerator keyword
            elif tag(elem) == 'enumerator-stmt' and elem.text is not None:
                if ':' in elem.text:
                    if inDeclDelim:
                        elem.text = re.sub(r":[  ]*:", r"::", elem.text)
                    if beforeDeclDelim is not None:
                        elem.text = re.sub(r"[ ]*(:[  ]*:)", ' ' * beforeDeclDelim + r"\1",
                                           elem.text)
                    if afterDeclDelim is not None:
                        elem.text = re.sub(r"(:[  ]*:)[ ]*", r"\1" + ' ' * afterDeclDelim,
                                           elem.text)
                elif afterTypeDecl is not None:
                    elem.text = elem.text.rstrip(' ') + ' ' * afterTypeDecl

            # Between the program unit type and its name
            elif (tag(elem) in ('subroutine-stmt', 'program-stmt', 'module-stmt', 'function-stmt',
                                'submodule-stmt', 'procedure-stmt', 'interface-stmt',
                                'end-subroutine-stmt', 'end-program-stmt',
                                'end-module-stmt', 'end-function-stmt',
                                'end-submodule-stmt', 'end-procedure-stmt', 'end-interface-stmt')
                  and afterProgunit is not None):
                if elem.text is not None:
                    elem.text = elem.text.rstrip(' ') + ' ' * afterProgunit

            # Around '=' sign in DO and FORALL statements
            elif tag(elem) in ('do-V', 'V') and elem.tail is not None and '=' in elem.tail:
                if beforeEqDo is not None:
                    elem.tail = re.sub('[ ]*=', ' ' * beforeEqDo + '=', elem.tail)
                if afterEqDo is not None:
                    elem.tail = re.sub('=[ ]*', '=' + ' ' * beforeEqDo, elem.tail)

            # Around '=' sign in CALL statements
            elif tag(elem) == 'arg-N' and elem.tail is not None and '=' in elem.tail:
                if beforeEqCall is not None:
                    elem.tail = re.sub('[ ]*=', ' ' * beforeEqCall + '=', elem.tail)
                if afterEqCall is not None:
                    elem.tail = re.sub('=[ ]*', '=' + ' ' * beforeEqCall, elem.tail)

            # Around '=' sign for init values
            elif (tag(elem) in ('EN-N', 'named-constant') and
                  elem.tail is not None and '=' in elem.tail):
                if beforeEqInit is not None:
                    elem.tail = re.sub('[ ]*=', ' ' * beforeEqInit + '=', elem.tail)
                if afterEqInit is not None:
                    elem.tail = re.sub('=[ ]*', '=' + ' ' * beforeEqInit, elem.tail)
            # Around the command separator ';'
            elif tag(elem) == 'smc':
                if beforeCmdsep is not None:
                    prev = self.getSiblings(elem, after=False)
                    if len(prev) != 0 and prev[-1].tail is not None:
                        prev[-1].tail = ' ' * beforeCmdsep + prev[-1].tail.lstrip(' ')
                if afterCmdsep is not None and elem.tail is not None:
                    elem.tail = elem.tail.rstrip(' ') + ' ' * afterCmdsep

            # Around and in association operators (affectation case done after)
            elif tag(elem) == 'associate-N' and elem.tail is not None and '=' in elem.tail:
                if beforeAffectation is not None:
                    elem.tail = re.sub('[ ]*=', ' ' * beforeAffectation + '=', elem.tail)
                if afterAffectation is not None:
                    elem.tail = re.sub('>[ ]*', '>' + ' ' * beforeAffectation, elem.tail)
                if inAffectation:
                    elem.tail = re.sub(r'=[ ]*>', '=>', elem.tail)

            # After a reserved keyword
            # elif afterKeywords is not None and tag(elem).endswith('-stmt'):
            #     num = getvalAfter(tag(elem))
            #     if num is not None and elem.text is not None:
            #         elem.text = elem.text.rstrip(' ') + ' ' * num

        # Another loop on elements
        # All the transformations are not put in a single loop because the following one act
        # on sub-elements. Putting them all in the same loop would prevent to control in which order
        # the different transformations occur.
        # For instance, the suppression on the space after the parenthesis must be done before
        # the adding of a space before a THEN keyword
        for elem in self.iter():
            # Around and in operators
            if tag(elem) == 'op-E':  # op are always (?) in op-E nodes
                for op in elem.findall('{*}op'):
                    if beforeOp is not None:
                        io = list(elem).index(op)
                        if io != 0:
                            prev = elem[io - 1]
                            if prev.tail is None:
                                prev.tail = ' ' * beforeOp
                            else:
                                prev.tail = prev.tail.rstrip(' ') + ' ' * beforeOp
                    if afterOp is not None:
                        if op.tail is None:
                            op.tail = ' ' * afterOp
                        else:
                            op.tail = op.tail.lstrip(' ') + ' ' * afterOp
                    if inOperator:
                        for oo in op.findall('{*}o'):
                            if oo.tail is not None:
                                oo.tail = oo.tail.strip(' ')

            # Around and in affectation operators (association case done before)
            elif tag(elem) in ('a-stmt', 'pointer-a-stmt'):
                # a are always (?) in a-stmt or pointer-a-stmt nodes
                for aff in elem.findall('{*}a'):
                    if beforeAffectation is not None:
                        prev = elem[list(elem).index(aff) - 1]
                        if prev.tail is None:
                            prev.tail = ' ' * beforeAffectation
                        else:
                            prev.tail = prev.tail.rstrip(' ') + ' ' * beforeAffectation
                    if afterAffectation is not None:
                        if aff.tail is None:
                            aff.tail = ' ' * afterAffectation
                        else:
                            aff.tail = aff.tail.lstrip(' ') + ' ' * afterAffectation
                    if inAffectation:
                        aff.text = aff.text.replace(' ', '')

            # After a IF, WHERE, ELSEIF, ELSEWHERE, SELECTCASE, CASE and FORALL keyword,
            # and before THEN keyword
            elif tag(elem) in ('if-stmt', 'if-then-stmt', 'else-if-stmt',
                               'where-stmt', 'where-construct-stmt', 'else-where-stmt',
                               'select-case-stmt', 'case-stmt',
                               'forall-stmt', 'forall-construct-stmt'):
                if afterIfwherecase is not None and elem.text is not None:
                    if tag(elem) == 'case-stmt':
                        # the (eventual) parenthesis is not in the text of the node
                        elem.text = elem.text.rstrip(' ') + ' ' * afterIfwherecase
                    else:
                        elem.text = re.sub(r'[ ]*\(', ' ' * afterIfwherecase + '(',
                                           elem.text, count=1)
                if tag(elem) in ('if-then-stmt', 'else-if-stmt') and beforeThen is not None:
                    cond = elem.find('{*}condition-E')
                    cond.tail = re.sub(r'\)[ ]*([a-zA-Z]*$)', ')' + ' ' * beforeThen + r'\1',
                                       cond.tail)
                elif tag(elem) == 'if-stmt' and beforeIfaction is not None:
                    cond = elem.find('{*}condition-E')
                    cond.tail = re.sub(r'\)[ ]*$', ')' + ' ' * beforeIfaction, cond.tail)
                elif tag(elem) == 'where-stmt' and beforeIfaction is not None:
                    cond = elem.find('{*}mask-E')
                    cond.tail = re.sub(r'\)[ ]*$', ')' + ' ' * beforeIfaction, cond.tail)
                elif tag(elem) == 'forall-stmt' and beforeIfaction is not None:
                    sub = elem.find('{*}forall-triplet-spec-LT')
                    sub.tail = re.sub(r'\)[ ]*$', ')' + ' ' * beforeIfaction, sub.tail)

        # Direct search to prevent using the costly getParent function
        if beforeEndcnt is not None or afterBegincnt is not None:
            for elem in self.findall('.//{*}cnt/..'):  # node containing continuation characters
                for cnt in elem.findall('{*}cnt'):  # continuation characters
                    ic = list(elem).index(cnt)
                    if ic == 0:
                        # the string before the continuation character is in the parent text
                        prev = elem
                        pstring = prev.text
                    else:
                        # the string before the continuation character is in previsous sibling tail
                        prev = elem[ic - 1]
                        pstring = prev.tail
                    if '\n' in pstring and '\n' in cnt.tail:
                        # continuation character alone on a line
                        pass
                    elif '\n' in pstring and afterBegincnt is not None:
                        # continuation character at the begining of a line
                        cnt.tail = ' ' * afterBegincnt + cnt.tail.lstrip(' ')
                    elif beforeEndcnt is not None:
                        # continuation character at the end of a line
                        # (eventually followed by a comment)
                        if prev == elem:
                            prev.text = prev.text.rstrip(' ') + ' ' * beforeEndcnt
                        else:
                            prev.tail = prev.tail.rstrip(' ') + ' ' * beforeEndcnt

        # In adjacent keywords
        for key, val in adjaKeyDesc.items():
            num = getvalAdja(key)
            if num is not None:
                for node in self.findall(val[1]):
                    lf = "[ ]*".join(["(" + p + ")" for p in key.split('_')])
                    repl = (" " * num).join([r"\{i}".format(i=i + 1)
                                             for i, _ in enumerate(key.split('_'))])
                    node.text = re.sub(lf, repl, node.text, flags=re.IGNORECASE)

    @debugDecor
    def changeIfStatementsInIfConstructs(self, singleItem=None):
        """
        Convert if-stmt to if-then-stmt. If singleItem is not filled, conversion to entire
        object is performed.
        E.g., before :
        IF(A=B) print*,"C
        after :
        IF(A=B) THEN
            print*,"C
        END IF
        Conversion is not done if 'CYLE' is found in action-stmt
        :param singleItem: single if-stmt; in case transformation is applied on one if-stmt only
        """
        if singleItem is not None:
            ifstmt = [singleItem]
        else:
            ifstmt = self.findall('.//{*}if-stmt')
        for item in ifstmt:
            cycleStmt = item.findall('.//{*}cycle-stmt')
            if len(cycleStmt) == 0:
                # Get indentation from last sibling
                par = self.getParent(item)
                ind = par[:].index(item)
                if ind != 0 and par[ind - 1].tail is not None:
                    # if tail of previous sibling exists
                    currIndent = len(par[ind - 1].tail) - len(par[ind - 1].tail.rstrip(' '))
                else:
                    # no tail = no indentation
                    currIndent = 0

                # Convert if-stmt into if-construct
                # <if-stmt>IF(<condition-E>...</condition-E>) <f:action-stmt>...
                #      ...</f:action-stmt></f:if-stmt>
                # <if-construct><if-block><if-then-stmt>IF(<f:condition-E>...
                #      ...</condition-E>) THEN</f:if-then-stmt>
                #                         ...
                #              <f:end-if-stmt>ENDIF</f:end-if-stmt></f:if-block></f:if-construct>
                # 1 create missing blocks
                item.tag = f'{{{NAMESPACE}}}if-construct'
                ifBlock = createElem('if-block')
                ifThenStmt = createElem('if-then-stmt')
                endif = createElem('end-if-stmt')
                ifBlock.append(ifThenStmt)
                item.append(ifBlock)
                # 2 move 'IF(' text
                ifThenStmt.text = item.text  # copy 'IF(' text
                ifThenStmt.tail = '\n' + (2 + currIndent) * ' '  # indentation for main statement
                item.text = None  # remove olf 'IF(' text
                # 3 move condition and add THEN
                condition = item.find('{*}condition-E')
                if not condition.tail.endswith(' '):
                    condition.tail += ' '
                condition.tail += 'THEN'
                ifThenStmt.append(condition)
                item.remove(condition)
                # 4 move action
                action = item.find('{*}action-stmt')
                action[0].tail = '\n' + currIndent * ' '  # indentation for the ENDIF
                ifBlock.append(action[0])
                item.remove(action)
                # 5 add ENDIF
                endif.text = 'END IF'
                ifBlock.append(endif)
                # 6 remove any cnt which was directly in the if-stmt node
                # (replaced by '\n' after THEN)
                for cnt in item.findall('./{*}cnt'):
                    item.remove(cnt)

    @debugDecor
    def removeEmptyCONTAINS(self):
        """
        Remove the CONTAINS statement if this section is empty
        """
        for contains in self.findall('.//{*}contains-stmt'):
            par = self.getParent(contains)
            index = list(par).index(contains)
            nextStmt = index + 1
            while tag(par[nextStmt]) == 'C':
                nextStmt += 1
            if tag(par[nextStmt]) in ('end-subroutine-stmt', 'end-function-stmt',
                                      'end-module-stmt'):
                # CONTAINS bloc is empty
                par.remove(contains)
