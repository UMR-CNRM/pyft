"""
This module contains functions usefull to build scripts around the pyfortool library
"""

import sys
from multiprocessing import cpu_count, Pool
from multiprocessing.managers import BaseManager
import re
import shlex
import os
import argparse
import logging
import traceback

from pyfortool.pyfortool import PYFT
from pyfortool.tree import Tree
from pyfortool.util import isint, PYFTError
from pyfortool import __version__


def task(filename):
    """
    Function to use on each file
    :param clsPYFT: PYFT class to use
    :param filename: file name
    """
    global PYFT
    global allFileArgs
    allArgs, orderedOptions = allFileArgs[filename]
    try:
        # Opening and reading of the FORTRAN file
        with PYFT(filename, filename, parser=allArgs.parser,
                  parserOptions=getParserOptions(allArgs), verbosity=allArgs.logLevel,
                  wrapH=allArgs.wrapH,
                  enableCache=allArgs.enableCache) as pft:

            # apply the transformation in the order they were specified
            for arg in orderedOptions:
                logging.debug('Applying %s on %s', arg, filename)
                applyTransfo(pft, arg, allArgs,
                             filename if filename == allArgs.plotCentralFile else None)
                logging.debug('  -> Done')

            # Writing
            if not allArgs.dryRun:
                pft.write()

        # Reporting
        return (0, filename)

    except Exception as exc:  # pylint: disable=broad-except
        logging.error("The following error has occurred in the file %s", filename)
        traceback.print_exception(exc, file=sys.stdout)
        sys.stdout.flush()
        return (1, filename)


def mainParallel():
    """
    Core of the pyfortool_parallel.py command
    """

    class MyManager(BaseManager):
        """
        Custom manager to deal with Tree instances
        """

    MyManager.register('Tree', Tree)

    def init(cls, afa):
        """
        Pool initializer
        """
        # After many, many attempts, it seems very difficult (if not impossible)
        # to do without global variables
        global PYFT  # pylint: disable=global-statement
        global allFileArgs  # pylint: disable=global-statement
        PYFT = cls
        allFileArgs = afa

    parser = argparse.ArgumentParser(description='Python FORTRAN tool', allow_abbrev=False,
                                     epilog="The argument order matters.")

    updateParser(parser, withInput=False, withOutput=False, withXml=False, withPlotCentralFile=True,
                 treeIsOptional=False, nbPar=True, restrictScope=False)
    commonArgs, getFileArgs = getArgs(parser)

    # Manager to share the Tree instance
    with MyManager() as manager:
        # Set-up the Tree instance
        sharedTree = getDescTree(commonArgs, manager.Tree)

        # Prepare PYFT to be used in parallel
        PYFT.setParallel(sharedTree)

        # Set-up the processes
        allFileArgs = {file: getFileArgs(file) for file in sharedTree.getFiles()}
        logging.info('Executing in parallel on %i files with a maximum of %i processes',
                     len(allFileArgs), commonArgs.nbPar)
        with Pool(commonArgs.nbPar, initializer=init, initargs=(PYFT, allFileArgs)) as pool:
            result = pool.map(task, sharedTree.getFiles())

        # Writting the descTree object
        sharedTree.toJson(commonArgs.descTree)

        # General error
        errors = [item[1] for item in result if item[0] != 0]
        status = len(errors)
        if status != 0:
            logging.error('List of files with error:')
            for error in errors:
                logging.error('  - %s', error)
            raise PYFTError(f"Errors have been reported in {status} file(s).")


def main():
    """
    Core of the pyfortool.py command
    """
    parser = argparse.ArgumentParser(description='Python FORTRAN tool', allow_abbrev=False,
                                     epilog="The argument order matters.")

    updateParser(parser, withInput=True, withOutput=True, withXml=True, withPlotCentralFile=False,
                 treeIsOptional=True, nbPar=False, restrictScope=True)
    args, orderedOptions = getArgs(parser)[1]()

    parserOptions = getParserOptions(args)
    descTree = getDescTree(args)

    try:
        # Opening and reading of the FORTRAN file
        pft = PYFT(args.INPUT, args.OUTPUT, parser=args.parser, parserOptions=parserOptions,
                   verbosity=args.logLevel, wrapH=args.wrapH, tree=descTree,
                   enableCache=args.enableCache)
        if args.restrictScope != '':
            pft = pft.getScopeNode(args.restrictScope)

        # apply the transformation in the order they were specified
        for arg in orderedOptions:
            logging.debug('Applying %s on %s', arg, args.INPUT)
            applyTransfo(pft, arg, args, plotCentralFile=args.INPUT)
            logging.debug('  -> Done')

        # Writing
        if descTree is not None:
            descTree.toJson(args.descTree)
        if args.xml is not None:
            pft.mainScope.writeXML(args.xml)
        if not args.dryRun:
            pft.mainScope.write()

        # Closing
        pft.mainScope.close()

    except:  # noqa E722
        # 'exept' everything and re-raise error systematically
        logging.error("The following error has occurred in the file %s", args.INPUT)
        raise


ARG_UPDATE_CNT = ('--alignContinuation', '--addBeginContinuation',
                  '--removeBeginContinuation',
                  '--emoveALLContinuation')


def getArgs(parser):
    """
    Parse arguments and interpret the --optsByEnv option
    :param parser: argparse parser
    :return: a tuple with
               - an argparse namespace containing common arguments (not using the --optsEnv option)
               - a function taking a filename as input and returning
                  - an argparse namespace with the common arguments and the ones added by
                    interpreting the --optsEnv option
                  - an ordered list of arguments
    """
    args = parser.parse_args()

    def getFileArgs(filename=args.INPUT if hasattr(args, 'INPUT') else None):
        """
        :param filename: name of source code file
        :return: argparse namespace to use with this file and
                 a list given the order in which the arguments were provided
        """
        # Decode the --optsByEnv option
        arguments = sys.argv[1:]
        if args.optsByEnv is not None:
            extra = ''
            for line in [] if args.optsByEnv is None else os.environ[args.optsByEnv].split('\n'):
                if ':=:' in line:
                    if re.match(line.split(':=:')[0], filename):
                        extra = line.split(':=:')[1]
                else:
                    extra = line
            arguments.extend(shlex.split(extra))

        # Compute the ordered list
        updateCnt = False
        optList = []
        for arg in arguments:
            if arg.startswith('--') and arg not in optList:
                if arg in ARG_UPDATE_CNT:
                    if not updateCnt:
                        updateCnt = True
                        optList.append(arg)
                else:
                    optList.append(arg)

        return parser.parse_args(arguments), optList

    return args, getFileArgs


def getParserOptions(args):
    """
    Get the options to use for the fxtran parser
    :param args: arguments parsed by the argparse parser
    """
    if args.parserOption is None:
        parserOptions = PYFT.DEFAULT_FXTRAN_OPTIONS.copy()
    else:
        parserOptions = [el for elements in args.parserOption for el in elements]
    if args.addIncludes:
        parserOptions = [opt for opt in parserOptions if opt not in ('-no-include', '-noinclude')]
    return parserOptions


def getDescTree(args, cls=Tree):
    """
    get the Tree object built with the parsed arguments
    :param args: arguments parsed by the argparse parser
    :param cls: class to use (usefull for manager)
    :return: a Tree instance
    """
    parserOptions = getParserOptions(args)
    if args.descTree:
        descTree = cls(tree=args.tree, descTreeFile=args.descTree,
                       parser=args.parser, parserOptions=parserOptions,
                       wrapH=args.wrapH, verbosity=args.logLevel)
    else:
        descTree = None
    return descTree


def updateParser(parser, withInput, withOutput, withXml, withPlotCentralFile, treeIsOptional,
                 nbPar, restrictScope):
    """
    Updates an argparse parser with arguments common to all the different tools
    :param parser: parser in which arguments are added
    :param withOutput: do we need the INPUT argument
    :param withOutput: do we need the OUTPUT argument
    :param withXml: do we need to be able to define an XML output file
    :param withPlotCentralFile: to add the --plotCentralFile argument
    :param treeIsOptional: is the --tree argument optional?
    :param nbPar: number of parallel processes
    :param restrictScope: can we specify the scope path
    """

    # ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
    # IMPORTANT NOTE
    # Argument order matters but argparse is not able to give the order
    # Therefore, arguments are processed twice. The first time by argparse to fully decode them.
    # The a second pass is made direcly on sys.argv. This mechanism has two implications:
    # allow_abbrev must be set to False in ArgumentParser
    # only long argument options are allowed (begining with two dashes)
    # ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
    assert not parser.allow_abbrev, 'parser must be created with allow_abbrev=False'

    parser.add_argument('--version', action='version',
                        version='%(prog)s {version}'.format(version=__version__))
    parser.add_argument('--simplify', default=False, action='store_true',
                        help='After a deletion, recursively deletes the code ' +
                             'and variables that have become useless')
    parser.add_argument('--logLevel', default='warning',
                        help='Provide logging level. Example --logLevel debug (default is warning)')
    parser.add_argument('--enableCache', default=False, action='store_true',
                        help='Precompute parent of each xml node and store the result')
    if nbPar:
        parser.add_argument('--nbPar', default=cpu_count(), type=int,
                            help='Number of parallel processes, 0 to get as many processes ' +
                                 'as the number of cores (default=0)')
    parser.add_argument('--optsByEnv', default=None, type=str,
                        help='Name of the environment variable containing additional arguments ' +
                             'to use. These arguments are processed after all other arguments. ' +
                             'The variable can contain a multi-lines string. The ' +
                             'variable is read line by line and the last applicable line is ' +
                             'used. A line can take one of these two forms: ' +
                             '1) "FILE_DESCRIPTOR:=:OPTIONS" (where FILE_DESCRIPTOR is a ' +
                             'regular expression to test against the filename. If there ' +
                             'is a match, the OPTIONS can be used for the file) and ' +
                             '2) "OPTIONS" (if the line doesn\'t contain the FILE_DESCRIPTOR ' +
                             'part, it applies to all source code).')

    if restrictScope:
        parser.add_argument('--restrictScope', default='', type=str, metavar='SCOPEPATH',
                            help="Limit the action to this scope path (SUBROUTINE/FUNCTION/" +
                                 "MODULE/TYPE). It is '/'-separated path with each element " +
                                 "having the form 'module:<name of the module>', " +
                                 "'sub:<name of the subroutine>', " +
                                 "'func:<name of the function>' or 'type:<name of the type>'.")

    # Inputs and outputs
    updateParserInputsOutputs(parser, withInput, withOutput, withXml)

    # fxtran
    updateParserFxtran(parser)

    # Variables
    updateParserVariables(parser)

    # Cosmetics
    updateParserCosmetics(parser)

    # Applications
    updateParserApplications(parser)

    # openACC
    updateParserOpenACC(parser)

    # Checks
    updateParserChecks(parser)

    # Statements
    updateParserStatements(parser)

    # Misc
    updateParserMisc(parser)

    # Tree
    updateParserTree(parser, withPlotCentralFile, treeIsOptional)

    # Preprocessot
    updateParserPreprocessor(parser)


def updateParserInputsOutputs(parser, withInput, withOutput, withXml):
    """
    Updates an argparse parser with input/output arguments
    :param parser: parser in which arguments are added
    :param withOutput: do we need the INPUT argument
    :param withOutput: do we need the OUTPUT argument
    :param withXml: do we need to be able to define an XML output file
    """
    gInOut = parser.add_argument_group('Input and output')
    if withInput:
        gInOut.add_argument('INPUT', help='FORTRAN input file')
    if withOutput:
        gInOut.add_argument('OUTPUT', default=None, help='FORTRAN output file', nargs='?')
    gInOut.add_argument('--renamefF', default=False, action='store_true',
                        help='Put file extension in upper case')
    gInOut.add_argument('--renameFf', default=False, action='store_true',
                        help='Put file extension in lower case')
    if withXml:
        gInOut.add_argument('--xml', default=None, type=str,
                            help='Output file for xml')
    gInOut.add_argument('--dryRun', default=False, action='store_true',
                        help='Dry run without writing the FORTRAN file (the xml ' +
                             'is still written')


def updateParserFxtran(parser):
    """
    Updates an argparse parser with fxtran arguments
    """
    gParser = parser.add_argument_group('fxtran parser relative options')
    gParser.add_argument('--parser', default=None, type=str,
                         help='Path to the fxtran parser binary')
    gParser.add_argument('--parserOption', nargs='*', action='append',
                         help='Option to pass to fxtran, defaults ' +
                              f'to {PYFT.DEFAULT_FXTRAN_OPTIONS}')
    gParser.add_argument('--wrapH', default=False, action='store_true',
                         help='Wrap .h file content into a MODULE to enable the reading')


def updateParserVariables(parser):
    """
    Updates an argparse parser with variables arguments
    """
    gVariables = parser.add_argument_group('Options to deal with variables')
    gVariables.add_argument('--showVariables', default=False, action='store_true',
                            help='Show the declared variables')
    gVariables.add_argument('--removeVariable', nargs=2, action='append',
                            metavar=('SCOPEPATH', 'VARNAME'),
                            help="Variable to remove from declaration. The first argument " +
                                 "is the SUBROUTINE/FUNCTION/MODULE/TYPE where the variable " +
                                 "is declared. It is '/'-separated path with each element having " +
                                 "the form 'module:<name of the module>', " +
                                 "'sub:<name of the subroutine>', " +
                                 "'func:<name of the function>' or 'type:<name of the type>'. " +
                                 "The second argument is the variable name")
    gVariables.add_argument('--attachArraySpecToEntity', default=False, action='store_true',
                            help='Find all T-decl-stmt elements that have a child element ' +
                                 'attribute with attribute-N=DIMENSION and move the attribute ' +
                                 'into EN-N elements')
    gVariables.add_argument('--addVariable', nargs=4, action='append',
                            metavar=('SCOPEPATH', 'VARNAME', 'DECLARATION', 'POSITION'),
                            help='Add a variable. First argument is the scope path (as for ' +
                                 'the --removeVariable option. The second is the variable ' +
                                 'name, the third is the declarative statement to insert, ' +
                                 'the fourth is the position (python indexing) the new ' +
                                 'variable will have in the calling statment of the ' +
                                 'routine (non-integer value for a local variable).')
    gVariables.add_argument('--addModuleVariable', nargs=3, action='append',
                            metavar=('SCOPEPATH', 'MODULENAME', 'VARNAME'),
                            help='Add a USE statement. The first argument is the scope path ' +
                                 '(as for the --removeVariable option). The second is the module ' +
                                 'name; the third is the variable name.')
    gVariables.add_argument('--showUnusedVariables', default=False, action='store_true',
                            help='Show a list of unused variables.')
    gVariables.add_argument('--removeUnusedLocalVariables',
                            help='Remove unused local variables, excluding some variables (comma-' +
                                 'separated list or NONE to exclude nothing).')
    gVariables.add_argument('--removePHYEXUnusedLocalVariables',
                            help='Remove unused local variables, excluding some variables (comma-' +
                                 'separated list or NONE to exclude nothing). This option takes ' +
                                 'into account the mnh_expand directives to prevent from ' +
                                 'removing useful variables.')
    gVariables.add_argument('--addExplicitArrayBounds', action='store_true',
                            help='Adds explicit bounds to arrays that already have parentheses.')
    gVariables.add_argument('--addArrayParentheses', action='store_true',
                            help='Adds parentheses to arrays (A => A(:))')
    gVariables.add_argument('--modifyAutomaticArrays', metavar="DECL#START#END",
                            help='Transform all automatic arrays declaration using the templates.' +
                                 ' The DECL part of the template will replace the declaration ' +
                                 'statement, the START part will be inserted as the first ' +
                                 'executable statement while the END part will be inserted as ' +
                                 'the last executable statement. Each part ' +
                                 'of the template can use the following place holders: ' +
                                 '"{doubledotshape}", "{shape}", "{lowUpList}", "{name}" and ' +
                                 '"{type}" which are, respectively modified into ' +
                                 '":, :, :", "I, I:J, 0:I", "1, I, I, J, 0, I", "A", "REAL" ' +
                                 'if the original declaration statement ' +
                                 'was "A(I, I:J, 0:I)". For example, the template ' +
                                 '"{type}, DIMENSION({doubledotshape}), ALLOCATABLE :: ' +
                                 '{name}#ALLOCATE({name}({shape}))#DEALLOCATE({name})"' +
                                 'will replace automatic arrays by allocatables.')
    gVariables.add_argument('--replaceAutomaticWithAllocatable', action='store_true',
                            help='Replace all automatic arrays with allocatable arrays.')
    gVariables.add_argument('--addArgInTree', default=None, action='append', nargs=3,
                            metavar=('VARNAME', 'DECLARATION', 'POSITION'),
                            help='Add an argument variable. The first argument is the variable ' +
                                 'name, the second one is the declarative statement to insert, ' +
                                 'the third one is the position (python indexing) the new ' +
                                 'variable will have in the calling statement of the ' +
                                 'routine. Needs the --stopScopes argument')


def updateParserCosmetics(parser):
    """
    Updates an argparse parser with cosmetics arguments
    """
    gCosmetics = parser.add_argument_group('Cosmetics options')
    gCosmetics.add_argument('--upperCase', default=False, action='store_true',
                            help='Put FORTRAN code in upper case letters')
    gCosmetics.add_argument('--lowerCase', default=False, action='store_true',
                            help='Put FORTRAN code in lower case letters')
    gCosmetics.add_argument('--changeIfStatementsInIfConstructs', default=False,
                            action='store_true',
                            help='Find all if-statement and convert it to if-then-statement')
    gCosmetics.add_argument('--indent', default=False, action='store_true',
                            help='Correct indentation')
    gCosmetics.add_argument('--removeIndent', default=False, action='store_true',
                            help='Remove indentation')
    gCosmetics.add_argument('--removeEmptyLines', default=False, action='store_true',
                            help='Remove empty lines')
    gCosmetics.add_argument('--removeComments', default=False, action='store_true',
                            help='Remove comments')
    gCosmetics.add_argument('--updateSpaces', default=False, action='store_true',
                            help='Updates spaces around operators, commas, parenthesis and ' +
                                 'at the end of line')
    gCosmetics.add_argument('--alignContinuation', default=False, action='store_true',
                            help='Align the beginings of continued lines')
    gCosmetics.add_argument('--addBeginContinuation', default=False, action='store_true',
                            help='Add missing continuation characters (\'&\') at the ' +
                                 'begining of lines')
    gCosmetics.add_argument('--removeBeginContinuation', default=False, action='store_true',
                            help='Remove continuation characters (\'&\') at the begining of lines')
    gCosmetics.add_argument('--removeALLContinuation', default=False, action='store_true',
                            help='Remove all continuation characters(\'&\')')
    gCosmetics.add_argument('--prettify', default=False, action='store_true',
                            help='Prettify the source code (indentation, spaces...)')
    gCosmetics.add_argument('--minify', default=False, action='store_true',
                            help='Simplify the source code (indentation, spaces...)')
    gCosmetics.add_argument('--removeEmptyCONTAINS', default=False, action='store_true',
                            help='Remove useless CONTAINS statements')


def updateParserApplications(parser):
    """
    Updates an argparse parser with applications arguments
    """
    gApplications = parser.add_argument_group('Options to apply upper level transformation')
    gApplications.add_argument('--deleteDrHook', default=False, action='store_true',
                               help='Delete DR HOOK use')
    gApplications.add_argument('--addDrHook', default=False, action='store_true',
                               help='Add DR HOOK')
    gApplications.add_argument('--deleteBudgetDDH', default=False, action='store_true',
                               help='Delete Budget/DDH use')
    gApplications.add_argument('--deleteRoutineCallsMesoNHGPU', default=False, action='store_true',
                               help='Delete parts of the code not compatible with MesoNH-OpenACC' +
                               'such as OCND2 blocks')
    gApplications.add_argument('--splitModuleRoutineFile', default=False, action='store_true',
                               help='Split a file')
    gApplications.add_argument('--deleteNonColumnCallsPHYEX', default=False, action='store_true',
                               help='Delete call to PHYEX routines that needs information on ' +
                                    'horizontal points (multiple column dependency')
    gApplications.add_argument('--removeIJDim', default=False, action='store_true',
                               help='Remove I and J dimensions (1, KLON). ' +
                                    'Needs the --stopScopes argument.')
    gApplications.add_argument('--expandAllArraysPHYEX', default=False, action='store_true',
                               help='Expand all array syntax (computing and where block) ' +
                               'using PHYEX conventions')
    gApplications.add_argument('--expandAllArraysPHYEXConcurrent', default=False,
                               action='store_true',
                               help='Expand all array syntax with DO CONCURRENT loops ' +
                                    '(computing and where block) using PHYEX conventions')
    gApplications.add_argument('--expandAllArrays', default=False, action='store_true',
                               help='Expand all array syntax (computing and where block) ' +
                                    'using mnh directives if present')
    gApplications.add_argument('--expandAllArraysConcurrent', default=False, action='store_true',
                               help='Expand all array syntax with DO CONCURRENT loops ' +
                                    '(computing and where block) using mnh directives if present')
    gApplications.add_argument('--inlineContainedSubroutinesPHYEX', default=False,
                               action='store_true',
                               help='Inline containted subroutines in main routine, using ' +
                                    'PHYEX conventions')
    gApplications.add_argument('--addStack', metavar='MODEL', type=str,
                               help='Add local arrays to the stack. The argument is the ' +
                                    'the model name in which stack must be added ("AROME" ' +
                                    'or "MESONH"). Needs the --stopScopes argument for AROME.')
    gApplications.add_argument('--addIncludes', default=False, action='store_true',
                               help='Add .h includes in the file and remove the INCLUDE statement')
    gApplications.add_argument('--mnhExpand', default=False, action='store_true',
                               help='Apply the mnh_expand directives with DO loops')
    gApplications.add_argument('--mnhExpandConcurrent', default=False, action='store_true',
                               help='Apply the mnh_expand directives with DO CONCURRENT loops')
    gApplications.add_argument('--addMPPDB_CHECKS', default=False, action='store_true',
                               help='Add MPPDB_CHEKS bit-repro checking routines of MesoNH for ' +
                                    'all in and inout arrays in subroutines')
    gApplications.add_argument('--shumanFUNCtoCALL', default=False, action='store_true',
                               help='Transform shuman functions to call statements')
    gApplications.add_argument('--mathFunctoBRFunc', default=False, action='store_true',
                               help='Convert intrinsic math functions **, LOG, ATAN, **2, **3, ' +
                                    '**4, EXP, COS, SIN, ATAN2 into a self defined function BR_ ' +
                                    'for MesoNH bit-repro.')
    gApplications.add_argument('--convertTypesInCompute', default=False, action='store_true',
                               help='Use single variable instead of variable contained in ' +
                                    'structure in compute statement for optimization issue ')
    gApplications.add_argument('--buildModi', default=False, action='store_true',
                               help='Builds the corresponding modi_ file')


def updateParserOpenACC(parser):
    """
    Updates an argparse parser with openACC arguments
    """
    gOpenACC = parser.add_argument_group('OpenACC')
    gOpenACC.add_argument('--addACCData', default=False, action='store_true',
                          help='Add !$acc data present and !$acc end data directives')
    gOpenACC.add_argument('--addACCRoutineSeq', default=False, action='store_true',
                          help='Add "!$acc routine seq" to routines under stopScopes')
    gOpenACC.add_argument('--craybyPassDOCONCURRENT', default=False, action='store_true',
                          help='remove acc loop independant collapse for BR_ fonctions and ' +
                               'mnh_undef(OPENACC) macro' +
                               ' use DO CONCURRENT with mnh_undef(LOOP)')
    gOpenACC.add_argument('--removeACC', default=False, action='store_true',
                          help='remove all ACC directives')
    gOpenACC.add_argument('--removebyPassDOCONCURRENT', default=False, action='store_true',
                          help='remove macro !$mnh_(un)def(OPENACC) and !$mnh_(un)def(LOOP) ' +
                               'directives')
    gOpenACC.add_argument('--buildACCTypeHelpers', default=False, action='store_true',
                          help='build module files containing helpers to copy user ' +
                               'type structures')


def updateParserChecks(parser):
    """
    Updates an argparse parser with checks arguments
    """

    gChecks = parser.add_argument_group('Check options')
    gChecks.add_argument('--checkIMPLICIT', choices={'Warn', 'Err'}, default=None,
                         help='Send a warning or raise an error if the "IMPLICIT NONE" ' +
                              'is missing')
    gChecks.add_argument('--checkINTENT', choices={'Warn', 'Err'}, default=None,
                         help='Send a warning or raise an error if the "INTENT" ' +
                              'attribute is missing for a dummy argument')
    gChecks.add_argument('--checkOpInCall', choices={'Warn', 'Err'}, default=None,
                         help='Send a warning or raise an error if a call argument is an '
                              'operation.')


def updateParserStatements(parser):
    """
    Updates an argparse parser with statements arguments
    """

    gStatement = parser.add_argument_group('Statements options')
    gStatement.add_argument('--removeCall', action='append',
                            help="Call to remove from the source code. The argument " +
                                 "is the subprogram name")
    gStatement.add_argument('--removePrints', default=False, action='store_true',
                            help="Remove print statements from the source code.")
    gStatement.add_argument('--inlineContainedSubroutines', default=False, action='store_true',
                            help='Inline containted subroutines in main routine')
    gStatement.add_argument('--setFalseIfStmt', default=None,
                            help='Replace this value by .FALSE. in if statements')


def updateParserMisc(parser):
    """
    Updates an argparse parser with misc arguments
    """
    gMisc = parser.add_argument_group('Miscellaneous')
    gMisc.add_argument('--showScopes', default=False, action='store_true',
                       help='Show the different scopes found in the source code')
    gMisc.add_argument('--empty', default=False, action='store_true',
                       help='Empty the different scopes')


def updateParserTree(parser, withPlotCentralFile, treeIsOptional):
    """
    Updates an argparse parser with statements arguments
    :param withPlotCentralFile: to add the --plotCentralFile argumen
    :param treeIsOptional: is the --tree argument optional?
    """
    gTree = parser.add_argument_group('Tree')
    gTree.add_argument('--tree', default=None, action='append', required=not treeIsOptional,
                       help='Directories where source code must be searched for')
    gTree.add_argument('--descTree', default=None, required=not treeIsOptional,
                       help='File to write and/or read the description of the tree.')
    if withPlotCentralFile:
        gTree.add_argument('--plotCentralFile', default=None, type=str,
                           help='Central file of the plot')
    gTree.add_argument('--plotCompilTree', default=None,
                       help='File name for compilation dependency graph (.dot or image extension)')
    gTree.add_argument('--plotExecTree', default=None,
                       help='File name for execution dependency graph (.dot or image extension)')
    gTree.add_argument('--plotMaxUpper', default=None, type=int,
                       help='Maximum number of upper elements in the plot tree')
    gTree.add_argument('--plotMaxLower', default=None, type=int,
                       help='Maximum number of lower elements in the plot tree')
    gTree.add_argument('--stopScopes', default=None, type=str,
                       help='#-separated list of scopes ' +
                            'where the recursive inclusion of an argument variable ' +
                            'must stop (needed for some transformations).')


def updateParserPreprocessor(parser):
    """
    Updates an argparse parser with statements arguments
    """
    gCpp = parser.add_argument_group('Preprocessor')
    gCpp.add_argument('--applyCPPifdef', nargs='*', action='append',
                      help="This option is followed by the list of defined or undefined " +
                           "CPP keys. " +
                           "All #ifdef and #ifndef concerning these keys are evaluated. " +
                           "Undefined keys are preceded by a percentage sign.")


def applyTransfo(pft, arg, args, plotCentralFile):
    """
    Apply transformation on a PYFT instance
    :param pft: PYFT instance
    :param arg: argument to deal with
    :param args: parsed argparsed arguments
    :param plotCentralFile: central file for plots
    """
    simplify = {'simplify': args.simplify}
    parserOptions = getParserOptions(args)
    stopScopes = args.stopScopes.split('#') if args.stopScopes is not None else None

    # File name manipulations
    applyTransfoFileName(pft, arg)

    # Variables
    applyTransfoVariables(pft, arg, args, simplify, parserOptions, stopScopes)

    # Applications
    applyTransfoApplications(pft, arg, args, simplify, parserOptions, stopScopes)

    # OpenACC
    applyTransfoOpenACC(pft, arg, args, stopScopes)

    # Cosmetics
    applyTransfoCosmetics(pft, arg, args)

    # Checks
    applyTransfoChecks(pft, arg, args)

    # Statements
    applyTransfoStatements(pft, arg, args, simplify)

    # Misc
    applyTransfoMisc(pft, arg, args, simplify)

    # Tree
    applyTransfoTree(pft, arg, args, plotCentralFile)

    # Preprocessor
    applyTransfoPreprocessor(pft, arg, args)


def applyTransfoFileName(pft, arg):
    """
    Apply file name transformations on a PYFT instance
    :param pft: PYFT instance
    :param arg: argument to deal with
    :param args: parsed argparsed arguments
    """

    # File name manipulations
    if arg == '--renamefF':
        pft.renameUpper()
    elif arg == '--renameFf':
        pft.renameLower()


def applyTransfoVariables(pft, arg, args, simplify, parserOptions, stopScopes):
    """
    Apply variables transformations on a PYFT instance
    :param pft: PYFT instance
    :param arg: argument to deal with
    :param args: parsed argparsed arguments
    :param simplify: kwargs to simplify
    :param parserOptions: fxtran parser options
    :param stopScopes: upper limit in call tree for some transformations
    """
    if arg == '--showVariables':
        pft.varList.showVarList()
    elif arg == '--attachArraySpecToEntity':
        pft.attachArraySpecToEntity()
    elif arg == '--removeVariable':
        pft.removeVar(args.removeVariable, **simplify)
    elif arg == '--addVariable':
        pft.addVar([[v[0], v[1], v[2], (int(v[3]) if isint(v[3]) else None)]
                    for v in args.addVariable])
    elif arg == '--addModuleVariable':
        pft.addModuleVar([[v[0], v[1], v[2]] for v in args.addModuleVariable])
    elif arg == '--showUnusedVariables':
        pft.showUnusedVar()
    elif arg == '--removeUnusedLocalVariables':
        pft.removeUnusedLocalVar(
            [item.strip() for item in args.removeUnusedLocalVariables.split(',')]
            if args.removeUnusedLocalVariables != 'NONE' else None, **simplify)
    elif arg == '--removePHYEXUnusedLocalVariables':
        pft.removePHYEXUnusedLocalVar(
            [item.strip() for item in args.removePHYEXUnusedLocalVariables.split(',')]
            if args.removePHYEXUnusedLocalVariables != 'NONE' else None, **simplify)
    elif arg == '--addExplicitArrayBounds':
        pft.addExplicitArrayBounds()
    elif arg == '--addArrayParentheses':
        pft.addArrayParentheses()
    elif arg == '--modifyAutomaticArrays':
        pft.modifyAutomaticArrays(*(args.modifyAutomaticArrays.split('#')))
    elif arg == '--replaceAutomaticWithAllocatable':
        pft.modifyAutomaticArrays(
            "{type}, DIMENSION({doubledotshape}), ALLOCATABLE :: {name}",
            "ALLOCATE({name}({shape}))", "DEALLOCATE({name})")
    elif arg == '--addArgInTree':
        for varName, declStmt, pos in args.addArgInTree:
            pft.addArgInTree(varName, declStmt, int(pos), stopScopes,
                             parser=args.parser,
                             parserOptions=parserOptions,
                             wrapH=args.wrapH)


def applyTransfoApplications(pft, arg, args, simplify, parserOptions, stopScopes):
    """
    Apply applications transformations on a PYFT instance
    :param pft: PYFT instance
    :param arg: argument to deal with
    :param args: parsed argparsed arguments
    :param simplify: kwargs to simplify
    :param parserOptions: fxtran parser options
    :param stopScopes: upper limit in call tree for some transformations
    """
    if arg == '--addStack':
        pft.addStack(args.addStack, stopScopes,
                     parser=args.parser, parserOptions=parserOptions,
                     wrapH=args.wrapH)
    elif arg == '--deleteDrHook':
        pft.deleteDrHook(**simplify)
    elif arg == '--addDrHook':
        pft.addDrHook()
    elif arg == '--deleteBudgetDDH':
        pft.deleteBudgetDDH(**simplify)
    elif arg == '--deleteRoutineCallsMesoNHGPU':
        pft.deleteRoutineCallsMesoNHGPU(**simplify)
    elif arg == '--deleteNonColumnCallsPHYEX':
        pft.deleteNonColumnCallsPHYEX(**simplify)
    elif arg == '--addMPPDB_CHECKS':
        pft.addMPPDB_CHECKS()
    # mnhExpand must be before inlineContainedSubroutines as inlineContainedSubroutines
    # can change variable names used by mnh_expand directives
    assert not (args.mnhExpand and args.mnhExpandConcurrent), \
        "Only one of --mnhExpand and --mnhExpandConcurrent"
    if arg == '--mnhExpand':
        pft.removeArraySyntax(everywhere=False, addAccIndependentCollapse=False)
    elif arg == '--mnhExpandConcurrent':
        pft.removeArraySyntax(concurrent=True, everywhere=False)
    elif arg == '--inlineContainedSubroutines':
        pft.inlineContainedSubroutines(**simplify)
    elif arg == '--inlineContainedSubroutinesPHYEX':
        pft.inlineContainedSubroutinesPHYEX(**simplify)
    elif arg == '--expandAllArrays':
        pft.removeArraySyntax()
    elif arg == '--expandAllArraysConcurrent':
        pft.removeArraySyntax(concurrent=True)
    elif arg == '--expandAllArraysPHYEX':
        pft.expandAllArraysPHYEX()
    elif arg == '--expandAllArraysPHYEXConcurrent':
        pft.expandAllArraysPHYEX(concurrent=True)
    elif arg == '--removeIJDim':
        pft.removeIJDim(stopScopes,
                        parser=args.parser, parserOptions=parserOptions,
                        wrapH=args.wrapH, **simplify)
    elif arg == '--shumanFUNCtoCALL':
        pft.shumanFUNCtoCALL()
    elif arg == '--buildACCTypeHelpers':
        pft.buildACCTypeHelpers()
    elif arg == '--mathFunctoBRFunc':
        pft.mathFunctoBRFunc()
    elif arg == '--convertTypesInCompute':
        pft.convertTypesInCompute()
    elif arg == '--buildModi':
        pft.buildModi()
    elif arg == '--splitModuleRoutineFile':
        pft.splitModuleRoutineFile()


def applyTransfoOpenACC(pft, arg, args, stopScopes):  # pylint: disable=unused-argument
    """
    Apply openACC transformations on a PYFT instance
    :param pft: PYFT instance
    :param arg: argument to deal with
    :param args: parsed argparsed arguments
    :param stopScopes: upper limit in call tree for some transformations
    """
    if arg == '--addACCData':
        pft.addACCData()
    elif arg == '--craybyPassDOCONCURRENT':
        pft.craybyPassDOCONCURRENT()
    elif arg == '--removebyPassDOCONCURRENT':
        pft.removebyPassDOCONCURRENT()
    elif arg == '--addACCRoutineSeq':
        pft.addACCRoutineSeq(stopScopes)
    elif arg == '--removeACC':
        pft.removeACC()


def applyTransfoCosmetics(pft, arg, args):
    """
    Apply cosmetics transformations on a PYFT instance
    :param pft: PYFT instance
    :param arg: argument to deal with
    :param args: parsed argparsed arguments
    """
    if arg == '--upperCase':
        pft.upperCase()
    elif arg == '--lowerCase':
        pft.lowerCase()
    elif arg == '--changeIfStatementsInIfConstructs':
        pft.changeIfStatementsInIfConstructs()
    elif arg == '--indent':
        pft.indent()
    elif arg == '--removeIndent':
        pft.indent(indentProgramunit=0, indentBranch=0)
    elif arg == '--removeEmptyLines':
        pft.removeEmptyLines()
    elif arg == '--removeComments':
        pft.removeComments()
    elif arg == '--updateSpaces':
        pft.updateSpaces()
    elif arg in ARG_UPDATE_CNT:
        pft.updateContinuation(align=args.alignContinuation,
                               addBegin=args.addBeginContinuation,
                               removeBegin=args.removeBeginContinuation,
                               removeALL=args.removeALLContinuation)
    elif arg == '--prettify':
        pft.indent()
        pft.upperCase()
        pft.removeEmptyLines()
        pft.updateSpaces()
        pft.updateContinuation()
    elif arg == '--minify':
        pft.indent(indentProgramunit=0, indentBranch=0)
        pft.upperCase()
        pft.removeComments()
        pft.removeEmptyLines()
        pft.updateSpaces()
        pft.updateContinuation(align=False, removeALL=True, addBegin=False)
    elif arg == '--removeEmptyCONTAINS':
        pft.removeEmptyCONTAINS()


def applyTransfoChecks(pft, arg, args):
    """
    Apply checks transformations on a PYFT instance
    :param pft: PYFT instance
    :param arg: argument to deal with
    :param args: parsed argparsed arguments
    """
    if arg == '--checkIMPLICIT':
        pft.checkImplicitNone(args.checkIMPLICIT == 'Err')
    elif arg == '--checkINTENT':
        pft.checkIntent(args.checkINTENT == 'Err')
    elif arg == '--checkOpInCall':
        pft.checkOpInCall(args.checkOpInCall == 'Err')


def applyTransfoStatements(pft, arg, args, simplify):
    """
    Apply statements transformations on a PYFT instance
    :param pft: PYFT instance
    :param arg: argument to deal with
    :param args: parsed argparsed arguments
    :param simplify: kwargs to simplify
    """
    if arg == '--removeCall':
        for rc in args.removeCall:
            pft.removeCall(rc, **simplify)
    elif arg == '--removePrints':
        pft.removePrints(**simplify)
    elif arg == '--setFalseIfStmt':
        pft.setFalseIfStmt(args.setFalseIfStmt, **simplify)


def applyTransfoMisc(pft, arg, args, simplify):  # pylint: disable=unused-argument
    """
    Apply misc transformations on a PYFT instance
    :param pft: PYFT instance
    :param arg: argument to deal with
    :param args: parsed argparsed arguments
    :param simplify: kwargs to simplify
    """
    if arg == '--showScopes':
        pft.showScopesList()
    elif arg == '--empty':
        pft.empty(**simplify)


def applyTransfoTree(pft, arg, args, plotCentralFile):
    """
    Apply tree transformations on a PYFT instance
    :param pft: PYFT instance
    :param arg: argument to deal with
    :param args: parsed argparsed arguments
    :param plotCentralFile: central file for plots
    """
    if arg == '--plotCompilTree' and plotCentralFile is not None:
        pft.tree.plotCompilTreeFromFile(plotCentralFile, args.plotCompilTree,
                                        args.plotMaxUpper, args.plotMaxLower)
    elif arg == '--plotExecTree' and plotCentralFile is not None:
        pft.tree.plotExecTreeFromFile(plotCentralFile, args.plotExecTree,
                                      args.plotMaxUpper, args.plotMaxLower)


def applyTransfoPreprocessor(pft, arg, args):
    """
    Apply preprocessor transformations on a PYFT instance
    :param pft: PYFT instance
    :param arg: argument to deal with
    :param args: parsed argparsed arguments
    """
    if arg == '--applyCPPifdef':
        pft.applyCPPifdef([k for aList in args.applyCPPifdef for k in aList])
