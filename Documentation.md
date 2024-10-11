# User's guide

## Introduction

This package contains two python script, pyft\_tool.py and pyft\_parallel\_tool.py,
that read FORTRAN codes, parse them in xml, perform some manipulation, revert them
in FORTRAN and write them back on disk.
pyft\_tool.py performs these tasks on a per file basis whereas pyft\_parallel\_tool.py
perform the transformations in parallel on all files found in a tree.
In addition, the package can be used for scripting applicative transformations.

This package supposes that the original source code is written using UTF-8
encoding. If not, some special characters could be altered by the double
conversion. Apart from this, the resulting FORTRAN source code is exactly
the same as the INPUT source code if no manipulation is performed.

DEPENDENCIES:

 - [fxtran](https://github.com/pmarguinaud/fxtran) must be installed.
 - pyft needs, at least, version 3.8 of python

LIMITATIONS:

 - Depending on where there are put, pre-processor directives can break
   the parsing by fxtran
 - Other encoding than UTF-8 is not supported

Content of this documentation:

- [presentation of the scope path concept](#concepts)
- [tool options](#tool-options)
- [python module short description](#python-module)
- [examples and tests](#examples-and-tests)

## Concepts

Especially when a FORTRAN source file contains several subroutine, functions
or type declaration, it is necessary to specify on which part of the source
code a modification must be done.
This is achieved through the scope concept.
The scope path is a string representing a kind of path to access the source code
fragment on which the action must be performed.
A scope path is a succession of path elements separated by '/'; each path elements
has one of the following forms:

 - **module:_NAME_** to refer to the module named _NAME_
 - **sub:_NAME_** to refer to the subroutine named _NAME_
 - **func:_NAME_** to refer to the function named _NAME_
 - **type:_NAME_** to refer to the definition of the type named _NAME_

## Tool options

If only one file name is given, the output file will replace the input file.

The order of the arguments matters: transformation are done in that specific order.

### Parser options

**\--parser=PATH** can be used to specify the full path to the fxtran executable.
Usefull if one wants to use a specific version.

**\--parserOption=OPTIONS** the list of available options can be found in the fxtran
documentation. If no option is provided, the defaults one will be used (the
list of default options can be seen with "pyft.py -h"). In case this option
is used, the default options will be replaced by the ones specified.

**\--wrapH** Wrap .h file content into a MODULE to enable the parsing by fxtran.

### Input and output

**\--renamefF** transforms in upper case the file extension.

**\--renameFf** transforms in lower case the file extension.

**\--xml=filename** writes the xml file (after transformation) in filename.

**\--dryRun** prevents the FORTRAN writting.

### General options

**\--simplify** some transformations may render other parts of the code useless.
If this option is set, these parts are automatically removed.

**\--logLevel=LEVEL** specifies the log level to use (e.g. debug).
With the info level, execution time and number of calls are printed
for each called functions. In addition, with the debug level, input
and output of all the called functions are printed.

**\--enableCache** activates a cache to obtain the node's parent faster.

**\--nbPar** sets the number of parallel processes for the pyft\_parallel\tool.py
tool. 0 (default) to use as many processes as the number of cores.

**\--optsByEnv** Name of the environment variable containing additional arguments
to use. These arguments are processed after all other arguments.  The variable can
contain a multi-lines string. The variable is read line by line and the last
applicable line is used. A line can take one of these two forms:

  * "FILE\_DESCRIPTOR:=:OPTIONS": where FILE\_DESCRIPTOR is a regular expression to
    test against the filename. If there is a match, the OPTIONS can be used for the file.
  * "OPTIONS": if the line doesn\'t contain the FILE\_DESCRIPTOR part, it applies to
    all source code.

### Dealing with variables

**\--showVariables** displays a list of all the declared variables
with some characteristics.

**\--removeVariable SCOPEPATH VARNAME** removes the declaration of a local variable, a module variable or
of a dummy argument. In the case of a dummy argument, it is also suppresssed
from the argument of the subroutine. In case of a module variable, if the module
becomes unused, the use statement is also removed.
This options takes two argument, the first one describes where the variable
is declared (to distinguish between several variables holding the same name
but in different subroutines) and the second one is the variable name.
The first argument is a scope path as described in [Concepts](#concepts).

**\--addVariable SCOPEPATH VARNAME DECLARATION POSITION** adds a new variable.
The first argument describes where the variable
must be declared (it is a scope path as described in [Concepts](#concepts)).
The second one is the variable name, the third one is the declarative statement to insert,
and the fourth one is the position (python indexing) the new variable will have in the
calling statment of the routine (non-integer value for a local variable)

**--addModuleVariable SCOPEPATH MODULENAME VARNAME** adds a USE statement with an ONLY attribute. The first
argument describes where the variable must be declared (it is a scope path as
described in [Concepts](#concepts)). The second one is the module name and
the third one is the variable name.

**\--attachArraySpecToEntity** move the array declaration attributes to the right
part of the declaration statement (e.g. "REAL, DIMENSION(5) :: X" becomes "REAL :: X(5)")

**\--showUnusedVariables [SCOPEPATH]** lists the unused varibales. Without argument all the
unused variables are shown. If one argument is given it is the scope path (as described
in [Concepts](#concepts)) where unused variables are searched for.

**\--removeUnusedLocalVariables SCOPEPATH EXCLUDE** remove unused local variables. Without argument all the
unused variables are suppressed. If one argument is given it is the scope path (as described
in [Concepts](#concepts)) where unused variables are searched for.

**\--removePHYEXUnusedLocalVariables SCOPEPATH EXCLUDE** variation aroud the \--removeUnusedLocalVariables
to deal with the variables declared in the mnh\_expand directives.

**\--addExplicitArrayBounds** Adds explicit bounds to arrays that already have parentheses.

**\--addArrayParentheses** Adds parentheses to arrays.

**\--modifyAutomaticArrays DECL#START#END** modifies all automatic
arrays declaration in subroutine and functions. The declaration is replaced by the DECL template,
the START template is inserted as first executable statement and the END template as last executable
statement. Each template can use the following place holders: "{doubledotshape}", "{shape}", "{lowUpList}",
"{name}" and "{type}" which are, respectively modified into ":, :, :", "I, I:J, 0:I", "1, I, I, J, 0, I",
"A", "REAL" if the original declaration statement was "A(I, I:J, 0:I)". The template
"{type}, DIMENSION({doubledotshape}), ALLOCATABLE :: {name}#ALLOCATE({name}({shape}))#DEALLOCATE({name})"
replaces automatic arrays by allocatables.

**\--replaceAutomaticWithAllocatable** replace all automatic arrays with allocatables

**\--addArgInTree** Add an argument variable recursively. First argument is the scope (as
described in [Concepts](#concepts), the second is the variable name, the third is the
declarative statement to insert, the fourth is the position (python indexing) the new
variable will have in the calling statment of the routine. The recursive inclusion of
the argument variable stop at the scopes defined by the --stopScopes option.

### Cosmetics

**\--upperCase** puts the FORTRAN code into upper case letters.

**\--lowerCase** puts the FORTRAN code into lower case letters.

**\--changeIfStatementsInIfConstructs** transforms one line 'IF' contructs
in 'IF-THEN' constructs

**\--indent** correct the indentation of the source code.

**\--removeIndent** remove the indentation

**\--removeEmptyLines** remove empty lines.

**\--removeComments** remove the comments.

**\--updateSpaces** suppress and/or add spaces. Delimiters and operators must be surrounded by spaces.
Commas must be followed by a space. Lines must not end with spaces. Parenthesis must not be
surrounded by spaces...

**\--alignContinuation** align the beginings of continued lines.

**\--addBeginContinuation** add missing continuation characters ('&') at the begining of lines.

**\--removeBeginContinuation** remove continuation characters ('&') at the begining of lines.

**\--removeALLContinuation** remove all continuation characters('&').

**\--pretify** equivalent to --indent --upperCase --removeEmptyLines --updateSpaces
--addBeginContinuation --alignContinuation

**\--minify** equivalent to --removeIndent --upperCase --removeEmptyLines --removeComments
--updateSpaces --removeALLContinuation

**\--removeEmptyCONTAINS** removes CONTAINS statement when the section is empty

### Checks

**\--checkIMPLICIT=Warn\|Err** if the 'IMPLICIT NONE' statment is missing,
issue a warning if option is 'Warn'; otherwise issue an error message and
raise an exception.

**\--checkINTENT=Warn\|Err** if an INTENT attribute is missing for a
dummy argument, issue a warning if option is 'Warn'; otherwise issue
an error message and raise an exception.

**\--checkOpInCall=Warn\|Err** if a call argument is an operation, issue a warning
if option is 'Warn'; otherwise issue an error message and raise an exception.

### Dealing with statements

**\--removeCall SCOPEPATH CALLNAME** removes call statements. The first argument describes from where the
call statements must be suppressed (it is a scope path as described in [Concepts](#concepts)).
The second argument is the subprogram name.

**\--removePrints** removes print statements. The argument describes from where the
call statements must be suppressed (it is a scope path as described in [Concepts](#concepts)).

**\--inlineContainedSubroutines** inline containted subroutines in main routine.

**\--setFalseIfStmt VAR** replace VAR by .FALSE. in if statements.

### Miscellaneous

**\--showScopes** print the different scopes found in the source code.

**\--empty** empty the different scopes found in the source code.

### Applications

**\--deleteDrHook** removes DrHook statements.

**\--addDrHook** adds DrHook statements.

**\--deleteBudgetDDH** delete Budget/DDH use.

**\--deleteNonColumnCallsPHYEX** delete call to PHYEX routines that needs information on horizontal
points (multiple column dependency).

**\--removeIJDim** remove DO I and J dimensions (1, KLON).

**\--expandAllArraysPHYEX** expand all array syntax (computing and where block)
in DO loops using PHYEX conventions.

**\--expandAllArraysPHYEXConcurrent** expand all array syntax (computing and where block) 
in DO CONCURRENT loops using PHYEX conventions.

**\--expandAllArrays** expand all array syntax (computing and where block) in DO loops.

**\--expandAllArraysConcurrent** expand all array syntax (computing and where block) in DO CONCURRENT loops.

**\--inlineContainedSubroutinesPHYEX** inline containted subroutines in main routine using PHYEX conventions.

**\--addStack MODEL** add local automatic arrays to the stack. The argument is the
the model name in which stack must be added ("AROME" or "MESONH"). Needs the --stopScopes argument
for the "AROME" case.

**\--addIncludes** add .h includes in the file and remove the INCLUDE statement.

**\--mnhExpand** apply the mnh\_expand directives using DO loops.

**\--mnhExpandConcurrent** apply the mnh\_expand directives using DO CONCURRENT loops.

**\--addMPPDB_CHECKS** Add MPPDB\_CHEKS bit-repro checking routines of MesoNH for all in and
inout arrays in subroutines.

**\--shumanFUNCtoCALL** Transform shuman functions to call statements.

**\--mathFunctoBRFunc** Convert intrinsic math functions **, LOG, ATAN, **2, **3, **4, EXP,
COS, SIN, ATAN2 into a self defined function BR_ for MesoNH bit-repro.

### OpenACC

**\--addACC_data** add !$acc data present and !$acc end data directives

**\--addACC_routine_seq** add "!$acc routine seq" to routines called directly or indirectly
by scopes defined by the --stopScopes option.

**\--buildACCTypeHelpers** build module files containing helpers to copy user type structures.

**\--removeACC** remove ACC directives

### Preprocessor

**\--applyCPPifdef** This option is followed by the list of defined or undefined CPP keys.
All #ifdef and #ifndef concerning these keys are evaluated. Undefined keys are preceded by
a percentage sign '%' (e.g. if we use '--applyCPPifdef K', '#ifdef K' is evaluated to True;
whereas if we use '--applyCPPifdef %K', '#ifdef K' is evaluated to False.
But the method does not evaluate more complicated cpp directives such as '#if defined'.

### Tree

**\--tree** Directories where source code must be searched for.

**\--descTree** File name where the description of the tree is stored. If the file doesn't
exist, it will be created using the \--tree option.

**\--plotCompilTree** File name for compilation dependency graph (.dot or image extension).
If \--descTree is used, the descTree file will be used, otherwise the tree (provided
with the \--tree option) is explored. See \--plotMaxUpper and \--plotMaxLower options.

**\--plotExecTree** File name for the calling graph (.dot or image extension).
If \--descTree is used, the descTree file will be used, otherwise the tree (provided
with the \--tree option) is explored. See \--plotMaxUpper and \--plotMaxLower options.

**\--plotMaxUpper** Maximum number of elements to plot, upper than the central element.

**\--plotMaxLower** Maximum number of elements to plot, lower than the central element.

**\--stopScopes** #-separated list of scopes where the recursive inclusion of an
argument variable must stop (needed for some transformations)

## Python module

### Module overview

The main objet is the PYFTscope one; this class represents a FORTRAN scope (module,
subroutine, function, program or type). To ease the development, the different methods
of this class have been distributed to several files. In each of these files the
defined methods are attached to an abstract class (Variables in variables.py, Statements
in statements.py and so on for applications.py, cosmetics.py, cpp.py and openacc.py).
The main class PYFTscope is built by inhereting from all of these classes but none
of them is directly usable (cross-dependencies).

The PYFTscope is not directly instantiable because it doesn't contain read/write feature.
The PYFT class extends the PYFTscope class with those features.
And so, PYFT is the class publicly exposed.

An instance of PYFT represents the entire file and can contain several FORTRAN scopes.
If p is an instance of PYFT, p.getScopes() returns a list of PYFTscope instances, each
one representing a specific FORTRAN scope.

In order to deal with complex operations involving several source code files, a Tree
object can be created. This object analizes all the source code files present in the
specified directories to build a compilation tree and an execution tree. These trees
can be explored and/or plotted through methods of this object.

Another class, VarList, represents the list of variables defined in a scope.

In addition to these four classes, the module contains some functions in pyft.util and
pyft.expressions.

### The developer's point of view

New methods must be added in one of the abstract classes defined in variables.py,
statements.py... according to its main topic.
The application.py file deals with methods specific to PHYEX that are not universal.
The scripting.py modules contains functions used to build the two main tools.

Several decorators are available:
 - debugDecor (defined in tool): ease the debugging/profiling when logLevel is set
   to info or debug. The additional cost is low, except when the method is called
   many times; in this case, the decorator should not be used.
 - updateTree() (defined in tree): this decorator should be used for methods that can
   modify the execution tree. When decorated, the file is analized again to update
   the Tree object. updateTree can take a parameter:
   - 'file': in his case the current PYFTscope object is analyzed
   - 'scan': the Tree looks for new or removed files (caution, this method is expensive)
   - 'signal': some files or PYFTscope objects are analysed to update the Tree object.
     These files or PYFTscope objects are designed with the signal method of the Tree
     object.
  - updateVarList (defined in variables): this decorator should be used for methods that
    can modify the variables (existence, name, characteristics). It suppresses the
    cached version of the variable list (VarList instance) attached to the PYFT
    instance.
  - noParallel (defined in util): methods decorated with it cannot be executed in
    parallel with other methods decorated the same way. This is used to decorate
    methods that introduce modifications into the tree. For this reason, the
    noParallel decorator *must appear* before the updateTree one.

## Examples and tests

The examples directory contains a script (tests.sh) that performs a non-regression test.
But, this directory can also be used as a registry of transformation examples.
The files \*\_after.F90 are obtained by transforming the \*\_before.F90 files using
the command line options in comment at the very begining of these files.
