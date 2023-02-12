[GPF Home Page](http://www.urbanjost.altervista.org/LIBRARY/libGPF/GPF.html)

# KRACKEN(3F): The Fortran Command Line Argument Cracker  
*(Extended Version)*

## Supports FPM ![fpm](docs/images/fpm_logo.gif)
This is an fpm(1) package and not a stand-alone module. It requires fpm(1)
to build.

   download the github repository and build it with 
   fpm ( as described at [Fortran Package Manager](https://github.com/fortran-lang/fpm) )
   
   ```bash
        git clone https://github.com/urbanjost/M_kracken.git
        cd M_kracken
        fpm build
        fpm test
   ```
   
   or just list it as a dependency in your fpm.toml project file.
   
```toml
        [dependencies]
        M_kracken = { git = "https://github.com/urbanjost/M_kracken.git" }
```
## Index

  - [Abstract](#ABSTRACT)
  - [Example of typical use](#EX1)
  - [Routine Descriptions](#DESCRIPTION)
  - [Using the interactive menu mode of prompting](#MENU)
  - [Usage notes](#USAGE)
  - [Alternate parsing of the command line arguments](#ARGS)

## <span id="NAME">NAME</span>

M\_kracken(3fm) - parse command line options of
Fortran programs using Unix-like syntax (LICENSE:PD)

## <span id="ABSTRACT">ABSTRACT</span>

KRACKEN(3f) is a Fortran command line argument parser designed to
provide for easy entry of lists of negative numbers, strings, and
exponential numbers without generally requiring quotes on the command
line. It provides:

  - a standard Unix-like style for parsing arguments and keywords
  - a clear way to specify allowable keywords and default values
  - simple access to the parsed data from procedures
  - easy conversion from strings to numbers
  - easy conversion from strings to arrays
  - a simple menu-driven interactive mode for modifying parameters

You can call your command like this:

``` 
 mycode -r 333.333 -file /home/testin -l -i 300
```

with *very* little code:

## <span id="EX1">Example Usage</span>

    program myprogram
    
       use M_kracken
       character(255) filename
       logical lval
       !  define command arguments, default values and crack command line
       call kracken('cmd','-i 10 -r 10e3 -d 4.1123344d0 -l .false. -file input')
       !  get values
       call retrev('cmd_f',filename,iflen,ier) ! get -f FILENAME
       lval = lget('cmd_l')                    ! get -l present?
       rval = rget('cmd_r')                    ! get -r RVAL
       dval = dget('cmd_d')                    ! get -d DBLEVAL
       ival = iget('cmd_i')                    ! get -i IVAL
       !  all done parsing; do something with the values
       print *, "filename=",filename(:iflen)
       print *, " i=",ival, " r=",rval, " l=",lval, "d=",dval
    end program myprogram

See the documentation for the procedures for detailed descriptions. Each
procedure description includes a working example program.

## <span id="DESCRIPTION">The Routines</span>

[kracken](docs/kracken.3m_kracken.html) define command options and defaults and parse
command line

[retrev](docs/retrev.3m_kracken.html) get value for a keyword as a string

The returned strings obtained by calls to RETREV(3f) can be converted to
numeric values using procedures from the M\_strings(3fm) module such as
STRING\_TO\_VALUE(3F), which converts strings to a numeric value, and
SPLIT(3F), which can break a string into a list of words. But more
commonly, the following convenience routines are used ...

There are scalar convenience functions for getting simple values that
are used in most cases as an alternative to RETREV(3f) that convert the
values directly to common scalar types:

``` 
      lval=lget(VERB_ARGNAME) !gets a "logical" value.
      rval=rget(VERB_ARGNAME) !gets a "real" value.
      dval=dget(VERB_ARGNAME) !gets a "doubleprecision" value.
      ival=iget(VERB_ARGNAME) !gets a "integer" value
      sval=sget(VERB_ARGNAME) !gets a "character" value
```

There are also convenience routines for returning arrays of scalar
values that typically use allocatable arrays. Just add 's' to the end of
the scalar convenience functions.

``` 
      lvals=lgets(VERB_ARGNAME) !gets a "logical" array.
      rvals=rgets(VERB_ARGNAME) !gets a "real" array.
      dvals=dgets(VERB_ARGNAME) !gets a "doubleprecision" array.
      ivals=igets(VERB_ARGNAME) !gets a "integer" array
      svals=sgets(VERB_ARGNAME) !gets a "character" array
```

### SPECIAL-PURPOSE PUBLIC ROUTINES:

#### Setting command prompts

``` 
   public :: setprompts             ! define prompts for commands in interactive mode
```

#### Only needed for parsing input files, not cracking command line arguments

``` 
   dissect  ! for user-defined commands: define defaults, then process user input
   parse    ! parse user command and store tokens into Language Dictionary
   store    ! replace dictionary name's value (if allow=add add name if necessary)
   show     ! display dictionary contents for information
```

#### length of verbs and entries in Language dictionary

NOTE: many parameters may be reduced in size so as to just accommodate
being used as a command line parser. In particular, some might want to
change:

``` 
   logical,public            :: stop_command=.false. ! indication to return stop_command as false in interactive mode
   integer, parameter,public :: IPvalue=4096*16      ! length of keyword value
   integer, parameter,public :: IPverb=20            ! length of verb
   character(len=1),save,public         :: kracken_comment='#'
   character(len=:),allocatable,public  :: leftover                 ! remaining command(s) on line
   integer,public,save                  :: current_command_length=0 ! length of options for current command
```

## <span id="MENU">Interactive menu mode</span>

The menu mode feature is in a state of flux and may change significantly
...

All commands automatically have the parameter "-?". If it is present, a
menu appears after any specified options have been applied that allows
for changing parameters interactively.

The default prompts are the keywords themselves and their current
values. To set your own prompts call SETPROMPTS(3f):

``` 
   call setprompts(verb_name,options_and_prompts)
```

where the special prompt string "\#N\#" means to not allow prompting for
this parameter. For example:

``` 
     ! set prompts for interactive mode ...
     call setprompts('copy','                           &
     & -oo "#N#"                                        &
     & -i Enter input file name                         &
     & -o Enter output file name                        &
     & -version "#N#"                                   &
     & -help "#N#"                                      &
     & ')
     call kracken('copy','-i -o -version .false. -help .false')
```

Then the command

``` 
      copy -?
   
```

would only prompt for the -i and -o parameters.

A description on how to use the menu mode can be generated by entering a
question mark ("?") at the prompt once menu mode has been invoked.

## <span id="USAGE">Usage Notes</span>

### the reserved -oo keyword

Everything before any switch is always referred to as 'VERB\_oo' in
RETREV(3f). This same value can also be set later in the command line by
using the reserved keyword -oo (or the alias --). Often, you can ignore
it exists, but the -oo option is always there. Unlike other parameters a
default value is ignored unless no parameters are specified on the
command line. That is, in general do not set a default value for the -oo
parameter. It should almost always be initially a blank string.

Note that you can just put the calls to RETREV() or the convenience
routines where you need the information in your program instead of
parsing everything in a single routine. But parsing them and storing
them into a COMMON or MODULE is more efficient if the routine doing the
parsing is called many times.

``` 
  Sample showing -oo parameter and retrieving data in subroutines

     program demo_M_kracken
     use M_kracken, only : kracken
     ! define and crack command line arguments
     call kracken('cmd',' DEFAULT STRING -x 123 -y 456 ')
     call showstring()
     call showvalue()
     contains

     subroutine showstring()
     use M_kracken, only : sget
     character(len=:),allocatable :: string
     ! get value of string before any switch
     string=trim(sget('cmd_oo'))
     write(*,*)'string is ',string
     end subroutine showstring

     subroutine showvalue()
     use M_kracken, only : rget
     ! show values for -x and -y parameters
     x=rget('cmd_x')
     y=rget('cmd_y')
     write(*,*)' X and Y are ',x,y
     end subroutine showvalue

     end program demo_M_kracken

      xxx
       string is DEFAULT STRING
        X and Y are    123.000000       456.000000

      xxx -x 987.653992
       string is
        X and Y are    987.653992       456.000000

      xxx -oo BBBB -oo CCCC
       string is BBBB CCCC
        X and Y are    123.000000       456.000000

      xxx AAAA BBBB -oo CCCC
       string is AAAA BBBB CCCC
        X and Y are    123.000000       456.000000
```

You may note that the parsing rules are not identical to Unix, although
very similar.

    SYNTAX:
    verb[-oo|--] value for kw_oo  [-kw1 value_for_kw1] [-kw2 value_for_kw2] [-kw3 value_for_kw3] ...
    where
      "kw" stands for a keyword name

  - Quotes are rarely needed. A keyword is assumed whenever "
    -\[A-Za-Z\]" (space followed by dash followed by letter) is
    encountered. So
    
    ``` 
            cmd -title This is my title -value 10.3e2
            
    ```
    
    would produce a value of "This is my title" for dictionary value
    "cmd\_title". This does mean if your value contains " -letter" you
    must quote your command such that the program sees the string
    surrounded with double-quotes. Depending on the shell you are using
    this can be awkward. For example, in the bash shell you might use
    
    ``` 
            cmd -title '"-A is a bad title to need"'
            cmd -title /"-A is a bad title to need/"
            
    ```

  - The keyword -oo is implied after the verb.

  - There is no way to terminate a keyword value except by starting a
    new keyword. This means when you use shell globbing you often want
    filenames to be the *first* parameter (and dictionary "cmd\_oo" will
    hold the filenames):
    
    ``` 
            cmd * -value 10.3e2
          
    ```
    
    Many (but not all) Unix commands have such values allowed wherever
    another value is not allowed (Surprised? "ls -r \* -l" works as well
    as "ls -r -l \*" .). This is why quoting and specification of which
    keywords require values and which do not is usually required on Unix
    commands. Alternatively, just ignore the -oo field and always
    require keywords for all values.

  - You cannot combine keywords (-ir is not equivalent to -i -r, which
    is sometimes allowed on Unix commands).

  - Although this is rarely needed in practice, You may find the way to
    include a literal double-quote character (") as part of an input
    value is the most unlike Unix -- Double the double-quote. Again,
    shells often expand double-quotes, so in the bash(1) shell you might
    have to enter
    
    ``` 
             cmd  -string \"\"
          
    ```
    
    to give the "cmd\_string" dictionary value the simple value '"'.

  - \--KEYWORD is equivalent to -KEYWORD; primarily so that the
    --version and --help options are easily supported.

  - If a keyword is specified multiple times the values are concatenated
    into a single value with a space delimiter between the values. That
    is,
    
    ``` 
          cmd -D 10 -D 20 -D 30
          
    ```
    
    would set the dictionary variable "cmd\_D" to '10 20 30'.

  - All commands automatically have the -? keyword, which evokes
    interactive menu mode.

## <span id="ARGS">Alternate command line input syntax</span>

If you actually prefer quoting your arguments and having your unnamed
variables at the end of your command and support for the "--" option
supported by some GNU and Unix commands you can set the command line
parsing option "style='args'". This will *not* change how you specify
the parameters to the kracken(3f) command except possibly for Boolean
switches, but allows for entering commands in a manner more like the C
routine getopts(3c). If you turn the mode on the "-oo" parameter is
ignored unless you specifically assign it a value by name; the values
need quoted if they contain spaces and the option "--" specifies that
the rest of the command line is composed of unnamed strings. The unnamed
values will go into the CHARACTER array "unnamed". The way the command
line input is parsed is the same as in the M\_args(3f) module. That
means instead of entering:

program show\_standard use M\_kracken, only : kracken, sgets, rget,
sget, lget implicit none character(len=:),allocatable :: files(:)
character(len=:),allocatable :: title real :: x,y integer :: i \! define
and crack command line arguments call kracken('cmd',' --title this is my
title -x 123 -y 456 --help .false.') title=sget('cmd\_title')
x=rget('cmd\_x') y=rget('cmd\_y') write(\*,\*)'help=',lget('cmd\_help')
write(\*,\*)' title is ',title write(\*,\*)' x and y are ',x,y \! get
value of string before any switch files=sgets('cmd\_oo')
if(size(files).gt.0)then do i=1,size(files) write(\*,\*)i,files(i) enddo
endif end program show\_standard

Add "style='args'" to the kracken call and "unnamed" from the
M\_kracken(3fm) module, and the unnamed values will be in the string
array "unnamed" instead of obtained from something like
"sgets('cmd\_oo').

With "style='args' other differences are in how you specify your command
prototype. You MUST use the string ".false." for any boolean switch
variable default value; and basically the values ".true." and ".false."
are reserved, regardless of case. This is normally how you specify
booleans anyway; but in the default style you can set your default to
".true." or use other values for ".false." like "F" or ".F.".

The way the arguments will then be read upon input are the same as
described in the M\_args(3fm) module.

program show\_alternate use M\_kracken, only : kracken, sget, rget, lget
USE M\_KRACKEN, ONLY : UNNAMED implicit none
character(len=:),allocatable :: title real :: x,y integer :: i \! define
and crack command line arguments \! \! call kracken('cmd',' --title this
is my title & & -x 123 -y 456 --help .false.',style='args')
title=sget('cmd\_title') x=rget('cmd\_x') y=rget('cmd\_y')
write(\*,\*)'help=',lget('cmd\_help') write(\*,\*)' title is ',title
write(\*,\*)' x and y are ',x,y IF(SIZE(UNNAMED).GT.0)THEN DO
I=1,SIZE(UNNAMED) WRITE(\*,\*)I,UNNAMED(I) ENDDO ENDIF end program
show\_alternate

``` 
 Sample usage:

    # first program uses default M_kracken parsing

       cmd *.f90 -x 100 --title A new title -y 200

    # or

       cmd -x 100 --title A new title -y 200 -oo *.f90

    # second program uses alternate command-line parsing.
    # the unnamed values go into the string array
    # UNNAMED but can appear anywhere on the command.
    # multi-word values require being quoted.

    cmd -x 100 -y 200 --title 'A new title' *.f90
```

> Contributors:  
> 
>   - John S. Urban -- Author (last change: Oct. 2019)
>   - Felix Becker -- Enhancements to reduce limitations on parameter
>     lengths (2013-05-28)
>   - Walid Keyrouz -- Upgrades to bring code into conformance with
>     recommended practices (2013-12-06)

