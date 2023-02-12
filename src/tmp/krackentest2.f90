!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
program krackentest2

   use M_kracken, only : kracken, lget,  rget,  iget,  dget,  sget,  retrev
   use M_kracken, only :          lgets, rgets, igets, dgets, sgets
   implicit none
   character(len=255)          :: filename

   integer                     :: ival
   logical                     :: lval
   real                        :: rval
   doubleprecision             :: dval

   integer,allocatable                :: ivals(:)
   logical,allocatable                :: lvals(:)
   real,allocatable                   :: rvals(:)
   doubleprecision,allocatable        :: dvals(:)
   character(len=:),allocatable       :: strings(:)

   integer :: iflen
   integer :: ier
   integer :: i
!  define the command options and default values and apply arguments from user command line
   call kracken("cmd", " &
   ! set some scalar values
   & -i 10 -r 10e3 -l ""#N#"" -f input -d 5.0d0 &
   ! set some vector values
   & -ds 1 2 3 4 5 -ls .t. .t. .f. .t. -rs 3.3 4.4 -is 10 20 30 40 &
   & -strings This is a sentence &
!  common versions of help switch
   & -h .F. -help .F.    &
!  common versions of version switch
   & -v .F. -version .F. &
   &")
!----------------------------------------------------------------------------------------
!  handle version and help requests allowing most common variants
   call help_version("cmd")
!----------------------------------------------------------------------------------------
!  get the values specified on the command line
   write(*,*)'retrev:'
   call retrev("cmd_f",filename,iflen,ier)  ! get -f FILENAME
   lval = lget("cmd_l")                     ! get -l present?
   rval = rget("cmd_r")                     ! get -r RVAL
   ival = iget("cmd_i")                     ! get -i INTEGER
   dval = dget("cmd_d")                     ! get -d DOUBLEPRECISION
   lvals= lgets("cmd_ls")                   ! get -ls present?
   rvals= rgets("cmd_rs")                   ! get -rs RVAL
   ivals= igets("cmd_is")                   ! get -is INTEGER
   dvals= dgets("cmd_ds")                   ! get -ds DOUBLEPRECISION
   strings= sgets("cmd_strings")            ! get -strings words
!----------------------------------------------------------------------------------------
   write(*,*)'show values:'
   print *, "filename=",filename(:iflen)
!----------------------------------------------------------------------------------------
   print *, "i=",ival
   print *, "r=",rval
   print *, "l=",lval
   print *, "d=",dval
!----------------------------------------------------------------------------------------
   print *, "is=",ivals
   print *, "rs=",rvals
   print *, "ls=",lvals
   print *, "ds=",dvals
   !print *, "strings=",(trim(strings(i)),new_line("A"),i=1,size(strings))
   print *, "strings=",('['//trim(strings(i))//']',i=1,size(strings))
!----------------------------------------------------------------------------------------

   call retrev("cmd_oo",filename,iflen,ier) ! verb shows on ifort, does not on g95
   print *, "cmd_oo=",filename(:iflen)

   call get_command_argument(0,filename)
   print *, "arg(0)=",trim(filename)

!  ANOTHER STRING EXAMPLE
   filename=sget('cmd_f')
   write(*,*)'filename=',trim(filename)
end program krackentest2
!----------------------------------------------------------------------------------------
   subroutine help_version(verb)
!  just about every program should handle --version and --help options the same

   use M_kracken
   implicit none
   character(len=*),parameter :: ident="@(#)M_kracken::help_version: handle version and help requests allowing most common variants"
   character(len=*)           :: verb
   logical :: stopit=.false.

   if( lget(trim(verb)//'_v').or.lget(trim(verb)//'_version')) then ! see if -v or -version was specified
      write(*,*)'*TESTIT* Version 1-A'
      stopit=.true.
   endif

   if( lget(trim(verb)//'_h').or.lget(trim(verb)//'_help')) then ! see if -h or -help was specified
      write(*,*)' cmd '
      write(*,*)' [-i ANY_INTEGER_VALUE]  # default value = 10'
      write(*,*)' [-r ANY_REAL_VALUE]     # default_value= 1000'
      write(*,*)' [-l LOGICAL_VALUE]      # default_value=.F.'
      write(*,*)' [-f FILENAME]           # default_value="input"'
      write(*,*)' [-h|-help|--help]|      # help flag=.F.'
      write(*,*)' [-v|-version|--version] # version flag=.F.'
      write(*,*)''
      stopit=.true.
   endif

   if(stopit)then                                    ! if version or help were called end program
      stop
   endif

   end subroutine help_version
!----------------------------------------------------------------------------------------
