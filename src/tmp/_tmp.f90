module M_msg
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT,OUTPUT_UNIT    ! access computing environment
implicit none
private
!-----------------------------------------------------------------------------------------------------------------------------------
! USED SO FREQUENTLY IN OTHER MODULES PUT IN THIS ONE WITH NO DEPENDENCIES TO PREVENT CIRCULAR DEPENDENCY
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_1="@(#)M_msg::str(3f): {msg_scalar,msg_one}"

public str
public stderr
public wrt
public fmt
!!public :: a,i,f,g

interface str
   module procedure msg_scalar, msg_one
end interface str

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    str(3f) - [M_msg] converts any standard scalar type to a string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    Syntax:
!!
!!      function str(g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,&
!!      & ga,gb,gc,gd,ge,gf,gg,gh,gi,gj,sep)
!!      class(*),intent(in),optional  :: g0,g1,g2,g3,g4,g5,g6,g7,g8,g9
!!      class(*),intent(in),optional  :: ga,gb,gc,gd,ge,gf,gg,gh,gi,gj
!!      character(len=*),intent(in),optional :: sep
!!      character,len=(:),allocatable :: str
!!
!!##DESCRIPTION
!!    str(3f) builds a space-separated string from up to twenty scalar values.
!!
!!##OPTIONS
!!    g[0-9a-j]   optional value to print the value of after the message. May
!!                be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION,
!!                COMPLEX, or CHARACTER.
!!
!!                Optionally, all the generic values can be
!!                single-dimensioned arrays. Currently, mixing scalar
!!                arguments and array arguments is not supported.
!!
!!    sep         separator string used between values. Defaults to a space.
!!
!!##RETURNS
!!    str     description to print
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_msg
!!    use M_msg, only : str
!!    implicit none
!!    character(len=:),allocatable :: pr
!!    character(len=:),allocatable :: frmt
!!    integer                      :: biggest
!!
!!    pr=str('HUGE(3f) integers',huge(0),&
!!    &'and real',huge(0.0),'and double',huge(0.0d0))
!!    write(*,'(a)')pr
!!    pr=str('real            :',huge(0.0),0.0,12345.6789,tiny(0.0) )
!!    write(*,'(a)')pr
!!    pr=str('doubleprecision :',huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
!!    write(*,'(a)')pr
!!    pr=str('complex         :',cmplx(huge(0.0),tiny(0.0)) )
!!    write(*,'(a)')pr
!!
!!    ! create a format on the fly
!!    biggest=huge(0)
!!    frmt=str('(*(i',int(log10(real(biggest))),':,1x))',sep='')
!!    write(*,*)'format=',frmt
!!
!!    ! although it will often work, using str(3f)
!!    ! in an I/O statement is not recommended
!!    ! because if an error occurs str(3f) will try
!!    ! to write while part of an I/O statement
!!    ! which not all compilers can handle and is currently non-standard
!!    write(*,*)str('program will now stop')
!!
!!    end program demo_msg
!!
!!  Output
!!
!!    HUGE(3f) integers 2147483647 and real 3.40282347E+38 and double 1.7976931348623157E+308
!!    real            : 3.40282347E+38 0.00000000 12345.6787 1.17549435E-38
!!    doubleprecision : 1.7976931348623157E+308 0.0000000000000000 12345.678900000001 2.2250738585072014E-308
!!    complex         : (3.40282347E+38,1.17549435E-38)
!!     format=(*(i9:,1x))
!!     program will now stop
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function msg_scalar(generic0, generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9, &
                  & generica, genericb, genericc, genericd, generice, genericf, genericg, generich, generici, genericj, &
                  & sep)
implicit none

! ident_2="@(#)M_msg::msg_scalar(3fp): writes a message to a string composed of any standard scalar types"

class(*),intent(in),optional  :: generic0, generic1, generic2, generic3, generic4
class(*),intent(in),optional  :: generic5, generic6, generic7, generic8, generic9
class(*),intent(in),optional  :: generica, genericb, genericc, genericd, generice
class(*),intent(in),optional  :: genericf, genericg, generich, generici, genericj
character(len=:),allocatable  :: msg_scalar
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
   if(present(sep))then
      increment=len(sep)+1
      sep_local=sep
   else
      increment=2
      sep_local=' '
   endif

   istart=1
   line=''
   if(present(generic0))call print_generic(generic0)
   if(present(generic1))call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   if(present(generica))call print_generic(generica)
   if(present(genericb))call print_generic(genericb)
   if(present(genericc))call print_generic(genericc)
   if(present(genericd))call print_generic(genericd)
   if(present(generice))call print_generic(generice)
   if(present(genericf))call print_generic(genericf)
   if(present(genericg))call print_generic(genericg)
   if(present(generich))call print_generic(generich)
   if(present(generici))call print_generic(generici)
   if(present(genericj))call print_generic(genericj)
   msg_scalar=trim(line)
contains
!===================================================================================================================================
subroutine print_generic(generic)
!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, biggest=>int64, real32, real64, dp=>real128
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in) :: generic
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'(i0)') generic
      type is (integer(kind=int16));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int32));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int64));    write(line(istart:),'(i0)') generic
      type is (real(kind=real32));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real64));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real128));     write(line(istart:),'(1pg0)') generic
      type is (logical);                write(line(istart:),'(l1)') generic
      type is (character(len=*));       write(line(istart:),'(a)') trim(generic)
      type is (complex);                write(line(istart:),'("(",1pg0,",",1pg0,")")') generic
   end select
   istart=len_trim(line)+increment
   line=trim(line)//sep_local
end subroutine print_generic
!===================================================================================================================================
end function msg_scalar
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function msg_one(generic0,generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9,&
               & generica,genericb, genericc, genericd, generice, genericf, genericg, generich, generici, genericj,&
               & sep)
implicit none

! ident_3="@(#)M_msg::msg_one(3fp): writes a message to a string composed of any standard one dimensional types"

class(*),intent(in)           :: generic0(:)
class(*),intent(in),optional  :: generic1(:), generic2(:), generic3(:), generic4(:), generic5(:)
class(*),intent(in),optional  :: generic6(:), generic7(:), generic8(:), generic9(:)
class(*),intent(in),optional  :: generica(:), genericb(:), genericc(:), genericd(:), generice(:)
class(*),intent(in),optional  :: genericf(:), genericg(:), generich(:), generici(:), genericj(:)
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
character(len=:), allocatable :: msg_one
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(present(sep))then
      increment=1+len(sep)
      sep_local=sep
   else
      sep_local=' '
      increment=2
   endif

   istart=1
   line=' '
   call print_generic(generic0)
   if(present(generic1))call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   if(present(generica))call print_generic(generica)
   if(present(genericb))call print_generic(genericb)
   if(present(genericc))call print_generic(genericc)
   if(present(genericd))call print_generic(genericd)
   if(present(generice))call print_generic(generice)
   if(present(genericf))call print_generic(genericf)
   if(present(genericg))call print_generic(genericg)
   if(present(generich))call print_generic(generich)
   if(present(generici))call print_generic(generici)
   if(present(genericj))call print_generic(genericj)
   msg_one=trim(line)
contains
!===================================================================================================================================
subroutine print_generic(generic)
!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, biggest=>int64, real32, real64, dp=>real128
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in),optional :: generic(:)
integer :: i
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int16));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int32));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int64));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (real(kind=real32));      write(line(istart:),'("[",*(1pg0,1x))') generic
      type is (real(kind=real64));      write(line(istart:),'("[",*(1pg0,1x))') generic
      type is (real(kind=real128));     write(line(istart:),'("[",*(1pg0,1x))') generic
      !type is (real(kind=real256));     write(error_unit,'(1pg0)',advance='no') generic
      type is (logical);                write(line(istart:),'("[",*(l1,1x))') generic
      type is (character(len=*));       write(line(istart:),'("[",:*("""",a,"""",1x))') (trim(generic(i)),i=1,size(generic))
      type is (complex);                write(line(istart:),'("[",*("(",1pg0,",",1pg0,")",1x))') generic
      class default
         stop 'unknown type in *print_generic*'
   end select
   istart=len_trim(line)+increment+1
   line=trim(line)//']'//sep_local
end subroutine print_generic
!===================================================================================================================================
end function msg_one
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    fmt(3f) - [M_msg] convert any intrinsic to a string using specified format
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function fmt(value,format) result(string)
!!
!!     class(*),intent(in),optional :: value
!!     character(len=*),intent(in)  :: format
!!     character(len=:),allocatable :: string
!!##DESCRIPTION
!!    FMT(3f) converts any standard intrinsic value to a string using the specified
!!    format.
!!##OPTIONS
!!    value    value to print the value of. May be of type INTEGER, LOGICAL,
!!             REAL, DOUBLEPRECISION, COMPLEX, or CHARACTER.
!!    format   format to use to print value. It is up to the user to use an
!!             appropriate format. The format does not require being
!!             surrounded by parenthesis.
!!##RETURNS
!!    string   A string value
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_fmt
!!     use :: M_msg, only : fmt
!!     implicit none
!!     character(len=:),allocatable :: output
!!
!!        output=fmt(10,"'[',i0,']'")
!!        write(*,*)'result is ',output
!!
!!        output=fmt(10.0/3.0,"'[',g0.5,']'")
!!        write(*,*)'result is ',output
!!
!!        output=fmt(.true.,"'The final answer is [',g0,']'")
!!        write(*,*)'result is ',output
!!
!!     end program demo_fmt
!!
!!   Results:
!!
!!     result is [10]
!!     result is [3.3333]
!!     result is The final answer is [T]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
recursive function fmt(generic,format) result (line)

! ident_4="@(#)M_msg::fmt(3f): convert any intrinsic to a string using specified format"

use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in)          :: generic
character(len=*),intent(in)  :: format
character(len=:),allocatable :: line
character(len=:),allocatable :: fmt_local
integer                      :: ios
character(len=255)           :: msg
character(len=1),parameter   :: null=char(0)
integer                      :: ilen
   fmt_local=format
   ! add ",a" and print null and use position of null to find length of output
   ! add cannot use SIZE= or POS= or ADVANCE='NO' on WRITE() on INTERNAL READ,
   ! and do not want to trim as trailing spaces can be significant
   if(fmt_local.eq.'')then
      select type(generic)
         type is (integer(kind=int8));     fmt_local='(i0,a)'
         type is (integer(kind=int16));    fmt_local='(i0,a)'
         type is (integer(kind=int32));    fmt_local='(i0,a)'
         type is (integer(kind=int64));    fmt_local='(i0,a)'
         type is (real(kind=real32));      fmt_local='(1pg0,a)'
         type is (real(kind=real64));      fmt_local='(1pg0,a)'
         type is (real(kind=real128));     fmt_local='(1pg0,a)'
         type is (logical);                fmt_local='(l1,a)'
         type is (character(len=*));       fmt_local='(a,a)'
         type is (complex);                fmt_local='("(",1pg0,",",1pg0,")",a)'
      end select
   else
      if(format(1:1).eq.'(')then
         fmt_local=format(:len_trim(format)-1)//',a)'
      else
         fmt_local='('//fmt_local//',a)'
      endif
   endif
   allocate(character(len=256) :: line) ! cannot currently write into allocatable variable
   ios=0
   select type(generic)
      type is (integer(kind=int8));     write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (integer(kind=int16));    write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (integer(kind=int32));    write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (integer(kind=int64));    write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (real(kind=real32));      write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (real(kind=real64));      write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (real(kind=real128));     write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (logical);                write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (character(len=*));       write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (complex);                write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
   end select
   if(ios.ne.0)then
      line='<ERROR>'//trim(msg)
   else
      ilen=index(line,null,back=.true.)
      if(ilen.eq.0)ilen=len(line)
      line=line(:ilen-1)
   endif
end function fmt
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    stderr(3f) - [M_msg] write message to stderr
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine stderr(msg,[generic])
!!
!!     class(*),intent(in),optional :: msg
!!     class(*),intent(in),optional :: generic0,generic1,generic2,generic3,generic4
!!     class(*),intent(in),optional :: generic5,generic6,generic7,generic8,generic9
!!##DESCRIPTION
!!    STDERR(3f) writes a message to standard error using a standard f2003 method.
!!    Up to ten generic options are available.
!!##OPTIONS
!!    msg           - description to print
!!    generic[0-9]  - optional value to print the value of after the message. May
!!                    be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!                    or CHARACTER.
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_stderr
!!    use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
!!    use,intrinsic :: iso_fortran_env, only : real32, real64, real128
!!    use,intrinsic :: iso_fortran_env, only : real=> real32, integer=> int32
!!    use M_msg, only: stderr
!!    implicit none
!!
!!    call stderr('A simple message')
!!    call stderr('error: RVALUE=',3.0/4.0)
!!    call stderr('error: IVALUE=',123456789)
!!    call stderr('error: LVALUE=',.true.)
!!
!!    SEVERAL: block
!!    integer :: least=10, most=999, ival=-10
!!    call stderr('error: value',ival,'should be between',least,'and',most)
!!    endblock SEVERAL
!!
!!    call stderr('real32  :',huge(0.0_real32),0.0_real32,12345.6789_real32,tiny(0.0_real32))
!!    call stderr('real64  :',huge(0.0_real64),0.0_real64,12345.6789_real64,tiny(0.0_real64))
!!    call stderr('real128 :',huge(0.0_real128),0.0_real128,12345.6789_real128,tiny(0.0_real128))
!!    call stderr('complex :',cmplx(huge(0.0_real),tiny(0.0_real)))
!!
!!    call stderr('error: program will now stop')
!!    stop 1
!!
!!    end program demo_stderr
!!
!!   Results:
!!     A simple message
!!     error: RVALUE= 0.750000000
!!     error: IVALUE= 123456789
!!     error: LVALUE= T
!!     error: value -10 should be between 10 and 999
!!     real32  : 3.40282347E+38 ...
!!               0.00000000 ...
!!               12345.6787 ...
!!               1.17549435E-38
!!     real64  : 1.7976931348623157E+308 ...
!!               0.0000000000000000 ...
!!               12345.678900000001 ...
!!               2.2250738585072014E-308
!!     real128 : 1.18973149535723176508575932662800702E+4932 ...
!!               0.00000000000000000000000000000000000  ...
!!               12345.6789000000000000000000000000002 ...
!!               3.36210314311209350626267781732175260E-4932
!!     complex : (3.40282347E+38,1.17549435E-38)
!!     error: program will now stop
!!     STOP 1
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine stderr(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj)
implicit none

! ident_5="@(#)M_msg::stderr(3f): writes a message to standard error using a standard f2003 method"

class(*),intent(in),optional :: g0, g1, g2, g3, g4, g5, g6, g7, g8, g9
class(*),intent(in),optional :: ga, gb, gc, gd, ge, gf, gg, gh, gi, gj
integer                      :: ios
   write(error_unit,'(a)',iostat=ios) str(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj)
   flush(unit=output_unit,iostat=ios)
   flush(unit=error_unit,iostat=ios)
end subroutine stderr
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    wrt(3f) - [M_msg] write multiple scalar values to any number of files
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine wrt(luns,generic(s),iostat)
!!
!!     integer,intent(in)           :: luns(:)
!!     class(*),intent(in),optional :: generic0,generic1,generic2,generic3,generic4
!!     class(*),intent(in),optional :: generic5,generic6,generic7,generic8,generic9
!!     class(*),intent(in),optional :: generica,genericb,genericc,genericd,generice
!!     class(*),intent(in),optional :: genericf,genericg,generich,generici,genericj
!!     integer,intent(out),optional :: ios
!!##DESCRIPTION
!!    WRT(3f) writes a list of scalar values  to the list of unit numbers in LUNS(:).
!!##OPTIONS
!!    LUNS            Unit numbers to write to. If of size zero no output is generated
!!    generic[1-20]   optional value to print the value of after the message. May
!!                    be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!                    or CHARACTER.
!!##RETURNS
!!    IOSTAT          The value of the last non-zero IOSTAT value. Returns zero if
!!                    no errors occurred.
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_wrt
!!    use, intrinsic :: iso_fortran_env, only : &
!!     & stdin=>input_unit, &
!!     & stdout=>output_unit, &
!!     & stderr=>error_unit
!!    use M_msg, only: wrt, fmt
!!    implicit none
!!    integer,allocatable :: luns(:)
!!    integer :: iostat=0
!!    integer,parameter :: ints(3)=[1,2,3]
!!
!!    ! a null list allows for turning off verbose or debug mode output
!!    luns=[integer ::]
!!    call wrt(luns,'NULL LIST:',huge(0),'PI=',asin(1.0d0)*2.0d0,iostat=iostat)
!!    write(*,*)'IOSTAT=',iostat
!!
!!    ! multiple files can be used to create a log file, for example
!!    luns=[stderr,stdout]
!!    call wrt(luns,'TWO FILES:',huge(0),'PI=',asin(1.0d0)*2.0d0,iostat=iostat)
!!    write(*,*)'IOSTAT=',iostat
!!
!!    ! using fmt
!!    call wrt([stdout,stdout,stdout],'USING FMT :', &
!!     & huge(0),'PI=',asin(1.0d0)*2.0d0,fmt(ints(2),'i0.4'),iostat=iostat)
!!
!!
!!    end program demo_wrt
!!
!!##TWO FILES: 2147483647 PI= 3.1415926535897931
!!##TWO FILES: 2147483647 PI= 3.1415926535897931
!!  IOSTAT=           0
!!##USING FMT : 2147483647 PI= 3.1415926535897931 0002
!!##USING FMT : 2147483647 PI= 3.1415926535897931 0002
!!##USING FMT : 2147483647 PI= 3.1415926535897931 0002
!!  IOSTAT=           0
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine wrt(luns,g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj,iostat)

! ident_6="@(#)M_msg::write(3f): writes a message to any number of open files with any scalar values"

implicit none
integer,intent(in)           :: luns(:)
class(*),intent(in),optional :: g0, g1, g2, g3, g4, g5, g6, g7, g8, g9
class(*),intent(in),optional :: ga, gb, gc, gd, ge, gf, gg, gh, gi, gj
integer,intent(out),optional :: iostat
integer                      :: i
character(len=256)           :: msg
   do i=1,size(luns)
      write(luns(i),'(a)',iostat=iostat,iomsg=msg)str(g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,ga,gb,gc,gd,ge,gf,gg,gh,gi,gj)
      if(iostat.ne.0)then
         call stderr('<ERROR>*write*:',msg)
      endif
   enddo
end subroutine wrt
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_msg
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    M_verify(3fm) - [M_verify] a collection of Fortran routines for
!!                    supporting code development by providing error
!!                    processing, debugging procedures and unit testing.
!!                    (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!  Module procedures
!!
!!    use M_verify, only : unit_check, unit_check_start, unit_check_done
!!    use M_verify, only : unit_check_good, unit_check_bad
!!    use M_verify, only : unit_check_msg
!!    use M_verify, only : debug
!!    use M_verify, only : fstop
!!    use M_verify, only : assert
!!
!!  Module values
!!
!!    use M_verify, only : unit_check_limit, unit_check_keep_going
!!    use M_verify, only : unit_check_command
!!
!!##QUOTE
!!    Do not let your victories go to your head, nor let your failures go
!!    to your heart.
!!
!!##DESCRIPTION
!!    The M_verify(3fm) Fortran module provides procedures and variables
!!    useful for providing error processing, debugging capabilities, and
!!    unit testing.
!!
!!     o allows for a user-defined command to be called to collect results or
!!       mail failure alerts, ...
!!     o supports easily composing a message from up to nine scalar
!!       intrinsic values and different message levels
!!     o allows stopping on first failure or continuing
!!
!!    SET MODES
!!    unit_check_keep_going  logical variable that can be used to turn off
!!                           program termination on errors.
!!    unit_check_level       An integer that can be used to specify
!!                           different debug levels
!!    unit_check_command     name of command to execute. Defaults to the name
!!                           "goodbad".
!!    UNIT TESTS
!!    unit_check_start(3f)   start tests of a procedure and optionally call
!!
!!                              command NAME start ...
!!    unit_check(3f)         if expression is false optionally call
!!
!!                              command NAME bad
!!
!!                           and stop program (by default)
!!
!!    unit_check_done(3f)    call
!!
!!                              command NAME good
!!
!!                           if no failures; else call
!!
!!                              command NAME bad
!!
!!    unit_check_good(3f)    call command
!!
!!                              command NAME good
!!
!!    unit_check_bad(3f)     call command
!!
!!                              command NAME bad
!!
!!                           and stop program by default
!!    unit_check_msg(3f)     write message
!!
!!    BASIC DEBUGGING
!!    fstop(3f)             calls 'STOP VALUE' passing in a value (1-32),
!!                          with optional message
!!    pdec(3f)              write ASCII Decimal Equivalent (ADE) numbers
!!                          vertically beneath string
!!    debug                 logical variable that can be tested by routines
!!                          as a flag to process debug statements.
!!
!!    For unit testing, the existence of a command called "goodbad" is
!!    initially assumed. This is generally a script that makes entries
!!    for each unit in an SQLite data file which is then used to create
!!    CSV and HTML reports on the status of each unit. A sample goodbad(1)
!!    command written in the bash(1) shell and using the sqlite3(1) command
!!    should be included in this distribution as an example.
!!
!!    The flexibility introduced by calling an external script or program
!!    is that The goodbad(1) command can be changed as desired to write CSV
!!    files or simple logs or to notify developers with e-mail as desired.
!!
!!    RELATED FUNCTIONS
!!
!!    The routines in M_verify(3f) are often combined with the M_hashkeys(3fm)
!!    routines and various math and statistical routines to quickly create
!!    unit tests.
!!
!!    Comparisons of real values can be done with a tolerance with
!!    M_Compare_Float_Numbers(3fm), for example.
!!
!!    The intrinsics ANY(3f) and ALL(3f) are particularly useful in calls
!!    to unit_check(3f).
!!
!!##EXAMPLES
!!
!!   Sample program
!!
!!     !!program demo_unit_tests
!!     module M_demo
!!     private
!!     public one !! regular routines
!!     public two !! regular routines
!!     public test_suite_M_demo !! special name for use with test_suite(1bash).
!!     contains
!!
!!     !!  regular routines
!!     subroutine one()
!!     end subroutine one
!!
!!     subroutine two()
!!     end subroutine two
!!
!!     !! unit test
!!     subroutine test_suite_M_demo
!!     use M_verify, only: unit_check_start, unit_check
!!     use M_verify, only: unit_check_good, unit_check_bad, unit_check_done
!!     use M_verify, only: unit_check_msg
!!     implicit none
!!     integer :: i, j, k
!!     integer,allocatable :: array(:)
!!     integer :: arr(4)=[21,51,14,45]
!!     integer :: a=21, b=51, c=14, d=45
!!     ! TEST-DRIVEN DEVELOPMENT
!!     ! optional set-up       perform initialization operations common to all tests within a module
!!        i=1
!!        j=2
!!        k=3
!!        array=[10,20,30,40,50,60,70]
!!        call test_one()
!!        call test_two()
!!     ! optional tear-down    perform finalization operations common to all tests within a module
!!     contains
!!
!!     subroutine test_one()
!!     !  register an entry for specified name ("one") in database with status of zero (0)
!!     call unit_check_start('one')
!!
!!     !  if mask test fails, can
!!     !  * produce a SUCCESS: or FAIL: message and stop program
!!     !  * change database status for specified entry to -1 and stop program, else continue
!!     !  * produce a SUCCESS: or FAIL: message and keep going
!!     !  * produce a FAIL: message if test fails but no SUCCESS: message if test passes
!!     call unit_check('one',i.gt.0,msg='I > 0')
!!
!!     ! using ANY(3f) and ALL(3f)
!!     call unit_check('one',all([i,j,k].gt.0),      'testing if everyone greater than zero')
!!     ! display message built of scalars as well
!!     call unit_check('one',all(.not.[i,j,k].eq.4),'for set ',i,j,k,'testing if no one is equal to four')
!!
!!     ! for tests that are hard to reduce to a logical test just call unit_check_bad(3f) if fail
!!     if(i+j+k.lt.1)then
!!        call unit_check_bad('one')
!!     endif
!!
!!     call unit_check_done('one','checks on "one" ended')
!!     end subroutine test_one
!!
!!     subroutine test_two
!!     ! use of all(3f), any(3f), merge(3f) can be useful
!!     ! if you know what these would produce
!!     ! write(*,*)['A','X','X','X','X','B'].eq.'B'      ! this would return an array, the last element having the value T, else F
!!     ! write(*,*)all(['A','X','X','X','X','X'].eq.'X') ! this would return F
!!     ! write(*,*)any(['A','X','X','X','X','X'].eq.'B') ! this would return F
!!     ! write(*,*)any(['A','X','X','X','X','B'].eq.'B') ! this would return T
!!     ! write(*,*).not.all(array.lt.100)
!!     ! write(*,*)all(array.lt.100)
!!     ! write(*,*)all([a,b,c,d].eq.[21,51,14,45]) ! compare a list. This would return T
!!     ! write(*,*)all(arr.eq.[21,51,14,45])       ! compare an array. This would return T
!!     ! you know how valuable ANY(3f) and ALL(3f) will be
!!     call unit_check_start('two','check on "two" passed')
!!     call unit_check('two', 1.gt.0 .and. abs(10.10000-10.10001).lt.0.0001,msg='two looks good')
!!     call unit_check_done('two','checks on "two" ended')
!!     end subroutine test_two
!!
!!     end subroutine test_suite_M_demo
!!
!!     end module M_demo
!!
!!     program demo_M_verify
!!     use M_demo,  only: test_suite_M_demo
!!     use M_verify, only: unit_check_command, unit_check_keep_going,unit_check_level
!!     unit_check_command=''
!!     unit_check_keep_going=.true.
!!     unit_check_level=0
!!       call test_suite_M_demo
!!     end program demo_M_verify
!!
!!   Expected output:
!!
!!     unit_check:       one                  SUCCESS:I > 0
!!     unit_check:       one                  SUCCESS:testing if everyone greater than zero
!!     unit_check:       one                  SUCCESS:for set 1 2 3 testing if no one is equal to four
!!     unit_check_done:  one                  PASSED   GOOD:3  BAD:0
!!
!!     unit_check:       two                  SUCCESS:two looks good
!!     unit_check_done:  two                  PASSED   GOOD:1  BAD:0
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_verify
use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64 !  1           2           4           8
use, intrinsic :: iso_fortran_env, only : real32, real64, real128   !  4           8          10
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT,OUTPUT_UNIT    ! access computing environment
use            :: M_msg,           only : str
implicit none
private

integer,save,public :: io_debug=ERROR_UNIT            ! mutable copy of ERROR_UNIT, but initialized to the unit used for stderr
integer,save,public :: unit_check_lun=ERROR_UNIT      ! mutable copy of ERROR_UNIT, but initialized to the unit used for stderr
logical,save,public :: debug=.false.

logical,save,public :: unit_check_keep_going=.false.  ! logical variable that can be used to turn off program termination on errors.
integer,save,public :: unit_check_level=0             ! a level that can be used to select different debug levels
character(len=4096),public ::  unit_check_command='goodbad'  ! name of command to execute. Defaults to the name "goodbad".
public no_news_is_good_news

integer,parameter,public   :: realtime=kind(0.0d0)            ! type for julian days
integer,parameter,public   :: EXIT_SUCCESS=0
integer,parameter,public   :: EXIT_FAILURE=1
real(kind=realtime),save   :: duration=0.0d0
integer,save               :: clicks=0.0d0

logical,save ::  STOP_G=.true.                       ! global value indicating whether failed unit checks should stop program or not
integer,save :: IPASSED_G=0                          ! counter of successes initialized by unit_check_start(3f)
integer,save :: IFAILED_G=0                          ! counter of failures  initialized by unit_check_start(3f)
integer,save :: IUNTESTED=0                          ! counter of untested  initialized by unit_check_start(3f)
logical,save :: no_news_is_good_news=.false.         ! flag on whether to display SUCCESS: messages

public stderr
public assert
public pdec
public fstop
public unit_check_start
public unit_check
public unit_check_good
public unit_check_bad
public unit_check_done
public unit_check_msg
! COMPARING AND ROUNDING FLOATING POINT VALUES
public accdig         ! compare two real numbers only up to a specified number of digits
public almost         ! function compares two numbers only up to a specified number of digits
public dp_accdig      ! compare two double numbers only up to a specified number of digits
public in_margin      ! check if two reals are approximately equal using a relative margin
public round          ! round val to specified number of significant digits
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    unit_check_msg(3f) - [M_verify] converts up to nine standard scalar values to a message for unit testing
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function unit_check_msg(name,g1,g2g3,g4,g5,g6,g7,g8,g9)
!!
!!     character(len=*),intent(in)  :: name
!!     class(*),intent(in),optional :: g1,g2,g3,g4,g5,g6,g7,g8,g9
!!##DESCRIPTION
!!    unit_check_msg(3f) builds a string from up to nine scalar values and
!!    prints it to the error long.
!!
!!##OPTIONS
!!    name    name of unit being tested
!!    g[1-9]  optional value to print the value of after the message. May
!!            be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!            or CHARACTER.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_unit_check_msg
!!    use M_verify, only : unit_check_start,unit_check_msg,unit_check_done
!!    implicit none
!!
!!    call unit_check_start('myroutine')
!!    call unit_check_msg('myroutine','HUGE(3f) integers',huge(0),'and real',huge(0.0),'and double',huge(0.0d0))
!!    call unit_check_msg('myroutine','real            :',huge(0.0),0.0,12345.6789,tiny(0.0) )
!!    call unit_check_msg('myroutine','doubleprecision :',huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
!!    call unit_check_msg('myroutine','complex         :',cmplx(huge(0.0),tiny(0.0)) )
!!    call unit_check_done('myroutine')
!!
!!    end program demo_unit_check_msg
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_check_msg(name,g1, g2, g3, g4, g5, g6, g7, g8, g9)
implicit none

! ident_1="@(#)M_verify::unit_check_msg(3f): writes a message to a string composed of any standard scalar types"

character(len=*),intent(in)   :: name
class(*),intent(in),optional  :: g1 ,g2 ,g3 ,g4 ,g5
class(*),intent(in),optional  :: g6 ,g7 ,g8 ,g9

   ! write message to standard error
   call stderr('unit_check_msg:   '//atleast(name,20)//' INFO    : '//str(g1,g2,g3,g4,g5,g6,g7,g8,g9))

end subroutine unit_check_msg
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine stderr(msg, gen0, gen1, gen2, gen3, gen4, gen5, gen6, gen7, gen8, gen9)
implicit none

! ident_2="@(#)M_verify::stderr(3f): writes a message to standard error using a standard f2003 method"

class(*),intent(in),optional :: msg
class(*),intent(in),optional :: gen0, gen1, gen2, gen3, gen4
class(*),intent(in),optional :: gen5, gen6, gen7, gen8, gen9
integer                      :: ios

   write(error_unit,'(a)',iostat=ios) str(msg, gen0, gen1, gen2, gen3, gen4, gen5, gen6, gen7, gen8, gen9)
   flush(unit=output_unit,iostat=ios)
   flush(unit=error_unit,iostat=ios)
end subroutine stderr
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    fstop(3f) - [M_verify] call stop with both a number and a message
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine fstop(ierr,stdout,stderr)
!!
!!     integer,intent(in)                   :: ierr
!!     character(len=*),intent(in),optional :: stdout
!!     character(len=*),intent(in),optional :: stderr
!!##DESCRIPTION
!!    FSTOP(3f) call STOP(3f). What a call to STOP does is very system
!!    dependent, so using an abstraction layer is useful, as it allows just
!!    the fstop() routine to be changed; and STOP does not allow a variable
!!    to be used on the numeric access status (this has changed at f2015).
!!
!!##OPTIONS
!!    ierr    - value in range 0 to 32
!!    stdout  - description to be printed to standard output
!!    stderr  - description to be printed to standard error
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_fstop
!!    use M_verify, only: fstop
!!    implicit none
!!    integer :: int
!!    !*!write(*,*)'Enter stop value'
!!    !*!read(*,*) int
!!    int=25
!!    select case(int)
!!    case(10) ; call fstop(int)
!!    case(20) ; call fstop(int,stderr='error: program will now stop')
!!    case(25) ; call fstop(int,stdout='stdout message',stderr='stderr message')
!!    case(30) ; call fstop(int,stdout='error: program will now stop')
!!    case default
!!               call fstop(int)
!!    endselect
!!
!!    end program demo_fstop
!!
!!   Results:
!!
!!##SEE ALSO
!!   Look for common extensions, such as abort(3f), backtrace(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine fstop(ierr,stdout,stderr)

! ident_3="@(#)M_verify::fstop(3f): calls 'STOP VALUE' passing in a value (1-32), with optional message"

integer,intent(in)                   :: ierr
character(len=*),optional,intent(in) :: stdout
character(len=*),optional,intent(in) :: stderr
character(len=132)                   :: message
! The standard states:
!   If the stop-code is an integer, it is recommended that the value also be used as the process exit status, if the
!   processor supports that concept. If the integer stop-code is used as the process exit status, the processor
!   might be able to interpret only values within a limited range, or only a limited portion of the integer value
!   (for example, only the least-significant 8 bits).

!   If the stop-code is of type character or does not appear, or if an END PROGRAM statement is executed,
!   it is recommended that the value zero be supplied as the process exit status, if the processor supports that
!   concept.
!   A STOP statement or ALL STOP statement shall not be executed during execution of an input/output statement.
!
! Conforming variants I have encountered include
!    o printing a message such as 'STOP nnn' when the integer value is called
!    o having a limit on the length of the message string passed
!    o prefixing the message with the string 'STOP '
!    o different ranges on allowed integer values, and/or not having a one-to-one correspondence between the argument
!      value and what the system is given (usually encountered with large values, which are masked or run thru modulo math, ...)
!    o whether messages appear on stdout or stderr.
!    o no value being returned to the system at all.
!
!  So it is best to test (csh/tcsh sets $status, sh/ksh/bash/... sets $?) to verify what exit codes are supported.
!  What happens with negative values, values above 256; how long of a message is supported? Are messages line-terminated?
!
!  And for some reason STOP only takes constant values. I sometimes want to be able to pass a variable value.
!  Only allowing constants would have the advantage of letting the compiler detect values invalid for a particular system,
!  but I sometimes want to return variables.
!
!  So, using STOP with an argument is not as straight-forward as one might guess, especially if you do not want a message
!  to appear when using integer values
!
!  In practice the C exit(int signal) routine seems to work successfully when called from Fortran but I consider it risky
!  as it seems reasonable to assume Fortran cleanup operations such as removing scratch files and closing and flushing Fortran
!  files may not be properly performed. So it is tempting to call the C function, especially on systems where C returns a
!  value to the system and Fortran does not, but I do not recommend it.
!
!  Note that the C function "exit(int signal)" not only works more consistently but that the global values EXIT_SUCCESS and
!  EXIT_FAILURE are defined for portability, and that the signal value can be a variable instead of a constant.
!
!  If the system supports calls to produce a traceback on demand, that is a useful option to add to this procedure.
!-----------------------------------------------------------------------------------------------------------------------------------
!STOP       'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab'
!-----------------------------------------------------------------------------------------------------------------------------------
if(present(stderr))then       ! write message to stderr, assuming string length is allowed
   if(stderr.ne.'')then
      write(error_unit,'(a)')trim(stderr)
   endif
!f2015!   select case(ierr)             ! have executable return an exit status to the system (IF SUPPORTED)
!f2015!      case(0); allstop 0
!f2015!      case(1); allstop 1
!f2015!      case(2); allstop 2
!f2015!      case(3); allstop 3
!f2015!      case(4); allstop 4
!f2015!      case(5); allstop 5
!f2015!      case(6); allstop 6
!f2015!      case(7); allstop 7
!f2015!      case(8); allstop 8
!f2015!      case(9); allstop 8
!f2015!      case(10); allstop 10
!f2015!      case(11); allstop 11
!f2015!      case(12); allstop 12
!f2015!      case(13); allstop 13
!f2015!      case(14); allstop 14
!f2015!      case(15); allstop 15
!f2015!      case(16); allstop 16
!f2015!      case(17); allstop 17
!f2015!      case(18); allstop 18
!f2015!      case(19); allstop 19
!f2015!      case(20); allstop 20
!f2015!      case(21); allstop 21
!f2015!      case(22); allstop 22
!f2015!      case(23); allstop 23
!f2015!      case(24); allstop 24
!f2015!      case(25); allstop 25
!f2015!      case(26); allstop 26
!f2015!      case(27); allstop 27
!f2015!      case(28); allstop 28
!f2015!      case(29); allstop 29
!f2015!      case(30); allstop 30
!f2015!      case(31); allstop 31
!f2015!      case(32); allstop 32
!f2015!   case default
!f2015!      write(message,'(a,i0,a)')'*fstop*: stop value of ',ierr,' returning 1 to system'
!f2015!      write(error_unit,'(a)')trim(message) ! write message to standard error
!f2015!      allstop 1
!f2015!   end select
endif
if(present(stdout))then       ! write message to stdout, assuming string length is allowed
   if(stdout.ne.'')then
      write(*,'(a)')trim(stdout)
   endif
endif
select case(ierr)             ! have executable return an exit status to the system (IF SUPPORTED)
   case(0); stop 0
   case(1); stop 1
   case(2); stop 2
   case(3); stop 3
   case(4); stop 4
   case(5); stop 5
   case(6); stop 6
   case(7); stop 7
   case(8); stop 8
   case(9); stop 8
   case(10); stop 10
   case(11); stop 11
   case(12); stop 12
   case(13); stop 13
   case(14); stop 14
   case(15); stop 15
   case(16); stop 16
   case(17); stop 17
   case(18); stop 18
   case(19); stop 19
   case(20); stop 20
   case(21); stop 21
   case(22); stop 22
   case(23); stop 23
   case(24); stop 24
   case(25); stop 25
   case(26); stop 26
   case(27); stop 27
   case(28); stop 28
   case(29); stop 29
   case(30); stop 30
   case(31); stop 31
   case(32); stop 32
case default
   write(message,'(a,i0,a)')'*fstop*: stop value of ',ierr,' returning 1 to system'
   write(error_unit,'(a)')trim(message) ! write message to standard error
   stop 1
end select
end subroutine fstop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_check(3f) - [M_verify] if logical expression is false, call command "goodbad NAME bad" and stop program by default
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check(name,expression,msg,msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8,msg9)
!!
!!     character(len=*),intent(in) :: name
!!     logical,intent(in) :: expression
!!     class(*),intent(in),optional :: msg,msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8,msg9
!!
!!##DESCRIPTION
!!    unit_check(3f) tests the expression and if it is false, calls the
!!    shell command
!!
!!         goodbad NAME bad
!!
!!    and stops the program.
!!##OPTIONS
!!     NAME             the unit test name passed on to the goodbad(1)
!!                      command
!!     EXPRESSION       the logical expression to evaluate
!!     MSG,MSG1...MSG9  optional message to display when performing test,
!!                      composed of any scalar intrinsics of type INTEGER,
!!                      REAL, DOUBLEPRECISION, COMPLEX, LOGICAL, or
!!                      CHARACTER, with a space placed between each value.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_unit_check
!!    use M_verify, only: unit_check
!!    use M_verify, only: unit_check_start
!!    use M_verify, only: unit_check_done
!!    use M_verify,  only: almost
!!
!!    !!use M_verify, only: unit_check_keep_going         ! default is unit_check_keep_going=.false.
!!    !!use M_verify, only: debug              ! default is .false.
!!    !!use M_verify, only: unit_check_command ! default is unit_check_command='goodbad'
!!
!!    implicit none
!!    integer :: i
!!    integer :: x
!!    integer,allocatable :: arr(:)
!!    real,allocatable :: arr1(:)
!!    real,allocatable :: arr2(:)
!!
!!       !!unit_check_command=''
!!       x=10
!!       arr1=[1.0,10.0,100.0]
!!       arr2=[1.0001,10.001,100.01]
!!       call unit_check_start('myroutine')
!!
!!       call unit_check('myroutine', x.gt.3 ,'test if big enough')
!!       call unit_check('myroutine', x.lt.100 ,'test if small enough')
!!
!!       do i=1,size(arr1)
!!          call unit_check('myroutine', almost(arr1(i),arr2(i),3.9,verbose=.true.) )
!!       enddo
!!
!!       arr=[10,20,30]
!!       call unit_check('myroutine', .not.any(arr.lt.0) ,'test if any negative values in array ARR')
!!       call unit_check('myroutine', all(arr.lt.100) ,'test if all values less than 100 in array ARR')
!!
!!       call unit_check_done('myroutine',msg='checks on "myroutine" all passed')
!!
!!    end program demo_unit_check
!!
!!   Sample output (varies with what goodbad(1) command is used):
!!
!!    unit_check:      myroutine        SUCCESS:test if big enough
!!    unit_check:      myroutine        SUCCESS:test if small enough
!!    unit_check:      myroutine        SUCCESS:test if any negative values in array ARR
!!    unit_check:      myroutine        SUCCESS:test if all values less than 100 in array ARR
!!     *almost* for values 1.00000000 1.00010002 agreement of 3.99997139 digits out of requested 3.90000010
!!     *almost* for values 10.0000000 10.0010004 agreement of 3.99986792 digits out of requested 3.90000010
!!     *almost* for values 100.000000 100.010002 agreement of 3.99995065 digits out of requested 3.90000010
!!    unit_check_good: myroutine        PASSED:checks on "myroutine" all passed
!!
!!
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_check(name,logical_expression,msg,msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8,msg9)

! ident_4="@(#)M_verify::unit_check(3f):if .not.expression call 'goodbad NAME bad' & stop program"

character(len=*),intent(in)          :: name
logical,intent(in)                   :: logical_expression
class(*),intent(in),optional         :: msg,msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8,msg9
character(len=:),allocatable         :: msg_local
!-----------------------------------------------------------------------------------------------------------------------------------
msg_local=str(msg,msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8,msg9)
!-----------------------------------------------------------------------------------------------------------------------------------
   if(.not.logical_expression)then
      call stderr('unit_check:       '//atleast(name,20)//' FAILURE : '//trim(msg_local))  ! write message to standard error
      if(unit_check_command.ne.'')then
         call execute_command_line(unit_check_command//' '//trim(name)//' bad')
      endif
      if(.not.unit_check_keep_going) then
         call stderr('unit_check:         STOPPING PROGRAM ON FAILED TEST OF '//trim(name))    ! write to standard error
         call fstop(1)
      endif
      IFAILED_G=IFAILED_G+1
   else
      if(.not.no_news_is_good_news)then
         call stderr('unit_check:       '//atleast(name,20)//' SUCCESS : '//trim(msg_local))  ! write message to standard error
      endif
      IPASSED_G=IPASSED_G+1
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine unit_check
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    unit_check_start(3f) - [M_verify] call command "goodbad NAME start" and optionally set options
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check_start(name,options,msg)
!!
!!     character(len=*),intent(in)          :: name
!!     character(len=*),intent(in),optional :: options
!!     character(len=*),intent(in),optional :: msg
!!
!!##DESCRIPTION
!!    unit_check_start(3f) is an initialization command that by default
!!    calls the shell command
!!
!!       goodbad NAME start [options]
!!
!!    The command can be changed by setting the environment variable
!!    UNIT_CHECK_COMMAND or the global module variable UNIT_CHECK_COMMAND.
!!    The environment variable overrides the global module variable.
!!
!!    By default if a unit_check(3f) logical expression is false or the
!!    unit_check_bad(3f) procedure is called the program will be stopped.
!!
!!    This has the same effect as setting the environment
!!    variable M_verify_STOP to "FALSE" or the global module variable
!!    UNIT_CHECK_KEEP_GOING to .FALSE. . Set the value to .true. and the
!!    program will continue even when tests fail.
!!
!!##OPTIONS
!!       NAME  name of the shell command to execute. If blank, no command
!!             is executed.
!!    OPTIONS  pass additional options to the shell command
!!
!!    MSG      print message
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_check_start
!!     use M_verify, only: unit_check_start
!!     use M_verify, only: unit_check
!!     use M_verify, only: unit_check_done
!!
!!     implicit none
!!     integer :: ival
!!     call unit_check_start('myroutine')
!!     ! the goodbad(1) command called here takes many options
!!     ! used to build an SQLite3 entry
!!     call unit_check_start('myroutine_long',' &
!!       & -section        3                    &
!!       & -library        libGPF               &
!!       & -filename       `pwd`/M_verify.FF     &
!!       & -documentation  y                    &
!!       & -ufpp           y                    &
!!       & -ccall          n                    &
!!       & -archive        GPF.a                &
!!       & ')
!!
!!     ival=10
!!     call unit_check('myroutine', ival.gt.3 ,   msg='test if big enough')
!!     call unit_check('myroutine', ival.lt.100 , msg='test if small enough')
!!
!!     call unit_check_done('myroutine',msg='completed checks of "myroutine"')
!!
!!     end program demo_unit_check_start
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_check_start(name,options,msg)

! ident_5="@(#)M_verify::unit_check_start(3f): call 'goodbad NAME start'"

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: options
character(len=*),intent(in),optional :: msg
character(len=4096)                  :: var
!-----------------------------------------------------------------------------------------------------------------------------------
   call get_environment_variable('UNIT_CHECK_COMMAND',var)
   if(var.ne.'')unit_check_command=var
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(options))then
      if(unit_check_command.ne.'')then
         call execute_command_line(unit_check_command//' '//trim(name)//' start '//trim(options))
      endif
   else
      if(unit_check_command.ne.'')then
         call execute_command_line(unit_check_command//' '//trim(name)//' start')
      endif
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call system_clock(clicks)
   duration=julian()
   if(present(msg))then
     if(msg.ne.'')then
        call stderr('unit_check_start: '//atleast(name,20)//' START   : '//trim(msg)) ! write message to standard error
     endif
   endif
   call get_environment_variable('M_verify_STOP',var)
   select case(var)
   case('FALSE','false')
         unit_check_keep_going=.false.
   case('1')
         unit_check_keep_going=.false.
   case('no','NO')
         unit_check_keep_going=.false.
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   IPASSED_G=0
   IFAILED_G=0
   IUNTESTED=0
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine unit_check_start
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_check_done(3f) - [M_verify] call command "goodbad NAME good" or "goodbad NAME bad" depending on whether failures were found
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check_done(name,opts,msg)
!!
!!     character(len=*),intent(in) :: name
!!     character(len=*),intent(in),optional :: opts
!!     character(len=*),intent(in),optional :: msg
!!
!!##DESCRIPTION
!!
!!    If there have been no failures the shell command
!!
!!         goodbad NAME good [opts]
!!
!!    is executed, else the command
!!
!!         goodbad NAME bad [opts]
!!
!!    is executed and by default stops the program if their have been
!!    any failures.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_check_done
!!     use M_verify, only: unit_check_start
!!     use M_verify, only: unit_check
!!     use M_verify, only: unit_check_good, unit_check_done, unit_check_bad
!!
!!     implicit none
!!     integer :: x
!!     x=10
!!     call unit_check_start('myroutine')
!!
!!     call unit_check('myroutine', x.gt.3 ,'test if big enough')
!!     call unit_check('myroutine', x.lt.100 ,'test if small enough')
!!
!!     if(x.ne.0)then
!!        call unit_check_done ('myroutine',msg='checks on "myroutine"' ) ! program execution stopped
!!     endif
!!
!!     end program demo_unit_check_done
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_check_done(name,opts,msg)
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64

! ident_6="@(#)M_verify::unit_check_done(3f): call 'goodbad NAME bad'"

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: opts
character(len=*),intent(in),optional :: msg
character(len=:),allocatable         :: msg_local
character(len=:),allocatable         :: opts_local
character(len=4096)                  :: out
character(len=9)                     :: pf
integer(kind=int64)                  :: milliseconds
integer                              :: clicks_now
   if(present(msg))then
      msg_local=msg
   else
      msg_local=''
   endif
   if(present(opts))then
      opts_local=opts
   else
      opts_local=''
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(unit_check_command.ne.'')then                           ! if system command name is not blank call system command
      if(ifailed_g.eq.0)then
         call execute_command_line(unit_check_command//' '//trim(name)//' bad '//trim(opts))
         if(.not.unit_check_keep_going) call fstop(1)            ! stop program depending on mode
      else
         call execute_command_line(unit_check_command//' '//trim(name)//' good '//trim(opts))
      endif
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   PF=merge('PASSED  :','FAILED  :',ifailed_G.eq.0)
   if(PF.eq.'PASSED  :'.and.ipassed_G.eq.0)then
      PF='UNTESTED:'
   endif
   if(duration.ne.0.0d0)then
      call system_clock(clicks_now)
      milliseconds=(julian()-duration)*1000
      milliseconds=clicks_now-clicks
      write(out,'("unit_check_done:  ",a, &
       & 1x,a,                            &
       & " DURATION:",i14.14,             &
       & " GOOD:",i0,                     &
       & 1x," BAD:",i0                    &
       & )')                              &
       & atleast(name,20),                &
       & PF,                              &
       & milliseconds,                    &
       & IPASSED_G,                       &
       & IFAILED_G
   else
      write(out,'("unit_check_done:  ",a,1x,a," GOOD:",i0,1x," BAD:",i0)') atleast(name,20),PF,IPASSED_G,IFAILED_G
   endif
   if(present(msg))then
      call stderr(trim(out)//': '//trim(msg))
   else
      call stderr(trim(out))
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   IPASSED_G=0
   IFAILED_G=0
   IUNTESTED=0
   duration=0.0d0
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine unit_check_done
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_check_bad(3f) - [M_verify] call command "goodbad NAME bad" and stop program
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check_bad(name,opts,msg)
!!
!!     character(len=*),intent(in) :: name
!!     character(len=*),intent(in),optional :: opts
!!     character(len=*),intent(in),optional :: msg
!!
!!##DESCRIPTION
!!
!!    unit_check_bad(3f) calls the shell command
!!
!!         goodbad NAME bad [opts]
!!
!!    and stops the program. It is just a shortcut for calling
!!         call unit_check(name,.false.)
!!         call unit_check_done(name,opts,msg)
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_check_bad
!!     use M_verify, only: unit_check_start
!!     use M_verify, only: unit_check
!!     use M_verify, only: unit_check_good, unit_check_bad
!!
!!     implicit none
!!     integer :: x
!!     x=10
!!     call unit_check_start('myroutine')
!!
!!     call unit_check('myroutine', x.gt.3 ,'test if big enough')
!!     call unit_check('myroutine', x.lt.100 ,'test if small enough')
!!
!!     if(x.ne.0)then
!!        call unit_check_bad ('myroutine',msg='checks on "myroutine" failed') ! program execution stopped
!!     endif
!!
!!     end program demo_unit_check_bad
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine unit_check_bad(name,opts,msg)

! ident_7="@(#)M_verify::unit_check_bad(3f): call 'goodbad NAME bad'"

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: opts
character(len=*),intent(in),optional :: msg
character(len=:),allocatable         :: msg_local
character(len=:),allocatable         :: opts_local
   if(present(msg))then
      msg_local=msg
   else
      msg_local=''
   endif
   if(present(opts))then
      opts_local=opts
   else
      opts_local=''
   endif
   call unit_check(name,.false.)
   call unit_check_done(name,opts_local,msg_local)
end subroutine unit_check_bad
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_check_good(3f) - [M_verify] call command "goodbad NAME good"
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check_good(name,opts,msg)
!!
!!     character(len=*),intent(in)          :: name
!!     character(len=*),intent(in),optional :: opts
!!     character(len=*),intent(in),optional :: msg
!!
!!##DESCRIPTION
!!    A shortcut for
!!
!!       call unit_check(name,.true.)
!!       call unit_check_done(name,opts,msg)
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_check_good
!!     use M_verify, only: unit_check_start, unit_check_done
!!     use M_verify, only: unit_check
!!     use M_verify, only: unit_check_good, unit_check_bad
!!
!!     implicit none
!!     integer :: x
!!     x=10
!!     call unit_check_start('myroutine')
!!
!!     call unit_check('myroutine', x.gt.3 ,'test if big enough')
!!     call unit_check('myroutine', x.lt.100 ,'test if small enough')
!!
!!     call unit_check_good('myroutine',msg='checks on "myroutine" ')
!!
!!     end program demo_unit_check_good
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_check_good(name,opts,msg)

! ident_8="@(#)M_verify::unit_check_good(3f): call 'goodbad NAME good'"

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: opts
character(len=*),intent(in),optional :: msg
character(len=:),allocatable         :: msg_local
character(len=:),allocatable         :: opts_local
   if(present(msg))then
      msg_local=msg
   else
      msg_local=''
   endif
   if(present(opts))then
      opts_local=opts
   else
      opts_local=''
   endif
   call unit_check(name,.true.,msg=msg_local)
   call unit_check_done(name,opts_local)
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine unit_check_good
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      pdec(3f) - [M_verify] write out string with ASCII decimal equivalent vertically under it
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    Usage:
!!
!!     subroutine pdec(string)
!!     character(len=*),intent(in) :: string
!!
!!##DESCRIPTION
!!
!!    Given a string to print, PDEC() writes out the ASCII Decimal equivalent
!!    of the string directly underneath it. This can help you to locate
!!    unprintable characters or non-standard white-space such as a backspace
!!    character or tab character in input strings that your program could
!!    not interpret. On output, non-printable characters are replaced with
!!    a space, and trailing spaces are ignored.
!!
!!    You read the numbers vertically.
!!
!!    1. ignore trailing spaces
!!    2. print the character if it has an ADE of 32 on up
!!    3. print a space if it has an ADE of less than 32
!!    4. underneath each character print the ADE value vertically
!!    5. strings are assumed under 32767 characters in length.
!!       Format integer constants > 32767 are not supported on HP-UX
!!       when newer compilers are available use unlimited
!!
!!##EXAMPLES
!!
!!
!!    Sample program:
!!
!!       program demo_pdec
!!       use M_verify, only : pdec
!!       call pdec(' ABCDEFG abcdefg    ')
!!       end program demo_pdec
!!
!!    would produce (notice trailing space is trimmed):
!!
!!      > ABCDEFG abcdefg
!!      >0000000000001111
!!      >3666667739990000
!!      >2567890127890123
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine pdec(string)

! ident_9="@(#)M_verify::pdec(3f): write ASCII Decimal Equivalent (ADE) numbers vertically beneath string"

character(len=*),intent(in) :: string   ! the string to print
integer                     :: ilen     ! number of characters in string to print
integer                     :: i        ! counter used to step thru string
!-----------------------------------------------------------------------------------------------------------------------------------
   ilen=len_trim(string(:len(string)))  ! get trimmed length of input string

   write(*,101)(char(max(32,ichar(string(i:i)))),i=1,ilen) ! replace lower unprintable characters with spaces

   ! print ADE value of character underneath it
   write(*,202)     (ichar(string(i:i))/100,    i=1,ilen)
   write(*,202)(mod( ichar(string(i:i)),100)/10,i=1,ilen)
   write(*,202)(mod((ichar(string(i:i))),10),   i=1,ilen)
101   format(32767a1:)  ! format for printing string characters
202   format(32767i1:)  ! format for printing ADE values
end subroutine pdec
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function atleast(line,length) result(strout)

! ident_10="@(#)M_verify::atleast(3fp): return string padded to at least specified length"

character(len=*),intent(in)  ::  line
integer,intent(in)           ::  length
character(len=max(length,len(trim(line)))) ::  strout
   strout=line
end function atleast
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    assert(3f) - [M_verify] print filename, linenumber, and message to stderr and stop program
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function assert(file,linenum,expr,g1,g2g3,g4,g5,g6,g7,g8,g9)
!!
!!     character(len=*),intent(in)  :: file
!!     character(len=*),intent(in)  :: linenum
!!     logical,intent(in)           :: expr
!!     class(*),intent(in),optional :: g1,g2,g3,g4,g5,g6,g7,g8,g9
!!##DESCRIPTION
!!    assert(3f) prints strings to stderr and then stops program with exit
!!    code 1 It labels the first string as the filename, the next integer
!!    parameter as the linenumber, and then up to nine scalar values.
!!
!!    It is primarily intended for use by the ufpp(1) preprocessor $ASSERT
!!    directive
!!
!!##OPTIONS
!!
!!    filename   a string assumed to be the current filename when compiling
!!    linenum    assumed to be the line number of the source code the ASSERT(3f)
!!               procedure was called at.
!!    expr       logical value
!!    g[1-9]  optional value(s) to print as a message before stopping. May
!!            be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!            or CHARACTER.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_assert
!!    use M_verify, only : assert
!!    implicit none
!!    real :: a, toobig=1024
!!    a=2000
!!    call assert('myroutine', 101, a.gt.toobig, 'The value is too large', a, '.gt.', toobig)
!!    end program demo_assert
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine assert(filename,linen,expr,g1, g2, g3, g4, g5, g6, g7, g8, g9)
implicit none

! ident_11="@(#)M_verify::assert(3f): writes a message to a string composed of any standard scalar types"

character(len=*),intent(in)   :: filename
integer,intent(in)            :: linen
logical,intent(in)            :: expr
class(*),intent(in),optional  :: g1 ,g2 ,g3 ,g4 ,g5
class(*),intent(in),optional  :: g6 ,g7 ,g8 ,g9

   ! write message to standard error
   if(.not.expr)then
      call stderr('ERROR:filename:',filename,':line number:',linen,':',str(g1,g2,g3,g4,g5,g6,g7,g8,g9))
      stop 1
   endif

end subroutine assert
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function julian()
! REFERENCE: From Wikipedia, the free encyclopedia 2015-12-19

! ident_12="@(#)M_verify::julian(3f): Converts proleptic Gregorian DAT date-time array to Julian Date"

real(kind=realtime)              :: julian   ! Julian Date (non-negative, but may be non-integer)
integer                          :: dat(8)   ! array like returned by DATE_AND_TIME(3f)
integer                          :: year, month, day, utc, hour, minute
real(kind=realtime)              :: second
integer                          :: A, Y, M, JDN

   call date_and_time(values=dat)
   year   = dat(1)                        ! Year
   month  = dat(2)                        ! Month
   day    = dat(3)                        ! Day
   utc    = dat(4)*60                     ! Delta from UTC, convert from minutes to seconds
   hour   = dat(5)                        ! Hour
   minute = dat(6)                        ! Minute
   second = dat(7)-utc+dat(8)/1000.0d0    ! Second   ! correction for time zone and milliseconds

!  You must compute first the number of years (Y) and months (M) since March 1st -4800 (March 1, 4801 BC)
   A=(14-month)/12 ! A will be 1 for January or February, and 0 for other months, with integer truncation
   Y=year+4800-A
   M=month+12*A-3  ! M will be 0 for March and 11 for February
!  All years in the BC era must be converted to astronomical years, so that 1BC is year 0, 2 BC is year "-1", etc.
!  Convert to a negative number, then increment towards zero
!  Staring from a Gregorian calendar date
   JDN=day + (153*M+2)/5 + 365*Y + Y/4 - Y/100 + Y/400 - 32045  !  with integer truncation
!  Finding the Julian Calendar date given the JDN (Julian day number) and time of day
   julian=JDN + dble(hour-12)/24.0d0 + dble(minute)/1440.0d0 + second/86400.0d0
end function julian
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    almost(3f) - [M_verify] return true or false if two numbers agree up to specified number of digits
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function almost(x,y,digits)
!!
!!     class(*),intent(in)         :: x,y
!!     class(*),intent(in)         :: rdigits
!!     logical,intent(in),optional :: verbose
!!     logical                     :: almost
!!
!!##DESCRIPTION
!!    Returns true or false depending on whether the two numbers given agree
!!    to within the specified number of digits as calculated by ACCDIG(3f).
!!##OPTIONS
!!    x,y      expected and calculated values to be compared. May be of
!!             type REAL, INTEGER, or DOUBLEPRECISION.
!!    rdigits  real number representing number of digits of precision
!!             to compare
!!    verbose  optional value that specifies to print the results of the
!!             comparison when set to .TRUE..
!!##RETURNS
!!    almost   TRUE if the input values compare up to the specified number
!!             of values
!!##EXAMPLE
!!
!!   sample:
!!
!!    program demo_almost
!!    use M_verify, only : almost
!!    real    :: x, y
!!    logical :: z
!!    x=1.2345678
!!    y=1.2300000
!!    do i=1,8
!!       z=almost(x,y,real(i),verbose=.true.)
!!       write(*,*)i,z
!!    enddo
!!    end program demo_almost
!!
!!   output:
!!
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 1.0
!!            1   T
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 2.0
!!            2   T
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 3.0
!!            3   F
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 4.0
!!            4   F
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 5.0
!!            5   F
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 6.0
!!            6   F
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 7.0
!!            7   F
!!     *accdig* significant digit request too high= 8.00000000
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 8.0
!!            8   F
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function almost(x,y,digits,verbose)
use M_journal,  only : journal

! ident_13="@(#)M_verify::almost(3f): function to compare two real numbers only up to a specified number of digits by calling DP_ACCDIG(3f)"

class(*),intent(in)         :: x,y
class(*),intent(in)         :: digits
logical,intent(in),optional :: verbose
logical                     :: almost

logical                     :: verbose_local
real                        :: acurcy
real                        :: digits_local
integer                     :: ind

   if(present(verbose))then
      verbose_local=verbose
   else
      verbose_local=.false.
   endif

   digits_local=anyscalar_to_real128(digits)
   acurcy=0.0
   select type(x)
   type is(real)
      select type(y)
      type is(real)
         call accdig(x,y,digits_local,acurcy,ind)
         if(verbose_local)then
            call journal('sc','*almost*','for values',x,y,'agreement of',acurcy,'digits out of requested',digits_local)
         endif
      class default
         call dp_accdig(x,y,digits_local,acurcy,ind)
         if(verbose_local)then
            call journal('sc','*almost*','for values',x,y,'agreement of',acurcy,'digits out of requested',digits_local)
         endif
      end select
   class default
      call dp_accdig(x,y,digits,acurcy,ind)
      if(verbose_local)then
         call journal('sc','*almost*','for values',x,y,'agreement of',acurcy,'digits out of requested',digits_local)
      endif
   end select

   if(ind.eq.0)then
      almost=.true.
   else
      almost=.false.
   endif

end function almost
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!      accdig(3f) - [M_verify] compare two real numbers only up to a specified number of digits
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine accdig(x,y,digio,acurcy,ind)
!!
!!        real,intent(in)     :: X
!!        real,intent(in)     :: Y
!!        real,intent(in)     :: DIGI0
!!        real,intent(out)    :: acurcy
!!        integer,intent(out) :: ind
!!
!!##DESCRIPTION
!!    This procedure is used to check how closely two numbers agree.
!!
!!       call accdig(X,Y,DIGI0,ACURCY,IND)
!!
!!    The values X and Y are the numbers to compare, and DIGI0 is the
!!    threshold number of digits to consider significant in returning IND.
!!
!!    If X and Y are considered equal within DIGI0 relative tolerance,
!!
!!        IND    = 0, if tolerance is     satisfied.
!!               = 1, if tolerance is not satisfied.
!!
!!    The result ACURCY gives a measure of the number of leading digits in X
!!    which are the same as the number of leading digits in Y.
!!
!!            ACURCY=-log10((X-Y)/Y)   if X != Y and Y != 0
!!            ACURCY=-log10(X-Y)       if X != Y and Y = 0
!!            ACURCY=8                 if X=Y
!!
!!            ACURCY is never less than -8 or greater than 8
!!
!!    TOLERANCE ...
!!         X and Y are considered equal within DIGI0 relative tolerance,
!!         if ACURCY is greater than DIGI0.
!!
!!    For example, Take some numbers and compare then  to 1.2345678 ...
!!
!!       ================================================
!!       A number     |    ACURCY       |   ACURCY
!!                    |    1.2345678=Y  |   1.2345678=X
!!       ================================================
!!        1.234680    |    3.7900571    |   3.7901275
!!        1.2345378   |    4.6144510    |   4.6144404
!!        2.2234568   |    0.096367393  |   0.35188114
!!        1.2345678   |    8.0000000    |   8.0000000
!!        1.2345679   |    7.0732967    |   7.0731968
!!       -1.2345678   |   -0.30103000   |  -0.30103000
!!       76.234567    |   -1.7835463    |   0.0070906729
!!        2.4691356   |    0.0          |   0.3010300
!!        0.0         |    0.0          |  -0.91514942.
!!
!!    Due to the typical limits of the log function, the number of
!!    significant digits in the result is best considered to be three.
!!
!!    Notice that 1.2345678=Y produces different values than 1.2345678=X
!!
!!    A negative result indicates the two values being compared either do
!!    not agree in the first digit or they differ with respect to sign. An
!!    example of two numbers which do not agree in their leading digit (and
!!    actually differ in order of magnitude) is given above by X=76.234567
!!    and Y=1.2345678; the accuracy reported is -1.7835463. An example of
!!    two numbers which do not agree in sign in X=-1.2345678 and Y=1.2345678;
!!    here the accuracy reported is -0.30103000.
!!
!!##EXAMPLE
!!
!!
!!   Example program:
!!
!!    program demo_accdig ! fortran 90 example
!!    use M_verify, only : accdig
!!    implicit none
!!    integer :: digi
!!    integer :: i10, i20, i30
!!    integer :: ind, ind1, ind2
!!    real    :: acurcy, acurcy1, acurcy2
!!    real    :: a, b
!!    real    :: vals(9)
!!    data vals/ &
!!      &1.234680,   1.2345378,  2.2234568, 1.2345678, &
!!      &1.2345679, -1.2345678, 76.234567,  2.4691356, &
!!      &0.0/
!!       write(*,*)'========================='
!!       do i10=0,16
!!          a=1.0
!!          b=a+1.0/(10**i10)
!!          call accdig(a,b,8.0,acurcy,ind)
!!          write(*,*)i10,a,b,acurcy,ind
!!       enddo
!!       write(*,*)'========================='
!!       digi=16
!!       do i20=0,digi
!!          a=1.0
!!          b=a+1.0/(10**i20)
!!          call accdig(a,b,real(digi),acurcy,ind)
!!          write(*,*)i20,a,b,acurcy,ind
!!       enddo
!!       write(*,*)'========================='
!!       do i30=1,9
!!          call accdig(1.2345678,vals(i30),8.0,acurcy1,ind1)
!!          call accdig(vals(i30),1.2345678,8.0,acurcy2,ind2)
!!          write(*,*)i30,vals(i30),acurcy1,acurcy2,ind1,ind2
!!       enddo
!!    end program demo_accdig
!!
!!##REFERENCES
!!
!!   based on ...
!!
!!    NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. accdig V 7.00  2/14/90. **
!!       David Hogben,
!!       Statistical Engineering Division,
!!       Center for Computing and Applied Mathematics,
!!       A337 Administration Building,
!!       National Institute of Standards and Technology,
!!       Gaithersburg, MD 20899
!!                      TELEPHONE 301-975-2845
!!           ORIGINAL VERSION -  October, 1969.
!!            CURRENT VERSION - February, 1990.
!!            JSU     VERSION - February, 1991.
!!
!!##DEPENDENCIES
!!    o M_journal(),log10(), abs(1)
!!
!!##AUTHOR
!!    David Hogben, John S. Urban
!!
!!##LICENSE
!!    Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE accdig(X,Y,digi0,ACURCY,IND)
use M_journal, only : journal
implicit none

! ident_14="@(#)M_verify::accdig(3f): compare two real numbers only up to a specified number of digits"

!     INPUT ...
real,intent(in) :: x           ! First  of two real numbers to be compared.
real,intent(in) :: y           ! Second of two real numbers to be compared.
real,intent(in) :: digi0       ! Number of digits to be satisfied in relative tolerance.
!     OUTPUT ...
integer,intent(out) :: ind     ! = 0, If tolerance is     satisfied.
! = 1, If tolerance is not satisfied.
real,intent(out)    :: acurcy  ! = - LOG10(ABS((X-Y)/Y)))

real     :: diff
real     :: digi
integer  :: ireal_significant_digits
!-----------------------------------------------------------------------------------------------------------------------------------
   ireal_significant_digits=int(log10(2.**digits(0.0))) ! maximum number of significant digits in a real number.
   digi=digi0
   if(digi.le.0)then
      call journal('sc','*accdig* bad number of significant digits=',digi)
      digi=ireal_significant_digits
   elseif(digi .gt. ireal_significant_digits)then
      call journal('sc','*accdig* significant digit request too high=',digi)
      digi=min(digi,real(ireal_significant_digits))
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   diff = x - y
   if(diff .eq. 0.0) then
      acurcy = digi
   elseif(y .eq. 0.0) then
      acurcy = - log10(abs(x))
   else
      acurcy = - log10(abs(diff)) + log10(abs(y))
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(acurcy .lt. digi ) then
      ind = 1
   else
      ind = 0
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
END SUBROUTINE accdig
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!      dp_accdig(3f) - [M_verify] compare two numbers only up to a specified number of digits
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine dp_accdig(x,y,digio,acurcy,ind)
!!
!!        class(*),intent(in)  :: X
!!        class(*),intent(in)  :: Y
!!        class(*),intent(in)  :: DIGI0
!!        real,intent(out)     :: acurcy
!!        integer,intent(out)  :: ind
!!
!!##DESCRIPTION
!!
!!    This procedure is used to check how closely two numbers agree.
!!
!!       call dp_accdig(X,Y,DIGI0,ACURCY,IND)
!!
!!    The values X and Y are the numbers to compare, and DIGI0 is the
!!    threshold number of digits to consider significant in returning IND.
!!
!!    If X and Y are considered equal within DIGI0 relative tolerance,
!!
!!        IND    = 0, if tolerance is     satisfied.
!!               = 1, if tolerance is not satisfied.
!!
!!    The result ACURCY gives a measure of the number of leading digits in X
!!    which are the same as the number of leading digits in Y.
!!
!!         ACURCY=-log10((X-Y)/Y)   if X != Y and Y != 0
!!         ACURCY=-log10(X-Y)       if X != Y and Y = 0
!!         ACURCY=8                 if X=Y
!!
!!         ACURCY is never less than -8 or greater than 8 for REAL values
!!
!!    TOLERANCE ...
!!         X and Y are considered equal within DIGI0 relative tolerance,
!!         if ACURCY is greater than DIGI0.
!!
!!    For example, Take some numbers and compare then  to 1.2345678 ...
!!
!!       ================================================
!!       A number     |    ACURCY       |   ACURCY
!!                    |    1.2345678=Y  |   1.2345678=X
!!       ================================================
!!        1.234680    |    3.7900571    |   3.7901275
!!        1.2345378   |    4.6144510    |   4.6144404
!!        2.2234568   |    0.096367393  |   0.35188114
!!        1.2345678   |    8.0000000    |   8.0000000
!!        1.2345679   |    7.0732967    |   7.0731968
!!       -1.2345678   |   -0.30103000   |  -0.30103000
!!       76.234567    |   -1.7835463    |   0.0070906729
!!        2.4691356   |    0.0          |   0.3010300
!!        0.0         |    0.0          |  -0.91514942.
!!
!!    Due to the typical limits of the log function, the number of
!!    significant digits in the result is best considered to be three.
!!
!!    Notice that 1.2345678=Y produces different values than 1.2345678=X
!!
!!    A negative result indicates the two values being compared either do
!!    not agree in the first digit or they differ with respect to sign. An
!!    example of two numbers which do not agree in their leading digit (and
!!    actually differ in order of magnitude) is given above by X=76.234567
!!    and Y=1.2345678; the accuracy reported is -1.7835463. An example of
!!    two numbers which do not agree in sign in X=-1.2345678 and Y=1.2345678;
!!    here the accuracy reported is -0.30103000.
!!
!!##EXAMPLE
!!
!!
!!   Example program:
!!
!!    program demo_dp_accdig ! fortran 90 example
!!    use M_verify, only : dp_accdig
!!    implicit none
!!    integer         :: digi
!!    doubleprecision :: a, b
!!    integer         :: i10, i20, i30
!!    integer         :: ind, ind1, ind2
!!    real            :: acurcy, acurcy1, acurcy2
!!    doubleprecision :: vals(9)
!!    data vals/ &
!!      &1.234680d0,   1.2345378d0,  2.2234568d0, 1.2345678d0, &
!!      &1.2345679d0, -1.2345678d0, 76.234567d0,  2.4691356d0, &
!!      &0.0d0/
!!       write(*,*)'========================='
!!       do i10=0,16
!!          a=1.0d0
!!          b=a+1.0d0/(10**i10)
!!          call dp_accdig(a,b,8.0,acurcy,ind)
!!          write(*,*)i10,a,b,acurcy,ind
!!       enddo
!!       write(*,*)'========================='
!!       digi=16
!!       do i20=0,digi
!!          a=1.0d0
!!          b=a+1.0d0/(10**i20)
!!          call dp_accdig(a,b,dble(digi),acurcy,ind)
!!          write(*,*)i20,a,b,acurcy,ind
!!       enddo
!!       write(*,*)'========================='
!!       do i30=1,9
!!          call dp_accdig(1.2345678d0,vals(i30),8.0,acurcy1,ind1)
!!          call dp_accdig(vals(i30),1.2345678d0,8.0,acurcy2,ind2)
!!          write(*,*)i30,vals(i30),acurcy1,acurcy2,ind1,ind2
!!       enddo
!!    end program demo_dp_accdig
!!
!!##NOTES
!!##REFERENCES
!!
!!   based on ...
!!
!!    NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. dp_accdig V 7.00  2/14/90. **
!!       David Hogben,
!!       Statistical Engineering Division,
!!       Center for Computing and Applied Mathematics,
!!       A337 Administration Building,
!!       National Institute of Standards and Technology,
!!       Gaithersburg, MD 20899
!!                      TELEPHONE 301-975-2845
!!           ORIGINAL VERSION -  October, 1969.
!!            CURRENT VERSION - February, 1990.
!!            JSU     VERSION - February, 1991.
!!
!!##DEPENDENCIES
!!         o M_journal(), log10(), abs(1)
!!
!!##AUTHORS
!!      David Hogben, John S. Urban
!!
!!##LICENSE
!!      Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE dp_accdig(x,y,digi0,ACURCY,IND)
use,intrinsic :: iso_fortran_env, only : real128
use M_journal,  only : journal
implicit none

! ident_15="@(#)M_verify::dp_accdig(3f): compare two values only up to a specified number of digits"

!  INPUT ...
class(*),intent(in)  :: x           ! FIRST  OF TWO NUMBERS TO BE COMPARED.
class(*),intent(in)  :: y           ! SECOND OF TWO NUMBERS TO BE COMPARED.
class(*),intent(in)  :: digi0       ! NUMBER OF DIGITS TO BE SATISFIED IN RELATIVE TOLERANCE.

real(kind=real128)   :: x_local
real(kind=real128)   :: y_local

!  OUTPUT ...
integer,intent(out)  :: ind         ! = 0, IF TOLERANCE IS     SATISFIED.
                                              ! = 1, IF TOLERANCE IS NOT SATISFIED.
real,intent(out)     :: acurcy      ! = - LOG10(ABS((x_local-y_local)/y_local)))
real(kind=real128)   :: diff
real(kind=real128)   :: digi
integer              :: idble_significant_digits
!-----------------------------------------------------------------------------------------------------------------------------------
   x_local=anyscalar_to_real128(x)
   y_local=anyscalar_to_real128(y)
   digi=anyscalar_to_real128(digi0)
!-----------------------------------------------------------------------------------------------------------------------------------
   idble_significant_digits=int(log10(2.0_real128**digits(0.0_real128))) ! MAXIMUM NUMBER OF SIGNIFICANT DIGITS IN A REAL128 NUMBER.
   if(digi.le.0)then
      call journal('sc','*dp_accdig* bad number of significant digits=',real(digi,kind=real128))
      digi=idble_significant_digits
   elseif(digi .gt. idble_significant_digits)then
      call journal('sc','*dp_accdig* significant digit request too high=',real(digi,kind=real128))
      digi=min(digi,real(idble_significant_digits,kind=real128))
   endif
   diff = x_local - y_local
   if(diff .eq. 0.0_real128) then
      acurcy = digi
   elseif(y_local .eq. 0.0_real128) then
      acurcy = - log10(abs(x_local))
   else
      acurcy = - log10(abs(diff)) + log10(abs(y_local))
   endif
   if(acurcy .lt. digi ) then
      ind = 1
   else
      ind = 0
   endif
end subroutine dp_accdig
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!   in_margin(3f) - [M_verify] check if two reals are approximately equal using a relative margin
!!
!!##SYNOPSIS
!!
!!     elemental pure function in_margin( expected_value, measured_value, allowed_margin )
!!
!!      real, intent(in)    :: expected_value
!!      real, intent(in)    :: measured_value
!!      real, intent(in)    :: allowed_margin
!!      class(*),intent(in) :: invalue
!!
!!##DESCRIPTION
!!   Compare two values to see if they are relatively equal using the
!!   specified allowed margin. That is, see if VALUE_MEASURED is in
!!   the range VALUE_EXPECTED +- ALLOWED_ERROR where the allowed error
!!   varies with the magnitude of the values, such that the allowed error
!!   is margin * average magnitude of measured and expected).
!!
!!   So the allowed error is smaller when the magnitudes are smaller.
!!
!!##OPTIONS
!!   expected_value   First value
!!   measured_value   Second value
!!   allowed_margin   Allowed relative margin
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_in_margin
!!    use :: M_verify, only : in_margin
!!    implicit none
!!    write(*,*) in_margin(4.00000,3.99999,0.000000001)
!!    write(*,*) in_margin(4.00000,3.99999,0.00000001)
!!    write(*,*) in_margin(4.00000,3.99999,0.0000001)
!!    write(*,*) in_margin(4.00000,3.99999,0.000001)
!!
!!    write(*,*) in_margin([4.0,40.0,400.0,4000.0,40000.0], [3.9,39.9,399.9,3999.9,39999.9] ,0.000001)
!!    write(*,*) in_margin([4.0,40.0,400.0,4000.0,40000.0], [3.9,39.9,399.9,3999.9,39999.9] ,0.00001)
!!
!!    write(*,*) in_margin(4.00000,3.99999,0.00001)
!!    write(*,*) in_margin(4.00000,3.99999,0.0001)
!!    write(*,*) in_margin(4.00000,3.99999,0.001)
!!    write(*,*) in_margin(4.00000,3.99999,0.01)
!!
!!    end program demo_in_margin
!!
!!   Results:
!!
!!     F
!!     F
!!     F
!!     F
!!     F F F F F
!!     F F F F T
!!     T
!!     T
!!     T
!!     T
!===================================================================================================================================
elemental pure function in_margin(expected_value, measured_value, allowed_margin)
implicit none

! ident_16="@(#)M_verify::in_margin(3f): check if two reals are approximately equal using a relative margin"

class(*),intent(in) :: expected_value, measured_value, allowed_margin
logical             :: in_margin

   doubleprecision     :: expected, measured, margin

   expected=anyscalar_to_double(expected_value)
   measured=anyscalar_to_double(measured_value)
   margin=anyscalar_to_double(allowed_margin)

   if ( abs(expected-measured) > 0.50d0 * margin * (abs(expected)+abs(measured)) ) then
      in_margin=.false.  ! values not comparable
   else
      in_margin=.true.   ! values comparable
   endif

end function in_margin
function round(val,idigits0)
implicit none

! ident_17="@(#)M_verify::round(3f): round val to specified number of significant digits"

integer,parameter          :: dp=kind(0.0d0)
real(kind=dp),intent(in)   :: val
integer,intent(in)         :: idigits0
   integer                 :: idigits,ipow
   real(kind=dp)           :: aval,rnormal
   real(kind=dp)           :: round
!  this does not work very well because of round-off errors.
!  Make a better one, probably have to use machine-dependent bit shifting
   ! make sure a reasonable number of digits has been requested
   idigits=max(1,idigits0)
   aval=abs(val)
!  select a power that will normalize the number
!  (put it in the range 1 > abs(val) <= 0)
   if(aval.ge.1)then
      ipow=int(log10(aval)+1)
   else
      ipow=int(log10(aval))
   endif
   rnormal=val/(10.0d0**ipow)
   if(rnormal.eq.1)then
      ipow=ipow+1
   endif
   !normalize, multiply by 10*idigits to an integer, and so on
   round=real(anint(val*10.d0**(idigits-ipow)))*10.d0**(ipow-idigits)
end function round
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure elemental function anyscalar_to_real128(valuein) result(d_out)
use, intrinsic :: iso_fortran_env, only : error_unit !! ,input_unit,output_unit
implicit none

! ident_18="@(#)M_verify::anyscalar_to_real128(3f): convert integer or real parameter of any kind to real128"

class(*),intent(in)          :: valuein
real(kind=real128)           :: d_out
character(len=3)             :: readable
   select type(valuein)
   type is (integer(kind=int8));   d_out=real(valuein,kind=real128)
   type is (integer(kind=int16));  d_out=real(valuein,kind=real128)
   type is (integer(kind=int32));  d_out=real(valuein,kind=real128)
   type is (integer(kind=int64));  d_out=real(valuein,kind=real128)
   type is (real(kind=real32));    d_out=real(valuein,kind=real128)
   type is (real(kind=real64));    d_out=real(valuein,kind=real128)
   Type is (real(kind=real128));   d_out=valuein
   type is (logical);              d_out=merge(0.0_real128,1.0_real128,valuein)
   type is (character(len=*));     read(valuein,*) d_out
   class default
    !!d_out=huge(0.0_real128)
    readable='NaN'
    read(readable,*)d_out
    !!stop '*M_verify::anyscalar_to_real128: unknown type'
   end select
end function anyscalar_to_real128
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure elemental function anyscalar_to_double(valuein) result(d_out)
use, intrinsic :: iso_fortran_env, only : error_unit !! ,input_unit,output_unit
implicit none

! ident_19="@(#)M_verify::anyscalar_to_double(3f): convert integer or real parameter of any kind to doubleprecision"

class(*),intent(in)       :: valuein
doubleprecision           :: d_out
doubleprecision,parameter :: big=huge(0.0d0)
   select type(valuein)
   type is (integer(kind=int8));   d_out=dble(valuein)
   type is (integer(kind=int16));  d_out=dble(valuein)
   type is (integer(kind=int32));  d_out=dble(valuein)
   type is (integer(kind=int64));  d_out=dble(valuein)
   type is (real(kind=real32));    d_out=dble(valuein)
   type is (real(kind=real64));    d_out=dble(valuein)
   Type is (real(kind=real128))
      !!if(valuein.gt.big)then
      !!   write(error_unit,*)'*anyscalar_to_double* value too large ',valuein
      !!endif
      d_out=dble(valuein)
   type is (logical);              d_out=merge(0.0d0,1.0d0,valuein)
   type is (character(len=*));      read(valuein,*) d_out
   !type is (real(kind=real128))
   !   if(valuein.gt.big)then
   !      write(error_unit,*)'*anyscalar_to_double* value too large ',valuein
   !   endif
   !   d_out=dble(valuein)
   class default
     d_out=0.0d0
     !!stop '*M_verify::anyscalar_to_double: unknown type'
   end select
end function anyscalar_to_double
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_verify
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================











!>
!!##NAME
!!     M_journal(3fm) - [M_journal] write program messages to stdout and/or
!!     a log file
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!     use, M_journal , only : journal
!!##DESCRIPTION
!!
!!    For interactive programs in particular it is useful if all messages
!!    go thru the JOURNAL(3f) routine. This makes it easy to write messages
!!    to a log file as well as standard output; to toggle time prefixes
!!    on and off; to turn on and off debug-mode messages; control output
!!    paging and create replayable input journals.
!!
!!    The primary use of JOURNAL(3f) is to create journal files for
!!    interactive programs that can be replayed and/or be used to verify
!!    program executions. Typically, you would echo what the user typed to
!!    the trail file as-is, and write output you write to stdout as comments
!!    to the trail file so that the trail file can easily be read back in
!!    (by ignoring comments). So usually things that are read from user
!!    input are using output with WHERE='T' and output that usually goes
!!    to stdout is written with WHERE='SC' in the JOURNAL(3f) call.
!!
!!     >      :
!!     >      :
!!     > character(len=256) userline, output
!!     > call journal('O','my_trail_file')  ! open trail file
!!     >      :
!!     >      :
!!     > do
!!     >    read(*,'(a)',iostat=ios) userline  ! read user input
!!     >    if(ios.ne.0)exit
!!     >    ! echo user input to trail file
!!     >    call journal('T',userline)
!!     >    ! assume user input causes values i1, i2, and i3 to be calculated
!!     >    write(output,'(i0,1x,i0,1x)')i1,i2,i3 ! build an output line
!!     >    ! write output to stdout and as comment to trail file
!!     >    call journal(output)
!!     >    !or you can specify the WHERE parameter and up to ten scalar values
!!     >    call journal('SC','i1=',i1,'i2=',i2,'i3=',i3)
!!     > enddo
!!
!!    In this example an output line was built with an internal write; but calls
!!    to journal(3f) with numeric values with and without advancing I/O turned on
!!    are often used for simpler output:
!!
!!       I=10
!!       R=20.3
!!       ! write to stdout and trail file without advancing I/O
!!       call journal('+SC','I=',i)
!!       call journal('SC','AND R=',r)
!!
!!    writes to the trail file are ignored unless a trail file was opened with
!!
!!       CALL JOURNAL('O',filename)
!!
!!
!!    So that routines that do their output via JOURNAL(3f) can be used with and
!!    without programs generating trail files. That is, destinations 'T' and 'C'
!!    are ignored unless a trail file has been requested.
!!
!!    With no parameters, the trail file is flushed.
!!
!!##EXAMPLES
!!
!!
!!    The man(1) page for journal(3f) describes all the options for the WHERE field.
!!    In addition to being used to generate a journal, the routine can be used for
!!    producing optional debug messages and timing information.
!!
!!    Sample program for debug messages:
!!
!!      program demo_journal
!!      !! showing creating debug messages
!!      use M_journal, only : journal
!!      implicit none
!!      !! produces no output because trail is not on
!!      call journal('D','*demo* DEBUG MESSAGE 001 IGNORED')
!!      !! turn on debug messages
!!      call journal('>','debug on')
!!      !! produces output on stdout because debug mode
!!      !! is on but no named trail file
!!      call journal('D','*demo* DEBUG MESSAGE 002 ON STDOUT')
!!      !! open trail file
!!      call journal('O','mytrail.txt')
!!      !! debug messages now go to the trail file
!!      call journal('D','*demo* DEBUG MESSAGE 003 TO TRAIL')
!!      !! close trail file so messages go to stdout again
!!      call journal('O','')
!!      !! debug on stdout now
!!      call journal('D','*demo* DEBUG MESSAGE 004 TO STDOUT')
!!      call journal('<','debug off')
!!      !! back to no output from the next message
!!      call journal('D','*demo* DEBUG MESSAGE 005 IGNORED')
!!      end program demo_journal
!!
!!   Sample program for trail messages with optional timing information:
!!
!!      program testit
!!      use M_journal,only : journal
!!      implicit none
!!      call journal('a single string A -should be on S')
!!
!!      ! add time prefix to output
!!      call journal('%','%Y-%M-%DT%h:%m:%s.%x%u:%b')
!!      call journal('a single string B -should be on S with prefix')
!!      call journal('%','CPU_TIME: %c:CALLS: %C: %b')  ! change time prefix
!!      call journal('a single string B-1 -should be on S with prefix')
!!      call journal('a single string B-2 -should be on S with prefix')
!!      call journal('a single string B-3 -should be on S with prefix')
!!      !  Other useful time formats:
!!      !     %E -- Unix Epoch time
!!      !     %e -- integer value of Unix Epoch time
!!      !     %C -- number of times this format is used
!!      !     %c -- CPU_time(3f) output
!!      !     %S -- seconds since last use of this format
!!      !     %k -- CPU time in seconds from system_clock
!!      call journal('%','') ! turn off time prefix
!!      !
!!      call journal('a single string C -should be on S')
!!      !
!!      call journal('O','aaa.out') ! turn on trail file
!!      call journal('a single string D -should be on SC')
!!      call journal('a single string E -should be on SC')
!!      call journal('a single string F -should be on SC')
!!      call journal('O','') ! turn off trail file
!!      !
!!      call journal('a single string G -should be on S')
!!      call journal('a single string H -should be on S')
!!      call journal('a single string I -should be on S')
!!
!!      ! build one line of output with intrinsic scalar values added
!!      call journal('+sc','APPEND:')
!!      call journal('+sc',' integer',         1234)
!!      call journal('+sc',' and real',        1234.5678)
!!      call journal('+sc',' and double',1234567890.123456d0)
!!      call journal('+sc',' and logical',    .true.)
!!      call journal('sc','')
!!      !
!!      end program testit
!!
!!##AUTHOR
!!     John S. Urban
!!##LICENSE
!!     Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_journal
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, INPUT_UNIT, OUTPUT_UNIT     ! access computing environment
use :: M_msg,                      only : str
implicit none
private

!>
!!##NAME
!!      journal(3f) - [M_journal] provides public message routine, no paging or graphic mode change
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!    subroutine journal([where,],[VALUE(s)])
!!
!!     character(len=*),intent(in) :: where
!!     character(len=*)|real|integer|doubleprecision|complex,optional :: g1,g2,g3,g4,g5,g6,g7,g8,g9
!!
!!   WRITE MESSAGES
!!    basic messages
!!
!!       call journal(where,[VALUE(S)])
!!       call journal(message) # a shortcut for "call journal('sc',message)":
!!   OPEN OR CLOSE TRAIL FILE
!!    trail file
!!
!!       call journal('O',trailfile_name) # open trail file
!!       call journal('O','')             # close trail file
!!   SET OUTPUT TIME PREFIX
!!    set the function display format for timestamps. See the NOW(3f)
!!    procedure for allowable timestamp macros
!!
!!       call journal('%',time_stamp_prefix_specification)
!!
!!   MODES
!!
!!    Turn on/off writing DEBUG messages to trail file
!!
!!       call journal('>','debug on') # turn on debug mode
!!       call journal('<','debug off') # turn off debug mode
!!
!!   ASSIGN STDOUT TO AN ALTERNATE FILE
!!    change stdout to iunit and open filename; or close unit and go back to stdout if filename=''
!!
!!       call journal(iunit,filename)
!!
!!    change stdout to iunit to use a file already open
!!
!!       call journal(iunit)
!!
!!##DESCRIPTION
!!
!!    If a user procedure is used for outputting messages instead of calling
!!    WRITE(3f) it is easy to provide control of when messages are printed
!!    (ie. a "verbose" mode, or "quite" mode), creating files to replay
!!    program execution, duplicating output, ...
!!
!!##OPTIONS
!!   WHERE  indicates where messages are written. A combination of the
!!          following characters can be used...
!!
!!      Usually one of these to write to the standard output files ...
!!
!!      S   write to stdout or iounit set with journal(unit) or
!!          journal(unit,filename).
!!      E   write to stderr
!!
!!      And one of these to write to trail file (ignore if no trail file
!!      defined) ...
!!
!!      C   write to trail file as a comment (if file is open)
!!          Writing output "as a comment" means it is preceded by a pound(#)
!!          character.
!!      T   write to trail file (if file is open)
!!
!!      Usually used by itself
!!
!!      D   write to trail file as a comment with "DEBUG:" prefix in front
!!          of message (if file is open) if debug mode is on. Write to stdout
!!          if no trail file and debug mode is on.
!!
!!      Modifier for S|E|C|T|D specifiers
!!
!!      +   subsequent files are written to with advance='no'. Position is
!!          important. '+sc' does an advance='no' on both files, 's+c'
!!          only does the advance='no' for the trail file.
!!
!!      Mode changing options used by themselves:
!!
!!      >   turn off debug messages
!!      <   turn on debug messages
!!      O   open trail file using value of "message" parameter or close
!!          trail file if no filename or a blank filename.
!!      A   Auxiliary programs that also want to write to the current log file
!!          (a2b, z2a, ...) call this routine to see if there is a trail file
!!          being generated and then add to it so that a program like ush(1f)
!!          can call the auxiliary programs and still just generate one log file,
!!          but if the auxiliary program is used as a stand-alone program no trail
!!          is generated.
!!
!!   VALUES(S)   message to write to stdout, stderr, and the trail file.
!!               a numeric or character value to optionally be appended
!!               to the message. Up to nine values are allowed. The WHERE
!!               field is required if values are added.
!!   FILENAME    when WHERE="O" to turn the trail file on or off, the "message"
!!               field becomes the trail filename to open. If blank, writing
!!               to the trail file is turned off.
!!   TFORMAT     when WHERE="%" the message is treated as a time format
!!               specification as described under now(3f).
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_journal
!!    use M_journal, only : journal
!!    !! BASIC USAGE
!!    call journal('write to standard output as-is, and trail file as a comment if open')
!!    ! since we have not opened a trail file yet, only stdout will display output
!!    call journal('c','ignored, as trail file is not open')
!!    ! now open trail file "trail"
!!    call journal('o','trail')
!!    call journal('sc','same thing except now trail file is open')
!!    ! only write to trail file if open
!!    call journal('c','not ignored, as trail file is open. Written with # suffix')
!!    call journal('t','not ignored, as trail file is open. Written as-is')
!!    ! turn off trail file
!!    call journal('o','')
!!    end program demo_journal
!!
!!   Adding intrinsic scalar values to the message:
!!
!!    program test_journal
!!    use M_journal, only: journal
!!    implicit none
!!       call journal('S','This is a test with no optional value')
!!       call journal('S','This is a test with a logical value',.true.)
!!       call journal('S','This is a test with a double value',1234567890.123456789d0)
!!       call journal('S','This is a test with a real value',1234567890.123456789)
!!       call journal('S','This is a test with an integer value',1234567890)
!!       call journal('STDC','This is a test using STDC',1234567890)
!!       call journal('stdc','This is a test using stdc',1234567890)
!!       call journal('o','journal.txt')                        ! open trail file
!!       call journal('S',1,12.34,56789.111111111d0,.false.,'a bunch of values')
!!       ! the combinations that make sense
!!       call journal('st','stdout and trail')
!!       call journal('s' ,'stdout only')
!!       call journal('t' ,'trail only')
!!       call journal('sc','stdout and trail_comment')
!!       call journal('c' ,'trail_comment only ')
!!       call journal('d' ,'debug only')
!!       call journal('e' ,'stderr only')
!!       call journal('o' ,' ') ! closing trail file
!!    end program test_journal
!!
!!    program testit
!!    ! this is a utility program that calls the module routines. It is typically built using ccall(1).
!!    use M_journal, only : journal
!!       character(len=:),allocatable :: time_stamp_prefix
!!       call journal('s','--------------------------------------------------------------------------------')
!!       call journal('s','SIMPLE WRITES')
!!       call one()
!!       call two()
!!       call journal('sc','called ONE() and TWO() but did not generate a log file')
!!       call journal('s','--------------------------------------------------------------------------------')
!!       call journal('s','SIMPLE WRITES WITH LOG FILE')
!!       call journal('o','journal.txt')                        ! open trail file
!!       call one()
!!       call two()
!!       call journal('sc','called ONE() and TWO() and generated log file journal.txt')
!!       call journal('','journal.txt')                         ! close trail file
!!       call journal('s','--------------------------------------------------------------------------------')
!!       call journal('s','SIMPLE WRITES WITH TIMING INFORMATION')
!!       time_stamp_prefix='CPU_TIME=%c:CALLS=%C:SINCE=%S:%b'  ! change time prefix
!!       call journal('%',time_stamp_prefix) ! set a time prefix in front of messages
!!       call journal('o','timed.txt')                          ! open trail file
!!       call one()
!!       call two()
!!       call journal('sc','called ONE() and TWO() and generate log file timed.txt')
!!       call journal('','timed.txt')                           ! close trail file
!!       call journal('%','')                                   ! turn off time prefix
!!       call journal('o','timed.txt')                          ! open trail file
!!       call journal('s','--------------------------------------------------------------------------------')
!!
!!    contains
!!
!!       subroutine two()
!!          call journal('Entered subroutine two')
!!          call journal('Exited subroutine two')
!!       end subroutine two
!!
!!       subroutine one()
!!          call journal('Entered subroutine one')
!!          sum=-HUGE(1.0)
!!          do i=1,10000000
!!            sum=sum+sqrt(real(i))
!!          enddo
!!          write(*,*)'SUM=',sum
!!          call journal('Exited subroutine one')
!!       end subroutine one
!!
!!    end program testit
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
public journal

interface journal
   module procedure flush_trail               ! journal()                ! no options
   module procedure write_message_only        ! journal(c)               ! must have one string
   module procedure where_write_message_all   ! journal(where,[g1-g9])   ! must have two strings
   module procedure set_stdout_lun            ! journal(i)               ! first is not a string
end interface journal

! ident_1="@(#)M_journal::journal(3fg): provides public message routine, no paging or graphic mode change"

! global variables

!integer,parameter,private  :: stdin=INPUT_UNIT
integer,save,private       :: my_stdout=OUTPUT_UNIT
logical,save               :: debug=.false.
integer,save               :: last_int=0

interface
   function now_ex(format)
      character(len=*),intent(in),optional :: format
      character(len=:),allocatable         :: now_ex
   end function now_ex
end interface

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine where_write_message(where,msg)

! ident_2="@(#)M_journal::where_write_message(3fp): basic message routine used for journal files"

character(len=*),intent(in)  :: where
character(len=*),intent(in)  :: msg
!
!  writes error messages and general information text to stdout and the trace file
!     where=*C* write to trail file as a comment (if file is open)
!     where=*D* write to trail file as a comment with DEBUG: prefix in front of message (if file is open and debug mode on)
!     where=*E* write to stderr
!     where=*S* write to stdout or iounit set with journal(unit) or journal(unit,filename)
!     where=*T* write to trail file (if file is open)
!     where=*+* subsequent writes for this call are written with advance='no'

!     where=> turn on debug messages (change mode), which are ones with WHERE='D'
!     where=< turn off debug messages  (change mode), which are ones with WHERE='D'

!     where=O open trail file "msg" or close trail file if blank filename is given
!     where=% set prefix to run thru now(3f) to generate time prefix strings, blank turns off time prefix
!     where=N open new file and assign stdout to the file unless file name is blank; then revert to my_stdout being original stdout.
!
!  the trail file messages are preceded by a pound character (#) by default so they can easily be interpreted as comments
!  if the trace file is subsequently used as input data for a program
!
logical,save                       :: trailopen=.false.
integer,save                       :: itrail
character,save                     :: comment='#'
integer                            :: i
integer                            :: ios
integer                            :: times             ! number of times written to stdout
character(len=3)                   :: adv               ! whether remaining writes from this call use advancing I/O

character(len=:),allocatable,save  :: prefix_template   ! string to run thru now_ex(3f) to make prefix
character(len=:),allocatable       :: prefix            ! the prefix string to add to output
logical,save                       :: prefix_it=.false. ! flag whether time prefix mode is on or not
character(len=4096)                :: mssge
!-----------------------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------------------
   adv='yes'
!-----------------------------------------------------------------------------------------------------------------------------------
   if(prefix_it)then
      prefix=now_ex(prefix_template)
   else
      prefix=''
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   times=0
   do i=1,len_trim(where)
      select case(where(i:i))
      case('T','t')
         if(trailopen) then
            write(itrail,'(a)',advance=adv)prefix//trim(msg)
         !!elseif(times.eq.0)then
         !!   write(my_stdout,'(a)',advance=adv)prefix//trim(msg)
         !!   times=times+1
         endif
      !-----------------------------------------------------------------------------------------------------------------------------
      case('S','s')
         write(my_stdout,'(a)',advance=adv)prefix//trim(msg)
         times=times+1
      !-----------------------------------------------------------------------------------------------------------------------------
      case('E','e')
         write(stderr,'(a)',advance=adv)prefix//trim(msg)
         times=times+1
      !-----------------------------------------------------------------------------------------------------------------------------
      case('+'); adv='no'
      !-----------------------------------------------------------------------------------------------------------------------------
      case('>'); debug=.true.
      !-----------------------------------------------------------------------------------------------------------------------------
      case('<'); debug=.false.
      !-----------------------------------------------------------------------------------------------------------------------------
      case('%')                       ! setting timestamp prefix
         if(msg.eq.'')then            ! if message is blank turn off prefix
            prefix_it=.false.
         else                         ! store message as string to pass to now_ex() on subsequent calls to make prefix
            prefix_template=msg
            prefix_it=.true.
         endif
      !-----------------------------------------------------------------------------------------------------------------------------
      case('N')                                                   ! new name for my_stdout
         if(msg.ne.' '.and.msg.ne.'#N#'.and.msg.ne.'"#N#"')then   ! if filename not special or blank open new file
            close(unit=last_int,iostat=ios)
            open(unit=last_int,file=adjustl(trim(msg)),iostat=ios)
            if(ios.eq.0)then
               my_stdout=last_int
            else
               write(*,*)'*journal* error opening redirected output file, ioerr=',ios
               write(*,*)'*journal* msg='//trim(msg)
            endif
         elseif(msg.eq.' ')then
            close(unit=last_int,iostat=ios)
            my_stdout=6
         endif
      !-----------------------------------------------------------------------------------------------------------------------------
      case('C','c')
         if(trailopen)then
            write(itrail,'(3a)',advance=adv)prefix,comment,trim(msg)
         elseif(times.eq.0)then
            !! write(my_stdout,'(2a)',advance=adv)prefix,trim(msg)
            !! times=times+1
         endif
      case('D','d')
         if(debug)then
            if(trailopen)then
               write(itrail,'(4a)',advance=adv)prefix,comment,'DEBUG: ',trim(msg)
            elseif(times.eq.0)then
               write(my_stdout,'(3a)',advance=adv)prefix,'DEBUG:',trim(msg)
               times=times+1
            endif
         endif
      case('F','f')
         flush(unit=itrail,iostat=ios,iomsg=mssge)
         if(ios.ne.0)then
            write(*,'(a)') trim(mssge)
         endif
      case('A','a')
         if(msg.ne.'')then
            open(newunit=itrail,status='unknown',access='sequential',file=adjustl(trim(msg)),&
            & form='formatted',iostat=ios,position='append')
            trailopen=.true.
         endif
      case('O','o')
         if(msg.ne.'')then
            open(newunit=itrail,status='unknown',access='sequential', file=adjustl(trim(msg)),form='formatted',iostat=ios)
            trailopen=.true.
         else
            if(trailopen)then
               write(itrail,'(4a)',advance=adv)prefix,comment,'closing trail file:',trim(msg)
            endif
            close(unit=itrail,iostat=ios)
            trailopen=.false.
         endif
      case default
         write(my_stdout,'(a)',advance=adv)'*journal* bad WHERE value '//trim(where)//' when msg=['//trim(msg)//']'
      end select
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine where_write_message
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine flush_trail()

! ident_3="@(#)M_journal::flush_trail(3fp): flush trail file"

call where_write_message('F','IGNORE THIS STRING')
end subroutine flush_trail
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine set_stdout_lun(iounit)

! ident_4="@(#)M_journal::set_stdout_lun(3fp): change I/O logical unit value for standard writes"

integer,intent(in)                   :: iounit
   my_stdout=iounit
end subroutine set_stdout_lun
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    where_write_message_all(3f) - [M_journal] converts any standard scalar type to a string and calls journal(3f)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine where_write_message_all(where,g0,g1,g2g3,g4,g5,g6,g7,g8,g9,sep)
!!
!!     character(len=*),intent(in)   :: where
!!     class(*),intent(in)           :: g0
!!     class(*),intent(in),optional  :: g1,g2,g3,g4,g5,g6,g7,g8,g9
!!     character,intent(in),optional :: sep
!!
!!##DESCRIPTION
!!    where_write_message_all(3f) builds and writes a space-separated string from up to nine scalar values.
!!
!!##OPTIONS
!!
!!    where    string designating where to write message, as with journal(3f)
!!    g0       value to print. May
!!             be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!             or CHARACTER.
!!    g[1-9]   optional additional values to print the value of after g0.
!!    sep      separator to add between values. Default is a space
!!##RETURNS
!!    where_write_message_all  description to print
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_wm_all
!!    use M_journal, only : where_write_message_all
!!    implicit none
!!    end program program demo_wm_all
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine where_write_message_all(where, g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, sep)
implicit none

! ident_5="@(#)M_journal::where_write_message_all(3f): writes a message to a string composed of any standard scalar types"

character(len=*),intent(in)   :: where
class(*),intent(in)           :: g0
class(*),intent(in),optional  :: g1, g2, g3, g4, g5, g6, g7, g8 ,g9
character,intent(in),optional :: sep
call where_write_message(where,str(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9,sep))
end subroutine where_write_message_all
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine write_message_only(message)

! ident_6="@(#)M_journal::write_message_only(3fp): calls JOURNAL('sc',message)"

character(len=*),intent(in)          :: message
!-----------------------------------------------------------------------------------------------------------------------------------
   call where_write_message('sc',trim(message))
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine write_message_only
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_journal
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================











!>
!!##NAME
!!    M_strings(3f) - [M_strings:INTRO] Fortran string module
!!
!!##DESCRIPTION
!!    The M_strings(3fm) module is a collection of Fortran procedures
!!    that supplement the built-in intrinsic string routines. Routines
!!    for parsing, tokenizing, changing case, substituting new strings for
!!    substrings, locating strings with simple wildcard expressions, removing
!!    tabs and line terminators and other string manipulations are included.
!!
!!    M_strings_oop(3fm) is a companion module that provides an OOP interface
!!    to the M_strings module.
!!
!!##SYNOPSIS
!!
!!  public entities:
!!
!!      use M_strings, only : split,sep,delim,chomp,strtok
!!      use M_strings, only : substitute,change,modif,transliterate,reverse
!!      use M_strings, only : replace,join
!!      use M_strings, only : upper,lower,upper_quoted
!!      use M_strings, only : rotate13
!!      use M_strings, only : adjustc,compact,nospace,indent
!!      use M_strings, only : crop,unquote,quote
!!      use M_strings, only : len_white,atleast,stretch,lenset,merge_str
!!      use M_strings, only : switch,s2c,c2s
!!      use M_strings, only : noesc,notabs,dilate,expand,visible
!!      !!use M_strings, only : uc
!!      use M_strings, only : string_to_value,string_to_values,s2v,s2vs
!!      use M_strings, only : value_to_string,v2s,msg
!!      use M_strings, only : listout,getvals
!!      use M_strings, only : glob, ends_with
!!      use M_strings, only : fmt
!!      use M_strings, only : base, decodebase, codebase, base2
!!      use M_strings, only : isalnum, isalpha, iscntrl, isdigit
!!      use M_strings, only : isgraph, islower, isprint, ispunct
!!      use M_strings, only : isspace, isupper, isascii, isblank, isxdigit
!!      use M_strings, only : isnumber
!!      use M_strings, only : fortran_name
!!      use M_strings, only : describe
!!
!!   TOKENS
!!       split  subroutine parses string using specified delimiter characters
!!              and stores tokens into an array
!!       sep    function interface to split(3f)
!!       delim  subroutine parses string using specified delimiter characters
!!              and store tokens into an array
!!       chomp  function consumes input line as it returns next token in a
!!              string using specified delimiters
!!       fmt    convert a string into a paragraph
!!       strtok tokenize a string like C strtok(3c) routine
!!
!!   EDITING
!!       substitute     subroutine non-recursively globally replaces old
!!                      substring with new substring
!!       replace        function non-recursively globally replaces old
!!                      substring with new substring using allocatable string
!!                      (version of substitute(3f) without limitation on
!!                      length of output string)
!!       change         subroutine non-recursively globally replaces old
!!                      substring with new substring with a directive like
!!                      line editor
!!       modif          subroutine modifies a string with a directive like the
!!                      XEDIT line editor MODIFY command
!!       transliterate  replace characters found in set one with characters
!!                      from set two
!!       reverse        reverse character order in a string
!!       join           join an array of CHARACTER variables with specified
!!                      separator
!!       rotate13       apply trivial encryption algorithm ROT13 to a string
!!
!!   CASE
!!       upper          function converts string to uppercase
!!       lower          function converts string to miniscule
!!       upper_quoted   function converts string to uppercase skipping strings
!!                      quoted per Fortran rules
!!
!!   WHITE SPACE
!!       adjustc  elemental function centers text within the length of the
!!                input string
!!       compact  left justify string and replace duplicate whitespace with
!!                single characters or nothing
!!       nospace  function replaces whitespace with nothing
!!       indent   find number of leading spaces
!!       crop     function trims leading and trailing spaces
!!
!!   QUOTES
!!       unquote  remove quotes from string as if read with list-directed input
!!       quote    add quotes to string as if written with list-directed input
!!
!!   STRING LENGTH
!!       len_white  find location of last non-whitespace character
!!       lenset     return a string of specified length
!!       atleast    return a string of at least specified length
!!       stretch    return a string of at least specified length with suffix
!!       merge_str  make strings of equal length and then call MERGE(3f)
!!                  intrinsic
!!
!!   CHARACTER ARRAY VERSUS STRING
!!       switch  switch between a string and an array of single characters
!!       s2c     convert string to array of single characters and add null
!!               terminator for passing to C
!!       c2s     convert null-terminated array of single characters to
!!               string for converting strings returned from C
!!
!!   NONALPHA
!!       noesc    convert non-printable ASCII8 characters to a space
!!       notabs   convert tabs to spaces while maintaining columns,
!!                assuming tabs are set every 8 characters
!!       dilate   function to convert tabs to spaces assuming tabs are set
!!                every 8 characters
!!       expand   expand escape sequences in a string
!!       visible  expand escape sequences in a string to "control" and
!!                meta-control representations
!!
!!   NUMERIC STRINGS
!!       string_to_value   generic subroutine returns numeric value (REAL,
!!                         DOUBLEPRECISION, INTEGER) from string
!!       string_to_values  subroutine reads an array of numbers from a string
!!       getvals           subroutine reads a relatively arbitrary number
!!                         of values from a string using list-directed read
!!       s2v               function returns DOUBLEPRECISION numeric value
!!                         from string
!!       s2vs              function returns a DOUBLEPRECISION array of numbers
!!                         from a string
!!       msg               append the values of up to nine values into a string
!!
!!       value_to_string   generic subroutine returns string given numeric value
!!                         (REAL, DOUBLEPRECISION, INTEGER, LOGICAL )
!!       v2s               generic function returns string from numeric value
!!                         (REAL, DOUBLEPRECISION, INTEGER )
!!       listout           expand a list of numbers where negative numbers
!!                         denote range ends (1 -10 means 1 thru 10)
!!       isnumber          determine if string represents a number
!!
!!   CHARACTER TESTS
!!       glob        compares given string for match to pattern which may
!!                   contain wildcard characters
!!       ends_with   test whether strings ends with one of the specified suffixs
!!
!!       o isalnum   returns .true. if character is a letter or digit
!!       o isalpha   returns .true. if character is a letter and
!!                   .false. otherwise
!!       o iscntrl   returns .true. if character is a delete character or
!!                   ordinary control character
!!       o isdigit   returns .true. if character is a digit (0,1,...,9)
!!                   and .false. otherwise
!!       o isgraph   returns .true. if character is a printable character
!!                   except a space is considered non-printable
!!       o islower   returns .true. if character is a miniscule letter (a-z)
!!       o isprint   returns .true. if character is an ASCII printable
!!                   character
!!       o ispunct   returns .true. if character is a printable punctuation
!!                   character
!!       o isspace   returns .true. if character is a null, space, tab,
!!                   carriage return, new line, vertical tab, or formfeed
!!       o isupper   returns .true. if character is an uppercase letter (A-Z)
!!       o isascii   returns .true. if the character is in the range char(0)
!!                   to char(127)
!!       o isblank   returns .true. if character is a blank character
!!                   (space or horizontal tab.
!!       o isxdigit  returns .true. if character is a hexadecimal digit
!!                   (0-9, a-f, or A-F).
!!
!!       fortran_name   returns .true. if input string is a valid Fortran name
!!
!!   BASE CONVERSION
!!       base       convert whole number string in base [2-36] to string
!!                  in alternate base [2-36]
!!       base2      convert INTEGER to a string representing a binary value
!!       codebase   convert whole number string in base [2-36] to base
!!                  10 number
!!       decodebase convert whole number in base 10 to string in base [2-36]
!!
!!   MISCELLANEOUS
!!       describe   returns a string describing the name of a single character
!!
!!   INTRINSICS
!!    The M_strings(3fm) module supplements and works in combination with
!!    the Fortran built-in intrinsics. Stand-alone Fortran lets you access
!!    the characters in a string using ranges much like they are character
!!    arrays, assignment, comparisons with standard operators, supports
!!    dynamically allocatable strings and supports concatenation using the //
!!    operator, as well as a number of intrinsic string routines:
!!
!!        adjustl             Left adjust a string
!!        adjustr             Right adjust a string
!!        index               Position of a substring within a string
!!        repeat              Repeated string concatenation
!!        scan                Scan a string for the presence of a set
!!                            of characters
!!        trim                Remove trailing blank characters of a string
!!        verify              Scan a string for the absence of a set of
!!                            characters
!!        len                 It returns the length of a character string
!!        achar               converts an integer into a character
!!        iachar              converts a character into an integer
!!        len_trim            finds length of string with trailing spaces
!!                            ignored
!!        new_line            Newline character
!!        selected_char_kind  Choose character kind
!!        lge                 Lexical greater than or equal
!!        lgt                 Lexical greater than
!!        lle                 Lexical less than or equal
!!        llt                 Lexical less than
!!
!!   OOPS INTERFACE
!!    The M_strings_oop(3fm) module (included with the M_strings(3fm)
!!    module) provides an OOP (Object-Oriented Programming) interface to
!!    the M_strings(3fm) module.
!!
!!##SEE ALSO
!!    There are additional routines in other GPF modules for working with
!!    expressions (M_calculator), time strings (M_time), random strings
!!    (M_random, M_uuid), lists (M_list), and interfacing with the C regular
!!    expression library (M_regex).
!!
!!##EXAMPLES
!!
!!    Each of the procedural functions includes an example program in the
!!    corresponding man(1) page for the function. The object-oriented
!!    interface does not have individual man(1) pages, but is instead
!!    demonstrated using the following example program:
!!
!!
!!     program demo_M_strings
!!     use M_strings, only : split, delim, chomp, sep
!!     use M_strings, only : substitute, change, modif
!!     use M_strings, only : transliterate, reverse
!!     use M_strings, only : replace, join
!!     use M_strings, only : upper, lower, upper_quoted
!!     use M_strings, only : rotate13
!!     use M_strings, only : adjustc, compact, nospace, indent, crop
!!     use M_strings, only : unquote, quote
!!     use M_strings, only : len_white, atleast, stretch, lenset, merge_str
!!     use M_strings, only : switch, s2c, c2s
!!     use M_strings, only : noesc, notabs, dilate, expand, visible
!!     !!use M_strings, only : uc
!!     use M_strings, only : string_to_value, string_to_values, s2v, s2vs
!!     use M_strings, only : value_to_string, v2s, msg
!!     use M_strings, only : listout, getvals
!!     use M_strings, only : glob, ends_with
!!     use M_strings, only : fmt
!!     use M_strings, only : base, decodebase, codebase, base2
!!     use M_strings, only : isalnum, isalpha, iscntrl, isdigit, isgraph
!!     use M_strings, only : islower, isprint, ispunct, isspace, isupper
!!     use M_strings, only : isascii, isblank, isxdigit
!!     use M_strings, only : fortran_name
!!     end program demo_M_strings
!!
!!   Expected output
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
MODULE M_strings !
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT        ! access computing environment
use, intrinsic :: iso_fortran_env, only : output_unit, stderr=>error_unit
use M_journal,       only : journal
implicit none    ! change default for every procedure contained in the module

! ident_1="@(#)M_strings(3f): Fortran module containing routines that deal with character strings"

!-----------------------------------------------------------------------------------------------------------------------------------
PRIVATE

!----------------------# TOKENS
PUBLIC split           !  subroutine parses a string using specified delimiter characters and store tokens into an allocatable array
PUBLIC sep             !  function interface to split
PUBLIC chomp           !  function consumes input line as it returns next token in a string using specified delimiters
PUBLIC delim           !  subroutine parses a string using specified delimiter characters and store tokens into an array
PUBLIC strtok          !  gets next token. Used by change(3f)
PUBLIC fmt             !  convert a long string into a paragraph
!----------------------# EDITING
PUBLIC substitute      !  subroutine non-recursively globally replaces old substring with new substring in string
PUBLIC replace         !  function non-recursively globally replaces old substring with new substring in string
PUBLIC change          !  replaces old substring with new substring in string with a directive like a line editor
PUBLIC modif           !  change string using a directive using rules similar to XEDIT line editor MODIFY command
PUBLIC transliterate   !  when characters in set one are found replace them with characters from set two
PUBLIC reverse         !  elemental function reverses character order in a string
PUBLIC join            !  append an array of character variables with specified separator into a single CHARACTER variable
PUBLIC rotate13        !  apply trivial encryption algorithm ROT13 to string
!----------------------# CHARACTER ARRAY VERSUS STRING
PUBLIC switch          !  generic switch between a string and an array of single characters (a2s,s2a)
PRIVATE a2s            !  function to copy char array to string
PRIVATE s2a            !  function to copy string(1:Clen(string)) to char array
PUBLIC s2c             !  convert character variable to array of character(len=1) with null terminator for C compatibility
PUBLIC c2s             !  convert null-terminated array of character(len=1) to string for strings returned by C
!----------------------# CASE
PUBLIC upper           !  elemental function converts string to uppercase
PUBLIC lower           !  elemental function converts string to miniscule
PUBLIC upper_quoted          !  elemental function converts string to miniscule skipping strings quoted per Fortran syntax rules
!----------------------# WHITE SPACE
PUBLIC adjustc         !  elemental function centers string within the length of the input string
PUBLIC compact         !  left justify string and replace duplicate whitespace with single characters or nothing
PUBLIC nospace         !  function replaces whitespace with nothing
PUBLIC indent          !  count number of leading spaces
PUBLIC crop            !  function trims leading and trailing spaces
!----------------------# QUOTES
PUBLIC unquote         !  remove quotes from string as if read with list-directed input
PUBLIC quote           !  add quotes to string as if written with list-directed input
!----------------------# STRING LENGTH
PUBLIC lenset          !  return a string as specified length
PUBLIC atleast         !  return a string of at least specified length
PUBLIC stretch         !  return a string of at least specified length with suffix
PUBLIC merge_str       !  make strings of equal length and then call MERGE(3f) intrinsic
PUBLIC len_white       !  find location of last non-whitespace character
!----------------------# NONALPHA
PUBLIC noesc           !  elemental function converts non-printable ASCII8 characters to a space
PUBLIC notabs          !  convert tabs to spaces in output while maintaining columns, assuming a tab is set every 8 characters
PUBLIC dilate          !  convert tabs to spaces in output while maintaining columns, assuming a tab is set every 8 characters
PUBLIC expand          !  expand escape sequences in a string
PUBLIC visible         !  expand escape sequences in a string to control and meta-control representations
!----------------------# NUMERIC STRINGS
PUBLIC string_to_value !  generic subroutine returns REAL|DOUBLEPRECISION|INTEGER value from string (a2d,a2r,a2i)
 PRIVATE a2d           !  subroutine returns double value from string
 PRIVATE a2r           !  subroutine returns real value from string
 PRIVATE a2i           !  subroutine returns integer value from string
PUBLIC string_to_values!  subroutine returns values from a string
PUBLIC getvals         !  subroutine returns values from a string
PUBLIC s2v             !  function returns doubleprecision value from string
PUBLIC s2vs            !  function returns a doubleprecision array of numbers from a string
                       !------------------------------------------------------------------------------------------------------------
PUBLIC msg             !  function returns a string representing up to nine scalar intrinsic values
PUBLIC value_to_string !  generic subroutine returns string given numeric REAL|DOUBLEPRECISION|INTEGER|LOGICAL value
PUBLIC v2s             !  generic function returns string given numeric REAL|DOUBLEPRECISION|INTEGER|LOGICAL value
 PRIVATE d2s           !  function returns string from doubleprecision value
 PRIVATE r2s           !  function returns string from real value
 PRIVATE i2s           !  function returns string from integer value
 PRIVATE l2s           !  function returns string from logical value
PUBLIC v2s_bug         !  generic function returns string given numeric REAL|DOUBLEPRECISION|INTEGER value
PUBLIC isnumber        !  determine if string represents a number
 PRIVATE trimzeros_    !  Delete trailing zeros from numeric decimal string
PUBLIC listout         !  expand a list of numbers where  negative numbers denote range ends (1 -10 means 1 thru 10)
!-----------------------------------------------------------------------------------------------------------------------------------
!
! extend intrinsics to accept CHARACTER values
!
PUBLIC int, real, dble

interface int;     module procedure int_s2v;           end interface
interface real;    module procedure real_s2v;          end interface
interface dble;    module procedure dble_s2v;          end interface

interface int;     module procedure ints_s2v;          end interface
interface real;    module procedure reals_s2v;         end interface
interface dble;    module procedure dbles_s2v;         end interface

!-----------------------------------------------------------------------------------------------------------------------------------
!----------------------# BIT ROUTINES
PUBLIC setbits8        !  use a string representing a positive binary value to fill the bits of an INTEGER value
PUBLIC setbits16       !  use a string representing a positive binary value to fill the bits of an INTEGER value
PUBLIC setbits32       !  use a string representing a positive binary value to fill the bits of an INTEGER value
PUBLIC setbits64       !  use a string representing a positive binary value to fill the bits of an INTEGER value
!----------------------# BASE CONVERSION
PUBLIC base            !  convert whole number string in base [2-36] to string in alternate base [2-36]
PUBLIC codebase        !  convert whole number string in base [2-36] to base 10 number
PUBLIC decodebase      !  convert whole number in base 10 to string in base [2-36]
PUBLIC base2           !  convert INTEGER to a string representing a binary value
!----------------------# LOGICAL TESTS
PUBLIC glob            !  compares given string for match to pattern which may contain wildcard characters
PUBLIC matchw          !  clone of glob -- for backward compatibiity
PUBLIC ends_with       !  test whether strings ends with one of the specified suffix
PUBLIC isalnum         !  elemental function returns .true. if CHR is a letter or digit
PUBLIC isalpha         !  elemental function returns .true. if CHR is a letter and .false. otherwise
PUBLIC isascii         !  elemental function returns .true. if the low order byte of c is in the range char(0) to char(127)
PUBLIC isblank         !  elemental function returns .true. if CHR is a blank character (space or horizontal tab.
PUBLIC iscntrl         !  elemental function returns .true. if CHR is a delete character or ordinary control character
PUBLIC isdigit         !  elemental function returns .true. if CHR is a digit (0,1,...,9) and .false. otherwise
PUBLIC isgraph         !  elemental function true if CHR is an ASCII printable character except considers a space non-printable
PUBLIC islower         !  elemental function returns .true. if CHR is a miniscule letter (a-z)
PUBLIC isprint         !  elemental function determines if CHR is an ASCII printable character
PUBLIC ispunct         !  elemental function returns .true. if CHR is a printable punctuation character
PUBLIC isspace         !  elemental function true if CHR is a null, space, tab, carriage return, new line, vertical tab, or formfeed
PUBLIC isupper         !  elemental function returns .true. if CHR is an uppercase letter (A-Z)
PUBLIC isxdigit        !  elemental function returns .true. if CHR is a hexadecimal digit (0-9, a-f, or A-F).
!----------------------#
PUBLIC fortran_name    !  elemental function returns .true. if LINE is a valid Fortran name
!----------------------#
PUBLIC describe        !  returns a string describing character
!----------------------#

!-----------------------------------------------------------------------------------------------------------------------------------

! ident_2="@(#)M_strings::switch(3f): toggle between string and array of characters"

interface switch
   module procedure a2s, s2a
end interface switch
! note how returned result is "created" by the function
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_3="@(#)M_strings::string_to_value(3f): Generic subroutine converts numeric string to a number (a2d,a2r,a2i)"

interface string_to_value
   module procedure a2d, a2r, a2i
end interface
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_4="@(#)M_strings::v2s(3f): Generic function returns string given REAL|INTEGER|DOUBLEPRECISION value(d2s,r2s,i2s)"

interface v2s
   module procedure d2s, r2s, i2s, l2s
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
!-!interface setbits !! boz
!-!        module procedure setbits8, setbits16, setbits32, setbits64
!-!end interface
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_5="@(#)M_strings::msg(3f): convert up to nine scalar values to a string. Alternatively can also handle one-dimensional arrays"

interface msg
   module procedure msg_scalar, msg_one
end interface msg
!-----------------------------------------------------------------------------------------------------------------------------------
! ASCII character constants
character, public, parameter :: ascii_nul = char(0)   ! null
character, public, parameter :: ascii_bel = char(7)   ! bell
character, public, parameter :: ascii_bs  = char(8)   ! backspace
character, public, parameter :: ascii_ht  = char(9)   ! horizontal tab
character, public, parameter :: ascii_lf  = char(10)  ! line feed or newline
character, public, parameter :: ascii_ff  = char(12)  ! form feed or newpage
character, public, parameter :: ascii_cr  = char(13)  ! carriage return
character, public, parameter :: ascii_esc = char(27)  ! escape
!-----------------------------------------------------------------------------------------------------------------------------------
interface ends_with
    procedure :: ends_with_str
    procedure :: ends_with_any
end interface ends_with
!-----------------------------------------------------------------------------------------------------------------------------------
public :: split2020, string_tokens

interface split2020
   module procedure :: split_tokens, split_first_last, split_pos
end interface split2020
!-----------------------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------------------
! for compatibility allow old name for renamed procedures
interface matchw; module procedure glob ;  end interface
!-----------------------------------------------------------------------------------------------------------------------------------
CONTAINS
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    glob(3f) - [M_strings:COMPARE] compare given string for match to
!!    a pattern which may contain globbing wildcard characters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    logical function glob(string, pattern )
!!
!!     character(len=*),intent(in) :: string
!!     character(len=*),intent(in) :: pattern
!!
!!##DESCRIPTION
!!    glob(3f) compares given STRING for match to PATTERN which may
!!    contain basic wildcard "globbing" characters.
!!
!!    In this version to get a match the entire string must be described
!!    by PATTERN. Trailing whitespace is significant, so trim the input
!!    string to have trailing whitespace ignored.
!!
!!##OPTIONS
!!    string   the input string to test to see if it contains the pattern.
!!    pattern  the following simple globbing options are available
!!
!!             o "?" matching any one character
!!             o "*" matching zero or more characters.
!!               Do NOT use adjacent asterisks.
!!             o Both strings may have trailing spaces which
!!               are ignored.
!!             o There is no escape character, so matching strings with
!!               literal question mark and asterisk is problematic.
!!
!!##EXAMPLES
!!
!!   Example program
!!
!!    program demo_glob
!!    implicit none
!!    ! This main() routine passes a bunch of test strings
!!    ! into the above code.  In performance comparison mode,
!!    ! it does that over and over. Otherwise, it does it just
!!    ! once. Either way, it outputs a passed/failed result.
!!    !
!!    integer :: nReps
!!    logical :: allpassed
!!    integer :: i
!!     allpassed = .true.
!!
!!     nReps = 10000
!!     ! Can choose as many repetitions as you're expecting
!!     ! in the real world.
!!     nReps = 1
!!
!!     do i=1,nReps
!!      ! Cases with repeating character sequences.
!!      allpassed=allpassed .and. test("a*abab", "a*b", .true.)
!!      !!cycle
!!      allpassed=allpassed .and. test("ab", "*?", .true.)
!!      allpassed=allpassed .and. test("abc", "*?", .true.)
!!      allpassed=allpassed .and. test("abcccd", "*ccd", .true.)
!!      allpassed=allpassed .and. test("bLah", "bLaH", .false.)
!!      allpassed=allpassed .and. test("mississippi", "*sip*", .true.)
!!      allpassed=allpassed .and. &
!!       & test("xxxx*zzzzzzzzy*f", "xxx*zzy*f", .true.)
!!      allpassed=allpassed .and. &
!!       & test("xxxx*zzzzzzzzy*f", "xxxx*zzy*fffff", .false.)
!!      allpassed=allpassed .and. &
!!       & test("mississipissippi", "*issip*ss*", .true.)
!!      allpassed=allpassed .and. &
!!       & test("xxxxzzzzzzzzyf", "xxxx*zzy*fffff", .false.)
!!      allpassed=allpassed .and. &
!!       & test("xxxxzzzzzzzzyf", "xxxx*zzy*f", .true.)
!!      allpassed=allpassed .and. test("xyxyxyzyxyz", "xy*z*xyz", .true.)
!!      allpassed=allpassed .and. test("xyxyxyxyz", "xy*xyz", .true.)
!!      allpassed=allpassed .and. test("mississippi", "mi*sip*", .true.)
!!      allpassed=allpassed .and. test("ababac", "*abac*", .true.)
!!      allpassed=allpassed .and. test("aaazz", "a*zz*", .true.)
!!      allpassed=allpassed .and. test("a12b12", "*12*23", .false.)
!!      allpassed=allpassed .and. test("a12b12", "a12b", .false.)
!!      allpassed=allpassed .and. test("a12b12", "*12*12*", .true.)
!!
!!      ! Additional cases where the '*' char appears in the tame string.
!!      allpassed=allpassed .and. test("*", "*", .true.)
!!      allpassed=allpassed .and. test("a*r", "a*", .true.)
!!      allpassed=allpassed .and. test("a*ar", "a*aar", .false.)
!!
!!      ! More double wildcard scenarios.
!!      allpassed=allpassed .and. test("XYXYXYZYXYz", "XY*Z*XYz", .true.)
!!      allpassed=allpassed .and. test("missisSIPpi", "*SIP*", .true.)
!!      allpassed=allpassed .and. test("mississipPI", "*issip*PI", .true.)
!!      allpassed=allpassed .and. test("xyxyxyxyz", "xy*xyz", .true.)
!!      allpassed=allpassed .and. test("miSsissippi", "mi*sip*", .true.)
!!      allpassed=allpassed .and. test("miSsissippi", "mi*Sip*", .false.)
!!      allpassed=allpassed .and. test("abAbac", "*Abac*", .true.)
!!      allpassed=allpassed .and. test("aAazz", "a*zz*", .true.)
!!      allpassed=allpassed .and. test("A12b12", "*12*23", .false.)
!!      allpassed=allpassed .and. test("a12B12", "*12*12*", .true.)
!!      allpassed=allpassed .and. test("oWn", "*oWn*", .true.)
!!
!!      ! Completely tame (no wildcards) cases.
!!      allpassed=allpassed .and. test("bLah", "bLah", .true.)
!!
!!      ! Simple mixed wildcard tests suggested by IBMer Marlin Deckert.
!!      allpassed=allpassed .and. test("a", "*?", .true.)
!!
!!      ! More mixed wildcard tests including coverage for false positives.
!!      allpassed=allpassed .and. test("a", "??", .false.)
!!      allpassed=allpassed .and. test("ab", "?*?", .true.)
!!      allpassed=allpassed .and. test("ab", "*?*?*", .true.)
!!      allpassed=allpassed .and. test("abc", "?**?*?", .true.)
!!      allpassed=allpassed .and. test("abc", "?**?*&?", .false.)
!!      allpassed=allpassed .and. test("abcd", "?b*??", .true.)
!!      allpassed=allpassed .and. test("abcd", "?a*??", .false.)
!!      allpassed=allpassed .and. test("abcd", "?**?c?", .true.)
!!      allpassed=allpassed .and. test("abcd", "?**?d?", .false.)
!!      allpassed=allpassed .and. test("abcde", "?*b*?*d*?", .true.)
!!
!!      ! Single-character-match cases.
!!      allpassed=allpassed .and. test("bLah", "bL?h", .true.)
!!      allpassed=allpassed .and. test("bLaaa", "bLa?", .false.)
!!      allpassed=allpassed .and. test("bLah", "bLa?", .true.)
!!      allpassed=allpassed .and. test("bLaH", "?Lah", .false.)
!!      allpassed=allpassed .and. test("bLaH", "?LaH", .true.)
!!
!!      ! Many-wildcard scenarios.
!!      allpassed=allpassed .and. test(&
!!      &"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!!      &aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab",&
!!      &"a*a*a*a*a*a*aa*aaa*a*a*b",&
!!      &.true.)
!!      allpassed=allpassed .and. test(&
!!      &"abababababababababababababababababababaacacacacacacac&
!!      &adaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!      &"*a*b*ba*ca*a*aa*aaa*fa*ga*b*",&
!!      &.true.)
!!      allpassed=allpassed .and. test(&
!!      &"abababababababababababababababababababaacacacacacaca&
!!      &cadaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!      &"*a*b*ba*ca*a*x*aaa*fa*ga*b*",&
!!      &.false.)
!!      allpassed=allpassed .and. test(&
!!      &"abababababababababababababababababababaacacacacacacacad&
!!      &aeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!      &"*a*b*ba*ca*aaaa*fa*ga*gggg*b*",&
!!      &.false.)
!!      allpassed=allpassed .and. test(&
!!      &"abababababababababababababababababababaacacacacacacacad&
!!      &aeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!      &"*a*b*ba*ca*aaaa*fa*ga*ggg*b*",&
!!      &.true.)
!!      allpassed=allpassed .and. test("aaabbaabbaab", "*aabbaa*a*", .true.)
!!      allpassed=allpassed .and. &
!!      test("a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*",&
!!      &"a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .true.)
!!      allpassed=allpassed .and. test("aaaaaaaaaaaaaaaaa",&
!!      &"*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .true.)
!!      allpassed=allpassed .and. test("aaaaaaaaaaaaaaaa",&
!!      &"*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .false.)
!!      allpassed=allpassed .and. test(&
!!      &"abc*abcd*abcde*abcdef*abcdefg*abcdefgh*abcdefghi*abcdefghij&
!!      &*abcdefghijk*abcdefghijkl*abcdefghijklm*abcdefghijklmn",&
!!      & "abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc&
!!      &*abc*abc*abc*",&
!!      &.false.)
!!      allpassed=allpassed .and. test(&
!!      &"abc*abcd*abcde*abcdef*abcdefg*abcdefgh*abcdefghi*abcdefghij&
!!      &*abcdefghijk*abcdefghijkl*abcdefghijklm*abcdefghijklmn",&
!!      &"abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*",&
!!      &.true.)
!!      allpassed=allpassed .and. test("abc*abcd*abcd*abc*abcd",&
!!      &"abc*abc*abc*abc*abc", .false.)
!!      allpassed=allpassed .and. test( "abc*abcd*abcd*abc*abcd*abcd&
!!      &*abc*abcd*abc*abc*abcd", &
!!      &"abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abcd",&
!!      &.true.)
!!      allpassed=allpassed .and. test("abc",&
!!      &"********a********b********c********", .true.)
!!      allpassed=allpassed .and.&
!!      &test("********a********b********c********", "abc", .false.)
!!      allpassed=allpassed .and. &
!!      &test("abc", "********a********b********b********", .false.)
!!      allpassed=allpassed .and. test("*abc*", "***a*b*c***", .true.)
!!
!!      ! A case-insensitive algorithm test.
!!      ! allpassed=allpassed .and. test("mississippi", "*issip*PI", .true.)
!!     enddo
!!
!!     if (allpassed)then
!!        write(*,'(a)')"Passed",nReps
!!     else
!!        write(*,'(a)')"Failed"
!!     endif
!!    contains
!!    ! This is a test program for wildcard matching routines.
!!    ! It can be used either to test a single routine for correctness,
!!    ! or to compare the timings of two (or more) different wildcard
!!    ! matching routines.
!!    !
!!    function test(tame, wild, bExpectedResult) result(bpassed)
!!    use M_strings, only : glob
!!       character(len=*) :: tame
!!       character(len=*) :: wild
!!       logical          :: bExpectedResult
!!       logical          :: bResult
!!       logical          :: bPassed
!!       bResult = .true.    ! We'll do "&=" cumulative checking.
!!       bPassed = .false.   ! Assume the worst.
!!       write(*,*)repeat('=',79)
!!       bResult = glob(tame, wild) ! Call a wildcard matching routine.
!!
!!       ! To assist correctness checking, output the two strings in any
!!       ! failing scenarios.
!!       if (bExpectedResult .eqv. bResult) then
!!          bPassed = .true.
!!          if(nReps == 1) write(*,*)"Passed match on ",tame," vs. ", wild
!!       else
!!          if(nReps == 1) write(*,*)"Failed match on ",tame," vs. ", wild
!!       endif
!!
!!    end function test
!!    end program demo_glob
!!
!!   Expected output
!!
!!##AUTHOR
!!   John S. Urban
!!
!!##REFERENCE
!!   The article "Matching Wildcards: An Empirical Way to Tame an Algorithm"
!!   in Dr Dobb's Journal, By Kirk J. Krauss, October 07, 2014
!!
!!##LICENSE
!!   Public Domain
function glob(tame,wild)

! ident_6="@(#)M_strings::glob(3f): function compares text strings, one of which can have wildcards ('*' or '?')."

logical                    :: glob
character(len=*)           :: tame       ! A string without wildcards
character(len=*)           :: wild       ! A (potentially) corresponding string with wildcards
character(len=len(tame)+1) :: tametext
character(len=len(wild)+1) :: wildtext
character(len=1),parameter :: NULL=char(0)
integer                    :: wlen
integer                    :: ti, wi
integer                    :: i
character(len=:),allocatable :: tbookmark, wbookmark
! These two values are set when we observe a wildcard character. They
! represent the locations, in the two strings, from which we start once we've observed it.
   tametext=tame//NULL
   wildtext=wild//NULL
   tbookmark = NULL
   wbookmark = NULL
   wlen=len(wild)
   wi=1
   ti=1
   do                                            ! Walk the text strings one character at a time.
      if(wildtext(wi:wi) == '*')then             ! How do you match a unique text string?
         do i=wi,wlen                            ! Easy: unique up on it!
            if(wildtext(wi:wi).eq.'*')then
               wi=wi+1
            else
               exit
            endif
         enddo
         if(wildtext(wi:wi).eq.NULL) then        ! "x" matches "*"
            glob=.true.
            return
         endif
         if(wildtext(wi:wi) .ne. '?') then
            ! Fast-forward to next possible match.
            do while (tametext(ti:ti) .ne. wildtext(wi:wi))
               ti=ti+1
               if (tametext(ti:ti).eq.NULL)then
                  glob=.false.
                  return                         ! "x" doesn't match "*y*"
               endif
            enddo
         endif
         wbookmark = wildtext(wi:)
         tbookmark = tametext(ti:)
      elseif(tametext(ti:ti) .ne. wildtext(wi:wi) .and. wildtext(wi:wi) .ne. '?') then
         ! Got a non-match. If we've set our bookmarks, back up to one or both of them and retry.
         if(wbookmark.ne.NULL) then
            if(wildtext(wi:).ne. wbookmark) then
               wildtext = wbookmark;
               wlen=len_trim(wbookmark)
               wi=1
               ! Don't go this far back again.
               if (tametext(ti:ti) .ne. wildtext(wi:wi)) then
                  tbookmark=tbookmark(2:)
                  tametext = tbookmark
                  ti=1
                  cycle                          ! "xy" matches "*y"
               else
                  wi=wi+1
               endif
            endif
            if (tametext(ti:ti).ne.NULL) then
               ti=ti+1
               cycle                             ! "mississippi" matches "*sip*"
            endif
         endif
         glob=.false.
         return                                  ! "xy" doesn't match "x"
      endif
      ti=ti+1
      wi=wi+1
      if (tametext(ti:ti).eq.NULL) then          ! How do you match a tame text string?
         if(wildtext(wi:wi).ne.NULL)then
            do while (wildtext(wi:wi) == '*')    ! The tame way: unique up on it!
               wi=wi+1                           ! "x" matches "x*"
               if(wildtext(wi:wi).eq.NULL)exit
            enddo
         endif
         if (wildtext(wi:wi).eq.NULL)then
            glob=.true.
            return                               ! "x" matches "x"
         endif
         glob=.false.
         return                                  ! "x" doesn't match "xy"
      endif
   enddo
end function glob
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    ends_with(3f) - [M_strings:MATCH] test if string ends with specified suffix(es)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function ends_with(source_string,suffix)
!!
!!     or
!!
!!    function ends_with(source_string,[suffixs])
!!
!!     character(len=*),intent(in)          :: source_string
!!     character(len=*),intent(in)          :: suffix
!!     logical                              :: ends_with
!!
!!##DESCRIPTION
!!
!!##OPTIONS
!!     SOURCE_STRING  string to tokenize
!!     SUFFIX         list of separator characters. May be scalar or an array.
!!
!!##RETURNS
!!     ENDS_WITH      returns .TRUE. if one of the suffix match the end
!!                    of SOURCE_STRING.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_ends_with
!!    use M_strings, only : ends_with
!!    implicit none
!!       write(*,*)ends_with('prog.a',['.o','.i','.s'])
!!       write(*,*)ends_with('prog.f90',['.F90','.f90'])
!!       write(*,*)ends_with('prog.pdf','.pdf')
!!       write(*,*)ends_with('prog.doc','.txt')
!!    end program demo_ends_with
!!
!!   Results:
!!
!!     F
!!     T
!!     T
!!     F
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
pure function ends_with_str(string, ending) result(matched)
character(*), intent(in) :: string, ending
integer                  :: n1, n2
logical                  :: matched
   n1 = len(string) - len(ending) + 1
   n2 = len(string)
   if (n1 < 1) then
       matched = .false.
   else
       matched = (string(n1:n2) == ending)
   endif
end function ends_with_str
!-----------------------------------------------------------------------------------------------------------------------------------
pure function ends_with_any(string, endings) result(matched)
character(*), intent(in) :: string
character(*), intent(in) :: endings(:)
logical                  :: matched
integer                  :: i
   matched = .true.
   FINDIT: block
   do i=1, size(endings)
       if(ends_with_str(string,trim(endings(i)))) exit FINDIT
   enddo
   matched = .false.
   endblock FINDIT
end function ends_with_any
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    sep(3f) - [M_strings:TOKENS] function to parse string into an array using
!!    specified delimiters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function sep(input_line,delimiters,nulls)
!!
!!     character(len=*),intent(in)              :: input_line
!!     character(len=*),optional,intent(in)     :: delimiters
!!     character(len=*),optional,intent(in)     :: nulls
!!     character(len=:),allocatable             :: sep(:)
!!
!!##DESCRIPTION
!!     sep(3f) parses a string using specified delimiter characters and
!!     store tokens into an allocatable array
!!
!!##OPTIONS
!!    INPUT_LINE  Input string to tokenize
!!
!!    DELIMITERS  List of delimiter characters.
!!                The default delimiters are the "whitespace" characters
!!                (space, tab,new line, vertical tab, formfeed, carriage
!!                return, and null). You may specify an alternate set of
!!                delimiter characters.
!!
!!                Multi-character delimiters are not supported (Each
!!                character in the DELIMITERS list is considered to be
!!                a delimiter).
!!
!!                Quoting of delimiter characters is not supported.
!!
!!    NULLS=IGNORE|RETURN|IGNOREEND  Treatment of null fields.
!!                By default adjacent delimiters in the input string
!!                do not create an empty string in the output array. if
!!                NULLS='return' adjacent delimiters create an empty element
!!                in the output ARRAY. If NULLS='ignoreend' then only
!!                trailing delimiters at the right of the string are ignored.
!!##RETURNS
!!    SEP       Output array of tokens
!!
!!##EXAMPLES
!!
!!  Sample program:
!!
!!    program demo_sep
!!    use M_strings, only: sep
!!    character(len=*),parameter :: fo='(/,a,*(/,"[",g0,"]":,","))'
!!    character(len=*),parameter :: line=&
!!    '  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    '
!!       write(*,'(a)') 'INPUT LINE:['//LINE//']'
!!       write(*,fo) 'typical call:',sep(line)
!!       write(*,fo) 'delimiters ":|":',sep(line,':|')
!!       write(*,fo) 'count null fields ":|":',sep(line,':|','return')
!!    end program demo_sep
!!
!!  Output
!!
!!    INPUT LINE:[  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    ]
!!
!!    typical call:
!!    [cc        ],
!!    [B         ],
!!    [a         ],
!!    [333|333   ],
!!    [1:|:2     ],
!!    [qrstuvwxyz],
!!    [ghijklmnop],
!!    [aBcdef    ]
!!
!!    delimiters ":|":
!!    [333 a B cc                         ],
!!    [2     333                          ],
!!    [  aBcdef   ghijklmnop qrstuvwxyz  1]
!!
!!    count null fields ":|":
!!    [333 a B cc                         ],
!!    [2     333                          ],
!!    [                                   ],
!!    [                                   ],
!!    [  aBcdef   ghijklmnop qrstuvwxyz  1]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function sep(input_line,delimiters,nulls)
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_7="@(#)M_strings::sep(3f): parse string on delimiter characters and store tokens into an allocatable array"

!  John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
intrinsic index, min, present, len
!-----------------------------------------------------------------------------------------------------------------------------------
!  given a line of structure " par1 par2 par3 ... parn " store each par(n) into a separate variable in array.
!    o by default adjacent delimiters in the input string do not create an empty string in the output array
!    o no quoting of delimiters is supported
character(len=*),intent(in)              :: input_line  ! input string to tokenize
character(len=*),optional,intent(in)     :: delimiters  ! list of delimiter characters
character(len=*),optional,intent(in)     :: nulls       ! return strings composed of delimiters or not ignore|return|ignoreend
character(len=:),allocatable             :: sep(:)      ! output array of tokens
   call split(input_line,sep,delimiters,'right',nulls)
!-----------------------------------------------------------------------------------------------------------------------------------
end function sep
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    split(3f) - [M_strings:TOKENS] parse string into an array using
!!    specified delimiters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine split(input_line,array,delimiters,order,nulls)
!!
!!     character(len=*),intent(in)              :: input_line
!!     character(len=:),allocatable,intent(out) :: array(:)
!!     character(len=*),optional,intent(in)     :: delimiters
!!     character(len=*),optional,intent(in)     :: order
!!     character(len=*),optional,intent(in)     :: nulls
!!
!!##DESCRIPTION
!!     SPLIT(3f) parses a string using specified delimiter characters and
!!     store tokens into an allocatable array
!!
!!##OPTIONS
!!    INPUT_LINE  Input string to tokenize
!!
!!    ARRAY       Output array of tokens
!!
!!    DELIMITERS  List of delimiter characters.
!!                The default delimiters are the "whitespace" characters
!!                (space, tab,new line, vertical tab, formfeed, carriage
!!                return, and null). You may specify an alternate set of
!!                delimiter characters.
!!
!!                Multi-character delimiters are not supported (Each
!!                character in the DELIMITERS list is considered to be
!!                a delimiter).
!!
!!                Quoting of delimiter characters is not supported.
!!
!!    ORDER SEQUENTIAL|REVERSE|RIGHT  Order of output array.
!!                By default ARRAY contains the tokens having parsed
!!                the INPUT_LINE from left to right. If ORDER='RIGHT'
!!                or ORDER='REVERSE' the parsing goes from right to left.
!!                (This can be accomplished with array syntax in modern
!!                Fortran, but was more useful pre-fortran90).
!!
!!    NULLS=IGNORE|RETURN|IGNOREEND  Treatment of null fields.
!!                By default adjacent delimiters in the input string
!!                do not create an empty string in the output array. if
!!                NULLS='return' adjacent delimiters create an empty element
!!                in the output ARRAY. If NULLS='ignoreend' then only
!!                trailing delimiters at the right of the string are ignored.
!!
!!##EXAMPLES
!!
!!  Sample program:
!!
!!    program demo_split
!!    use M_strings, only: split
!!    implicit none
!!    integer :: i
!!    character(len=*),parameter     :: line=&
!!    '  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    '
!!    character(len=:),allocatable :: array(:) ! output array of tokens
!!       write(*,*)'INPUT LINE:['//LINE//']'
!!       write(*,'(70("="))')
!!       write(*,*)'typical call:'
!!       CALL split(line,array)
!!       write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!       write(*,*)'SIZE:',SIZE(array)
!!       write(*,'(70("-"))')
!!       write(*,*)'custom list of delimiters (colon and vertical line):'
!!       CALL split(line,array,delimiters=':|',&
!!       & order='sequential',nulls='ignore')
!!       write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!       write(*,*)'SIZE:',SIZE(array)
!!       write(*,'(70("-"))')
!!       write(*,*) 'custom list of delimiters, &
!!       &reverse array order and count null fields:'
!!       CALL split(line,array,delimiters=':|',&
!!       &order='reverse',nulls='return')
!!       write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!       write(*,*)'SIZE:',SIZE(array)
!!       write(*,'(70("-"))')
!!       write(*,*)'INPUT LINE:['//LINE//']'
!!       write(*,*) 'default delimiters and reverse array order &
!!       &and return null fields:'
!!       CALL split(line,array,delimiters='',order='reverse',nulls='return')
!!       write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!       write(*,*)'SIZE:',SIZE(array)
!!    end program demo_split
!!
!!  Output
!!
!!   >INPUT LINE:[  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|
!!   333 a B cc    ]
!!   >=================================================================
!!   > typical call:
!!   >1 ==> aBcdef
!!   >2 ==> ghijklmnop
!!   >3 ==> qrstuvwxyz
!!   >4 ==> 1:|:2
!!   >5 ==> 333|333
!!   >6 ==> a
!!   >7 ==> B
!!   >8 ==> cc
!!   > SIZE:           8
!!   >----------------------------------------------------------------
!!   > custom list of delimiters (colon and vertical line):
!!   >1 ==>   aBcdef   ghijklmnop qrstuvwxyz  1
!!   >2 ==> 2     333
!!   >3 ==> 333 a B cc
!!   > SIZE:           3
!!   >----------------------------------------------------------------
!!   > custom list of delimiters, reverse array order and
!!   return null fields:
!!   >1 ==> 333 a B cc
!!   >2 ==> 2     333
!!   >3 ==>
!!   >4 ==>
!!   >5 ==>   aBcdef   ghijklmnop qrstuvwxyz  1
!!   > SIZE:           5
!!   >----------------------------------------------------------------
!!   > INPUT LINE:[  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|
!!   333 a B cc    ]
!!   > default delimiters and reverse array order and count null fields:
!!   >1 ==>
!!   >2 ==>
!!   >3 ==>
!!   >4 ==> cc
!!   >5 ==> B
!!   >6 ==> a
!!   >7 ==> 333|333
!!   >8 ==>
!!   >9 ==>
!!   >10 ==>
!!   >11 ==>
!!   >12 ==> 1:|:2
!!   >13 ==>
!!   >14 ==> qrstuvwxyz
!!   >15 ==> ghijklmnop
!!   >16 ==>
!!   >17 ==>
!!   >18 ==> aBcdef
!!   >19 ==>
!!   >20 ==>
!!   > SIZE:          20
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine split(input_line,array,delimiters,order,nulls)
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_8="@(#)M_strings::split(3f): parse string on delimiter characters and store tokens into an allocatable array"

!  John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
intrinsic index, min, present, len
!-----------------------------------------------------------------------------------------------------------------------------------
!  given a line of structure " par1 par2 par3 ... parn " store each par(n) into a separate variable in array.
!    o by default adjacent delimiters in the input string do not create an empty string in the output array
!    o no quoting of delimiters is supported
character(len=*),intent(in)              :: input_line  ! input string to tokenize
character(len=*),optional,intent(in)     :: delimiters  ! list of delimiter characters
character(len=*),optional,intent(in)     :: order       ! order of output array sequential|[reverse|right]
character(len=*),optional,intent(in)     :: nulls       ! return strings composed of delimiters or not ignore|return|ignoreend
character(len=:),allocatable,intent(out) :: array(:)    ! output array of tokens
!-----------------------------------------------------------------------------------------------------------------------------------
integer                       :: n                      ! max number of strings INPUT_LINE could split into if all delimiter
integer,allocatable           :: ibegin(:)              ! positions in input string where tokens start
integer,allocatable           :: iterm(:)               ! positions in input string where tokens end
character(len=:),allocatable  :: dlim                   ! string containing delimiter characters
character(len=:),allocatable  :: ordr                   ! string containing order keyword
character(len=:),allocatable  :: nlls                   ! string containing nulls keyword
integer                       :: ii,iiii                ! loop parameters used to control print order
integer                       :: icount                 ! number of tokens found
integer                       :: ilen                   ! length of input string with trailing spaces trimmed
integer                       :: i10,i20,i30            ! loop counters
integer                       :: icol                   ! pointer into input string as it is being parsed
integer                       :: idlim                  ! number of delimiter characters
integer                       :: ifound                 ! where next delimiter character is found in remaining input string data
integer                       :: inotnull               ! count strings not composed of delimiters
integer                       :: ireturn                ! number of tokens returned
integer                       :: imax                   ! length of longest token
!-----------------------------------------------------------------------------------------------------------------------------------
   ! decide on value for optional DELIMITERS parameter
   if (present(delimiters)) then                                     ! optional delimiter list was present
      if(delimiters.ne.'')then                                       ! if DELIMITERS was specified and not null use it
         dlim=delimiters
      else                                                           ! DELIMITERS was specified on call as empty string
         dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0) ! use default delimiter when not specified
      endif
   else                                                              ! no delimiter value was specified
      dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0)    ! use default delimiter when not specified
   endif
   idlim=len(dlim)                                                   ! dlim a lot of blanks on some machines if dlim is a big string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(order))then; ordr=lower(adjustl(order)); else; ordr='sequential'; endif ! decide on value for optional ORDER parameter
   if(present(nulls))then; nlls=lower(adjustl(nulls)); else; nlls='ignore'    ; endif ! optional parameter
!-----------------------------------------------------------------------------------------------------------------------------------
   n=len(input_line)+1                        ! max number of strings INPUT_LINE could split into if all delimiter
   if(allocated(ibegin))deallocate(ibegin)    !*! intel compiler says allocated already ?
   if(allocated(iterm))deallocate(iterm)      !*! intel compiler says allocated already ?
   allocate(ibegin(n))                        ! allocate enough space to hold starting location of tokens if string all tokens
   allocate(iterm(n))                         ! allocate enough space to hold ending location of tokens if string all tokens
   ibegin(:)=1
   iterm(:)=1
!-----------------------------------------------------------------------------------------------------------------------------------
   ilen=len(input_line)                                           ! ILEN is the column position of the last non-blank character
   icount=0                                                       ! how many tokens found
   inotnull=0                                                     ! how many tokens found not composed of delimiters
   imax=0                                                         ! length of longest token found
!-----------------------------------------------------------------------------------------------------------------------------------
   if(ilen.gt.0)then                                              ! there is at least one non-delimiter in INPUT_LINE if get here
      icol=1                                                      ! initialize pointer into input line
      INFINITE: do i30=1,ilen,1                                   ! store into each array element
         ibegin(i30)=icol                                         ! assume start new token on the character
         if(index(dlim(1:idlim),input_line(icol:icol)).eq.0)then  ! if current character is not a delimiter
            iterm(i30)=ilen                                       ! initially assume no more tokens
            do i10=1,idlim                                        ! search for next delimiter
               ifound=index(input_line(ibegin(i30):ilen),dlim(i10:i10))
               IF(ifound.gt.0)then
                  iterm(i30)=min(iterm(i30),ifound+ibegin(i30)-2)
               endif
            enddo
            icol=iterm(i30)+2                                     ! next place to look as found end of this token
            inotnull=inotnull+1                                   ! increment count of number of tokens not composed of delimiters
         else                                                     ! character is a delimiter for a null string
            iterm(i30)=icol-1                                     ! record assumed end of string. Will be less than beginning
            icol=icol+1                                           ! advance pointer into input string
         endif
         imax=max(imax,iterm(i30)-ibegin(i30)+1)
         icount=i30                                               ! increment count of number of tokens found
         if(icol.gt.ilen)then                                     ! no text left
            exit INFINITE
         endif
      enddo INFINITE
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(nlls)))
   case ('ignore','','ignoreend')
      ireturn=inotnull
   case default
      ireturn=icount
   end select
   allocate(character(len=imax) :: array(ireturn))                ! allocate the array to return
   !allocate(array(ireturn))                                       ! allocate the array to return
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(ordr)))                              ! decide which order to store tokens
   case ('reverse','right') ; ii=ireturn ; iiii=-1                ! last to first
   case default             ; ii=1       ; iiii=1                 ! first to last
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   do i20=1,icount                                                ! fill the array with the tokens that were found
      if(iterm(i20).lt.ibegin(i20))then
         select case (trim(adjustl(nlls)))
         case ('ignore','','ignoreend')
         case default
            array(ii)=' '
            ii=ii+iiii
         end select
      else
         array(ii)=input_line(ibegin(i20):iterm(i20))
         ii=ii+iiii
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   end subroutine split
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    chomp(3f) - [M_strings:TOKENS] Tokenize a string, consuming it one
!!    token per call
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function chomp(source_string,token[,delimiters])
!!
!!     character(len=*)                     :: source_string
!!     character(len=:),intent(out)         :: token
!!     character(len=:),intent(in),optional :: delimiters
!!     integer                              :: chomp
!!
!!##DESCRIPTION
!!    The CHOMP(3f) function is used to isolate sequential tokens in a
!!    string, SOURCE_STRING. These tokens are delimited in the string by at
!!    least one of the characters in DELIMITERS. This routine consumes the
!!    source_string one token per call. It returns -1 when complete. The
!!    default delimiter list is "space,tab,carriage return,newline".
!!
!!##OPTIONS
!!     SOURCE_STRING  string to tokenize
!!     DELIMITERS     list of separator characters
!!
!!##RETURNS
!!     TOKEN          returned token
!!     CHOMP          status flag. 0 = success, -1 = no tokens remain
!!
!!##EXAMPLES
!!
!!  Sample program:
!!
!!    program demo_chomp
!!
!!    use M_strings, only : chomp
!!    implicit none
!!    character(len=100)            :: inline
!!    character(len=:),allocatable  :: token
!!    character(len=*),parameter    :: delimiters=' ;,'
!!    integer                       :: ios
!!    integer                       :: icount
!!    integer                       :: itoken
!!       icount=0
!!       do        ! read lines from stdin until end-of-file or error
!!          read (unit=*,fmt="(a)",iostat=ios) inline
!!          if(ios.ne.0)stop
!!          icount=icount+1
!!          itoken=0
!!          write(*,*)'INLINE ',trim(inline)
!!          do while ( chomp(inline,token,delimiters).ge. 0)
!!             itoken=itoken+1
!!             print *, itoken,'TOKEN=['//trim(token)//']'
!!          enddo
!!       enddo
!!
!!    end program demo_chomp
!!
!!   sample input file
!!
!!     this is a test of chomp; A:B :;,C;;
!!
!!   sample output file
!!
!!     > INLINE     this is a test of chomp; A:B :;,C;;
!!     >           1 TOKEN=[this]
!!     >           2 TOKEN=[is]
!!     >           3 TOKEN=[a]
!!     >           4 TOKEN=[test]
!!     >           5 TOKEN=[of]
!!     >           6 TOKEN=[chomp]
!!     >           7 TOKEN=[A:B]
!!     >           8 TOKEN=[:]
!!     >           9 TOKEN=[C]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
FUNCTION chomp(source_string,token,delimiters)

! ident_9="@(#)M_strings::chomp(3f): Tokenize a string : JSU- 20151030"

character(len=*)                         :: source_string    ! string to tokenize
character(len=:),allocatable,intent(out) :: token            ! returned token
character(len=*),intent(in),optional     :: delimiters       ! list of separator characters
integer                                  :: chomp            ! returns copy of shifted source_string
character(len=:),allocatable             :: delimiters_local
integer                                  :: token_start      ! beginning of token found if function result is .true.
integer                                  :: token_end        ! end of token found if function result is .true.
integer                                  :: isource_len
!-----------------------------------------------------------------------------------------------------------------------------------
!  calculate where token_start should start for this pass
   if(present(delimiters))then
      delimiters_local=delimiters
   else                                          ! increment start to previous end + 1
      delimiters_local=char(32)//char(09)//char(10)//char(13) ! space,horizontal tab, newline, carriage return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   isource_len=len(source_string)                ! length of input string
!-----------------------------------------------------------------------------------------------------------------------------------
   ! find beginning of token
   token_start=1
   do while (token_start .le. isource_len)       ! step thru each character to find next delimiter, if any
      if(index(delimiters_local,source_string(token_start:token_start)) .ne. 0) then
         token_start = token_start + 1
      else
         exit
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   token_end=token_start
   do while (token_end .le. isource_len-1)                         ! step thru each character to find next delimiter, if any
      if(index(delimiters_local,source_string(token_end+1:token_end+1)) .ne. 0) then  ! found a delimiter in next character
         exit
      endif
      token_end = token_end + 1
   enddo
   !write(*,*)'TOKEN_START ',token_start
   !write(*,*)'TOKEN_END   ',token_end
   chomp=isource_len-token_end
   if(chomp.ge.0)then
      token=source_string(token_start:token_end)
      source_string=source_string(token_end+1:)
   else
      token=''
      source_string=''
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function chomp
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      delim(3f) - [M_strings:TOKENS] parse a string and store tokens into
!!      an array
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine delim(line,array,n,icount,ibegin,iterm,ilen,dlim)
!!
!!     character(len=*),intent(in)  :: line
!!     integer,integer(in)          :: n
!!     integer,intent(out)          :: icount
!!     character(len=*)             :: array(n)
!!     integer,intent(out)          :: ibegin(n)
!!     integer,intent(out)          :: iterm(n)
!!     integer,intent(out)          :: ilen
!!     character(len=*)             :: dlim
!!
!!##DESCRIPTION
!!      Given a LINE of structure " par1 par2 par3 ... parn "
!!      store each par(n) into a separate variable in ARRAY (UNLESS
!!      ARRAY(1).eq.'#N#')
!!
!!      Also set ICOUNT to number of elements of array initialized, and
!!      return beginning and ending positions for each element in IBEGIN(N)
!!      and ITERM(N).
!!
!!      Return position of last non-blank character (even if more
!!      than N elements were found) in ILEN
!!
!!      No quoting or escaping of delimiter is allowed, so the delimiter
!!      character can not be placed in a token.
!!
!!      No checking for more than N parameters; If any more they are ignored.
!!
!!##OPTIONS
!!    LINE      input string to parse into tokens
!!    ARRAY(N)  array that receives tokens
!!    N         size of arrays ARRAY, IBEGIN, ITERM
!!    ICOUNT    number of tokens found
!!    IBEGIN(N) starting columns of tokens found
!!    ITERM(N)  ending columns of tokens found
!!    ILEN      position of last non-blank character in input string LINE
!!    DLIM      delimiter characters
!!
!!##EXAMPLES
!!
!!  Sample program:
!!
!!     program demo_delim
!!
!!     use M_strings, only: delim
!!     implicit none
!!     character(len=80) :: line
!!     character(len=80) :: dlm
!!     integer,parameter :: n=10
!!     character(len=20) :: array(n)=' '
!!     integer           :: ibegin(n),iterm(n)
!!     integer           :: i20, icount, ilen, i10
!!     line=' first  second 10.3 words_of_stuff  '
!!     do i20=1,4
!!        ! change delimiter list and what is calculated or parsed
!!        if(i20.eq.1)dlm=' '
!!        if(i20.eq.2)dlm='o'
!!        if(i20.eq.3)dlm=' aeiou'    ! NOTE SPACE IS FIRST
!!        if(i20.eq.3)ARRAY(1)='#N#'  ! QUIT RETURNING STRING ARRAY
!!        if(i20.eq.4)line='AAAaBBBBBBbIIIIIi  J K L'
!!
!!        ! write out a break line composed of =========== ..
!!        write(*,'(57("="))')
!!        ! show line being parsed
!!        write(*,'(a)')'PARSING=['//trim(line)//'] on '//trim(dlm)
!!        ! call parsing procedure
!!        call delim(line,array,n,icount,ibegin,iterm,ilen,dlm)
!!        write(*,*)'number of tokens found=',icount
!!        write(*,*)'last character in column ',ilen
!!        if(icount.gt.0)then
!!           if(ilen.ne.iterm(icount))then
!!              write(*,*)'ignored from column ',iterm(icount)+1,' to ',ilen
!!           endif
!!           do i10=1,icount
!!              ! check flag to see if ARRAY() was set
!!              if(array(1).ne.'#N#')then
!!                 ! from returned array
!!                 write(*,'(a,a,a)',advance='no')&
!!                 &'[',array(i10)(:iterm(i10)-ibegin(i10)+1),']'
!!              endif
!!           enddo
!!           ! using start and end positions in IBEGIN() and ITERM()
!!           write(*,*)
!!           do i10=1,icount
!!              ! from positions in original line
!!              write(*,'(a,a,a)',advance='no')&
!!              &'[',line(ibegin(i10):iterm(i10)),']'
!!           enddo
!!           write(*,*)
!!        endif
!!     enddo
!!     end program demo_delim
!!
!!  Expected output
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine delim(line,array,n,icount,ibegin,iterm,ilen,dlim)

! ident_10="@(#)M_strings::delim(3f): parse a string and store tokens into an array"

!
!     given a line of structure " par1 par2 par3 ... parn "
!     store each par(n) into a separate variable in array.
!
!     IF ARRAY(1) == '#N#' do not store into string array  (KLUDGE))
!
!     also count number of elements of array initialized, and
!     return beginning and ending positions for each element.
!     also return position of last non-blank character (even if more
!     than n elements were found).
!
!     no quoting of delimiter is allowed
!     no checking for more than n parameters, if any more they are ignored
!
character(len=*),intent(in)    :: line
integer,intent(in)             :: n
character(len=*)               :: array(n)
integer,intent(out)            :: icount
integer,intent(out)            :: ibegin(n)
integer,intent(out)            :: iterm(n)
integer,intent(out)            :: ilen
character(len=*),intent(in)    :: dlim
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=len(line)):: line_local
logical             :: lstore
integer             :: i10
integer             :: iarray
integer             :: icol
integer             :: idlim
integer             :: iend
integer             :: ifound
integer             :: istart
!-----------------------------------------------------------------------------------------------------------------------------------
      icount=0
      ilen=len_trim(line)
      line_local=line

      idlim=len(dlim)
      if(idlim > 5)then
         idlim=len_trim(dlim)      ! dlim a lot of blanks on some machines if dlim is a big string
         if(idlim == 0)then
            idlim=1     ! blank string
         endif
      endif

      if(ilen == 0)then                                        ! command was totally blank
         return
      endif
!
!     there is at least one non-blank character in the command
!     ilen is the column position of the last non-blank character
!     find next non-delimiter
      icol=1

      if(array(1) == '#N#')then                                ! special flag to not store into character array
         lstore=.false.
      else
         lstore=.true.
      endif

      do iarray=1,n,1                                          ! store into each array element until done or too many words
         NOINCREMENT: do
            if(index(dlim(1:idlim),line_local(icol:icol)) == 0)then  ! if current character is not a delimiter
               istart=icol                                     ! start new token on the non-delimiter character
               ibegin(iarray)=icol
               iend=ilen-istart+1+1                            ! assume no delimiters so put past end of line
               do i10=1,idlim
                  ifound=index(line_local(istart:ilen),dlim(i10:i10))
                  if(ifound > 0)then
                     iend=min(iend,ifound)
                  endif
               enddo
               if(iend <= 0)then                               ! no remaining delimiters
                 iterm(iarray)=ilen
                 if(lstore)then
                    array(iarray)=line_local(istart:ilen)
                 endif
                 icount=iarray
                 return
               else
                 iend=iend+istart-2
                 iterm(iarray)=iend
                 if(lstore)then
                    array(iarray)=line_local(istart:iend)
                 endif
               endif
               icol=iend+2
               exit NOINCREMENT
            endif
            icol=icol+1
         enddo NOINCREMENT
!        last character in line was a delimiter, so no text left
!        (should not happen where blank=delimiter)
         if(icol > ilen)then
           icount=iarray
           if( (iterm(icount)-ibegin(icount)) < 0)then         ! last token was all delimiters
              icount=icount-1
           endif
           return
         endif
      enddo
      icount=n  ! more than n elements
end subroutine delim
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    replace(3f) - [M_strings:EDITING] function replaces one
!!    substring for another in string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!! syntax:
!!
!!      function replace(targetline,old,new,cmd,&
!!       & occurrence, &
!!       & repeat, &
!!       & ignorecase, &
!!       & ierr) result (newline)
!!      character(len=*)                       :: targetline
!!      character(len=*),intent(in),optional   :: old
!!      character(len=*),intent(in),optional   :: new
!!      character(len=*),intent(in),optional   :: cmd
!!      integer,intent(in),optional            :: occurrence
!!      integer,intent(in),optional            :: repeat
!!      logical,intent(in),optional            :: ignorecase
!!      integer,intent(out),optional           :: ierr
!!      character(len=:),allocatable           :: newline
!!
!!##DESCRIPTION
!!    Replace one substring for another in string.
!!    Either CMD or OLD and NEW must be specified.
!!
!!##OPTIONS
!!     targetline  input line to be changed
!!     old         old substring to replace
!!     new         new substring
!!     cmd         alternate way to specify old and new string, in
!!                 the form c/old/new/; where "/" can be any character
!!                 not in "old" or "new"
!!     occurrence  if present, start changing at the Nth occurrence of the
!!                 OLD string. If negative start replacing from the left
!!                 end of the string.
!!     repeat      number of replacements to perform. Defaults to a global
!!                 replacement.
!!     ignorecase  whether to ignore ASCII case or not. Defaults
!!                 to .false.
!!##RETURNS
!!     newline     allocatable string returned
!!     ierr        error code. iF ier = -1 bad directive, >= 0 then
!!                 count of changes made
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_replace
!!    use M_strings, only : replace
!!    implicit none
!!    character(len=:),allocatable :: line
!!
!!    write(*,*)replace('Xis is Xe string','X','th')
!!    write(*,*)replace('Xis is xe string','x','th',ignorecase=.true.)
!!    write(*,*)replace('Xis is xe string','X','th',ignorecase=.false.)
!!
!!    ! a null old substring means "at beginning of line"
!!    write(*,*) replace('my line of text','','BEFORE:')
!!
!!    ! a null new string deletes occurrences of the old substring
!!    write(*,*) replace('I wonder i ii iii','i','')
!!
!!    ! Examples of the use of RANGE
!!
!!    line=replace('aaaaaaaaa','a','A',occurrence=1,repeat=1)
!!    write(*,*)'replace first a with A ['//line//']'
!!
!!    line=replace('aaaaaaaaa','a','A',occurrence=3,repeat=3)
!!    write(*,*)'replace a with A for 3rd to 5th occurrence ['//line//']'
!!
!!    line=replace('ababababa','a','',occurrence=3,repeat=3)
!!    write(*,*)'replace a with null instances 3 to 5 ['//line//']'
!!
!!    line=replace( &
!!     & 'a b ab baaa aaaa aa aa a a a aa aaaaaa',&
!!     & 'aa','CCCC',occurrence=-1,repeat=1)
!!    write(*,*)'replace lastaa with CCCC ['//line//']'
!!
!!    write(*,*)replace('myf90stuff.f90.f90','f90','for',occurrence=-1,repeat=1)
!!    write(*,*)replace('myf90stuff.f90.f90','f90','for',occurrence=-2,repeat=2)
!!
!!    end program demo_replace
!!
!!   Results:
!!
!!     this is the string
!!     this is the string
!!     this is xe string
!!     BEFORE:my line of text
!!     I wonder
!!     replace first a with A [Aaaaaaaaa]
!!     replace a with A for 3rd to 5th occurrence [aaAAAaaaa]
!!     replace a with null instances 3 to 5 [ababbb]
!!     replace lastaa with CCCC [a b ab baaa aaaa aa aa a a a aa aaaaCCCC]
!!     myf90stuff.f90.for
!!     myforstuff.for.f90
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine crack_cmd(cmd,old,new,ierr)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)              :: cmd
character(len=:),allocatable,intent(out) :: old,new                ! scratch string buffers
integer                                  :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=1)                         :: delimiters
integer                                  :: itoken
integer,parameter                        :: id=2                   ! expected location of delimiter
logical                                  :: ifok
integer                                  :: lmax                   ! length of target string
integer                                  :: start_token,end_token
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=0
   old=''
   new=''
   lmax=len_trim(cmd)                       ! significant length of change directive

   if(lmax.ge.4)then                      ! strtok ignores blank tokens so look for special case where first token is really null
      delimiters=cmd(id:id)               ! find delimiter in expected location
      itoken=0                            ! initialize strtok(3f) procedure

      if(strtok(cmd(id:),itoken,start_token,end_token,delimiters)) then        ! find OLD string
         old=cmd(start_token+id-1:end_token+id-1)
      else
         old=''
      endif

      if(cmd(id:id).eq.cmd(id+1:id+1))then
         new=old
         old=''
      else                                                                     ! normal case
         ifok=strtok(cmd(id:),itoken,start_token,end_token,delimiters)         ! find NEW string
         if(end_token .eq. (len(cmd)-id+1) )end_token=len_trim(cmd(id:))       ! if missing ending delimiter
         new=cmd(start_token+id-1:min(end_token+id-1,lmax))
      endif
   else                                                                        ! command was two or less characters
      ierr=-1
      call journal('sc','*crack_cmd* incorrect change directive -too short')
   endif

end subroutine crack_cmd
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function replace(targetline,old,new,cmd,occurrence,repeat,ignorecase,ierr) result (newline)

! ident_11="@(#)M_strings::replace(3f): replace one substring for another in string"

!-----------------------------------------------------------------------------------------------------------------------------------
! parameters
character(len=*),intent(in)            :: targetline   ! input line to be changed
character(len=*),intent(in),optional   :: old          ! old substring to replace
character(len=*),intent(in),optional   :: new          ! new substring
character(len=*),intent(in),optional   :: cmd          ! contains the instructions changing the string
integer,intent(in),optional            :: occurrence   ! Nth occurrence of OLD string to start replacement at
integer,intent(in),optional            :: repeat       ! how many replacements
logical,intent(in),optional            :: ignorecase
integer,intent(out),optional           :: ierr         ! error code. if ierr = -1 bad directive, >=0 then ierr changes made
!-----------------------------------------------------------------------------------------------------------------------------------
! returns
character(len=:),allocatable  :: newline               ! output string buffer
!-----------------------------------------------------------------------------------------------------------------------------------
! local
character(len=:),allocatable  :: new_local, old_local, old_local_for_comparison
integer                       :: icount,ichange,ier2
integer                       :: original_input_length
integer                       :: len_old, len_new
integer                       :: ladd
integer                       :: left_margin, right_margin
integer                       :: ind
integer                       :: ic
integer                       :: ichar
integer                       :: range_local(2)
character(len=:),allocatable  :: targetline_for_comparison   ! input line to be changed
logical                       :: ignorecase_local
logical                       :: flip
character(len=:),allocatable  :: targetline_local   ! input line to be changed
!-----------------------------------------------------------------------------------------------------------------------------------
   flip=.false.
   ignorecase_local=.false.
   original_input_length=len_trim(targetline)          ! get non-blank length of input line

!  get old_local and new_local from cmd or old and new
   if(present(cmd))then
      call crack_cmd(cmd,old_local,new_local,ier2)
      if(ier2.ne.0)then
         newline=targetline  ! if no changes are made return original string on error
         if(present(ierr))ierr=ier2
         return
      endif
   elseif(present(old).and.present(new))then
      old_local=old
      new_local=new
   else
      newline=targetline  ! if no changes are made return original string on error
      call journal('sc','*replace* must specify OLD and NEW or CMD')
      return
   endif
   if(present(ignorecase))then
      ignorecase_local=ignorecase
   else
      ignorecase_local=.false.
   endif
   if(present(occurrence))then
      range_local(1)=abs(occurrence)
   else
      range_local(1)=1
   endif
   if(present(repeat))then
      range_local(2)=range_local(1)+repeat-1
   else
      range_local(2)=original_input_length
   endif
   if(ignorecase_local)then
      targetline_for_comparison=lower(targetline)
      old_local_for_comparison=lower(old_local)
   else
      targetline_for_comparison=targetline
      old_local_for_comparison=old_local
   endif
   if(present(occurrence))then
      if(occurrence.lt.0)then
         flip=.true.
         targetline_for_comparison=reverse(targetline_for_comparison)
         targetline_local=reverse(targetline)
         old_local_for_comparison=reverse(old_local_for_comparison)
         old_local=reverse(old_local)
         new_local=reverse(new_local)
      else
         targetline_local=targetline
      endif
   else
      targetline_local=targetline
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   icount=0                                            ! initialize error flag/change count
   ichange=0                                           ! initialize error flag/change count
   len_old=len(old_local)                              ! length of old substring to be replaced
   len_new=len(new_local)                              ! length of new substring to replace old substring
   left_margin=1                                       ! left_margin is left margin of window to change
   right_margin=len(targetline)                        ! right_margin is right margin of window to change
   newline=''                                          ! begin with a blank line as output string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(len_old.eq.0)then                                ! c//new/ means insert new at beginning of line (or left margin)
      ichar=len_new + original_input_length
      if(len_new.gt.0)then
         newline=new_local(:len_new)//targetline_local(left_margin:original_input_length)
      else
         newline=targetline_local(left_margin:original_input_length)
      endif
      ichange=1                                        ! made one change. actually, c/// should maybe return 0
      if(present(ierr))ierr=ichange
      if(flip) newline=reverse(newline)
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ichar=left_margin                                   ! place to put characters into output string
   ic=left_margin                                      ! place looking at in input string
   loop: do
                                                       ! try finding start of OLD in remaining part of input in change window
      ind=index(targetline_for_comparison(ic:),old_local_for_comparison(:len_old))+ic-1
      if(ind.eq.ic-1.or.ind.gt.right_margin)then       ! did not find old string or found old string past edit window
         exit loop                                     ! no more changes left to make
      endif
      icount=icount+1                                  ! found an old string to change, so increment count of change candidates
      if(ind.gt.ic)then                                ! if found old string past at current position in input string copy unchanged
         ladd=ind-ic                                   ! find length of character range to copy as-is from input to output
         newline=newline(:ichar-1)//targetline_local(ic:ind-1)
         ichar=ichar+ladd
      endif
      if(icount.ge.range_local(1).and.icount.le.range_local(2))then    ! check if this is an instance to change or keep
         ichange=ichange+1
         if(len_new.ne.0)then                                          ! put in new string
            newline=newline(:ichar-1)//new_local(:len_new)
            ichar=ichar+len_new
         endif
      else
         if(len_old.ne.0)then                                          ! put in copy of old string
            newline=newline(:ichar-1)//old_local(:len_old)
            ichar=ichar+len_old
         endif
      endif
      ic=ind+len_old
   enddo loop
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (ichange)
   case (0)                                            ! there were no changes made to the window
      newline=targetline_local                         ! if no changes made output should be input
   case default
      if(ic.le.len(targetline))then                    ! if there is more after last change on original line add it
         newline=newline(:ichar-1)//targetline_local(ic:max(ic,original_input_length))
      endif
   end select
   if(present(ierr))ierr=ichange
   if(flip) newline=reverse(newline)
!-----------------------------------------------------------------------------------------------------------------------------------
end function replace
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    substitute(3f) - [M_strings:EDITING] subroutine globally substitutes
!!    one substring for another in string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine substitute(targetline,old,new,ierr,start,end)
!!
!!     character(len=*)              :: targetline
!!     character(len=*),intent(in)   :: old
!!     character(len=*),intent(in)   :: new
!!     integer,intent(out),optional  :: ierr
!!     integer,intent(in),optional   :: start
!!     integer,intent(in),optional   :: end
!!
!!##DESCRIPTION
!!    Globally substitute one substring for another in string.
!!
!!##OPTIONS
!!     TARGETLINE  input line to be changed. Must be long enough to
!!                 hold altered output.
!!     OLD         substring to find and replace
!!     NEW         replacement for OLD substring
!!     IERR        error code. If IER = -1 bad directive, >= 0 then
!!                 count of changes made.
!!     START       sets the left margin to be scanned for OLD in
!!                 TARGETLINE.
!!     END         sets the right margin to be scanned for OLD in
!!                 TARGETLINE.
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_substitute
!!    use M_strings, only : substitute
!!    implicit none
!!    ! must be long enough to hold changed line
!!    character(len=80) :: targetline
!!
!!    targetline='this is the input string'
!!    write(*,*)'ORIGINAL    : '//trim(targetline)
!!
!!    ! changes the input to 'THis is THe input string'
!!    call substitute(targetline,'th','TH')
!!    write(*,*)'th => TH    : '//trim(targetline)
!!
!!    ! a null old substring means "at beginning of line"
!!    ! changes the input to 'BEFORE:this is the input string'
!!    call substitute(targetline,'','BEFORE:')
!!    write(*,*)'"" => BEFORE: '//trim(targetline)
!!
!!    ! a null new string deletes occurrences of the old substring
!!    ! changes the input to 'ths s the nput strng'
!!    call substitute(targetline,'i','')
!!    write(*,*)'i => ""     : '//trim(targetline)
!!
!!    end program demo_substitute
!!
!!   Expected output
!!
!!     ORIGINAL    : this is the input string
!!     th => TH    : THis is THe input string
!!     "" => BEFORE: BEFORE:THis is THe input string
!!     i => ""     : BEFORE:THs s THe nput strng
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine substitute(targetline,old,new,ierr,start,end)

! ident_12="@(#)M_strings::substitute(3f): Globally substitute one substring for another in string"

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*)               :: targetline         ! input line to be changed
character(len=*),intent(in)    :: old                ! old substring to replace
character(len=*),intent(in)    :: new                ! new substring
integer,intent(out),optional   :: ierr               ! error code. if ierr = -1 bad directive, >=0 then ierr changes made
integer,intent(in),optional    :: start              ! start sets the left margin
integer,intent(in),optional    :: end                ! end sets the right margin
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=len(targetline)) :: dum1               ! scratch string buffers
integer                        :: ml, mr, ier1
integer                        :: maxlengthout       ! MAXIMUM LENGTH ALLOWED FOR NEW STRING
integer                        :: original_input_length
integer                        :: len_old, len_new
integer                        :: ladd
integer                        :: ir
integer                        :: ind
integer                        :: il
integer                        :: id
integer                        :: ic
integer                        :: ichar
!-----------------------------------------------------------------------------------------------------------------------------------
   if (present(start)) then                            ! optional starting column
      ml=start
   else
      ml=1
   endif
   if (present(end)) then                              ! optional ending column
      mr=end
   else
      mr=len(targetline)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ier1=0                                              ! initialize error flag/change count
   maxlengthout=len(targetline)                        ! max length of output string
   original_input_length=len_trim(targetline)          ! get non-blank length of input line
   dum1(:)=' '                                         ! initialize string to build output in
   id=mr-ml                                            ! check for window option !! change to optional parameter(s)
!-----------------------------------------------------------------------------------------------------------------------------------
   len_old=len(old)                                    ! length of old substring to be replaced
   len_new=len(new)                                    ! length of new substring to replace old substring
   if(id.le.0)then                                     ! no window so change entire input string
      il=1                                             ! il is left margin of window to change
      ir=maxlengthout                                  ! ir is right margin of window to change
      dum1(:)=' '                                      ! begin with a blank line
   else                                                ! if window is set
      il=ml                                            ! use left margin
      ir=min0(mr,maxlengthout)                         ! use right margin or rightmost
      dum1=targetline(:il-1)                           ! begin with what's below margin
   endif                                               ! end of window settings
!-----------------------------------------------------------------------------------------------------------------------------------
   if(len_old.eq.0)then                                ! c//new/ means insert new at beginning of line (or left margin)
      ichar=len_new + original_input_length
      if(ichar.gt.maxlengthout)then
         call journal('sc','*substitute* new line will be too long')
         ier1=-1
         if (present(ierr))ierr=ier1
         return
      endif
      if(len_new.gt.0)then
         dum1(il:)=new(:len_new)//targetline(il:original_input_length)
      else
         dum1(il:)=targetline(il:original_input_length)
      endif
      targetline(1:maxlengthout)=dum1(:maxlengthout)
      ier1=1                                           ! made one change. actually, c/// should maybe return 0
      if(present(ierr))ierr=ier1
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ichar=il                                            ! place to put characters into output string
   ic=il                                               ! place looking at in input string
   loop: do
      ind=index(targetline(ic:),old(:len_old))+ic-1    ! try to find start of old string in remaining part of input in change window
      if(ind.eq.ic-1.or.ind.gt.ir)then                 ! did not find old string or found old string past edit window
         exit loop                                     ! no more changes left to make
      endif
      ier1=ier1+1                                      ! found an old string to change, so increment count of changes
      if(ind.gt.ic)then                                ! if found old string past at current position in input string copy unchanged
         ladd=ind-ic                                   ! find length of character range to copy as-is from input to output
         if(ichar-1+ladd.gt.maxlengthout)then
            ier1=-1
            exit loop
         endif
         dum1(ichar:)=targetline(ic:ind-1)
         ichar=ichar+ladd
      endif
      if(ichar-1+len_new.gt.maxlengthout)then
         ier1=-2
         exit loop
      endif
      if(len_new.ne.0)then
         dum1(ichar:)=new(:len_new)
         ichar=ichar+len_new
      endif
      ic=ind+len_old
   enddo loop
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (ier1)
   case (:-1)
      call journal('sc','*substitute* new line will be too long')
   case (0)                                                ! there were no changes made to the window
   case default
      ladd=original_input_length-ic
      if(ichar+ladd.gt.maxlengthout)then
         call journal('sc','*substitute* new line will be too long')
         ier1=-1
         if(present(ierr))ierr=ier1
         return
      endif
      if(ic.lt.len(targetline))then
         dum1(ichar:)=targetline(ic:max(ic,original_input_length))
      endif
      targetline=dum1(:maxlengthout)
   end select
   if(present(ierr))ierr=ier1
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine substitute
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    change(3f) - [M_strings:EDITING] change old string to new string with
!!    a directive like a line editor
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine change(target_string,cmd,ierr)
!!
!!     character(len=*),intent(inout) :: target_string
!!     character(len=*),intent(in)    :: cmd
!!     integer                        :: ierr
!!
!!##DESCRIPTION
!!    change an old substring into a new substring in a character variable
!!    like a line editor. Primarily used to create interactive utilities
!!    such as input history editors for interactive line-mode programs. The
!!    output string is assumed long enough to accommodate the change.
!!    a directive resembles a line editor directive of the form
!!
!!       C/old_string/new_string/
!!
!!    where / may be any character which is not included in old_string
!!    or new_string.
!!
!!    a null old_string implies "beginning of string".
!!
!!##OPTIONS
!!    target_string  line to be changed
!!    cmd            contains instructions to change the string
!!    ierr           error code.
!!
!!       o =-1 bad directive
!!       o =0 no changes made
!!       o >0 count of changes made
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_change
!!
!!     use M_strings, only : change
!!     implicit none
!!     character(len=132) :: line='This is a test string to change'
!!     integer            :: ierr
!!        write(*,*)trim(line)
!!        ! change miniscule a to uppercase A
!!        call change(line,'c/a/A/',ierr)
!!        write(*,*)trim(line)
!!        ! put string at beginning of line
!!        call change(line,'c//prefix: /',ierr)
!!        write(*,*)trim(line)
!!        ! remove blanks
!!        call change(line,'c/ //',ierr)
!!        write(*,*)trim(line)
!!    end program demo_change
!!
!!   Expected output
!!
!!     This is a test string to change
!!     This is A test string to chAnge
!!     prefix: This is A test string to chAnge
!!     prefix:ThisisAteststringtochAnge
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine change(target_string,cmd,ierr)
! Change a string assumed long enough to accommodate the change, with a directive that resembles a line editor directive of the form
!    C/old_string/new_string/
! where / may be any character which is not included in old_string or new_string.
! a null old_string implies "beginning of string"
!===================================================================================================================================

! ident_13="@(#)M_strings::change(3f): change a character string like a line editor"

character(len=*),intent(inout)   :: target_string          ! line to be changed
character(len=*),intent(in)      :: cmd                    ! contains the instructions changing the string
character(len=1)                 :: delimiters
integer                          :: ierr                   ! error code. ier=-1 bad directive;=0 no changes made;>0 ier changes made
integer                          :: itoken
integer,parameter                :: id=2                   ! expected location of delimiter
character(len=:),allocatable     :: old,new                ! scratch string buffers
logical                          :: ifok
integer                          :: lmax                   ! length of target string
integer                          :: start_token,end_token
!-----------------------------------------------------------------------------------------------------------------------------------
   lmax=len_trim(cmd)                                                          ! significant length of change directive
   if(lmax.ge.4)then                         ! strtok ignores blank tokens so look for special case where first token is really null
      delimiters=cmd(id:id)                                                    ! find delimiter in expected location
      itoken=0                                                                 ! initialize strtok(3f) procedure

      if(strtok(cmd(id:),itoken,start_token,end_token,delimiters)) then        ! find OLD string
         old=cmd(start_token+id-1:end_token+id-1)
      else
         old=''
      endif

      if(cmd(id:id).eq.cmd(id+1:id+1))then
         new=old
         old=''
      else                                                                     ! normal case
         ifok=strtok(cmd(id:),itoken,start_token,end_token,delimiters)         ! find NEW string
         if(end_token .eq. (len(cmd)-id+1) )end_token=len_trim(cmd(id:))       ! if missing ending delimiter
         new=cmd(start_token+id-1:min(end_token+id-1,lmax))
      endif

      call substitute(target_string,old,new,ierr,1,len_trim(target_string))    ! change old substrings to new substrings
   else                                                                        ! command was two or less characters
      ierr=-1
      call journal('sc','*change* incorrect change directive -too short')
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine change
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     strtok(3f) - [M_strings:TOKENS] Tokenize a string
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  function strtok(source_string,itoken,token_start,token_end,delimiters)
!!  result(strtok_status)
!!
!!   ! returned value
!!   logical                      :: strtok_status
!!   ! string to tokenize
!!   character(len=*),intent(in)  :: source_string
!!   ! token count since started
!!   integer,intent(inout)        :: itoken
!!   ! beginning of token
!!   integer,intent(out)          :: token_start
!!   ! end of token
!!   integer,intent(inout)        :: token_end
!!   ! list of separator characters
!!   character(len=*),intent(in)  :: delimiters
!!
!!##DESCRIPTION
!!     The STRTOK(3f) function is used to isolate sequential tokens in a
!!     string, SOURCE_STRING. These tokens are delimited in the string by
!!     at least one of the characters in DELIMITERS. The first time that
!!     STRTOK(3f) is called, ITOKEN should be specified as zero. Subsequent
!!     calls, wishing to obtain further tokens from the same string,
!!     should pass back in TOKEN_END  and ITOKEN until the function result
!!     returns .false.
!!
!!     This routine assumes no other calls are made to it using any other
!!     input string while it is processing an input line.
!!
!!##OPTIONS
!!     source_string  input string to parse
!!     itoken         token count should be set to zero for a new string
!!     delimiters     characters used to determine the end of tokens
!!
!!##RETURN
!!     token_start    beginning position in SOURCE_STRING where token was found
!!     token_end      ending position in SOURCE_STRING where token was found
!!     strtok_status
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_strtok
!!     use M_strings, only : strtok
!!     implicit none
!!     character(len=264)          :: inline
!!     character(len=*),parameter  :: delimiters=' ;,'
!!     integer                     :: ios, itoken, istart, iend
!!        do ! read lines from stdin until end-of-file or error
!!           read (unit=*,fmt="(a)",iostat=ios) inline
!!           if(ios.ne.0)stop
!!           ! must set ITOKEN=0 before looping on strtok(3f)
!!           ! on a new string.
!!           itoken=0
!!           do while &
!!           &( strtok(inline,itoken,istart,iend,delimiters) )
!!              print *, itoken,&
!!              & 'TOKEN=['//(inline(istart:iend))//']',istart,iend
!!           enddo
!!        enddo
!!     end program demo_strtok
!!
!!     sample input file
!!
!!      this is a test of strtok; A:B :;,C;;
!!
!!     sample output file
!!
!!     1  TOKEN=[this]    2   5
!!     2  TOKEN=[is]      7   8
!!     3  TOKEN=[a]       10  10
!!     4  TOKEN=[test]    12  15
!!     5  TOKEN=[of]      17  18
!!     6  TOKEN=[strtok]  20  25
!!     7  TOKEN=[A:B]     28  30
!!     8  TOKEN=[:]       32  32
!!     9  TOKEN=[C]       35  35
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
FUNCTION strtok(source_string,itoken,token_start,token_end,delimiters) result(strtok_status)
! JSU- 20151030

! ident_14="@(#)M_strings::strtok(3f): Tokenize a string"

character(len=*),intent(in)  :: source_string    ! Source string to tokenize.
character(len=*),intent(in)  :: delimiters       ! list of separator characters. May change between calls
integer,intent(inout)        :: itoken           ! token count since started
logical                      :: strtok_status    ! returned value
integer,intent(out)          :: token_start      ! beginning of token found if function result is .true.
integer,intent(inout)        :: token_end        ! end of token found if function result is .true.
integer,save                 :: isource_len
!----------------------------------------------------------------------------------------------------------------------------
!  calculate where token_start should start for this pass
   if(itoken.le.0)then                           ! this is assumed to be the first call
      token_start=1
   else                                          ! increment start to previous end + 1
      token_start=token_end+1
   endif
!----------------------------------------------------------------------------------------------------------------------------
   isource_len=len(source_string)                ! length of input string
!----------------------------------------------------------------------------------------------------------------------------
   if(token_start.gt.isource_len)then            ! user input error or at end of string
      token_end=isource_len                      ! assume end of token is end of string until proven otherwise so it is set
      strtok_status=.false.
      return
   endif
!----------------------------------------------------------------------------------------------------------------------------
   ! find beginning of token
   do while (token_start .le. isource_len)       ! step thru each character to find next delimiter, if any
      if(index(delimiters,source_string(token_start:token_start)) .ne. 0) then
         token_start = token_start + 1
      else
         exit
      endif
   enddo
!----------------------------------------------------------------------------------------------------------------------------
   token_end=token_start
   do while (token_end .le. isource_len-1)       ! step thru each character to find next delimiter, if any
      if(index(delimiters,source_string(token_end+1:token_end+1)) .ne. 0) then  ! found a delimiter in next character
         exit
      endif
      token_end = token_end + 1
   enddo
!----------------------------------------------------------------------------------------------------------------------------
   if (token_start .gt. isource_len) then        ! determine if finished
      strtok_status=.false.                      ! flag that input string has been completely processed
   else
      itoken=itoken+1                            ! increment count of tokens found
      strtok_status=.true.                       ! flag more tokens may remain
   endif
!----------------------------------------------------------------------------------------------------------------------------
end function strtok
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    modif(3f) - [M_strings:EDITING] emulate the MODIFY command from the
!!    line editor XEDIT
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine modif(cline,cmod)
!!
!!     character(len=*) :: cline ! input string to change
!!     ! directive provides directions on changing string
!!     character(len=*) :: cmod
!!
!!##DESCRIPTION
!!   MODIF(3f) Modifies the line currently pointed at using a directive
!!   that acts much like a line editor directive.
!!   Primarily used to create interactive utilities such as input history
!!   editors for interactive line-mode programs.
!!
!!   the modify directives are as follows-
!!
!!    DIRECTIVE EXPLANATION
!!
!!    ^STRING#   Causes the string of characters between the ^ and the
!!               next # to be inserted before the characters pointed to
!!               by the ^. an ^ or & within the string is treated as a
!!               regular character. If the closing # is not specified,
!!               MODIF(3f) inserts the remainder of the line as if a # was
!!               specified after the last nonblank character.
!!
!!               There are two exceptions. the combination ^# causes a #
!!               to be inserted before the character pointed to by the
!!               ^, and an ^ as the last character of the directives
!!               causes a blank to be inserted.
!!
!!    #          (When not the first # after an ^) causes the character
!!               above it to be deleted.
!!
!!    &          Replaces the character above it with a space.
!!
!!    (SPACE)    A space below a character leaves it unchanged.
!!
!!    Any other character replaces the character above it.
!!
!!##EXAMPLES
!!
!!   Example input/output:
!!
!!    THE INPUT LINE........ 10 THIS STRING  TO BE MORTIFD
!!    THE DIRECTIVES LINE...        ^ IS THE#        D#  ^IE
!!    ALTERED INPUT LINE.... 10 THIS IS THE STRING  TO BE MODIFIED
!!
!!   Sample program:
!!
!!    program demo_modif
!!    use M_strings, only : modif
!!    implicit none
!!    character(len=256)           :: line
!!    integer                      :: ios
!!    integer                      :: count
!!    integer                      :: COMMAND_LINE_LENGTH
!!    character(len=:),allocatable :: COMMAND_LINE
!!       ! get command name length
!!       call get_command_argument(0,length=count)
!!       ! get command line length
!!       call get_command(length=COMMAND_LINE_LENGTH)
!!       ! allocate string big enough to hold command line
!!       allocate(character(len=COMMAND_LINE_LENGTH+200) :: COMMAND_LINE)
!!       ! get command line as a string
!!       call get_command(command=COMMAND_LINE)
!!       ! trim leading spaces just in case
!!       COMMAND_LINE=adjustl(COMMAND_LINE)
!!       ! remove command name
!!       COMMAND_LINE=adjustl(COMMAND_LINE(COUNT+2:))
!!       INFINITE: do
!!          read(*,'(a)',iostat=ios)line
!!          if(ios.ne.0)exit
!!          call modif(line,COMMAND_LINE)
!!          write(*,'(a)')trim(line)
!!       enddo INFINITE
!!    end program demo_modif
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE MODIF(CLINE,MOD)

!$@(#) M_strings::modif(3f): Emulate the MODIFY command from the line editor XEDIT

!
! MODIF
! =====
! ACTION- MODIFIES THE LINE CURRENTLY POINTED AT. THE INPUT STRING CLINE IS ASSUMED TO BE LONG ENOUGH TO ACCOMMODATE THE CHANGES
!         THE MODIFY DIRECTIVES ARE AS FOLLOWS-
!
!   DIRECTIVE                       EXPLANATION
!   ---------                       ------------
!   ^STRING#   CAUSES THE STRING OF CHARACTERS BETWEEN THE ^ AND THE
!              NEXT  # TO BE INSERTED BEFORE THE CHARACTERS POINTED TO
!              BY THE ^. AN ^ OR & WITHIN THE STRING IS TREATED AS A
!              REGULAR CHARACTER. IF THE CLOSING # IS NOT SPECIFIED,
!              MODIF(3f) INSERTS THE REMAINDER OFTHELINE AS IF A # WAS
!              SPECIFIED AFTER THE LAST NONBLANK CHARACTER.
!
!              THERE ARE TWO EXCEPTIONS. THE COMBINATION ^# CAUSES A #
!              TO BE INSERTED BEFORE THE CHARACTER POINTED TO BY THE
!              ^,  AND AN ^ AS THE LAST CHARACTER OF THE DIRECTIVES
!              CAUSES A BLANK TO BE INSERTED.
!
!   #          (WHEN NOT THE FIRST # AFTER AN ^) CAUSES THE CHARACTER
!              ABOVE IT TO BE DELETED.
!
!   &          REPLACES THE CHARACTER ABOVE IT WITH A SPACE.
!
!   (SPACE)    A SPACE BELOW A CHARACTER LEAVES IT UNCHANGED.
!
!   ANY OTHER CHARACTER REPLACES THE CHARACTER ABOVE IT.
!
! EXAMPLE-
! THE INPUT LINE........ 10 THIS STRING  TO BE MORTIFD
! THE DIRECTIVES LINE...        ^ IS THE#        D#  ^IE
! ALTERED INPUT LINE.... 10 THIS IS THE STRING  TO BE MODIFIED
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
character(len=*)            :: cline        !STRING TO BE MODIFIED
character(len=*),intent(in) :: mod          !STRING TO DIRECT MODIFICATION
character(len=len(cline))   :: cmod
character(len=3),parameter  :: c='#&^'      !ASSIGN DEFAULT EDIT CHARACTERS
integer                     :: maxscra      !LENGTH OF SCRATCH BUFFER
character(len=len(cline))   :: dum2         !SCRATCH CHARACTER BUFFER
logical                     :: linsrt       !FLAG FOR INSERTING DATA ON LINE
integer :: i, j, ic, ichar, iend, lmax, lmx1
maxscra=len(cline)
   CMOD=TRIM(MOD)
   LMAX=MIN0(LEN(CLINE),MAXSCRA)         !DETERMINE MAXIMUM LINE LENGTH
   LMX1=LMAX-1                           !MAX LINE LENGTH -1
   DUM2=' '                              !INITIALIZE NEW LINE
   LINSRT=.FALSE.                        !INITIALIZE INSERT MODE
   IEND=len_trim(CMOD)                   !DETERMINE END OF MODS
   I=0                                   !CHAR COUNTER FOR MOD LINE CMOD
   IC=0                                  !CHAR COUNTER FOR CURRENT LINE CLINE
   ICHAR=0                               !CHAR COUNTER NEW LINE DUM2
11 CONTINUE
   I=I+1                                 !NEXT CHAR IN MOD LINE
   IF(ICHAR.GT.LMX1)GOTO 999             !IF TOO MANY CHARS IN NEW LINE
   IF(LINSRT) THEN                       !IF INSERTING NEW CHARS
      IF(I.GT.IEND) CMOD(I:I)=C(1:1)     !FORCE END OF INSERT MODE
      IF(CMOD(I:I).EQ.C(1:1))THEN        !IF END OF INSERT MODE
         LINSRT=.FALSE.                  !RESET INSERT MODE FLAG
         IF(IC+1.EQ.I)THEN               !NULL INSERT STRING
            ICHAR=ICHAR+1                !INCREMENT COUNTER FOR NEW LINE
            DUM2(ICHAR:ICHAR)=C(1:1)     !INSERT INSERT MODE TERMINATOR
         ENDIF
         DO J=IC,I                       !LOOP OF NUMBER OF CHARS INSERTED
            ICHAR=ICHAR+1                !INCREMENT COUNTER FOR NEW LINE
            IF(ICHAR.GT.LMAX)GOTO 999    !IF AT BUFFER LIMIT, QUIT
            DUM2(ICHAR:ICHAR)=CLINE(J:J) !APPEND CHARS FROM ORIG LINE
         ENDDO                           !...WHICH ALIGN WITH INSERTED CHARS
         IC=I                            !RESET CHAR COUNT TO END OF INSERT
         GOTO 1                          !CHECK NEW LINE LENGTH AND CYCLE
      ENDIF                              !END OF TERMINATED INSERT LOGIC
      ICHAR=ICHAR+1                      !INCREMENT NEW LINE COUNT
      DUM2(ICHAR:ICHAR)=CMOD(I:I)        !SET NEWLINE CHAR TO INSERTED CHAR
   ELSE                                  !IF NOT INSERTING CHARACTERS
      IC=IC+1                            !INCREMENT ORIGINAL LINE COUNTER
      IF(CMOD(I:I).EQ.C(1:1))GOTO 1      !IF DELETE CHAR. NO COPY AND CYCLE
      IF(CMOD(I:I).EQ.C(3:3))THEN        !IF BEGIN INSERT MODE
         LINSRT=.TRUE.                   !SET INSERT FLAG TRUE
         GOTO 1                          !CHECK LINE LENGTH AND CONTINUE
      ENDIF                              !IF NOT BEGINNING INSERT MODE
      ICHAR=ICHAR+1                      !INCREMENT NEW LINE COUNTER
      IF(CMOD(I:I).EQ.C(2:2))THEN        !IF REPLACE WITH BLANK
         DUM2(ICHAR:ICHAR)=' '           !SET NEWLINE CHAR TO BLANK
         GOTO 1                          !CHECK LINE LENGTH AND CYCLE
      ENDIF                              !IF NOT REPLACE WITH BLANK
      IF(CMOD(I:I).EQ.' ')THEN           !IF BLANK, KEEP ORIGINAL CHARACTER
         DUM2(ICHAR:ICHAR)=CLINE(IC:IC)  !SET NEW CHAR TO ORIGINAL CHAR
      ELSE                               !IF NOT KEEPING OLD CHAR
         DUM2(ICHAR:ICHAR)=CMOD(I:I)     !REPLACE ORIGINAL CHAR WITH NEW
      ENDIF                              !END CHAR KEEP OR REPLACE
   ENDIF                                 !END INSERT OR NO-INSERT
1  CONTINUE
   IF(I.LT.LMAX)GOTO 11                  !CHECK FOR END OF LINE REACHED
                                         !AND CYCLE IF OK
999   CONTINUE
   CLINE=DUM2                            !SET ORIGINAL CHARS TO NEW CHARS
END SUBROUTINE MODIF                     !RETURN
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      len_white(3f) - [M_strings:LENGTH] get length of string trimmed
!!      of whitespace.
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    integer function len_white(string)
!!
!!     character(len=*) :: string
!!
!!##DESCRIPTION
!!      len_white(3f) returns the position of the last character in
!!      string that is not a whitespace character. The Fortran90 intrinsic
!!      LEN_TRIM() should be used when trailing whitespace can be assumed
!!      to always be spaces.
!!
!!      This procedure was heavily used in the past because ANSI FORTRAN
!!      77 character objects are fixed length and blank padded and the
!!      LEN_TRIM() intrinsic did not exist. It should now be used only when
!!      whitespace characters other than blanks are likely.
!!
!!##OPTIONS
!!      string     input string whose trimmed length is being calculated
!!                 ignoring all trailing whitespace characters.
!!##RETURNS
!!      len_white  the number of characters in the trimmed string
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_len_white
!!
!!      use M_strings, only : len_white
!!      implicit none
!!      character(len=80) ::  s
!!      integer           :: ilen, lastnb
!!      intrinsic len
!!
!!      s=' ABCDEFG abcdefg '
!!      ilen = len(s)
!!      lastnb = len_white(s)
!!
!!      write(*,*) 'total length of variable is ',ilen
!!      write(*,*) 'trimmed length of variable is ',lastnb
!!      write(*,*) 'trimmed string=[',s(:lastnb),']'
!!
!!     end program demo_len_white
!!
!!##NOTES
!!
!! o len_white
!!
!!      is a resource-intensive routine. Once the end of
!!      the string is found, it is probably best to keep track of it in
!!      order to avoid repeated calls to len_white. Because they
!!      might be more efficient, consider looking for vendor-supplied or
!!      system-optimized equivalents. For example:
!!
!!         o lnblnk - Solaris f77
!!         o len_trim - FORTRAN 90
!!
!! o Some compilers seem to have trouble passing a string of variable
!!   length properly. To be safe, use something like this:
!!
!!       subroutine message(s)
!!        character(len=*) :: s ! s is of variable length
!!           ilen=len(s)        ! get total length of variable
!!           ! explicitly specify a substring instead of just variable name
!!           lastnb = len_white(s(:ilen))
!!           write(*,*)'error:[',s(:lastnb),']'
!!       end subroutine messages
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental integer function len_white(string)
!  DEPRECATED. Use len_trim(3f),trim(3f) unless you might have trailing nulls (common when interacting with C procedures)"
!  John S. Urban, 1984, 1997-12-31
!  Note that if the string is blank, a length of 0 is returned; which is not a legal string length in Fortran77.
!  this routine used to return one instead of zero.
!   - mod 1:     1994
!                added null (char(0)) because HP and some Suns not padding
!                strings with blank, but with null characters; 1994 JSU
!   - mod 2:     1999
!                update syntax with INTENT(), ENDDO, no RETURN
!                still need instead of LEN_TRIM() because some systems stil pad CHARACTER with NULL
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_15="@(#)M_strings::len_white(3f): return position of last non-blank/non-null character in string"

character(len=*),intent(in):: string ! input string to determine length of
integer                    :: i10
intrinsic len
   len_white=0
   do i10=len(string),1,-1
      select case(string(i10:i10))
      case(' ')                 ! space(32)
      case(char(0))             ! null(0)
      case(char(9):char(13))    ! tab(9), new line(10), vertical tab(11), formfeed(12), carriage return(13)
      case default
         len_white=i10
         exit
      end select
   enddo
end function len_white
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    crop(3f) - [M_strings:WHITESPACE] trim leading and trailing blanks and control characters from a string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function crop(strin) result (strout)
!!
!!     character(len=*),intent(in)  :: strin
!!     character(len=:),allocatable :: strout
!!
!!##DESCRIPTION
!!    All control characters throughout the string are replaced with spaces
!!    and leading and trailing spaces are trimmed from the resulting string.
!!    Tabs are expanded assuming a stop every eight characters.
!!
!!
!!##OPTIONS
!!    strin   input string to trim leading and trailing space and control
!!    characters
!!            from
!!
!!##RETURNS
!!    strout  cropped version of input string
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_crop
!!    use M_strings, only: crop
!!    implicit none
!!    character(len=20) ::  untrimmed = '   ABCDEFG abcdefg  '
!!       write(*,*) 'untrimmed string=[',untrimmed,']'
!!       write(*,*) 'cropped string=[',crop(untrimmed),']'
!!    end program demo_crop
!!
!!   Expected output
!!
!!      untrimmed string=[   ABCDEFG abcdefg                      ]
!!      cropped string=[ABCDEFG abcdefg]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function crop(strin) result (strout)

! ident_16="@(#)M_strings::crop(3f): replace control characters with whitespace and trim leading and trailings spaces from resulting string"

character(len=*),intent(in)  :: strin
character(len=:),allocatable :: strout
   strout=trim(adjustl(noesc(dilate(strin))))
end function crop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    transliterate(3f) - [M_strings:EDITING] replace characters from old set with new set
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    pure function transliterate(instr,old_set,new_set) result(outstr)
!!
!!     character(len=*),intent(in)  :: instr
!!     character(len=*),intent(in)  :: old_set
!!     character(len=*),intent(in)  :: new_set
!!     character(len=len(instr))    :: outstr
!!
!!##DESCRIPTION
!!    Translate, squeeze, and/or delete characters from the input string.
!!
!!##OPTIONS
!!    instr    input string to change
!!    old_set  list of letters to change in INSTR if found
!!
!!             Each character in the input string that matches a character
!!             in the old set is replaced.
!!
!!    new_set  list of letters to replace letters in OLD_SET with.
!!
!!             If the new_set is the empty set the matched characters
!!             are deleted.
!!
!!             If the new_set is shorter than the old set the last character
!!             in the new set is used to replace the remaining characters
!!             in the new set.
!!
!!##RETURNS
!!    outstr   instr with substitutions applied
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_transliterate
!!
!!     use M_strings, only : transliterate
!!     implicit none
!!     character(len=80)   :: STRING
!!
!!     STRING='aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ'
!!     write(*,'(a)') STRING
!!
!!     ! convert a string to uppercase:
!!     write(*,*) TRANSLITERATE(STRING, &
!!     & 'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ')
!!
!!     ! change all miniscule letters to a colon (":"):
!!     write(*,*) TRANSLITERATE(STRING, &
!!     & 'abcdefghijklmnopqrstuvwxyz',':')
!!
!!     ! delete all miniscule letters
!!     write(*,*) TRANSLITERATE(STRING, &
!!     & 'abcdefghijklmnopqrstuvwxyz','')
!!
!!    end program demo_transliterate
!!
!!    Expected output
!!
!!     > aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ
!!     > AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ
!!     > :A:B:C:D:E:F:G:H:I:J:K:L:M:N:O:P:Q:R:S:T:U:V:W:X:Y:Z
!!     > ABCDEFGHIJKLMNOPQRSTUVWXYZ
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
PURE FUNCTION transliterate(instr,old_set,new_set) RESULT(outstr)

! ident_17="@(#)M_strings::transliterate(3f): replace characters from old set with new set"

!-----------------------------------------------------------------------------------------------------------------------------------
CHARACTER(LEN=*),INTENT(IN)  :: instr                             ! input string to change
CHARACTER(LEN=*),intent(in)  :: old_set
CHARACTER(LEN=*),intent(in)  :: new_set
!-----------------------------------------------------------------------------------------------------------------------------------
CHARACTER(LEN=LEN(instr))    :: outstr                            ! output string to generate
!-----------------------------------------------------------------------------------------------------------------------------------
INTEGER                      :: i10                               ! loop counter for stepping thru string
INTEGER                      :: ii,jj
!-----------------------------------------------------------------------------------------------------------------------------------
   jj=LEN(new_set)
   IF(jj.NE.0)THEN
      outstr=instr                                                ! initially assume output string equals input string
      stepthru: DO i10 = 1, LEN(instr)
         ii=iNDEX(old_set,instr(i10:i10))                         ! see if current character is in old_set
         IF (ii.NE.0)THEN
            if(ii.le.jj)then                                      ! use corresponding character in new_set
               outstr(i10:i10) = new_set(ii:ii)
            else
               outstr(i10:i10) = new_set(jj:jj)                   ! new_set not as long as old_set; use last character in new_set
            endif
         ENDIF
      ENDDO stepthru
   else                                                           ! new_set is null string so delete characters in old_set
      outstr=' '
      hopthru: DO i10 = 1, LEN(instr)
         ii=iNDEX(old_set,instr(i10:i10))                         ! see if current character is in old_set
         IF (ii.EQ.0)THEN                                         ! only keep characters not in old_set
            jj=jj+1
            outstr(jj:jj) = instr(i10:i10)
         ENDIF
      ENDDO hopthru
   endif
END FUNCTION transliterate
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    rotate13(3f) - [M_strings] apply trivial ROT13 encryption to a string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    rotate13(input) result(output)
!!
!!     character(len=*),intent(in) :: input
!!     character(len=len(input))   :: output
!!
!!##DESCRIPTION
!!    ROT13 ("rotate by 13 places", sometimes hyphenated ROT-13) is a simple
!!    letter substitution cipher that replaces a letter with the 13th letter
!!    after it in the alphabet; wrapping around if necessary.
!!
!!    The transformation can be done using a lookup table, such as the
!!    following:
!!
!!       Input  ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz
!!       Output NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm
!!
!!    ROT13 is used in online forums as a means of hiding spoilers,
!!    punchlines, puzzle solutions, and offensive materials from the casual
!!    glance. ROT13 has inspired a variety of letter and word games on-line,
!!    and is frequently mentioned in newsgroup conversations.
!!
!!    The algorithm provides virtually no cryptographic security, and is
!!    often cited as a canonical example of weak encryption.
!!
!!    ROT13 is a special case of the Caesar cipher which was developed in
!!    ancient Rome.
!!
!!    ALGORITHM
!!
!!    Applying ROT13 to a piece of text merely requires examining its
!!    alphabetic characters and replacing each one by the letter 13 places
!!    further along in the alphabet, wrapping back to the beginning if
!!    necessary. A becomes N, B becomes O, and so on up to M, which becomes
!!    Z, then the sequence continues at the beginning of the alphabet: N
!!    becomes A, O becomes B, and so on to Z, which becomes M. Only those
!!    letters which occur in the English alphabet are affected; numbers,
!!    symbols, whitespace, and all other characters are left unchanged.
!!
!!    SAME ALGORITHM FOR ENCODING AND DECODING
!!
!!    Because there are 26 letters in the English alphabet and 26 = 2 x 13,
!!    the ROT13 function is its own inverse: so the same action can be used
!!    for encoding and decoding. In other words, two successive applications
!!    of ROT13 restore the original text (in mathematics, this is sometimes
!!    called an involution; in cryptography, a reciprocal cipher).
!!
!!    TRIVIAL SECURITY
!!
!!    The use of a constant shift means that the encryption effectively
!!    has no key, and decryption requires no more knowledge than the fact
!!    that ROT13 is in use. Even without this knowledge, the algorithm is
!!    easily broken through frequency analysis.
!!
!!    In encrypted normal English-language text of any significant size,
!!    ROT13 is recognizable from some letter/word patterns. The words "n",
!!    "V" (capitalized only), and "gur" (ROT13 for "a", "I", and "the"),
!!    and words ending in "yl" ("ly") are examples.
!!
!!##REFERENCES
!!    Wikipedia, the free encyclopedia
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_rotate13
!!    use M_strings, only : rotate13
!!    implicit none
!!    character(len=256) :: line
!!    integer            :: ios
!!    do
!!       read(*,'(a)',iostat=ios)line
!!       if(ios.ne.0)exit
!!       write(*,'(a)')rotate13(line)
!!    enddo
!!    end program demo_rotate13
!!
!!  Sample usage:
!!
!!    demo_rotate13
!!    United we stand, divided we fall.
!!    Havgrq jr fgnaq, qvivqrq jr snyy.
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function rotate13 (input)
implicit none

! ident_18="@(#)M_strings::rotate13(3f): converts a character to its ROT13 equivalent, which is a trivial encryption."

character(len=*),intent(in) :: input
character(len=len(input))   :: rotate13
integer                     :: itemp
integer                     :: i
   rotate13=' '
   do i=1,len_trim(input)
      itemp = ichar (input(i:i))
      select case(itemp)
       case(65:77,97:109)
         itemp = itemp + 13
       case(78:90,110:122)
         itemp = itemp - 13
      end select
      rotate13(i:i) = char ( itemp )
   enddo

end function rotate13
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    join(3f) - [M_strings:EDITING] append CHARACTER variable array into
!!    a single CHARACTER variable with specified separator
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    pure function join(str,sep,trm,left,right,start,end) result (string)
!!
!!     character(len=*),intent(in)          :: str(:)
!!     character(len=*),intent(in),optional :: sep
!!     logical,intent(in),optional          :: trm
!!     character(len=*),intent(in),optional :: right
!!     character(len=*),intent(in),optional :: left
!!     character(len=*),intent(in),optional :: start
!!     character(len=*),intent(in),optional :: end
!!     character(len=:),allocatable         :: string
!!
!!##DESCRIPTION
!!   JOIN(3f) appends the elements of a CHARACTER array into a single
!!   CHARACTER variable, with elements 1 to N joined from left to right.
!!   By default each element is trimmed of trailing spaces and the
!!   default separator is a null string.
!!
!!##OPTIONS
!!      STR(:)  array of CHARACTER variables to be joined
!!      SEP     separator string to place between each variable. defaults
!!              to a null string.
!!      LEFT    string to place at left of each element
!!      RIGHT   string to place at right of each element
!!      START   prefix string
!!      END     suffix string
!!      TRM     option to trim each element of STR of trailing
!!              spaces. Defaults to .TRUE.
!!
!!##RESULT
!!      STRING  CHARACTER variable composed of all of the elements of STR()
!!              appended together with the optional separator SEP placed
!!              between the elements.
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!   program demo_join
!!   use M_strings, only: join
!!   implicit none
!!   character(len=:),allocatable  :: s(:)
!!   character(len=:),allocatable  :: out
!!   integer                       :: i
!!     s=[character(len=10) :: 'United',' we',' stand,', &
!!     & ' divided',' we fall.']
!!     out=join(s)
!!     write(*,'(a)') out
!!     write(*,'(a)') join(s,trm=.false.)
!!     write(*,'(a)') (join(s,trm=.false.,sep='|'),i=1,3)
!!     write(*,'(a)') join(s,sep='<>')
!!     write(*,'(a)') join(s,sep=';',left='[',right=']')
!!     write(*,'(a)') join(s,left='[',right=']')
!!     write(*,'(a)') join(s,left='>>')
!!   end program demo_join
!!
!!  Expected output:
!!
!!   United we stand, divided we fall.
!!   United     we        stand,    divided   we fall.
!!   United    | we       | stand,   | divided  | we fall.
!!   United    | we       | stand,   | divided  | we fall.
!!   United    | we       | stand,   | divided  | we fall.
!!   United<> we<> stand,<> divided<> we fall.
!!   [United];[ we];[ stand,];[ divided];[ we fall.]
!!   [United][ we][ stand,][ divided][ we fall.]
!!   >>United>> we>> stand,>> divided>> we fall.
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
pure function join(str,sep,trm,left,right,start,end) result (string)

! ident_19="@(#)M_strings::join(3f): merge string array into a single CHARACTER value adding specified separators, caps, prefix and suffix"

character(len=*),intent(in)          :: str(:)
character(len=*),intent(in),optional :: sep, right, left, start, end
logical,intent(in),optional          :: trm
character(len=:),allocatable         :: sep_local, left_local, right_local
character(len=:),allocatable         :: string
logical                              :: trm_local
integer                              :: i
   if(present(sep))then   ; sep_local=sep     ; else ; sep_local=''     ; endif
   if(present(trm))then   ; trm_local=trm     ; else ; trm_local=.true. ; endif
   if(present(left))then  ; left_local=left   ; else ; left_local=''    ; endif
   if(present(right))then ; right_local=right ; else ; right_local=''   ; endif
   string=''
   if(size(str).eq.0)then
      string=string//left_local//right_local
   else
      do i = 1,size(str)-1
         if(trm_local)then
            string=string//left_local//trim(str(i))//right_local//sep_local
         else
            string=string//left_local//str(i)//right_local//sep_local
         endif
      enddo
      if(trm_local)then
         string=string//left_local//trim(str(i))//right_local
      else
         string=string//left_local//str(i)//right_local
      endif
   endif
   if(present(start))string=start//string
   if(present(end))string=string//end
end function join
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      reverse(3f) - [M_strings:EDITING] Return a string reversed
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental pure function reverse(str) result (string)
!!
!!     character(*), intent(in) :: str
!!     character(len(str))      :: string
!!
!!##DESCRIPTION
!!      reverse(string) returns a copy of the input string with
!!      all characters reversed from right to left.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!       program demo_reverse
!!       use M_strings, only: reverse
!!       implicit none
!!       character(len=:),allocatable  :: s
!!          write(*,*)'REVERSE STRINGS:',reverse('Madam, I''m Adam')
!!          s='abcdefghijklmnopqrstuvwxyz'
!!          write(*,*) 'original input string is ....',s
!!          write(*,*) 'reversed output string is ...',reverse(s)
!!       end program demo_reverse
!!
!!    Expected output
!!
!!      original input string is ....abcdefghijklmnopqrstuvwxyz
!!      reversed output string is ...zyxwvutsrqponmlkjihgfedcba
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental function reverse(string ) result (rev)

! ident_20="@(#)M_strings::reverse(3f): Return a string reversed"

character(len=*),intent(in)    :: string   ! string to reverse
character(len=len(string))     :: rev      ! return value (reversed string)
integer                        :: length
integer                        :: i
   length = len(string)
   do i = 1,length
      rev(i:i)=string(length-i+1:length-i+1)
   enddo
end function reverse
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!! upper_quoted(3f) - [M_strings:CASE] elemental function converts string to miniscule skipping strings quoted per Fortran syntax rules
!! (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental pure function upper_quoted(str) result (string)
!!
!!     character(*), intent(in)    :: str
!!     character(len(str))         :: string  ! output string
!!
!!##DESCRIPTION
!!    upper_quoted(string) returns a copy of the input string with all not-quoted
!!    characters converted to uppercase, assuming ASCII character sets
!!    are being used. The quoting rules are the same as for Fortran source.
!!    Either a single or double quote starts a quoted string, and a quote
!!    character of the same type is doubled when it appears internally in
!!    the quoted string. If a double quote quotes the string single quotes
!!    may appear in the quoted string as single characters, and vice-versa
!!    for single quotes.
!!
!!##OPTIONS
!!    str    string to convert to uppercase
!!
!!##RESULTS
!!    upper  copy of the input string with all unquoted characters converted
!!           to uppercase
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_upper_quoted
!!     use M_strings, only: upper_quoted
!!     implicit none
!!     character(len=:),allocatable  :: s
!!     s=' ABCDEFG abcdefg "Double-Quoted" ''Single-Quoted'' "with ""&
!!        & Quote" everything else'
!!        write(*,*) 'mixed-case input string is ....',s
!!        write(*,*) 'upper-case output string is ...',upper_quoted(s)
!!        write(*,'(1x,a,*(a:,"+"))') 'upper_quoted(3f) is elemental ==>', &
!!        & upper_quoted(["abc","def","ghi"])
!!     end program demo_upper_quoted
!!
!!    Expected output:
!!
!!     mixed-case input string is .... ABCDEFG abcdefg "Double-Quoted"
!!     'Single-Quoted' "with "" Quote" everything else
!!     upper-case output string is ... ABCDEFG ABCDEFG "Double-Quoted"
!!     'Single-Quoted' "with "" Quote" EVERYTHING ELSE
!!     upper_quoted(3f) is elemental ==>ABC+DEF+GHI
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental pure function upper_quoted(str) result (string)

! ident_21="@(#)M_strings::upper_quoted(3f): elemental function converts string to miniscule skipping strings quoted per Fortran syntax rules"

character(len=*), intent(in)   :: str     ! The input string
character(len=len(str))        :: string  ! The output string
logical                        :: toggle
character(len=1)               :: togglechar
integer                        :: irnk
integer                        :: i
character(len=26), parameter   :: large="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
character(len=26), parameter   :: small="abcdefghijklmnopqrstuvwxyz"

   string=str
   toggle = .TRUE.
   do i = 1, len_trim(string)
      if(toggle) then
         if(string(i:i) == '"' .or. string(i:i) == "'") then
            toggle = .not. toggle
            togglechar = string(i:i)
         endif
         irnk = index(small, string(i:i))
         if(irnk > 0) then
            string(i:i) = large(irnk:irnk)
         endif
      else
         if(string(i:i) == togglechar) toggle = .not. toggle
      endif
   enddo
end function upper_quoted
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!! upper(3f) - [M_strings:CASE] changes a string to uppercase
!! (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental pure function upper(str,begin,end) result (string)
!!
!!     character(*), intent(in)    :: str
!!     integer,optional,intent(in) :: begin,end
!!     character(len(str))         :: string  ! output string
!!
!!##DESCRIPTION
!!      upper(string) returns a copy of the input string with all characters
!!      converted in the optionally specified range to uppercase, assuming
!!      ASCII character sets are being used. If no range is specified the
!!      entire string is converted to uppercase.
!!
!!##OPTIONS
!!    str    string to convert to uppercase
!!    begin  optional starting position in "str" to begin converting to
!!           uppercase
!!    end    optional ending position in "str" to stop converting to
!!           uppercase
!!
!!##RESULTS
!!    upper  copy of the input string with all characters converted to
!!           uppercase over optionally specified range.
!!
!!##TRIVIA
!!    The terms "uppercase" and "lowercase" date back to the early days of
!!    the mechanical printing press. Individual metal alloy casts of each
!!    needed letter, or punctuation symbol, were meticulously added to a
!!    press block, by hand, before rolling out copies of a page. These
!!    metal casts were stored and organized in wooden cases. The more
!!    often needed miniscule letters were placed closer to hand, in the
!!    lower cases of the work bench. The less often needed, capitalized,
!!    majuscule letters, ended up in the harder to reach upper cases.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_upper
!!     use M_strings, only: upper
!!     implicit none
!!     character(len=:),allocatable  :: s
!!        s=' ABCDEFG abcdefg '
!!        write(*,*) 'mixed-case input string is ....',s
!!        write(*,*) 'upper-case output string is ...',upper(s)
!!        write(*,*) 'make first character uppercase  ... ',&
!!        & upper('this is a sentence.',1,1)
!!        write(*,'(1x,a,*(a:,"+"))') 'UPPER(3f) is elemental ==>',&
!!        & upper(["abc","def","ghi"])
!!     end program demo_upper
!!
!!    Expected output
!!
!!     mixed-case input string is .... ABCDEFG abcdefg
!!     upper-case output string is ... ABCDEFG ABCDEFG
!!     make first character uppercase  ... This is a sentence.
!!     UPPER(3f) is elemental ==>ABC+DEF+GHI
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
! Timing
!
!    Several different methods have been proposed for changing case.
!    A simple program that copies a large file and converts it to
!    uppercase was timed and compared to a simple copy. This was used
!    to select the default function.
!
! NULL:    83.41user  9.25system 1:37.94elapsed 94%CPU
! upper:  101.44user 10.89system 1:58.36elapsed 94%CPU
! upper2: 105.04user 10.69system 2:04.17elapsed 93%CPU
! upper3: 267.21user 11.69system 4:49.21elapsed 96%CPU
elemental pure function upper(str,begin,end) result (string)

! ident_22="@(#)M_strings::upper(3f): Changes a string to uppercase"

character(*), intent(in)      :: str                 ! inpout string to convert to all uppercase
integer, intent(in), optional :: begin,end
character(len(str))           :: string              ! output string that contains no miniscule letters
integer                       :: i                   ! loop counter
integer                       :: ibegin,iend
integer,parameter             :: diff = iachar('A')-iachar('a')
   string = str                                      ! initialize output string to input string
   ibegin=1
   iend=len_trim(str)

   if (present(begin))then
      ibegin = min(max(ibegin,begin),iend)
   endif

   if (present(end))then
      iend= max(1,min(iend,end))
   endif

   do concurrent (i = ibegin:iend)                   ! step thru each letter in the string in specified range
       select case (str(i:i))
       case ('a':'z')                                ! located miniscule letter
          string(i:i) = char(iachar(str(i:i))+diff)  ! change miniscule letter to uppercase
       end select
   enddo

end function upper
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    lower(3f) - [M_strings:CASE] changes a string to lowercase over
!!    specified range
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental pure function lower(str,begin,end) result (string)
!!
!!     character(*), intent(in) :: str
!!     integer,optional         :: begin, end
!!     character(len(str))      :: string  ! output string
!!
!!##DESCRIPTION
!!      lower(string) returns a copy of the input string with all characters
!!      converted to miniscule over the specified range, assuming ASCII
!!      character sets are being used. If no range is specified the entire
!!      string is converted to miniscule.
!!
!!##OPTIONS
!!    str    string to convert to miniscule
!!    begin  optional starting position in "str" to begin converting to
!!           miniscule
!!    end    optional ending position in "str" to stop converting to
!!           miniscule
!!
!!##RESULTS
!!    lower  copy of the input string with all characters converted to
!!           miniscule over optionally specified range.
!!
!!##TRIVIA
!!    The terms "uppercase" and "lowercase" date back to the early days of
!!    the mechanical printing press. Individual metal alloy casts of each
!!    needed letter, or punctuation symbol, were meticulously added to a
!!    press block, by hand, before rolling out copies of a page. These
!!    metal casts were stored and organized in wooden cases. The more
!!    often needed miniscule letters were placed closer to hand, in the
!!    lower cases of the work bench. The less often needed, capitalized,
!!    majuscule letters, ended up in the harder to reach upper cases.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!       program demo_lower
!!       use M_strings, only: lower
!!       implicit none
!!       character(len=:),allocatable  :: s
!!          s=' ABCDEFG abcdefg '
!!          write(*,*) 'mixed-case input string is ....',s
!!          write(*,*) 'lower-case output string is ...',lower(s)
!!       end program demo_lower
!!
!!    Expected output
!!
!!       mixed-case input string is .... ABCDEFG abcdefg
!!       lower-case output string is ... abcdefg abcdefg
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental pure function lower(str,begin,end) result (string)

! ident_23="@(#)M_strings::lower(3f): Changes a string to lowercase over specified range"

character(*), intent(in)     :: str
character(len(str))          :: string
integer,intent(in),optional  :: begin, end
integer                      :: i
integer                      :: ibegin, iend
integer,parameter             :: diff = iachar('A')-iachar('a')
   string = str
   ibegin=1
   iend=len_trim(str)

   if (present(begin))then
      ibegin = min(max(1,begin),iend)
   endif

   if (present(end))then
      iend= max(1,min(iend,end))
   endif

   do concurrent (i = ibegin:iend)                   ! step thru each letter in the string in specified range
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = char(iachar(str(i:i))-diff)   ! change letter to miniscule
      case default
      end select
   enddo

end function lower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!
!!    switch(3f) - [M_strings:ARRAY] converts between CHARACTER scalar and
!!    array of single characters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    pure function switch(array) result (string)
!!
!!     character(len=1),intent(in) :: array(:)
!!     character(len=SIZE(array))  :: string
!!
!!      or
!!
!!    pure function switch(string) result (array)
!!
!!     character(len=1),intent(in) :: array(:)
!!     character(len=SIZE(array))  :: string
!!
!!##DESCRIPTION
!!    SWITCH(3f): generic function that switches CHARACTER string to an array
!!    of single characters or an array of single characters to a CHARACTER
!!    string. Useful in passing strings to C. New Fortran features may
!!    supersede these routines.
!!
!!##EXAMPLES
!!
!!  Sample program:
!!
!!    program demo_switch
!!    use M_strings, only : switch, isalpha, islower, nospace
!!    character(len=*),parameter :: &
!!    & dashes='-----------------------------------'
!!    character(len=*),parameter :: string='This is a string'
!!    character(len=1024)        :: line
!!
!!    ! First, examples of standard Fortran features
!!    ! returns array [F,T,T,T,T,T]
!!    write(*,*)['A','=','=','=','=','='].eq.'='
!!    ! this would return T
!!    write(*,*)all(['=','=','=','=','=','='].eq.'=')
!!    ! this would return F
!!    write(*,*)all(['A','=','=','=','=','='].eq.'=')
!!
!!    ! so to test if the string DASHES is all dashes
!!    ! using SWITCH(3f) is
!!    if(all(switch(dashes).eq.'-'))then
!!       write(*,*)'DASHES is all dashes'
!!    endif
!!
!!    ! so to test is a string is all letters
!!    ! isalpha(3f) returns .true. only if character is a letter
!!    ! false because dashes are not a letter
!!    write(*,*) all(isalpha(switch(dashes)))
!!    ! false because of spaces
!!    write(*,*) all(isalpha(switch(string)))
!!    ! true because removed whitespace
!!    write(*,*) all(isalpha(switch(nospace(string))))
!!
!!    ! to see if a string is all uppercase
!!    ! show the string
!!    write(*,*) string
!!    ! converted to character array
!!    write(*,'(1x,*("[",a,"]":))') switch(string)
!!    write(*,'(*(l3))') islower(switch(string))
!!
!!    ! we need a string that is all letters
!!    line=nospace(string)
!!    write(*,*)'LINE=',trim(line)
!!    ! all true except first character
!!    write(*,*) islower(switch(nospace(string)))
!!    ! should be false
!!    write(*,*) all(islower(switch(nospace(string))))
!!    ! should be true
!!    write(*,*) all(islower(switch(nospace(string(2:)))))
!!
!!    end program demo_switch
!!
!!  Expected output
!!
!!     F T T T T T
!!     T
!!     F
!!     DASHES is all dashes
!!     F
!!     F
!!     T
!!     This is a string
!!     [T][h][i][s][ ][i][s][ ][a][ ][s][t][r][i][n][g]
!!      F  T  T  T  F  T  T  F  T  F  T  T  T  T  T  T
!!     LINE=Thisisastring
!!     F T T T T T T T T T T T T
!!     F
!!     T
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
pure function a2s(array)  result (string)

! ident_24="@(#)M_strings::a2s(3fp): function to copy char array to string"

character(len=1),intent(in) :: array(:)
character(len=SIZE(array))  :: string
integer                     :: i
! ----------------------------------------------------------------------------------------------------------------------------------
   forall( i = 1:size(array)) string(i:i) = array(i)
! ----------------------------------------------------------------------------------------------------------------------------------
end function a2s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function s2a(string)  RESULT (array)

! ident_25="@(#)M_strings::s2a(3fp): function to copy string(1:Clen(string)) to char array"

character(len=*),intent(in) :: string
character(len=1)            :: array(len(string))
integer                     :: i
! ----------------------------------------------------------------------------------------------------------------------------------
   forall(i=1:len(string)) array(i) = string(i:i)
! ----------------------------------------------------------------------------------------------------------------------------------
end function s2a
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      s2c(3f) - [M_strings:ARRAY] convert character variable to array of
!!      characters with last element set to null
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function s2c(string)
!!
!!     character(len=*),intent=(in)  :: string
!!     character(len=1),allocatable  :: s2c(:)
!!
!!##DESCRIPTION
!!    Given a character variable convert it to an array of single-character
!!    character variables with the last element set to a null character.
!!    This is generally used to pass character variables to C procedures.
!!
!!##EXAMPLES
!!
!!    Sample Program:
!!
!!     program demo_s2c
!!     use M_strings, only : s2c
!!     implicit none
!!     character(len=*),parameter   :: string="single string"
!!     character(len=3),allocatable :: array(:)
!!        write(*,*)'INPUT STRING ',trim(string)
!!        ! put one character into each 3-character element of array
!!        array=s2c(string)
!!        ! write array with ASCII Decimal Equivalent below it except show
!!        ! unprintable characters like NULL as "XXX"
!!        write(*,'(1x,*("[",a3,"]":))')&
!!             & merge('XXX',array,ichar(array(:)(1:1)).lt.32)
!!        write(*,'(1x,*("[",i3,"]":))')&
!!             & ichar(array(:)(1:1))
!!     end program demo_s2c
!!
!!   Expected output:
!!
!!    INPUT STRING single string
!!    [s  ][i  ][n  ][g  ][l  ][e  ][   ][s  ][t  ][r  ][i  ][n  ][g  ][XXX]
!!    [115][105][110][103][108][101][ 32][115][116][114][105][110][103][  0]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
pure function s2c(string)  RESULT (array)
use,intrinsic :: ISO_C_BINDING, only : C_CHAR

! ident_26="@(#)M_strings::s2c(3f): copy string(1:Clen(string)) to char array with null terminator"

character(len=*),intent(in)     :: string

! This is changing, but currently the most portable way to pass a CHARACTER variable to C is to convert it to an array of
! character variables with length one and add a null character to the end of the array. The s2c(3f) function helps do this.
character(kind=C_CHAR,len=1)    :: array(len_trim(string)+1)
integer                         :: i
   do i = 1,size(array)-1
      array(i) = string(i:i)
   enddo
   array(size(array):)=achar(0)
end function s2c
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      c2s(3f) - [M_strings:ARRAY] convert C string pointer to Fortran
!!      character string
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function c2s(c_string_pointer) result(f_string)
!!
!!     type(c_ptr), intent(in)       :: c_string_pointer
!!     character(len=:), allocatable :: f_string
!!
!!##DESCRIPTION
!!    Given a C pointer to a character string return a Fortran character
!!    string.
!!
!!##OPTIONS
!!    c_string_pointer  C pointer to convert
!!
!!##RETURNS
!!    f_string          Fortran character variable to return
!!
!!##EXAMPLE
!!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function c2s(c_string_pointer) result(f_string)
! gets a C string (pointer), and returns the corresponding Fortran string;
! If the C string is null, it returns "NULL", similar to C's "(null)" printed in similar cases:
use, intrinsic :: iso_c_binding, only: c_ptr,c_f_pointer,c_char,c_null_char

! ident_27="@(#)M_strings::c2s(3f): copy pointer to C char array till a null is encountered to a Fortran string up to 4096 characters"

integer,parameter                             :: max_length=4096
type(c_ptr), intent(in)                       :: c_string_pointer
character(len=:), allocatable                 :: f_string
character(kind=c_char), dimension(:), pointer :: char_array_pointer => null()
character(len=max_length)                            :: aux_string
integer                                       :: i,length=0

   call c_f_pointer(c_string_pointer,char_array_pointer,[max_length])
   if (.not.associated(char_array_pointer)) then
     allocate(character(len=4)::f_string)
     f_string="NULL"
     return
   endif
   aux_string=" "
   do i=1,max_length
     if (char_array_pointer(i)==c_null_char) then
       length=i-1
       exit
     endif
     aux_string(i:i)=char_array_pointer(i)
   enddo
   allocate(character(len=length)::f_string)
   f_string=aux_string(1:length)

end function c2s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      indent(3f) - [M_strings:WHITESPACE] count number of leading spaces
!!      in a string
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function indent(line)
!!
!!     integer                        :: indent
!!     character(len=*),intent(in)    :: line
!!
!!##DESCRIPTION
!!    Count number of leading spaces in a CHARACTER variable.
!!
!!##EXAMPLES
!!
!!  Sample Program:
!!
!!    program demo_indent
!!    !  test filter to count leading spaces in a character variable
!!    !  might want to call notabs(3f) to expand tab characters
!!    use M_strings, only : indent
!!    implicit none
!!    character(len=1024) :: in
!!    integer             :: ios
!!       READFILE: do
!!          read(*,'(A)',iostat=ios)in
!!          if(ios /= 0) exit READFILE
!!          write(*,'(i3,"",a)')indent(in),trim(in)
!!       enddo READFILE
!!    end program demo_indent
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function indent(line)
implicit none

! ident_28="@(#)M_strings::indent(3f): find number of leading spaces in a string"

integer                        :: indent
character(len=*),intent(in)    :: line
integer                        :: i
   indent=0
   NOTSPACE: block
      SCAN: do i=1,len(line)
         if(line(i:i).ne.' ')then
            indent=i-1
            exit NOTSPACE
         endif
      enddo SCAN
      indent=len(line)
   endblock NOTSPACE
end function indent
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    visible(3f) - [M_strings:NONALPHA] expand a string to control and
!!    meta-control representations
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function visible(input) result(output)
!!
!!     character(len=*),intent(in)           :: input
!!     character(len=:),allocatable          :: output
!!
!!##DESCRIPTION
!!     visible(3f) expands characters to commonly used sequences used
!!     to represent the characters as control sequences or meta-control
!!     sequences.
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!     program demo_visible
!!     use M_strings, only : visible
!!     integer :: i
!!        do i=0,255
!!           write(*,'(i0,1x,a)')i,visible(char(i))
!!        enddo
!!     end program demo_visible
!!##BUGS
!!     The expansion is not reversible, as input sequences such as "M-" or
!!     "^a" will look like expanded sequences.
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function visible(input) result(output)
character(len=*),intent(in)  :: input
character(len=:),allocatable :: output

! ident_29="@(#)M_strings::visible(3f) expand escape sequences in a string to control and meta-control representations"

integer                      :: i
character(len=1)             :: c

character(len=*),parameter :: chars(0:255)= [ &
'^@  ', '^A  ', '^B  ', '^C  ', '^D  ', '^E  ', '^F  ', '^G  ', '^H  ', '^I  ', &
'^J  ', '^K  ', '^L  ', '^M  ', '^N  ', '^O  ', '^P  ', '^Q  ', '^R  ', '^S  ', &
'^T  ', '^U  ', '^V  ', '^W  ', '^X  ', '^Y  ', '^Z  ', '^[  ', '^\  ', '^]  ', &
'^^  ', '^_  ', '    ', '!   ', '"   ', '#   ', '$   ', '%   ', '&   ', '''   ', &
'(   ', ')   ', '*   ', '+   ', ',   ', '-   ', '.   ', '/   ', '0   ', '1   ', &
'2   ', '3   ', '4   ', '5   ', '6   ', '7   ', '8   ', '9   ', ':   ', ';   ', &
'<   ', '=   ', '>   ', '?   ', '@   ', 'A   ', 'B   ', 'C   ', 'D   ', 'E   ', &
'F   ', 'G   ', 'H   ', 'I   ', 'J   ', 'K   ', 'L   ', 'M   ', 'N   ', 'O   ', &
'P   ', 'Q   ', 'R   ', 'S   ', 'T   ', 'U   ', 'V   ', 'W   ', 'X   ', 'Y   ', &
'Z   ', '[   ', '\   ', ']   ', '^   ', '_   ', '`   ', 'a   ', 'b   ', 'c   ', &
'd   ', 'e   ', 'f   ', 'g   ', 'h   ', 'i   ', 'j   ', 'k   ', 'l   ', 'm   ', &
'n   ', 'o   ', 'p   ', 'q   ', 'r   ', 's   ', 't   ', 'u   ', 'v   ', 'w   ', &
'x   ', 'y   ', 'z   ', '{   ', '|   ', '}   ', '~   ', '^?  ', 'M-^@', 'M-^A', &
'M-^B', 'M-^C', 'M-^D', 'M-^E', 'M-^F', 'M-^G', 'M-^H', 'M-^I', 'M-^J', 'M-^K', &
'M-^L', 'M-^M', 'M-^N', 'M-^O', 'M-^P', 'M-^Q', 'M-^R', 'M-^S', 'M-^T', 'M-^U', &
'M-^V', 'M-^W', 'M-^X', 'M-^Y', 'M-^Z', 'M-^[', 'M-^\', 'M-^]', 'M-^^', 'M-^_', &
'M-  ', 'M-! ', 'M-" ', 'M-# ', 'M-$ ', 'M-% ', 'M-& ', 'M-'' ', 'M-( ', 'M-) ', &
'M-* ', 'M-+ ', 'M-, ', 'M-- ', 'M-. ', 'M-/ ', 'M-0 ', 'M-1 ', 'M-2 ', 'M-3 ', &
'M-4 ', 'M-5 ', 'M-6 ', 'M-7 ', 'M-8 ', 'M-9 ', 'M-: ', 'M-; ', 'M-< ', 'M-= ', &
'M-> ', 'M-? ', 'M-@ ', 'M-A ', 'M-B ', 'M-C ', 'M-D ', 'M-E ', 'M-F ', 'M-G ', &
'M-H ', 'M-I ', 'M-J ', 'M-K ', 'M-L ', 'M-M ', 'M-N ', 'M-O ', 'M-P ', 'M-Q ', &
'M-R ', 'M-S ', 'M-T ', 'M-U ', 'M-V ', 'M-W ', 'M-X ', 'M-Y ', 'M-Z ', 'M-[ ', &
'M-\ ', 'M-] ', 'M-^ ', 'M-_ ', 'M-` ', 'M-a ', 'M-b ', 'M-c ', 'M-d ', 'M-e ', &
'M-f ', 'M-g ', 'M-h ', 'M-i ', 'M-j ', 'M-k ', 'M-l ', 'M-m ', 'M-n ', 'M-o ', &
'M-p ', 'M-q ', 'M-r ', 'M-s ', 'M-t ', 'M-u ', 'M-v ', 'M-w ', 'M-x ', 'M-y ', &
'M-z ', 'M-{ ', 'M-| ', 'M-} ', 'M-~ ', 'M-^?']
output=''
do i=1,len(input)
   c=input(i:i)
   if(c.eq.' ')then
      output=output//' '
   else
      output=output//trim(chars(ichar(c)))
   endif
enddo
end function visible
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    expand(3f) - [M_strings:NONALPHA] expand C-like escape sequences
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function expand(line,escape) result(lineout)
!!
!!    character(len=*)                      :: line
!!    character(len=1),intent(in),optional  :: escape
!!    character(len=:),allocatable          :: lineout
!!
!!##DESCRIPTION
!!     EXPAND() expands sequences used to represent commonly used escape
!!     sequences or control characters. By default ...
!!
!!     Escape sequences
!!       \      backslash
!!       a      alert (BEL) -- g is an alias for a
!!       b      backspace
!!       c      suppress further output
!!       e      escape
!!       f      form feed
!!       n      new line
!!       r      carriage return
!!       t      horizontal tab
!!       v      vertical tab
!!       oNNN   byte with octal value NNN (3 digits)
!!       dNNN   byte with decimal value NNN (3 digits)
!!       xHH    byte with hexadecimal value HH (2 digits) -- h is an alias for x
!!
!!     The default escape character is the backslash, but this may be
!!     changed using the optional parameter ESCAPE.
!!
!!##EXAMPLES
!!
!!    Sample Program:
!!
!!     program demo_expand
!!     !  test filter to expand escape sequences in input lines
!!     use M_strings, only : expand
!!     character(len=1024) :: line
!!     integer             :: ios
!!        READFILE: block
!!           do
!!              read(*,'(A)',iostat=ios)line
!!              if(ios /= 0) exit READFILE
!!              write(*,'(a)')trim(expand(line))
!!           enddo
!!        endblock READFILE
!!     end program demo_expand
!!
!!    Sample input:
!!
!!      \e[2J
!!      \tABC\tabc
!!      \tA\a
!!      \nONE\nTWO\nTHREE
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function expand(line,escape) result(lineout)
!*!USE ISO_C_BINDING ,ONLY: c_horizontal_tab
implicit none

! ident_30="@(#)M_strings::expand(3f): return string with escape sequences expanded"

character(len=*),parameter            :: c_horizontal_tab=char(9)
character(len=*),intent(in)           :: line
character(len=1),intent(in),optional  :: escape ! escape character. Default is backslash
! expand escape sequences found in input string
! Escape sequences
!    %%      escape character           %a     alert (BEL) -- gi is an alias for a
!    %b      backspace                  %c     suppress further output
!    %e      escape                     %E     escape
!    %f      form feed                  %n     new line
!    %r      carriage return            %t     horizontal tab
!    %v      vertical tab
!    %oNNN   byte with octal value NNN (3 digits)
!    %dNNN   byte with decimal value NNN (3 digits)
!    %xHH    byte with hexadecimal value HH (2 digits) -- h is an alias for x
character(len=1)                      :: esc    ! escape character. Default is %
character(len=:),allocatable          :: lineout
integer                               :: i
integer                               :: ilen
character(len=3)                      :: thr
integer                               :: xxx
integer                               :: ios
   i=0 ! pointer into input

   ilen=len_trim(line)
   lineout=''

   if(ilen.eq.0)return

   if (present(escape))then
      esc=escape
   else
      esc=char(92)
   endif

   EXP: do
      i=i+1
      if(i.gt.ilen)exit
      if(line(i:i).eq.esc)then
         i=i+1
         if(i.gt.ilen)exit
         if(line(i:i).ne.esc)then
            BACKSLASH: select case(line(i:i))
            case('a','A','g','G');lineout=lineout//char(  7) ! %a     alert (BEL)
            case('b','B');lineout=lineout//char(  8)         ! %b     backspace
            case('c','C');exit EXP                           ! %c     suppress further output
            case('d','D')                                    ! %d     Dnnn decimal value
                      thr=line(i+1:)
                   read(thr,'(i3)',iostat=ios)xxx
                      lineout=lineout//char(xxx)
                   i=i+3
            case('e','E');lineout=lineout//char( 27)         ! %e     escape
            case('f','F');lineout=lineout//char( 12)         ! %f     form feed
            case('n','N');lineout=lineout//char( 10)         ! %n     new line
          !!case('n','N');lineout=lineout//new_line('A')     ! %n     new line
            case('o','O')
                      thr=line(i+1:)
                   read(thr,'(o3)',iostat=ios)xxx
                      lineout=lineout//char(xxx)
                   i=i+3
            case('r','R');lineout=lineout//char( 13)         ! %r     carriage return
            case('t','T');lineout=lineout//c_horizontal_tab  ! %t     horizontal tab
            case('v','V');lineout=lineout//char( 11)         ! %v     vertical tab
            case('x','X','h','H')                            ! %x     xHH  byte with hexadecimal value HH (1 to 2 digits)
                      thr=line(i+1:)
                   read(thr,'(z2)',iostat=ios)xxx
                      lineout=lineout//char(xxx)
                   i=i+2
            end select BACKSLASH
         else
            lineout=lineout//esc                             ! escape character, defaults to backslash
         endif
      else
         lineout=lineout//line(i:i)
      endif
      if(i.ge.ilen)exit EXP
   enddo EXP

end function expand
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    notabs(3f) - [M_strings:NONALPHA] expand tab characters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine notabs(INSTR,OUTSTR,ILEN)
!!
!!     character(len=*),intent=(in)  :: INSTR
!!     character(len=*),intent=(out) :: OUTSTR
!!     integer,intent=(out)          :: ILEN
!!
!!##DESCRIPTION
!!     NOTABS() converts tabs in INSTR to spaces in OUTSTR while maintaining
!!     columns. It assumes a tab is set every 8 characters. Trailing spaces
!!     are removed.
!!
!!     In addition, trailing carriage returns and line feeds are removed
!!     (they are usually a problem created by going to and from MSWindows).
!!
!!     What are some reasons for removing tab characters from an input line?
!!     Some Fortran compilers have problems with tabs, as tabs are not
!!     part of the Fortran character set. Some editors and printers will
!!     have problems with tabs. It is often useful to expand tabs in input
!!     files to simplify further processing such as tokenizing an input line.
!!
!!##OPTIONS
!!     instr     Input line to remove tabs from
!!
!!##RESULTS
!!     outstr    Output string with tabs expanded.
!!     ilen      Significant length of returned string
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_notabs
!!
!!    !  test filter to remove tabs and trailing white space from input
!!    !  on files up to 1024 characters wide
!!    use M_strings, only : notabs
!!    character(len=1024) :: in,out
!!    integer             :: ios,iout
!!       do
!!          read(*,'(A)',iostat=ios)in
!!          if(ios /= 0) exit
!!          call notabs(in,out,iout)
!!          write(*,'(a)')out(:iout)
!!       enddo
!!    end program demo_notabs
!!
!!##SEE ALSO
!!     GNU/Unix commands expand(1) and unexpand(1)
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine notabs(INSTR,OUTSTR,ILEN)

! ident_31="@(#)M_strings::notabs(3f): convert tabs to spaces while maintaining columns, remove CRLF chars"

CHARACTER(LEN=*),INTENT(IN)   :: instr        ! input line to scan for tab characters
CHARACTER(LEN=*),INTENT(OUT)  :: outstr       ! tab-expanded version of INSTR produced
INTEGER,INTENT(OUT)           :: ilen         ! column position of last character put into output string
                                              ! that is, ILEN holds the position of the last non-blank character in OUTSTR
!===================================================================================================================================
INTEGER,PARAMETER             :: tabsize=8    ! assume a tab stop is set every 8th column
INTEGER                       :: ipos         ! position in OUTSTR to put next character of INSTR
INTEGER                       :: lenin        ! length of input string trimmed of trailing spaces
INTEGER                       :: lenout       ! number of characters output string can hold
INTEGER                       :: istep        ! counter that advances thru input string INSTR one character at a time
CHARACTER(LEN=1)              :: c            ! character in input line being processed
INTEGER                       :: iade         ! ADE (ASCII Decimal Equivalent) of character being tested
!===================================================================================================================================
   IPOS=1                                     ! where to put next character in output string OUTSTR
   lenin=LEN(instr)                           ! length of character variable INSTR
   lenin=LEN_TRIM(instr(1:lenin))             ! length of INSTR trimmed of trailing spaces
   lenout=LEN(outstr)                         ! number of characters output string OUTSTR can hold
   OUTSTR=" "                                 ! this SHOULD blank-fill string, a buggy machine required a loop to set all characters
!===================================================================================================================================
      SCAN_LINE: DO istep=1,lenin             ! look through input string one character at a time
         c=instr(istep:istep)                 ! get next character
         iade=ICHAR(c)                        ! get ADE of the character
         expand_tabs : SELECT CASE (iade)     ! take different actions depending on which character was found
         CASE(9)                              ! test if character is a tab and move pointer out to appropriate column
            ipos = ipos + (tabsize - (MOD(ipos-1,tabsize)))
         CASE(10,13)                          ! convert carriage-return and new-line to space ,typically to handle DOS-format files
            ipos=ipos+1
         CASE DEFAULT                         ! c is anything else other than a tab,newline,or return  insert it in output string
            IF(ipos > lenout)THEN
               CALL journal("*notabs* output string overflow")
               EXIT
            ELSE
               outstr(ipos:ipos)=c
               ipos=ipos+1
            ENDIF
         END SELECT expand_tabs
      enddo SCAN_LINE
!===================================================================================================================================
      ipos=MIN(ipos,lenout)                   ! tabs or newline or return characters or last character might have gone too far
      ilen=LEN_TRIM(outstr(:ipos))            ! trim trailing spaces
!===================================================================================================================================
END SUBROUTINE notabs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    dilate(3f) - [M_strings:NONALPHA] expand tab characters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function dilate(INSTR) result(OUTSTR)
!!
!!     character(len=*),intent=(in)  :: INSTR
!!     character(len=:),allocatable  :: OUTSTR
!!
!!##DESCRIPTION
!!     dilate() converts tabs in INSTR to spaces in OUTSTR.  It assumes a
!!     tab is set every 8 characters. Trailing spaces are removed.
!!
!!     In addition, trailing carriage returns and line feeds are removed
!!     (they are usually a problem created by going to and from MSWindows).
!!
!!##OPTIONS
!!     instr     Input line to remove tabs from
!!
!!##RESULTS
!!     outstr    Output string with tabs expanded.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_dilate
!!
!!    !  test filter to remove tabs and trailing white space from input
!!    !  on files up to 1024 characters wide
!!    use M_strings, only : dilate
!!    implicit none
!!    character(len=:),allocatable :: in
!!    character(len=:),allocatable :: out
!!    integer                      :: i
!!       in='  this is my string  '
!!       ! change spaces to tabs to make a sample input
!!       do i=1,len(in)
!!          if(in(i:i).eq.' ')in(i:i)=char(9)
!!       enddo
!!       write(*,'(a)')in,dilate(in)
!!    end program demo_dilate
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!    Public Domain
function dilate(INSTR) result(OUTSTR)

! ident_32="@(#)M_strings::dilate(3f): convert tabs to spaces and trims line, removing CRLF chars"

CHARACTER(LEN=*),INTENT(IN)   :: instr        ! input line to scan for tab characters
CHARACTER(LEN=:),allocatable  :: outstr       ! tab-expanded version of INSTR produced
integer                       :: i
integer                       :: icount
integer                       :: ilen
   icount=0
   do i=1,len(instr)
      if(instr(i:i).eq.char(9))icount=icount+1
   enddo
   allocate(character(len=(len(instr)+8*icount)) :: outstr)
   call notabs(instr,outstr,ilen)
   outstr=outstr(:ilen)
!===================================================================================================================================
END function dilate
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    adjustc(3f) - [M_strings:WHITESPACE] center text
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   pure function adjustc(string[,length])
!!
!!    character(len=*),intent(in)  :: string
!!    integer,intent(in),optional  :: length
!!    character(len=:),allocatable :: adjustc
!!
!!##DESCRIPTION
!!     Centers input text in a string of the length specified. Returns a
!!     string of length LENGTH if LENGTH is present. Otherwise returns a
!!     string of the length of the input string.
!!
!!##OPTIONS
!!     string  input string to trim and center
!!     length  line length to center text in, optional.
!!
!!##RETURNS
!!     adjustc  centered output string
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_adjustc
!!    use M_strings, only : adjustc
!!    !  using length of the input string
!!       write(*,'(a)')       '================================'
!!       write(*,'(a)')adjustc('centered string                 ')
!!       write(*,'(a)')adjustc('                 centered string')
!!       write(*,'(a)')adjustc('  centered string               ')
!!    !  using explicit output string length
!!       write(*,'(a)')repeat('=',50)
!!       write(*,'(a)')adjustc('this is a centered string',50)
!!       write(*,'(a)')repeat('=',50)
!!    end program demo_adjustc
!!
!!   Expected output
!!
!!    ================================
!!            centered string
!!            centered string
!!            centered string
!!    ==================================================
!!                this is a centered string
!!    ==================================================
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
pure function adjustc(string,length)

! ident_33="@(#)M_strings::adjustc(3f): center text"

!>
!! PROCEDURE   adjustc(3f)
!! DESCRIPTION center text using implicit or explicit length
!!##VERSION     2.0, 20160711
!! AUTHOR      John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: string         ! input string to trim and center
integer,intent(in),optional  :: length         ! line length to center text in
character(len=:),allocatable :: adjustc        ! output string
integer                      :: inlen
integer                      :: ileft          ! left edge of string if it is centered
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(length))then                     ! optional length
      inlen=length                             ! length will be requested length
      if(inlen.le.0)then                       ! bad input length
         inlen=len(string)                     ! could not use input value, fall back to length of input string
      endif
   else                                        ! output length was not explicitly specified, use input string length
      inlen=len(string)
   endif
   allocate(character(len=inlen):: adjustc)    ! create output at requested length
   adjustc(1:inlen)=' '                        ! initialize output string to all blanks
!-----------------------------------------------------------------------------------------------------------------------------------
   ileft =(inlen-len_trim(adjustl(string)))/2  ! find starting point to start input string to center it
   if(ileft.gt.0)then                          ! if string will fit centered in output
      adjustc(ileft+1:inlen)=adjustl(string)   ! center the input text in the output string
   else                                        ! input string will not fit centered in output string
      adjustc(1:inlen)=adjustl(string)         ! copy as much of input to output as can
   endif
end function adjustc
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    nospace(3f) - [M_strings:WHITESPACE] remove all whitespace from
!!    input string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function nospace(str) - remove all whitespace from input string
!!
!!     character(len=*),intent(in)          :: str
!!     character(len=:),allocatable         :: nospace
!!
!!##DESCRIPTION
!!    nospace(3f) removes space, tab, carriage return, new line, vertical
!!    tab, formfeed and null characters (called "whitespace"). The output
!!    is returned trimmed.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_nospace
!!     use M_strings, only: nospace
!!     implicit none
!!     character(len=:),allocatable  :: s
!!        s='  This     is      a     test  '
!!        write(*,*) 'original input string is ....',s
!!        write(*,*) 'processed output string is ...',nospace(s)
!!        if(nospace(s).eq.'Thisisatest')then
!!           write(*,*)'nospace test passed'
!!        else
!!           write(*,*)'nospace test error'
!!        endif
!!     end program demo_nospace
!!
!!   Expected output
!!
!!     original input string is ....  This     is      a     test
!!     processed output string is ...Thisisatest
!!     nospace test passed
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function nospace(line)

! ident_34="@(#)M_strings::nospace(3f): remove all whitespace from input string"

character(len=*),intent(in)    ::  line             ! remove whitespace from this string and return it
character(len=:),allocatable   ::  nospace          ! returned string
integer                        ::  ipos             ! position to place next output character at
integer                        ::  i                ! counter to increment from beginning to end of input string
!-----------------------------------------------------------------------------------------------------------------------------------
   allocate(nospace,mold=line)                      ! initially make output line length of input line
   nospace(:len_trim(nospace))=' '
   ipos=0
   do i=1,len_trim(line)                            ! increment from first to last character of the input line
      if ( isspace( line(i:i) ) ) cycle             ! if a blank is encountered skip it
      ipos=ipos+1                                   ! increment count of non-blank characters found
      nospace(ipos:ipos)=line(i:i)                  ! store non-blank character in output
   enddo
   nospace=trim(nospace)                            ! blank out unpacked part of line
end function nospace
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    stretch(3f) - [M_strings:LENGTH] return string padded to at least
!!    specified length
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function stretch(str,length,pattern,suffix) result(strout)
!!
!!     character(len=*),intent(in)         :: str
!!     integer,intent(in)                  :: length
!!     character(len=*)intent(in),optional :: pattern
!!     character(len=*)intent(in),optional :: suffix
!!     character(len=:),allocatable        :: strout
!!
!!##DESCRIPTION
!!    stretch(3f) pads a string with spaces to at least the specified
!!    length. If the trimmed input string is longer than the requested
!!    length the original string is returned trimmed of trailing spaces.
!!
!!##OPTIONS
!!    str      the input string to return trimmed, but then padded to
!!             the specified length if shorter than length
!!    length   The minimum string length to return
!!    pattern  optional string to use as padding. Defaults to a space.
!!    suffix   optional string to append to output string
!!
!!##RETURNS
!!    strout  The input string padded to the requested length or
!!            the trimmed input string if the input string is
!!            longer than the requested length.
!!
!!##EXAMPLE
!!
!!  Sample Program:
!!
!!   program demo_stretch
!!    use M_strings, only : stretch
!!    implicit none
!!    character(len=10)            :: string='abcdefghij'
!!    character(len=:),allocatable :: answer
!!    integer                      :: i
!!       answer=stretch(string,5)
!!       write(*,'("[",a,"]")') answer
!!       answer=stretch(string,20)
!!       write(*,'("[",a,"]")') answer
!!       i=30
!!       write(*,*)
!!       write(*,'(1x,a,i0)') &
!!        & stretch('CHAPTER 1 : The beginning ',i,'.'), 1    ,&
!!        & stretch('CHAPTER 2 : The end ',i,'.'),       1234 ,&
!!        & stretch('APPENDIX ',i,'.'),                  1235
!!       write(*,*)
!!       write(*,'(1x,a,i7)') &
!!        & stretch('CHAPTER 1 : The beginning ',i,'.'), 1    ,&
!!        & stretch('CHAPTER 2 : The end ',i,'.'),       1234 ,&
!!        & stretch('APPENDIX ',i,'.'),                  1235
!!       write(*,*)
!!       write(*,*) &
!!        & stretch('CHAPTER 1 : The beginning ',i,suffix=': '), 1
!!       write(*,*) &
!!        & stretch('CHAPTER 2 : The end ',i,suffix=': '),1234
!!       write(*,*) &
!!        & stretch('APPENDIX ',i,suffix=': '),           1235
!!   end program demo_stretch
!!
!!   Results:
!!
!!    [abcdefghij]
!!    [abcdefghij          ]
!!
!!     CHAPTER 1 : The beginning ....1
!!     CHAPTER 2 : The end ..........1234
!!     APPENDIX .....................1235
!!
!!     CHAPTER 1 : The beginning ....      1
!!     CHAPTER 2 : The end ..........   1234
!!     APPENDIX .....................   1235
!!
!!     CHAPTER 1 : The beginning     :            1
!!     CHAPTER 2 : The end           :         1234
!!     APPENDIX                      :         1235
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function stretch(line,length,pattern,suffix) result(strout)

! ident_35="@(#)M_strings::stretch(3f): return string padded to at least specified length"

character(len=*),intent(in)                  :: line
integer,intent(in)                           :: length
character(len=*),intent(in),optional         :: pattern
character(len=*),intent(in),optional         :: suffix
!-!character(len=max(length,len(trim(line)))) :: strout
character(len=:),allocatable                 :: strout
   if(present(pattern))then
      strout=atleast(line,length,pattern)
   else
      strout=atleast(line,length)
   endif
   if(present(suffix))then
      strout=strout//suffix
   endif
end function stretch
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!   atleast(3f) - [M_strings:LENGTH] return string padded to at least
!!   specified length
!!   (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!   function atleast(str,length,pattern) result(strout)
!!
!!    character(len=*)                           :: str
!!    integer,intent(in)                         :: length
!!    character(len=max(length,len(trim(line)))) ::  strout
!!    character(len=*),optional                  ::  pattern
!!
!!##DESCRIPTION
!!   atleast(3f) pads a string with spaces to at least the specified
!!   length. If the trimmed input string is longer than the requested
!!   length the trimmed string is returned.
!!
!!##OPTIONS
!!   str      the input string to return trimmed, but then padded to
!!            the specified length if shorter than length
!!   length   The minimum string length to return
!!   pattern  optional string to use as padding. Defaults to a space.
!!
!!##RETURNS
!!   strout  The input string padded to the requested length or
!!           the trimmed input string if the input string is
!!           longer than the requested length.
!!
!!##EXAMPLE
!!
!!  Sample Program:
!!
!!    program demo_atleast
!!     use M_strings, only : atleast
!!     implicit none
!!     character(len=10)            :: string='abcdefghij'
!!     character(len=:),allocatable :: answer
!!     integer                      :: i
!!        answer=atleast(string,5)
!!        write(*,'("[",a,"]")') answer
!!        answer=atleast(string,20)
!!        write(*,'("[",a,"]")') answer
!!        i=30
!!        write(*,*)
!!        write(*,'(1x,a,1x,i0)') &
!!         & atleast('CHAPTER 1 : The beginning ',i,'.'), 1   , &
!!         & atleast('CHAPTER 2 : The end ',i,'.'),       1234, &
!!         & atleast('APPENDIX ',i,'.'),                  1235
!!        write(*,*)
!!        write(*,'(1x,a,i7)') &
!!         & atleast('CHAPTER 1 : The beginning ',i,'.'), 1   , &
!!         & atleast('CHAPTER 2 : The end ',i,'.'),       1234, &
!!         & atleast('APPENDIX ',i,'.'),                  1235
!!    end program demo_atleast
!!
!!  Results:
!!
!!    [abcdefghij]
!!    [abcdefghij          ]
!!
!!     CHAPTER 1 : The beginning .... 1
!!     CHAPTER 2 : The end .......... 1234
!!     APPENDIX ..................... 1235
!!
!!     CHAPTER 1 : The beginning ....      1
!!     CHAPTER 2 : The end ..........   1234
!!     APPENDIX .....................   1235
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
function atleast(line,length,pattern) result(strout)

! ident_36="@(#)M_strings::atleast(3f): return string padded to at least specified length"

character(len=*),intent(in)                :: line
integer,intent(in)                         :: length
character(len=*),intent(in),optional       :: pattern
character(len=max(length,len(trim(line)))) :: strout
if(present(pattern))then
   strout=line//repeat(pattern,len(strout)/len(pattern)+1)
else
   strout=line
endif
end function atleast
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    lenset(3f) - [M_strings:LENGTH] return string trimmed or padded to specified length
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function lenset(str,length) result(strout)
!!
!!     character(len=*)                     :: str
!!     character(len=length)                :: strout
!!     integer,intent(in)                   :: length
!!
!!##DESCRIPTION
!!    lenset(3f) truncates a string or pads it with spaces to the specified
!!    length.
!!
!!##OPTIONS
!!    str     input string
!!    length  output string length
!!
!!##RESULTS
!!    strout  output string
!!
!!##EXAMPLE
!!
!!    Sample Program:
!!
!!     program demo_lenset
!!      use M_strings, only : lenset
!!      implicit none
!!      character(len=10)            :: string='abcdefghij'
!!      character(len=:),allocatable :: answer
!!         answer=lenset(string,5)
!!         write(*,'("[",a,"]")') answer
!!         answer=lenset(string,20)
!!         write(*,'("[",a,"]")') answer
!!     end program demo_lenset
!!
!!    Expected output:
!!
!!     [abcde]
!!     [abcdefghij          ]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function lenset(line,length) result(strout)

! ident_37="@(#)M_strings::lenset(3f): return string trimmed or padded to specified length"

character(len=*),intent(in)  ::  line
integer,intent(in)           ::  length
character(len=length)        ::  strout
   strout=line
end function lenset
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    merge_str(3f) - [M_strings:LENGTH] pads strings to same length and
!!    then calls MERGE(3f)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function merge_str(str1,str2,expr) result(strout)
!!
!!     character(len=*),intent(in),optional :: str1
!!     character(len=*),intent(in),optional :: str2
!!     logical,intent(in)              :: expr
!!     character(len=:),allocatable    :: strout
!!
!!##DESCRIPTION
!!    merge_str(3f) pads the shorter of str1 and str2 to the longest length
!!    of str1 and str2 and then calls MERGE(padded_str1,padded_str2,expr).
!!    It trims trailing spaces off the result and returns the trimmed
!!    string. This makes it easier to call MERGE(3f) with strings, as
!!    MERGE(3f) requires the strings to be the same length.
!!
!!    NOTE: STR1 and STR2 are always required even though declared optional.
!!          this is so the call "STR_MERGE(A,B,present(A))" is a valid call.
!!          The parameters STR1 and STR2 when they are optional parameters
!!          can be passed to a procedure if the options are optional on the
!!          called procedure.
!!
!!##OPTIONS
!!    STR1    string to return if the logical expression EXPR is true
!!    STR2    string to return if the logical expression EXPR is false
!!    EXPR    logical expression to evaluate to determine whether to return
!!            STR1 when true, and STR2 when false.
!!##RESULT
!!     MERGE_STR  a trimmed string is returned that is otherwise the value
!!                of STR1 or STR2, depending on the logical expression EXPR.
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!     program demo_merge_str
!!     use M_strings, only : merge_str
!!     implicit none
!!     character(len=:), allocatable :: answer
!!        answer=merge_str('first string', &
!!         & 'second string is longer',10.eq.10)
!!        write(*,'("[",a,"]")') answer
!!        answer=merge_str('first string', &
!!         & 'second string is longer',10.ne.10)
!!        write(*,'("[",a,"]")') answer
!!     end program demo_merge_str
!!
!!   Expected output
!!
!!     [first string]
!!     [second string is longer]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function merge_str(str1,str2,expr) result(strout)
! for some reason the MERGE(3f) intrinsic requires the strings it compares to be of equal length
! make an alias for MERGE(3f) that makes the lengths the same before doing the comparison by padding the shorter one with spaces

! ident_38="@(#)M_strings::merge_str(3f): pads first and second arguments to MERGE(3f) to same length"

character(len=*),intent(in),optional :: str1
character(len=*),intent(in),optional :: str2
character(len=:),allocatable         :: str1_local
character(len=:),allocatable         :: str2_local
logical,intent(in)                   :: expr
character(len=:),allocatable         :: strout
integer                              :: big
   if(present(str2))then
      str2_local=str2
   else
      str2_local=''
   endif
   if(present(str1))then
      str1_local=str1
   else
      str1_local=''
   endif
   big=max(len(str1_local),len(str2_local))
   ! note: perhaps it would be better to warn or fail if an optional value that is not present is returned, instead of returning ''
   strout=trim(merge(lenset(str1_local,big),lenset(str2_local,big),expr))
end function merge_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    compact(3f) - [M_strings:WHITESPACE] converts contiguous whitespace
!!    to a single character (or nothing)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function compact(STR,CHAR) result (OUTSTR)
!!
!!     character(len=*),intent(in)          :: STR
!!     character(len=*),intent(in),optional :: CHAR
!!     character(len=len(str))              :: OUTSTR
!!
!!##DESCRIPTION
!!    COMPACT(3f) converts multiple spaces, tabs and control characters
!!    (called "whitespace") to a single character or nothing. Leading
!!    whitespace is removed.
!!
!!##OPTIONS
!!    STR     input string to reduce or remove whitespace from
!!    CHAR    By default the character that replaces adjacent
!!            whitespace is a space. If the optional CHAR parameter is supplied
!!            it will be used to replace the whitespace. If a null character is
!!            supplied for CHAR whitespace is removed.
!!
!!##RETURNS
!!    OUTSTR  string of same length as input string but with all contiguous
!!            whitespace reduced to a single space and leading whitespace
!!            removed
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_compact
!!     use M_strings, only : compact
!!     implicit none
!!     ! produces 'This is a test               '
!!     write(*,*)compact('  This     is      a     test  ')
!!     ! produces 'Thisisatest                  '
!!     write(*,*)compact('  This     is      a     test  ',char='')
!!     ! produces 'This:is:a:test               '
!!     write(*,*)compact('  This     is      a     test  ',char=':')
!!     ! note CHAR is used to replace the whitespace, but if CHAR is
!!     ! in the original string it is just copied
!!     write(*,*)compact('A  AA    A   AAAAA',char='A')
!!     ! produces (original A characters are left as-is) 'AAAAAAAAAAAA'
!!     ! not 'A'
!!    end program demo_compact
!!
!!    Expected output
!!
!!     >This is a test
!!     >Thisisatest
!!     >This:is:a:test
!!     >AAAAAAAAAAAA
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!elemental pure function compact(str,char) result (outstr)
function compact(str,char) result (outstr)

! ident_39="@(#)M_strings::compact(3f): Converts white-space to single spaces"

character(len=*),intent(in)          :: str
character(len=*),intent(in),optional :: char
character(len=len(str))              :: outstr
character(len=1)                     :: ch
integer                              :: i
integer                              :: position_in_output
logical                              :: last_was_space
character(len=1)                     :: char_p
logical                              :: nospace
if(present(char))then
   char_p=char
   if(len(char).eq.0)then
      nospace=.true.
   else
      nospace=.false.
   endif
else
   char_p=' '
   nospace=.false.
endif
   outstr=' '
   last_was_space=.false.
   position_in_output=0

   IFSPACE: do i=1,len_trim(str)
     ch=str(i:i)
     select case(ichar(ch))
       case(0:32,127)                                         ! space or tab character or control character
         if(position_in_output.eq.0)then                      ! still at beginning so ignore leading whitespace
            cycle IFSPACE
         elseif(.not.last_was_space) then                     ! if have not already put out a space output one
           if(.not.nospace)then
              position_in_output=position_in_output+1
              outstr(position_in_output:position_in_output)=char_p
           endif
         endif
         last_was_space=.true.
       case(:-1,33:126,128:)                                  ! not a space, quote, or control character so copy it
         position_in_output=position_in_output+1
         outstr(position_in_output:position_in_output)=ch
         last_was_space=.false.
     end select
   enddo IFSPACE

end function compact
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     noesc(3f) - [M_strings:NONALPHA] convert non-printable characters
!!     to a space
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental function noesc(INSTR)
!!
!!     character(len=*),intent(in) :: INSTR
!!     character(len=len(instr))   :: noesc
!!
!!##DESCRIPTION
!!      Convert non-printable characters to a space.
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_noesc
!!
!!     use M_strings, only : noesc
!!     implicit none
!!     character(len=128) :: ascii
!!     character(len=128) :: cleared
!!     integer            :: i
!!     ! fill variable with base ASCII character set
!!     do i=1,128
!!        ascii(i:i)=char(i-1)
!!     enddo
!!     cleared=noesc(ascii)
!!     write(*,*)'characters and their ADE (ASCII Decimal Equivalent)'
!!     call ade(ascii)
!!     write(*,*)'Cleared of non-printable characters'
!!     call ade(cleared)
!!     write(*,*)'Cleared string:'
!!     write(*,*)cleared
!!     contains
!!       subroutine ade(string)
!!       implicit none
!!       ! the string to print
!!       character(len=*),intent(in) :: string
!!       ! number of characters in string to print
!!       integer :: ilen
!!       ! counter used to step thru string
!!       integer :: i
!!          ! get trimmed length of input string
!!          ilen=len_trim(string(:len(string)))
!!
!!          ! replace lower unprintable characters with spaces
!!          write(*,101)(merge(string(i:i),' ',&
!!          & ichar(string(i:i)).ge.32         &
!!          & .and.                            &
!!          & ichar(string(i:i)).le.126)       &
!!          & ,i=1,ilen)
!!
!!          ! print ADE value of character underneath it
!!          write(*,202)     (ichar(string(i:i))/100,    i=1,ilen)
!!          write(*,202)(mod( ichar(string(i:i)),100)/10,i=1,ilen)
!!          write(*,202)(mod((ichar(string(i:i))),10),   i=1,ilen)
!!       ! format for printing string characters
!!       101   format(*(a1:))
!!       ! format for printing ADE values
!!       202   format(*(i1:))
!!       end subroutine ade
!!     end program demo_noesc
!!
!!    Expected output
!!
!!    The string is printed with the ADE value vertically beneath.
!!    The original string has all the ADEs from 000 to 127. After
!!    NOESC(3f) is called on the string all the "non-printable"
!!    characters are replaced with a space (ADE of 032).
!!
!!   characters and their ADE (ASCII Decimal Equivalent)
!!
!!    >                                 !"#$%&'()*+,-./0123456789
!!    :;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
!!    >0000000000000000000000000000000000000000000000000000000000
!!    0000000000000000000000000000000000000000001111111111111111111111111111
!!    >00000000001111111111222222222233333333334444444444555555555566666666
!!    667777777777888888888899999999990000000000111111111122222222
!!    >012345678901234567890123456789012345678901234567890123456789012345678
!!    90123456789012345678901234567890123456789012345678901234567
!!
!!   Cleared of non-printable characters
!!
!!    >                                 !"#$%&'()*+,-./0123456789
!!    :;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
!!    >0000000000000000000000000000000000000000000000000000000000
!!    000000000000000000000000000000000000000000111111111111111111111111111
!!    >3333333333333333333333333333333333333333444444444455555555
!!    556666666666777777777788888888889999999999000000000011111111112222222
!!    >2222222222222222222222222222222223456789012345678901234567
!!    890123456789012345678901234567890123456789012345678901234567890123456
!!
!!   Cleared string:
!!
!!    >                                  !"#$%&'()*+,-./0123456789:;<=>?@
!!    ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental function noesc(INSTR)

! ident_40="@(#)M_strings::noesc(3f): convert non-printable characters to a space"

character(len=*),intent(in) :: INSTR      ! string that might contain nonprintable characters
character(len=len(instr))   :: noesc
integer                     :: ic,i10
!-----------------------------------------------------------------------------------------------------------------------------------
   noesc=''                               ! initialize output string
   do i10=1,len_trim(INSTR(1:len(INSTR)))
      ic=ichar(INSTR(i10:i10))
      if(ic.le.31.or.ic.eq.127)then       ! find characters with ADE of 0-31, 127
         noesc(I10:I10)=' '               ! replace non-printable characters with a space
      else
         noesc(I10:I10)=INSTR(i10:i10)    ! copy other characters as-is from input string to output string
      endif
   enddo
end function noesc
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      string_to_value(3f) - [M_strings:NUMERIC] subroutine returns numeric
!!      value from string
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine string_to_value(chars,valu,ierr)
!!
!!     character(len=*),intent(in)              :: chars   ! input string
!!     integer|real|doubleprecision,intent(out) :: valu
!!     integer,intent(out)                      :: ierr
!!
!!##DESCRIPTION
!!    Returns a numeric value from a numeric character string.
!!
!!    Works with any g-format input, including integer, real, and
!!    exponential. If the input string begins with "B", "Z", or "O"
!!    and otherwise represents a positive whole number it is assumed to
!!    be a binary, hexadecimal, or octal value. If the string contains
!!    commas they are removed. If the string is of the form NN:MMM... or
!!    NN#MMM then NN is assumed to be the base of the whole number.
!!
!!    If an error occurs in the READ, IOSTAT is returned in IERR and
!!    value is set to zero. if no error occurs, IERR=0.
!!
!!##OPTIONS
!!       CHARS  input string to read numeric value from
!!
!!##RETURNS
!!    VALU   numeric value returned. May be INTEGER, REAL, or
!!              DOUBLEPRECISION.
!!    IERR   error flag (0 == no error)
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_string_to_value
!!     use M_strings, only: string_to_value
!!     implicit none
!!     real :: value
!!     integer :: ierr
!!     character(len=80) :: string
!!        string=' -40.5e-2 '
!!        call string_to_value(string,value,ierr)
!!        write(*,*) 'value of string ['//trim(string)//'] is ',value
!!    end program demo_string_to_value
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine a2r(chars,valu,ierr)

! ident_41="@(#)M_strings::a2r(3fp): subroutine returns real value from string"

character(len=*),intent(in) :: chars                      ! input string
real,intent(out)            :: valu                       ! value read from input string
integer,intent(out)         :: ierr                       ! error flag (0 == no error)
doubleprecision             :: valu8
   valu8=0.0d0
   call a2d(chars,valu8,ierr,onerr=0.0d0)
   if(ierr.eq.0)then
      if(valu8.le.huge(valu))then
         valu=real(valu8)
      else
         call journal('sc','*a2r*','- value too large',valu8,'>',huge(valu))
         valu=huge(valu)
         ierr=-1
      endif
   endif
end subroutine a2r
!----------------------------------------------------------------------------------------------------------------------------------
subroutine a2i(chars,valu,ierr)

! ident_42="@(#)M_strings::a2i(3fp): subroutine returns integer value from string"

character(len=*),intent(in) :: chars                      ! input string
integer,intent(out)         :: valu                       ! value read from input string
integer,intent(out)         :: ierr                       ! error flag (0 == no error)
doubleprecision             :: valu8
   valu8=0.0d0
   call a2d(chars,valu8,ierr,onerr=0.0d0)
   if(valu8.le.huge(valu))then
      if(valu8.le.huge(valu))then
         valu=int(valu8)
      else
         call journal('sc','*a2i*','- value too large',valu8,'>',huge(valu))
         valu=huge(valu)
         ierr=-1
      endif
   endif
end subroutine a2i
!----------------------------------------------------------------------------------------------------------------------------------
subroutine a2d(chars,valu,ierr,onerr)

! ident_43="@(#)M_strings::a2d(3fp): subroutine returns double value from string"

!     1989,2016 John S. Urban.
!
!  o works with any g-format input, including integer, real, and exponential.
!  o if an error occurs in the read, iostat is returned in ierr and value is set to zero. If no error occurs, ierr=0.
!  o if the string happens to be 'eod' no error message is produced so this string may be used to act as an end-of-data.
!    IERR will still be non-zero in this case.
!----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: chars                        ! input string
character(len=:),allocatable :: local_chars
doubleprecision,intent(out)  :: valu                         ! value read from input string
integer,intent(out)          :: ierr                         ! error flag (0 == no error)
class(*),optional,intent(in) :: onerr
!----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter   :: fmt="('(bn,g',i5,'.0)')"     ! format used to build frmt
character(len=15)            :: frmt                         ! holds format built to read input string
character(len=256)           :: msg                          ! hold message from I/O errors
integer                      :: intg
integer                      :: pnd
integer                      :: basevalue, ivalu
character(len=3),save        :: nan_string='NaN'
!----------------------------------------------------------------------------------------------------------------------------------
   ierr=0                                                       ! initialize error flag to zero
   local_chars=unquote(chars)
   msg=''
   if(len(local_chars).eq.0)local_chars=' '
   call substitute(local_chars,',','')                          ! remove any comma characters
   pnd=scan(local_chars,'#:')
   if(pnd.ne.0)then
      write(frmt,fmt)pnd-1                                      ! build format of form '(BN,Gn.0)'
      read(local_chars(:pnd-1),fmt=frmt,iostat=ierr,iomsg=msg)basevalue   ! try to read value from string
      if(decodebase(local_chars(pnd+1:),basevalue,ivalu))then
         valu=real(ivalu,kind=kind(0.0d0))
      else
         valu=0.0d0
         ierr=-1
      endif
   else
      select case(local_chars(1:1))
      case('z','Z','h','H')                                     ! assume hexadecimal
         frmt='(Z'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('b','B')                                             ! assume binary (base 2)
         frmt='(B'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('o','O')                                             ! assume octal
         frmt='(O'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case default
         write(frmt,fmt)len(local_chars)                        ! build format of form '(BN,Gn.0)'
         read(local_chars,fmt=frmt,iostat=ierr,iomsg=msg)valu   ! try to read value from string
      end select
   endif
   if(ierr.ne.0)then                                            ! if an error occurred ierr will be non-zero.
      if(present(onerr))then
         select type(onerr)
         type is (integer)
            valu=onerr
         type is (real)
            valu=onerr
         type is (doubleprecision)
            valu=onerr
         end select
      else                                                      ! set return value to NaN
         read(nan_string,'(g3.3)')valu
      endif
      if(local_chars.ne.'eod')then                           ! print warning message except for special value "eod"
         call journal('sc','*a2d* - cannot produce number from string ['//trim(chars)//']')
         if(msg.ne.'')then
            call journal('sc','*a2d* - ['//trim(msg)//']')
         endif
      endif
   endif
end subroutine a2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    s2v(3f) - [M_strings:NUMERIC] function returns doubleprecision
!!    numeric value from a string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function s2v(string[,ierr][,onerr])
!!
!!     character(len=*)             :: string
!!     doubleprecision              :: s2v
!!     integer,intent(out),optional :: ierr
!!     class(*),intent(in),optional :: onerr
!!
!!##DESCRIPTION
!!    This function converts a string to a DOUBLEPRECISION numeric value.
!!
!!    The intrinsics INT(3f), REAL(3f), and DBLE(3f) are also extended
!!    to take CHARACTER variables. The KIND= keyword is not supported
!!    on the extensions.
!!
!!##OPTIONS
!!
!!     string   holds string assumed to represent a numeric value
!!     ierr     If an error occurs the program is stopped if the optional
!!              parameter IERR is not present. If IERR returns a non-zero
!!              value an error occurred.
!!     onerr    The value to return on error. A value of NaN is
!!              returned on error by default.
!!
!!##RETURNS
!!     s2v      numeric value read from string
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_s2v
!!
!!     use M_strings, only: s2v, int, real, dble
!!     implicit none
!!     character(len=8)              :: s=' 10.345 '
!!     integer                       :: i
!!     character(len=14),allocatable :: strings(:)
!!     doubleprecision               :: dv
!!     integer                       :: errnum
!!
!!     ! different strings representing INTEGER, REAL, and DOUBLEPRECISION
!!     strings=[&
!!     &' 10.345       ',&
!!     &'+10           ',&
!!     &'    -3        ',&
!!     &'    -4.94e-2  ',&
!!     &'0.1           ',&
!!     &'12345.678910d0',&
!!     &'              ',& ! Note: will return zero without an error message
!!     &'1 2 1 2 1 . 0 ',& ! Note: spaces will be ignored
!!     &'WHAT?         ']  ! Note: error messages will appear, zero returned
!!
!!     ! a numeric value is returned,
!!     ! so it can be used in numeric expression
!!     write(*,*) '1/2 value of string is ',s2v(s)/2.0d0
!!     write(*,*)
!!     write(*,*)' STRING            VALUE                    ERROR_NUMBER'
!!     do i=1,size(strings)
!!        ! Note: not a good idea to use s2v(3f) in a WRITE(3f) statement,
!!        ! as it does I/O when errors occur, so called on a separate line
!!        dv=s2v(strings(i),errnum)
!!        write(*,*) strings(i)//'=',dv,errnum
!!     enddo
!!     write(*,*)"Extended intrinsics"
!!     write(*,*)'given inputs:',s,strings(:8)
!!     write(*,*)'INT(3f):',int(s),int(strings(:8))
!!     write(*,*)'REAL(3f):',real(s),real(strings(:8))
!!     write(*,*)'DBLE(3f):',dble(s),dble(strings(:8))
!!     write(*,*)"That's all folks!"
!!
!!     end program demo_s2v
!!
!!    Expected output
!!
!!     >1/2 value of string is    5.1725000000000003
!!     >
!!     > STRING            VALUE                    ERROR_NUMBER
!!     > 10.345       =   10.345000000000001                0
!!     >+10           =   10.000000000000000                0
!!     >    -3        =  -3.0000000000000000                0
!!     >    -4.94e-2  =  -4.9399999999999999E-002           0
!!     >0.1           =  0.10000000000000001                0
!!     >12345.678910d0=   12345.678910000001                0
!!     >              =   0.0000000000000000                0
!!     >1 2 1 2 1 . 0 =   12121.000000000000                0
!!     >*a2d* - cannot produce number from string [WHAT?]
!!     >*a2d* - [Bad value during floating point read]
!!     >WHAT?         =   0.0000000000000000             5010
!!     >Extended intrinsics
!!     >given inputs: 10.345 10.345 +10 -3 -4.94e-2 0.1
!!     12345.678910d0 1 2 1 2 1 . 0
!!     >INT(3f): 10 10 10 -3 0 0 12345 0 12121
!!     >REAL(3f): 10.3450003 10.3450003 10.0000000 -3.00000000
!!     -4.94000018E-02
!!     >          0.100000001 12345.6787 0.00000000 12121.0000
!!     >DBLE(3f): 10.345000000000001 10.345000000000001
!!     10.000000000000000
!!     >          -3.0000000000000000 -4.9399999999999999E-002
!!     0.10000000000000001
!!     >          12345.678910000001 0.0000000000000000
!!     12121.000000000000
!!     >That's all folks!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!>
!!##PROCEDURE:
!! DESCRIPTION: s2v(3f): function returns doubleprecision number from string;zero if error occurs
!!##VERSION:     2.0, 20160704
!! AUTHOR:      John S. Urban
doubleprecision function s2v(chars,ierr,onerr)
!  1989 John S. Urban

! ident_44="@(#)M_strings::s2v(3f): returns doubleprecision number from string"


character(len=*),intent(in)  :: chars
integer,optional             :: ierr
doubleprecision              :: valu
integer                      :: ierr_local
class(*),intent(in),optional :: onerr

   ierr_local=0
   if(present(onerr))then
      call a2d(chars,valu,ierr_local,onerr)
   else
      call a2d(chars,valu,ierr_local)
   endif
   if(present(ierr))then ! if error is not returned stop program on error
      ierr=ierr_local
      s2v=valu
   elseif(ierr_local.ne.0)then
      write(*,*)'*s2v* stopped while reading '//trim(chars)
      stop 1
   else
      s2v=valu
   endif
end function s2v
!===================================================================================================================================
! calls to s2v(3f) for extending intrinsics int(3f), real(3f), dble(3f)
!===================================================================================================================================
doubleprecision function dble_s2v(chars)
character(len=*),intent(in) :: chars
   dble_s2v=s2v(chars)
end function dble_s2v
!===================================================================================================================================
real function real_s2v(chars)
character(len=*),intent(in) :: chars
   real_s2v=real(s2v(chars))
end function real_s2v
!===================================================================================================================================
integer function int_s2v(chars)
character(len=*),intent(in) :: chars
   int_s2v=int(s2v(chars))
end function int_s2v
!===================================================================================================================================
function ints_s2v(chars)
integer,allocatable         :: ints_s2v(:)
character(len=*),intent(in) :: chars(:)
integer                     :: i,isize
   isize=size(chars)
   allocate(ints_s2v(isize))
   do i=1,isize
      ints_s2v(i)=int(s2v(chars(i)))
   enddo
end function ints_s2v
!===================================================================================================================================
function reals_s2v(chars)
real,allocatable            :: reals_s2v(:)
character(len=*),intent(in) :: chars(:)
integer                     :: i,isize
   isize=size(chars)
   allocate(reals_s2v(isize))
   do i=1,isize
      reals_s2v(i)=real(s2v(chars(i)))
   enddo
end function reals_s2v
!===================================================================================================================================
function dbles_s2v(chars)
doubleprecision,allocatable :: dbles_s2v(:)
character(len=*),intent(in) :: chars(:)
integer                     :: i,isize
   isize=size(chars)
   allocate(dbles_s2v(isize))
   do i=1,isize
      dbles_s2v(i)=s2v(chars(i))
   enddo
end function dbles_s2v
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()())()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      value_to_string(3f) - [M_strings:NUMERIC] return numeric string
!!      from a numeric value
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine value_to_string(value,chars[,ilen,ierr,fmt,trimz])
!!
!!     character(len=*) :: chars  ! minimum of 23 characters required
!!     !--------
!!     ! VALUE may be any <em>one</em> of the following types:
!!     doubleprecision,intent(in)               :: value
!!     real,intent(in)                          :: value
!!     integer,intent(in)                       :: value
!!     logical,intent(in)                       :: value
!!     !--------
!!     character(len=*),intent(out)             :: chars
!!     integer,intent(out),optional             :: ilen
!!     integer,optional                         :: ierr
!!     character(len=*),intent(in),optional     :: fmt
!!     logical,intent(in)                       :: trimz
!!
!!##DESCRIPTION
!!    value_to_string(3f) returns a numeric representation of a numeric
!!    value in a string given a numeric value of type REAL, DOUBLEPRECISION,
!!    INTEGER or LOGICAL. It creates the string using internal writes. It
!!    then removes trailing zeros from non-zero values, and left-justifies
!!    the string.
!!
!!##OPTIONS
!!       VALUE   input value to be converted to a string
!!       FMT     You may specify a specific format that produces a string
!!               up to the length of CHARS; optional.
!!       TRIMZ   If a format is supplied the default is not to try to trim
!!               trailing zeros. Set TRIMZ to .true. to trim zeros from a
!!               string assumed to represent a simple numeric value.
!!
!!##RETURNS
!!       CHARS   returned string representing input value, must be at least
!!               23 characters long; or what is required by optional FMT
!!               if longer.
!!       ILEN    position of last non-blank character in returned string;
!!               optional.
!!       IERR    If not zero, error occurred; optional.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!      program demo_value_to_string
!!      use M_strings, only: value_to_string
!!      implicit none
!!      character(len=80) :: string
!!      integer           :: ilen
!!         call value_to_string(3.0/4.0,string,ilen)
!!         write(*,*) 'The value is [',string(:ilen),']'
!!
!!         call value_to_string(3.0/4.0,string,ilen,fmt='')
!!         write(*,*) 'The value is [',string(:ilen),']'
!!
!!         call value_to_string&
!!         &(3.0/4.0,string,ilen,fmt='("THE VALUE IS ",g0)')
!!         write(*,*) 'The value is [',string(:ilen),']'
!!
!!         call value_to_string(1234,string,ilen)
!!         write(*,*) 'The value is [',string(:ilen),']'
!!
!!         call value_to_string(1.0d0/3.0d0,string,ilen)
!!         write(*,*) 'The value is [',string(:ilen),']'
!!
!!      end program demo_value_to_string
!!
!!    Expected output
!!
!!     The value is [0.75]
!!     The value is [      0.7500000000]
!!     The value is [THE VALUE IS .750000000]
!!     The value is [1234]
!!     The value is [0.33333333333333331]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine value_to_string(gval,chars,length,err,fmt,trimz)

! ident_45="@(#)M_strings::value_to_string(3fp): subroutine returns a string from a value"

class(*),intent(in)                      :: gval
character(len=*),intent(out)             :: chars
integer,intent(out),optional             :: length
integer,optional                         :: err
integer                                  :: err_local
character(len=*),optional,intent(in)     :: fmt         ! format to write value with
logical,intent(in),optional              :: trimz
character(len=:),allocatable             :: fmt_local
character(len=1024)                      :: msg

!  Notice that the value GVAL can be any of several types ( INTEGER,REAL,DOUBLEPRECISION,LOGICAL)

   if (present(fmt)) then
      select type(gval)
      type is (integer)
         fmt_local='(i0)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (real)
         fmt_local='(bz,g23.10e3)'
         fmt_local='(bz,g0.8)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (doubleprecision)
         fmt_local='(bz,g0)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (logical)
         fmt_local='(l1)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      class default
         call journal('*value_to_string* UNKNOWN TYPE')
         chars=' '
      end select
      if(fmt.eq.'') then
         chars=adjustl(chars)
         call trimzeros_(chars)
      endif
   else                                                  ! no explicit format option present
      err_local=-1
      select type(gval)
      type is (integer)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (real)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (doubleprecision)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (logical)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      class default
         chars=''
      end select
      chars=adjustl(chars)
      if(index(chars,'.').ne.0) call trimzeros_(chars)
   endif
   if(present(trimz))then
      if(trimz)then
         chars=adjustl(chars)
         call trimzeros_(chars)
      endif
   endif

   if(present(length)) then
      length=len_trim(chars)
   endif

   if(present(err)) then
      err=err_local
   elseif(err_local.ne.0)then
      !! cannot currently do I/O from a function being called from I/O
      !!write(ERROR_UNIT,'(a)')'*value_to_string* WARNING:['//trim(msg)//']'
      chars=chars//' *value_to_string* WARNING:['//trim(msg)//']'
   endif

end subroutine value_to_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      v2s(3f) - [M_strings:NUMERIC] return numeric string from a numeric value
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       function v2s(value) result(outstr)
!!
!!        integer|real|doubleprecision|logical,intent(in ) :: value
!!        character(len=:),allocatable :: outstr
!!        character(len=*),optional,intent(in) :: fmt
!!
!!##DESCRIPTION
!!    v2s(3f) returns a representation of a numeric value as a
!!    string when given a numeric value of type REAL, DOUBLEPRECISION,
!!    INTEGER or LOGICAL. It creates the strings using internal WRITE()
!!    statements. Trailing zeros are removed from non-zero values, and the
!!    string is left-justified.
!!
!!##OPTIONS
!!    VALUE   input value to be converted to a string
!!    FMT     format can be explicitly given, but is limited to
!!            generating a string of eighty or less characters.
!!
!!##RETURNS
!!    OUTSTR  returned string representing input value,
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_v2s
!!    use M_strings, only: v2s
!!    write(*,*) 'The value of 3.0/4.0 is ['//v2s(3.0/4.0)//']'
!!    write(*,*) 'The value of 1234    is ['//v2s(1234)//']'
!!    write(*,*) 'The value of 0d0     is ['//v2s(0d0)//']'
!!    write(*,*) 'The value of .false. is ['//v2s(.false.)//']'
!!    write(*,*) 'The value of .true. is  ['//v2s(.true.)//']'
!!    end program demo_v2s
!!
!!   Expected output
!!
!!     The value of 3.0/4.0 is [0.75]
!!     The value of 1234    is [1234]
!!     The value of 0d0     is [0]
!!     The value of .false. is [F]
!!     The value of .true. is  [T]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
! very odd compiler problems in many (but not all) programs using this routine; GNU Fortran (GCC) 5.4.0; 20161030
function v2s_bug(gval) result(outstr)

! ident_46="@(#)M_strings::v2s_bug(3f): function returns string given numeric value"

class(*),intent(in)          :: gval                         ! input value to convert to a string
character(len=:),allocatable :: outstr                       ! output string to generate
character(len=80)            :: string
   call value_to_string(gval,string)
   outstr=trim(string)
end function v2s_bug
!===================================================================================================================================
function d2s(dvalue,fmt) result(outstr)

! ident_47="@(#)M_strings::d2s(3fp): private function returns string given doubleprecision value"

doubleprecision,intent(in)   :: dvalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)            :: string
   if(present(fmt))then
      call value_to_string(dvalue,string,fmt=fmt)
   else
      call value_to_string(dvalue,string)
   endif
   outstr=trim(string)
end function d2s
!===================================================================================================================================
function r2s(rvalue,fmt) result(outstr)

! ident_48="@(#)M_strings::r2s(3fp): private function returns string given real value"

real,intent(in)              :: rvalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)            :: string
   if(present(fmt))then
      call value_to_string(rvalue,string,fmt=fmt)
   else
      call value_to_string(rvalue,string)
   endif
   outstr=trim(string)
end function r2s
!===================================================================================================================================
function i2s(ivalue,fmt) result(outstr)

! ident_49="@(#)M_strings::i2s(3fp): private function returns string given integer value"

integer,intent(in)           :: ivalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)            :: string
   if(present(fmt))then
      call value_to_string(ivalue,string,fmt=fmt)
   else
      call value_to_string(ivalue,string)
   endif
   outstr=trim(string)
end function i2s
!===================================================================================================================================
function l2s(lvalue,fmt) result(outstr)

! ident_50="@(#)M_strings::l2s(3fp): private function returns string given logical value"

logical,intent(in)           :: lvalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)             :: string
   if(present(fmt))then
      call value_to_string(lvalue,string,fmt=fmt)
   else
      call value_to_string(lvalue,string)
   endif
   outstr=trim(string)
end function l2s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    isnumber(3f) - [M_strings:NUMERIC] determine if a string represents a number
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function isnumber(str,msg)
!!
!!     character(len=*),intent(in)  :: str
!!     character(len=:),intent(out),allocatable,optional  :: msg
!!
!!##DESCRIPTION
!!     ISNUMBER(3f) returns a value greater than zero if the string represents
!!     a number, and a number less than or equal to zero if it is a bad number.
!!     Blank characters are ignored.
!!
!!##OPTIONS
!!     str  the string to evaluate as to whether it represents a numeric value
!!          or not
!!     msg  An optional message describing the string
!!
!!##RETURNS
!!     isnumber  the following values are returned
!!
!!                1 for an integer             [-+]NNNNN
!!                2 for a whole number         [-+]NNNNN.
!!                3 for a real value           [-+]NNNNN.MMMM
!!                4 for a exponential value    [-+]NNNNN.MMMM[-+]LLLL
!!                                             [-+]NNNNN.MMMM[ed][-+]LLLL
!!
!!               values less than 1 represent an error
!!
!!##EXAMPLES
!!
!!   As the example shows, you can use an internal READ(3f) along with the
!!   IOSTAT= parameter to check (and read) a string as well.
!!
!!     program demo_isnumber
!!     use M_strings, only : isnumber
!!     implicit none
!!     character(len=256) :: line
!!     real               :: value
!!     integer            :: ios
!!     integer            :: answer
!!     character(len=256) :: message
!!     character(len=:),allocatable :: description
!!        write(*,*)'Begin entering values, one per line'
!!        do
!!           read(*,'(a)',iostat=ios)line
!!           !
!!           ! try string as number using list-directed input
!!           line=''
!!           read(line,*,iostat=ios,iomsg=message) value
!!           if(ios.eq.0)then
!!              write(*,*)'VALUE=',value
!!           elseif( is_iostat_end(ios) ) then
!!              stop 'end of file'
!!           else
!!              write(*,*)'ERROR:',ios,trim(message)
!!           endif
!!           !
!!           ! try string using isnumber(3f)
!!           answer=isnumber(line,msg=description)
!!           if(answer.gt.0)then
!!              write(*,*) &
!!              & ' for ',trim(line),' ',answer,':',description
!!           else
!!              write(*,*) &
!!              & ' ERROR for ',trim(line),' ',answer,':',description
!!           endif
!!           !
!!        enddo
!!     end program demo_isnumber
!!
!!  Example run
!!
!!    > Begin entering values
!!    > ERROR:          -1 End of file
!!    >  ERROR for            -1 :null string
!!    >10
!!    > VALUE=   10.0000000
!!    >  for 10            1 :integer
!!    >20
!!    > VALUE=   20.0000000
!!    >  for 20            1 :integer
!!    >20.
!!    > VALUE=   20.0000000
!!    >  for 20.            2 :whole number
!!    >30.1
!!    > VALUE=   30.1000004
!!    >  for 30.1            3 :real number
!!    >3e1
!!    > VALUE=   30.0000000
!!    >  for 3e1            4 :value with exponent
!!    >1-2
!!    > VALUE=   9.99999978E-03
!!    >  for 1-2            4 :value with exponent
!!    >100.22d-4
!!    > VALUE=   1.00220004E-02
!!    >  for 100.22d-4            4 :value with exponent
!!    >1--2
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for 1--2           -5 :bad number
!!    >e
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for e           -6 :missing leading value before exponent
!!    >e1
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for e1           -6 :missing leading value before exponent
!!    >1e
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for 1e           -3 :missing exponent
!!    >1e+
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for 1e+           -4 :missing exponent after sign
!!    >1e+2.0
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for 1e+2.0           -5 :bad number
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function isNumber(string,msg,verbose)
implicit none

! ident_51="@(#)M_strings::isnumber(3f): Determines if a string is a number of not."

character(len=*),intent(in)    :: string
character(len=:),intent(out),allocatable,optional :: msg
logical,intent(in),optional                      :: verbose
integer                      :: isnumber

integer             :: i,iend
character(len=1),allocatable :: z(:)
character(len=:),allocatable :: message
logical                      :: founddigit
logical                      :: verbose_local

   i=1
   founddigit=.false.
   isnumber=0
   z=switch(trim(nospace(string)))
   iend=size(z)
   message='not a number'
   if(present(verbose))then
      verbose_local=verbose
   else
      verbose_local=.false.
   endif
   DONE : block
      if(iend.eq.0)then
         isnumber=-1                   ! string is null
         message='null string'
         exit DONE
      endif

      if(index('+-',z(i)).ne.0) i=i+1  ! skip optional leading sign
      if(i.gt.iend)then
         isnumber=-2                   ! string was just a sign
         message='just a sign'
         exit DONE
      endif

      call next()                      ! position I to next non-digit or end of string+1

      if(i.gt.iend)then
         isnumber=1                    ! [+-]NNNNNN
         message='integer'
         exit DONE
      endif
      if(z(i).eq.'.')then              ! a period would be OK at this point
         i=i+1
      endif

      if(i.gt.iend)then                ! [+-]NNNNNN.
         isnumber=2
         message='whole number'
         exit DONE
      endif

      call next()                      ! position I to next non-digit or end of string+1
      if(i.gt.iend)then
         isnumber=3                    ! [+-]NNNNNN.MMMM
         message='real number'
         exit DONE
      endif

      if(index('eEdD',z(i)).ne.0)then
         i=i+1
         if(i.eq.2)then
            isnumber=-6                   ! [+-]NNNNNN[.[MMMM]]e but a value must follow
            message='missing leading value before exponent'
            exit DONE
         endif
      endif
      if(i.gt.iend)then
         isnumber=-3                   ! [+-]NNNNNN[.[MMMM]]e but a value must follow
         message='missing exponent'
         exit DONE
      endif
      if(.not.founddigit)then
         isnumber=-7
         message='missing value before exponent'
         exit DONE
      endif
      if(index('+-',z(i)).ne.0) i=i+1
      if(i.gt.iend)then
         isnumber=-4                   ! [+-]NNNNNN[.[MMMM]]e[+-] but a value must follow
         message='missing exponent after sign'
         exit DONE
      endif
      call next()                      ! position I to next non-digit or end of string+1
      if(i.gt.iend)then
         isnumber=4                    ! [+-]NNNNNN.MMMMe[+-]LL
         message='value with exponent'
         exit DONE
      endif
      isnumber=-5
      message='bad number'
   endblock DONE
   if(verbose_local)then
      write(*,*)trim(string)//' is '//message
   endif
   if(present(msg))then
      msg=message
   endif

contains
   subroutine next() ! move to next non-digit or end of string+1
      integer :: j
      do j=i,iend
         if(.not.isdigit(z(j)))then
            exit
         endif
         founddigit=.true.
         if(verbose_local) write(*,*)'I=',i,' J=',j,' Z(j)=',z(j)
      enddo
      i=j
      if(verbose_local)then
         write(*,*)'I and J=',i
         if(i.le.iend) then
            write(*,*)'Z(I)=',z(i)
         else
            write(*,*)'====>'
         endif
      endif
   end subroutine next
end function isNumber
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    trimzeros_(3fp) - [M_strings:NUMERIC] Delete trailing zeros from
!!    numeric decimal string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine trimzeros_(str)
!!
!!     character(len=*)  :: str
!!
!!##DESCRIPTION
!!    TRIMZEROS_(3f) deletes trailing zeros from a string representing a
!!    number. If the resulting string would end in a decimal point, one
!!    trailing zero is added.
!!
!!##OPTIONS
!!    str   input string will be assumed to be a numeric value and have
!!          trailing zeros removed
!!##EXAMPLES
!!
!!    Sample program:
!!
!!       program demo_trimzeros_
!!       use M_strings, only : trimzeros_
!!       character(len=:),allocatable :: string
!!          write(*,*)trimzeros_('123.450000000000')
!!          write(*,*)trimzeros_('12345')
!!          write(*,*)trimzeros_('12345.')
!!          write(*,*)trimzeros_('12345.00e3')
!!       end program demo_trimzeros_
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine trimzeros_(string)

! ident_52="@(#)M_strings::trimzeros_(3fp): Delete trailing zeros from numeric decimal string"

! if zero needs added at end assumes input string has room
character(len=*)             :: string
character(len=len(string)+2) :: str
character(len=len(string))   :: exp          ! the exponent string if present
integer                      :: ipos         ! where exponent letter appears if present
integer                      :: i, ii
   str=string                                ! working copy of string
   ipos=scan(str,'eEdD')                     ! find end of real number if string uses exponent notation
   if(ipos>0) then                           ! letter was found
      exp=str(ipos:)                         ! keep exponent string so it can be added back as a suffix
      str=str(1:ipos-1)                      ! just the real part, exponent removed will not have trailing zeros removed
   endif
   if(index(str,'.').eq.0)then               ! if no decimal character in original string add one to end of string
      ii=len_trim(str)
      str(ii+1:ii+1)='.'                     ! add decimal to end of string
   endif
   do i=len_trim(str),1,-1                   ! scanning from end find a non-zero character
      select case(str(i:i))
      case('0')                              ! found a trailing zero so keep trimming
         cycle
      case('.')                              ! found a decimal character at end of remaining string
         if(i.le.1)then
            str='0'
         else
            str=str(1:i-1)
         endif
         exit
      case default
         str=str(1:i)                        ! found a non-zero character so trim string and exit
         exit
      end select
   enddo
   if(ipos>0)then                            ! if originally had an exponent place it back on
      string=trim(str)//trim(exp)
   else
      string=str
   endif
end subroutine trimzeros_
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    listout(3f) - [M_strings:NUMERIC] expand a list of numbers where negative
!!    numbers denote range ends (1 -10 means 1 thru 10)
!! (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine listout(icurve_lists,icurve_expanded,inums,ierr)
!!
!!    integer,intent(in)    :: icurve_lists(:)
!!    integer,intent(out)   :: icurve_expanded(:)
!!    integer,intent(out)   :: inums
!!    integer,intent(out)   :: ierr
!!
!!##DESCRIPTION
!!    expand a list of whole numbers where negative numbers indicate a range.
!!    So [10,-20] would be expanded to [10,11,12,13,14,15,16,17,18,19,20].
!!
!!##OPTIONS
!!    icurve_lists(:)      input array
!!
!!##RETURNS
!!    icurve_expanded(:)   output array; assumed large enough to hold
!!                         returned list
!!    inums                number of icurve_expanded numbers on output
!!    ierr                 zero if no error occurred
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_listout
!!     use M_strings, only : listout
!!     implicit none
!!     integer,allocatable :: icurve_lists(:)
!!     integer :: icurve_expanded(1000)
!!     ! icurve_lists is input array
!!     integer :: inums
!!     ! icurve_expanded is output array
!!     integer :: i
!!     ! number of icurve_lists values on input,
!!     ! number of icurve_expanded numbers on output
!!     integer :: ierr
!!        icurve_lists=[1, 20, -30, 101, 100, 99, 100, -120, 222, -200]
!!        inums=size(icurve_lists)
!!        call listout(icurve_lists,icurve_expanded,inums,ierr)
!!        if(ierr.eq.0)then
!!           write(*,'(i0)')(icurve_expanded(i),i=1,inums)
!!        else
!!           write(*,'(a,i0)')'error occurred in *listout* ',ierr
!!           write(*,'(i0)')(icurve_expanded(i),i=1,inums)
!!        endif
!!     end program demo_listout
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine listout(icurve_lists,icurve_expanded,inums_out,ierr)
implicit none

! ident_53="@(#)M_strings::listout(3f): copy icurve_lists to icurve_expanded expanding negative numbers to ranges (1 -10 means 1 thru 10)"

!   Created: 19971231
integer,intent(in)    :: icurve_lists(:)             ! input array
integer,intent(out)   :: icurve_expanded(:)          ! output array
integer,intent(out)   :: inums_out                   ! number of icurve_expanded numbers on output
integer,intent(out)   :: ierr                        ! status variable

character(len=80)     :: temp1
integer               :: i80, i90
integer               :: imin, imax
integer               :: idirection, icount
integer               :: iin
integer               :: inums_max

   ierr=0
   icurve_expanded=0                          ! initialize output array
   inums_out=0                                ! initialize number of significant values in output array

   inums_max=size(icurve_expanded)
   if(inums_max.eq.0)then
      ierr=-2
      return
   endif

   iin=size(icurve_lists)
   if(iin.gt.0)then
      icurve_expanded(1)=icurve_lists(1)
   endif

   icount=2
      do i90=2,iin
         if(icurve_lists(i90).lt.0)then
            imax=abs(icurve_lists(i90))
            imin=abs(icurve_lists(i90-1))
            if(imin.gt.imax)then
               idirection=-1
               imin=imin-1
            elseif(imax.gt.imin)then
               idirection=1
               imin=imin+1
            else
               idirection=1
            endif
            do i80=imin,imax,idirection
               if(icount.gt.inums_max) then
                  write(temp1,'(a,i5,a)')'*listout* only ',inums_max,' values allowed'
                  ierr=-1
                  call journal(temp1)
                  inums_out=icount-1
                  exit
               endif
               icurve_expanded(icount)=i80
               icount=icount+1
            enddo
         else
            icurve_expanded(icount)=icurve_lists(i90)
            icount=icount+1
         endif
      enddo
   inums_out=icount-1

end subroutine listout
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!     quote(3f) - [M_strings:QUOTES] add quotes to string as if written
!!     with list-directed input
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function quote(str,mode,clip) result (quoted_str)
!!
!!    character(len=*),intent(in)          :: str
!!    character(len=*),optional,intent(in) :: mode
!!    logical,optional,intent(in)          :: clip
!!    character(len=:),allocatable         :: quoted_str
!!
!!##DESCRIPTION
!!    Add quotes to a CHARACTER variable as if it was written using
!!    list-directed input. This is particularly useful for processing
!!    strings to add to CSV files.
!!
!!##OPTIONS
!!    str    input string to add quotes to, using the rules of
!!           list-directed input (single quotes are replaced by two
!!           adjacent quotes)
!!    mode   alternate quoting methods are supported:
!!
!!             DOUBLE   default. replace quote with double quotes
!!             ESCAPE   replace quotes with backslash-quote instead of
!!                      double quotes
!!
!!    clip   default is to trim leading and trailing spaces from the
!!           string. If CLIP is .FALSE. spaces are not trimmed
!!
!!##RESULT
!!    quoted_str  The output string, which is based on adding quotes to STR.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_quote
!!    use M_strings, only : quote
!!    implicit none
!!    character(len=:),allocatable :: str
!!    character(len=1024)          :: msg
!!    integer                      :: ios
!!    character(len=80)            :: inline
!!       do
!!          write(*,'(a)',advance='no')'Enter test string:'
!!          read(*,'(a)',iostat=ios,iomsg=msg)inline
!!          if(ios.ne.0)then
!!             write(*,*)trim(inline)
!!             exit
!!          endif
!!
!!          ! the original string
!!          write(*,'(a)')'ORIGINAL     ['//trim(inline)//']'
!!
!!          ! the string processed by quote(3f)
!!          str=quote(inline)
!!          write(*,'(a)')'QUOTED     ['//str//']'
!!
!!          ! write the string list-directed to compare the results
!!          write(*,'(a)',iostat=ios,iomsg=msg) 'LIST DIRECTED:'
!!          write(*,*,iostat=ios,iomsg=msg,delim='none') inline
!!          write(*,*,iostat=ios,iomsg=msg,delim='quote') inline
!!          write(*,*,iostat=ios,iomsg=msg,delim='apostrophe') inline
!!       enddo
!!    end program demo_quote
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function quote(str,mode,clip) result (quoted_str)
character(len=*),intent(in)          :: str                ! the string to be quoted
character(len=*),optional,intent(in) :: mode
logical,optional,intent(in)          :: clip
character(len=:),allocatable         :: quoted_str

character(len=1),parameter           :: double_quote = '"'
character(len=20)                    :: local_mode

   if(present(clip))then
      if(clip)then
         quoted_str=adjustl(str)
      else
         quoted_str=str
      endif
   else
      quoted_str=str
   endif

   local_mode=merge_str(mode,'DOUBLE',present(mode))

   select case(lower(local_mode))
   case('double')
      quoted_str=double_quote//trim(replace(quoted_str,'"','""'))//double_quote
   case('escape')
      quoted_str=double_quote//trim(replace(quoted_str,'"','\"'))//double_quote
   case default
      call journal('sc','*quote* ERROR: unknown quote mode ',local_mode)
      quoted_str=str
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
end function quote
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!     unquote(3f) - [M_strings:QUOTES] remove quotes from string as if
!!     read with list-directed input
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function unquote(quoted_str,esc) result (unquoted_str)
!!
!!    character(len=*),intent(in)          :: quoted_str
!!    character(len=1),optional,intent(in) :: esc
!!    character(len=:),allocatable         :: unquoted_str
!!
!!##DESCRIPTION
!!    Remove quotes from a CHARACTER variable as if it was read using
!!    list-directed input. This is particularly useful for processing
!!    tokens read from input such as CSV files.
!!
!!    Fortran can now read using list-directed input from an internal file,
!!    which should handle quoted strings, but list-directed input does not
!!    support escape characters, which UNQUOTE(3f) does.
!!
!!##OPTIONS
!!    quoted_str  input string to remove quotes from, using the rules of
!!                list-directed input (two adjacent quotes inside a quoted
!!                region are replaced by a single quote, a single quote or
!!                double quote is selected as the delimiter based on which
!!                is encountered first going from left to right, ...)
!!    esc         optional character used to protect the next quote
!!                character from being processed as a quote, but simply as
!!                a plain character.
!!
!!##RESULT
!!    unquoted_str  The output string, which is based on removing quotes
!!                  from quoted_str.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_unquote
!!       use M_strings, only : unquote
!!       implicit none
!!       character(len=128)           :: quoted_str
!!       character(len=:),allocatable :: unquoted_str
!!       character(len=1),parameter   :: esc='\'
!!       character(len=1024)          :: msg
!!       integer                      :: ios
!!       character(len=1024)          :: dummy
!!       do
!!          write(*,'(a)',advance='no')'Enter test string:'
!!          read(*,'(a)',iostat=ios,iomsg=msg)quoted_str
!!          if(ios.ne.0)then
!!             write(*,*)trim(msg)
!!             exit
!!          endif
!!
!!          ! the original string
!!          write(*,'(a)')'QUOTED       ['//trim(quoted_str)//']'
!!
!!          ! the string processed by unquote(3f)
!!          unquoted_str=unquote(trim(quoted_str),esc)
!!          write(*,'(a)')'UNQUOTED     ['//unquoted_str//']'
!!
!!          ! read the string list-directed to compare the results
!!          read(quoted_str,*,iostat=ios,iomsg=msg)dummy
!!          if(ios.ne.0)then
!!             write(*,*)trim(msg)
!!          else
!!             write(*,'(a)')'LIST DIRECTED['//trim(dummy)//']'
!!          endif
!!       enddo
!!    end program demo_unquote
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function unquote(quoted_str,esc) result (unquoted_str)
character(len=*),intent(in)          :: quoted_str              ! the string to be unquoted
character(len=1),optional,intent(in) :: esc                     ! escape character
character(len=:),allocatable         :: unquoted_str
integer                              :: inlen
character(len=1),parameter           :: single_quote = "'"
character(len=1),parameter           :: double_quote = '"'
integer                              :: quote                   ! whichever quote is to be used
integer                              :: before
integer                              :: current
integer                              :: iesc
integer                              :: iput
integer                              :: i
logical                              :: inside
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(esc))then                           ! select escape character as specified character or special value meaning not set
      iesc=ichar(esc)                             ! allow for an escape character
   else
      iesc=-1                                     ! set to value that matches no character
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   inlen=len(quoted_str)                          ! find length of input string
   allocate(character(len=inlen) :: unquoted_str) ! initially make output string length of input string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(inlen.ge.1)then                             ! double_quote is the default quote unless the first character is single_quote
      if(quoted_str(1:1).eq.single_quote)then
         quote=ichar(single_quote)
      else
         quote=ichar(double_quote)
      endif
   else
      quote=ichar(double_quote)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   before=-2                                      ! initially set previous character to impossible value
   unquoted_str(:)=''                             ! initialize output string to null string
   iput=1
   inside=.false.
   STEPTHROUGH: do i=1,inlen
      current=ichar(quoted_str(i:i))
      if(before.eq.iesc)then                      ! if previous character was escape use current character unconditionally
           iput=iput-1                            ! backup
           unquoted_str(iput:iput)=char(current)
           iput=iput+1
           before=-2                              ! this could be second esc or quote
      elseif(current.eq.quote)then                ! if current is a quote it depends on whether previous character was a quote
         if(before.eq.quote)then
           unquoted_str(iput:iput)=char(quote)    ! this is second quote so retain it
           iput=iput+1
           before=-2
         elseif(.not.inside.and.before.ne.iesc)then
            inside=.true.
         else                                     ! this is first quote so ignore it except remember it in case next is a quote
            before=current
         endif
      else
         unquoted_str(iput:iput)=char(current)
         iput=iput+1
         before=current
      endif
   enddo STEPTHROUGH
!-----------------------------------------------------------------------------------------------------------------------------------
   unquoted_str=unquoted_str(:iput-1)
!-----------------------------------------------------------------------------------------------------------------------------------
end function unquote
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    describe(3f) - [M_strings] returns a string describing the name of
!!    a single character
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function describe(ch) result (string)
!!
!!     character(len=1),intent(in)   :: ch
!!     character(len=:),allocatable  :: string
!!
!!##DESCRIPTION
!!    describe(3f) returns a string describing long name of a single
!!    character
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_describe
!!     use M_strings, only : describe
!!     implicit none
!!     integer :: i
!!        do i=1,128  ! fill variable with base ASCII character set
!!           write(*,*)describe(char(i-1))
!!        enddo
!!    end program demo_describe
!!
!!   Expected output
!!
!!     ctrl-@ or ctrl-? (NUL) null
!!     ctrl-A (SOH) start of heading
!!     ctrl-B (STX) start of text
!!     ctrl-C (ETX) end of text
!!     ctrl-D (EOT) end of transmission
!!     ctrl-E (ENQ) enquiry
!!     ctrl-F (ACK) acknowledge
!!     ctrl-G (BEL) bell
!!     ctrl-H (BS) backspace
!!     ctrl-I (HT) horizontal tabulation
!!     ctrl-J (LF) line feed
!!     ctrl-K (VT) vertical tabulation
!!     ctrl-L (FF) form feed
!!     ctrl-M (CR) carriage return
!!     ctrl-N (SO) shift out
!!     ctrl-O (SI) shift in
!!     ctrl-P (DLE) data link escape
!!     ctrl-Q (DC1) device control 1
!!     ctrl-R (DC2) device control 2
!!     ctrl-S (DC3) device control 3
!!     ctrl-T (DC4) device control 4
!!     ctrl-U (NAK) negative acknowledge
!!     ctrl-V (SYN) synchronous idle
!!     ctrl-W (ETB) end of transmission block
!!     ctrl-X (CAN) cancel
!!     ctrl-Y (EM) end of medium
!!     ctrl-Z (SUB) substitute
!!     ctrl-[ (ESC) escape
!!     ctrl-\ or ctrl-@ (FS) file separator
!!     ctrl-] (GS) group separator
!!     ctrl-^ or ctrl-= (RS) record separator
!!     ctrl-_ (US) unit separator
!!     space
!!     ! exclamation point
!!     " quotation marks
!!     # number sign
!!     $ currency symbol
!!     % percent
!!     & ampersand
!!     ' apostrophe
!!     ( left parenthesis
!!     ) right parenthesis
!!     * asterisk
!!     + plus
!!     , comma
!!     - minus
!!     . period
!!     / slash
!!     0 zero
!!     1 one
!!     2 two
!!     3 three
!!     4 four
!!     5 five
!!     6 six
!!     7 seven
!!     8 eight
!!     9 nine
!!     : colon
!!     ; semicolon
!!     < less than
!!     = equals
!!     > greater than
!!     ? question mark
!!     @ at sign
!!     majuscule A
!!     majuscule B
!!     majuscule C
!!     majuscule D
!!     majuscule E
!!     majuscule F
!!     majuscule G
!!     majuscule H
!!     majuscule I
!!     majuscule J
!!     majuscule K
!!     majuscule L
!!     majuscule M
!!     majuscule N
!!     majuscule O
!!     majuscule P
!!     majuscule Q
!!     majuscule R
!!     majuscule S
!!     majuscule T
!!     majuscule U
!!     majuscule V
!!     majuscule W
!!     majuscule X
!!     majuscule Y
!!     majuscule Z
!!     [ left bracket
!!     \ backslash
!!     ] right bracket
!!     ^ caret
!!     _ underscore
!!     ` grave accent
!!     miniscule a
!!     miniscule b
!!     miniscule c
!!     miniscule d
!!     miniscule e
!!     miniscule f
!!     miniscule g
!!     miniscule h
!!     miniscule i
!!     miniscule j
!!     miniscule k
!!     miniscule l
!!     miniscule m
!!     miniscule n
!!     miniscule o
!!     miniscule p
!!     miniscule q
!!     miniscule r
!!     miniscule s
!!     miniscule t
!!     miniscule u
!!     miniscule v
!!     miniscule w
!!     miniscule x
!!     miniscule y
!!     miniscule z
!!     { left brace
!!     | vertical line
!!     } right brace
!!     ~ tilde
!!     ctrl-? (DEL) delete
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function describe(ch) result (string)

! ident_54="@(#)M_strings::describe(3f): return string describing long name of a single character"

character(len=1),intent(in)   :: ch
character(len=:),allocatable  :: string
! LATER: add hex, octal, decimal, key-press description, alternate names
!  ASCII character codes
   select case (ichar(ch))
   case(     0  ); STRING="ctrl-@ or ctrl-? (NUL) null"
   case(     1  ); STRING="ctrl-A (SOH) start of heading"
   case(     2  ); STRING="ctrl-B (STX) start of text"
   case(     3  ); STRING="ctrl-C (ETX) end of text"
   case(     4  ); STRING="ctrl-D (EOT) end of transmission"
   case(     5  ); STRING="ctrl-E (ENQ) enquiry"
   case(     6  ); STRING="ctrl-F (ACK) acknowledge"
   case(     7  ); STRING="ctrl-G (BEL) bell"
   case(     8  ); STRING="ctrl-H (BS) backspace"
   case(     9  ); STRING="ctrl-I (HT) horizontal tabulation"
   case(    10  ); STRING="ctrl-J (LF) line feed"
   case(    11  ); STRING="ctrl-K (VT) vertical tabulation"
   case(    12  ); STRING="ctrl-L (FF) form feed"
   case(    13  ); STRING="ctrl-M (CR) carriage return"
   case(    14  ); STRING="ctrl-N (SO) shift out"
   case(    15  ); STRING="ctrl-O (SI) shift in"
   case(    16  ); STRING="ctrl-P (DLE) data link escape"
   case(    17  ); STRING="ctrl-Q (DC1) device control 1"
   case(    18  ); STRING="ctrl-R (DC2) device control 2"
   case(    19  ); STRING="ctrl-S (DC3) device control 3"
   case(    20  ); STRING="ctrl-T (DC4) device control 4"
   case(    21  ); STRING="ctrl-U (NAK) negative acknowledge"
   case(    22  ); STRING="ctrl-V (SYN) synchronous idle"
   case(    23  ); STRING="ctrl-W (ETB) end of transmission block"
   case(    24  ); STRING="ctrl-X (CAN) cancel"
   case(    25  ); STRING="ctrl-Y (EM) end of medium"
   case(    26  ); STRING="ctrl-Z (SUB) substitute"
   case(    27  ); STRING="ctrl-[ (ESC) escape"
   case(    28  ); STRING="ctrl-\ or ctrl-@ (FS) file separator"
   case(    29  ); STRING="ctrl-] (GS) group separator"
   case(    30  ); STRING="ctrl-^ or ctrl-= (RS) record separator"
   case(    31  ); STRING="ctrl-_ (US) unit separator"
   case(    32  ); STRING="space"
   case(    33  ); STRING="! exclamation point (screamer, gasper, slammer, startler, bang, shriek, pling)"
   case(    34  ); STRING=""" quotation marks"
   case(    35  ); STRING="# number sign (hash, pound sign, hashtag)"
   case(    36  ); STRING="$ currency symbol"
   case(    37  ); STRING="% percent"
   case(    38  ); STRING="& ampersand"
   case(    39  ); STRING="' apostrophe"
   case(    40  ); STRING="( left parenthesis"
   case(    41  ); STRING=") right parenthesis"
   case(    42  ); STRING="* asterisk"
   case(    43  ); STRING="+ plus"
   case(    44  ); STRING=", comma"
   case(    45  ); STRING="- minus"
   case(    46  ); STRING=". period"
   case(    47  ); STRING="/ slash"
   case(    48  ); STRING="0 zero"
   case(    49  ); STRING="1 one"
   case(    50  ); STRING="2 two"
   case(    51  ); STRING="3 three"
   case(    52  ); STRING="4 four"
   case(    53  ); STRING="5 five"
   case(    54  ); STRING="6 six"
   case(    55  ); STRING="7 seven"
   case(    56  ); STRING="8 eight"
   case(    57  ); STRING="9 nine"
   case(    58  ); STRING=": colon"
   case(    59  ); STRING="; semicolon"
   case(    60  ); STRING="< less than"
   case(    61  ); STRING="= equals"
   case(    62  ); STRING="> greater than"
   case(    63  ); STRING="? question mark"
   case(    64  ); STRING="@ at sign"
   case(    65  ); STRING="A majuscule A"
   case(    66  ); STRING="B majuscule B"
   case(    67  ); STRING="C majuscule C"
   case(    68  ); STRING="D majuscule D"
   case(    69  ); STRING="E majuscule E"
   case(    70  ); STRING="F majuscule F"
   case(    71  ); STRING="G majuscule G"
   case(    72  ); STRING="H majuscule H"
   case(    73  ); STRING="I majuscule I"
   case(    74  ); STRING="J majuscule J"
   case(    75  ); STRING="K majuscule K"
   case(    76  ); STRING="L majuscule L"
   case(    77  ); STRING="M majuscule M"
   case(    78  ); STRING="N majuscule N"
   case(    79  ); STRING="O majuscule O"
   case(    80  ); STRING="P majuscule P"
   case(    81  ); STRING="Q majuscule Q"
   case(    82  ); STRING="R majuscule R"
   case(    83  ); STRING="S majuscule S"
   case(    84  ); STRING="T majuscule T"
   case(    85  ); STRING="U majuscule U"
   case(    86  ); STRING="V majuscule V"
   case(    87  ); STRING="W majuscule W"
   case(    88  ); STRING="X majuscule X"
   case(    89  ); STRING="Y majuscule Y"
   case(    90  ); STRING="Z majuscule Z"
   case(    91  ); STRING="[ left bracket"
   case(    92  ); STRING="\ backslash"
   case(    93  ); STRING="] right bracket"
   case(    94  ); STRING="^ caret"
   case(    95  ); STRING="_ underscore"
   case(    96  ); STRING="` grave accent"
   case(    97  ); STRING="a miniscule a"
   case(    98  ); STRING="b miniscule b"
   case(    99  ); STRING="c miniscule c"
   case(   100  ); STRING="d miniscule d"
   case(   101  ); STRING="e miniscule e"
   case(   102  ); STRING="f miniscule f"
   case(   103  ); STRING="g miniscule g"
   case(   104  ); STRING="h miniscule h"
   case(   105  ); STRING="i miniscule i"
   case(   106  ); STRING="j miniscule j"
   case(   107  ); STRING="k miniscule k"
   case(   108  ); STRING="l miniscule l"
   case(   109  ); STRING="m miniscule m"
   case(   110  ); STRING="n miniscule n"
   case(   111  ); STRING="o miniscule o"
   case(   112  ); STRING="p miniscule p"
   case(   113  ); STRING="q miniscule q"
   case(   114  ); STRING="r miniscule r"
   case(   115  ); STRING="s miniscule s"
   case(   116  ); STRING="t miniscule t"
   case(   117  ); STRING="u miniscule u"
   case(   118  ); STRING="v miniscule v"
   case(   119  ); STRING="w miniscule w"
   case(   120  ); STRING="x miniscule x"
   case(   121  ); STRING="y miniscule y"
   case(   122  ); STRING="z miniscule z"
   case(   123  ); STRING="{ left brace"
   case(   124  ); STRING="| vertical line"
   case(   125  ); STRING="} right brace"
   case(   126  ); STRING="~ tilde"
   case(   127  ); STRING="ctrl-? (DEL) delete"
   case default
         STRING='UNKNOWN'//v2s(ICHAR(ch))
   end select
end function describe
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    getvals(3f) - [M_strings:NUMERIC] read arbitrary number of REAL values
!!    from a character variable up to size of VALUES() array
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine getvals(line,values,icount,ierr)
!!
!!     character(len=*),intent(in)  :: line
!!     class(*),intent(out)         :: values(:)
!!     integer,intent(out)          :: icount
!!     integer,intent(out),optional :: ierr
!!
!!##DESCRIPTION
!!   GETVALS(3f) reads a relatively arbitrary number of numeric values from
!!   a character variable into a REAL array using list-directed input.
!!
!!   NOTE: In this version null values are skipped instead of meaning to leave
!!         that value unchanged
!!
!!        1,,,,,,,2 / reads VALUES=[1.0,2.0]
!!
!!   Per list-directed rules when reading values, allowed delimiters are
!!   comma, semi-colon and space.
!!
!!   the slash separator can be used to add inline comments.
!!
!!        10.1, 20.43e-1 ; 11 / THIS IS TREATED AS A COMMENT
!!
!!   Repeat syntax can be used up to the size of the output array. These are
!!   equivalent input lines:
!!
!!        4*10.0
!!        10.0, 10.0, 10.0, 10.0
!!
!!##OPTIONS
!!   LINE      A character variable containing the characters representing
!!             a list of numbers
!!
!!##RETURNS
!!   VALUES()  array holding numbers read from string. May be of type
!!             INTEGER, REAL, DOUBLEPRECISION, or CHARACTER. If CHARACTER the
!!             strings are returned as simple words instead of numeric values.
!!   ICOUNT    number of defined numbers in VALUES(). If ICOUNT reaches
!!             the size of the VALUES() array parsing stops.
!!   IERR      zero if no error occurred in reading numbers. Optional.
!!             If not present and an error occurs the program is terminated.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!       program demo_getvals
!!       use M_strings, only: getvals
!!       implicit none
!!       integer,parameter  :: longest_line=256
!!       character(len=longest_line) :: line
!!       real               :: values(longest_line/2+1)
!!       integer            :: ios,icount,ierr
!!       INFINITE: do
!!          read(*,'(a)',iostat=ios) line
!!          if(ios.ne.0)exit INFINITE
!!          call getvals(line,values,icount,ierr)
!!          write(*,'(4(g0,1x))')'VALUES=',values(:icount)
!!       enddo INFINITE
!!       end program demo_getvals
!!
!!  Sample input lines
!!
!!        10,20 30.4
!!        1 2 3
!!        1
!!
!!        3 4*2.5 8
!!        32.3333 / comment 1
!!        30e3;300,    30.0, 3
!!        even 1 like this! 10
!!        11,,,,22,,,,33
!!
!!  Expected output:
!!
!!     VALUES=   10.0000000       20.0000000       30.3999996
!!     VALUES=   1.00000000       2.00000000       3.00000000
!!     VALUES=   1.00000000
!!     VALUES=
!!     VALUES=   3.00000000       2.50000000       2.50000000
!!     2.50000000       2.50000000       8.00000000
!!     VALUES=   32.3333015
!!     VALUES=   30000.0000       300.000000       30.0000000
!!     3.00000000
!!     *getvals* WARNING:[even] is not a number
!!     *getvals* WARNING:[like] is not a number
!!     *getvals* WARNING:[this!] is not a number
!!     VALUES=   1.00000000       10.0000000
!!     VALUES=   11.0000000       22.0000000       33.0000000
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine getvals(line,values,icount,ierr)
implicit none

! ident_55="@(#)M_strings::getvals(3f): read arbitrary number of values from a character variable"

! JSU 20170831

character(len=*),intent(in)  :: line
class(*),intent(out)         :: values(:)
integer,intent(out)          :: icount
integer,intent(out),optional :: ierr

character(len=:),allocatable :: buffer
character(len=len(line))     :: words(size(values))
integer                      :: ios, i, ierr_local,isize

   isize=0
   select type(values)
   type is (integer);          isize=size(values)
   type is (real);             isize=size(values)
   type is (doubleprecision);  isize=size(values)
   type is (character(len=*)); isize=size(values)
   end select

   ierr_local=0

   words=' '                            ! make sure words() is initialized to null+blanks
   buffer=trim(unquote(line))//"/"      ! add a slash to the end so how the read behaves with missing values is clearly defined
   read(buffer,*,iostat=ios) words      ! undelimited strings are read into an array
   icount=0
   do i=1,isize                         ! loop thru array and convert non-blank words to numbers
      if(words(i).eq.' ')cycle

      select type(values)
      type is (integer);          read(words(i),*,iostat=ios)values(icount+1)
      type is (real);             read(words(i),*,iostat=ios)values(icount+1)
      type is (doubleprecision);  read(words(i),*,iostat=ios)values(icount+1)
      type is (character(len=*)); values(icount+1)=words(i)
      end select

      if(ios.eq.0)then
         icount=icount+1
      else
         ierr_local=ios
         write(ERROR_UNIT,*)'*getvals* WARNING:['//trim(words(i))//'] is not a number of specified type'
      endif
   enddo

   if(present(ierr))then
      ierr=ierr_local
   elseif(ierr_local.ne.0)then        ! error occurred and not returning error to main program to print message and stop program
      write(ERROR_UNIT,*)'*getval* error reading line ['//trim(line)//']'
      stop 2
   endif

end subroutine getvals
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      string_to_values(3f) - [M_strings:NUMERIC] read a string representing
!!      numbers into a numeric array
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine string_to_values(line,iread,values,inums,delims,ierr)
!!
!!        character(len=*) :: line
!!        integer          :: iread
!!        real             :: values(*)
!!        integer          :: inums
!!        character(len=*) :: delims
!!        integer          :: ierr
!!
!!##DESCRIPTION
!!    This routine can take a string representing a series of numbers and
!!    convert it to a numeric array and return how many numbers were found.
!!
!!##OPTIONS
!!       LINE     Input string containing numbers
!!       IREAD    maximum number of values to try to read from input string
!!
!!##RESULTS
!!       VALUES   real array to be filled with numbers
!!       INUMS    number of values successfully read (before error occurs
!!                if one does)
!!       DELIMS   delimiter character(s), usually a space. must not be a
!!                null string. If more than one character, a space must
!!                not be the last character or it will be ignored.
!!       IERR     error flag (0=no error, else column number string starts
!!                at that error occurred on).
!!
!!##EXAMPLE
!!
!!    Sample Program:
!!
!!      program demo_string_to_values
!!       use M_strings, only : string_to_values
!!       implicit none
!!       character(len=80)  :: s=' 10 20e3;3.45 -400.3e-2;1234; 5678 '
!!       integer,parameter  :: isz=10
!!       real               :: array(isz)
!!       integer            :: inums, ierr, ii
!!
!!       call string_to_values(s,10,array,inums,' ;',ierr)
!!       call reportit()
!!
!!       call string_to_values('10;2.3;3.1416',isz,array,inums,' ;',ierr)
!!       call reportit()
!!
!!       contains
!!          subroutine reportit()
!!             write(*,*)'string_to_values:'
!!             write(*,*)'input string.............',trim(s)
!!             write(*,*)'number of values found...',inums
!!             write(*,*)'values...................',(array(ii),ii=1,inums)
!!          end subroutine reportit
!!      end program demo_string_to_values
!!
!!    Expected output
!!
!!     string_to_values:
!!     input string............. 10 20e3;3.45 -400.3e-2;1234; 5678
!!     number of values found...           6
!!     values...................   10.0000000  20000.0000  3.45000005
!!     -4.00299978  1234.00000  5678.00000
!!     string_to_values:
!!     input string............. 10 20e3;3.45 -400.3e-2;1234; 5678
!!     number of values found...           3
!!     values...................   10.0000000  2.29999995  3.14159989
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine string_to_values(line,iread,values,inums,delims,ierr)
implicit none
!----------------------------------------------------------------------------------------------------------------------------------
!   1989,1997-12-31,2014 John S. Urban

!   given a line of structure , string , string , string process each
!   string as a numeric value and store into an array.
!   DELIMS contain the legal delimiters. If a space is an allowed delimiter, it must not appear last in DELIMS.
!   There is no direct checking for more values than can fit in VALUES.
!   Quits if encounters any errors in read.
!----------------------------------------------------------------------------------------------------------------------------------

! ident_56="@(#)M_strings::string_to_values(3f): reads an array of numbers from a numeric string"

character(len=*),intent(in)  :: line          ! input string
integer,intent(in)           :: iread         ! maximum number of values to try to read into values
real,intent(inout)           :: values(iread) ! real array to be filled with values
integer,intent(out)          :: inums         ! number of values successfully read from string
character(len=*),intent(in)  :: delims        ! allowed delimiters
integer,intent(out)          :: ierr          ! 0 if no error, else column number undecipherable string starts at
!----------------------------------------------------------------------------------------------------------------------------------
character(len=256)           :: delims_local        ! mutable copy of allowed delimiters
integer                      :: istart,iend,ilen,icol
integer                      :: i10,i20,i40
real                         :: rval
integer                      :: ier
integer                      :: delimiters_length
!----------------------------------------------------------------------------------------------------------------------------------
      delims_local=delims                                 ! need a mutable copy of the delimiter list
      if(delims_local.eq.'')then                          ! if delimiter list is null or all spaces make it a space
         delims_local=' '                                 ! delimiter is a single space
         delimiters_length=1                        ! length of delimiter list
      else
         delimiters_length=len_trim(delims)         ! length of variable WITH TRAILING WHITESPACE TRIMMED
      endif
!----------------------------------------------------------------------------------------------------------------------------------
      ierr=0                                        ! initialize error code returned
      inums=0                                       ! initialize count of values successfully returned
      istart=0
!----------------------------------------------------------------------------------------------------------------------------------
      ilen=0                                        ! ilen will be the position of the right-most non-delimiter in the input line
      do i20=len(line),1,-1                         ! loop from end of string to beginning to find right-most non-delimiter
         if(index(delims_local(:delimiters_length),line(i20:i20)).eq.0)then   ! found a non-delimiter
            ilen=i20
            exit
         endif
      enddo
      if(ilen.eq.0)then                             ! command was totally composed of delimiters
         call journal('*string_to_values* blank line passed as a list of numbers')
         return
      endif
!----------------------------------------------------------------------------------------------------------------------------------
!     there is at least one non-delimiter sub-string
!     ilen is the column position of the last non-delimiter character
!     now, starting at beginning of string find next non-delimiter
      icol=1                                                     ! pointer to beginning of unprocessed part of LINE
      LOOP: dO i10=1,iread,1                                     ! each pass should find a value
         if(icol.gt.ilen) EXIT LOOP                              ! everything is done
         INFINITE: do
            if(index(delims_local(:delimiters_length),line(icol:icol)).eq.0)then           ! found non-delimiter
               istart=icol
               iend=0                                            ! FIND END OF SUBSTRING
               do i40=istart,ilen                                ! look at each character starting at left
                  if(index(delims_local(:delimiters_length),line(i40:i40)).ne.0)then       ! determine if character is a delimiter
                     iend=i40                                    ! found a delimiter. record where it was found
                     EXIT                                        ! found end of substring so leave loop
                  endif
               enddo
              if(iend.eq.0)iend=ilen+1                           ! no delimiters found, so this substring goes to end of line
               iend=iend-1                                       ! do not want to pass delimiter to be converted
               rval=0.0
               call string_to_value(line(istart:iend),rval,ier)  ! call procedure to convert string to a numeric value
               if(ier.eq.0)then                                  ! a substring was successfully converted to a numeric value
                  values(i10)=rval                               ! store numeric value in return array
                  inums=inums+1                                  ! increment number of values converted to a numeric value
               else                                              ! an error occurred converting string to value
                  ierr=istart                                    ! return starting position of substring that could not be converted
                  return
               endif
               icol=iend+2                                       ! set to next character to look at
               CYCLE LOOP                                        ! start looking for next value
            else                                                 ! this is a delimiter so keep looking for start of next string
               icol=icol+1                                       ! increment pointer into LINE
               CYCLE INFINITE
            endif
         enddo INFINITE
      enddo LOOP
!     error >>>>> more than iread numbers were in the line.
end subroutine string_to_values
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      s2vs(3f) - [M_strings:NUMERIC] given a string representing numbers
!!      return a numeric array
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       function s2vs(line[,delim])
!!
!!        character(len=*) :: line
!!        doubleprecision,allocatable :: s2vs(:)
!!
!!##DESCRIPTION
!!    The function S2VS(3f) takes a string representing a series of numbers
!!    and converts it to a numeric doubleprecision array. The string values
!!    may be delimited by spaces, semi-colons, and commas by default.
!!
!!##OPTIONS
!!       LINE   Input string containing numbers
!!       DELIM  optional list of delimiter characters. If a space is
!!              included, it should appear as the left-most character
!!              in the list. The default is " ;," (spaces, semi-colons,
!!              and commas).
!!
!!##RESULTS
!!       S2VS   doubleprecision array
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!      program demo_s2vs
!!      use M_strings, only : s2vs
!!      implicit none
!!      character(len=80) :: s=' 10 20e3;3.45 -400.3e-2;1234; 5678 '
!!      real,allocatable :: values(:)
!!      integer,allocatable :: ivalues(:)
!!      integer :: ii
!!
!!      values=s2vs(s)
!!      ivalues=int(s2vs(s))
!!      call reportit()
!!
!!      contains
!!        subroutine reportit()
!!          write(*,*)'S2VS:'
!!          write(*,*)'input string.............',&
!!           & trim(s)
!!          write(*,*)'number of values found...',&
!!           & size(values)
!!          write(*,*)'values...................',&
!!           & (values(ii),ii=1,size(values))
!!          write(*,'(*(g0,1x))')'ivalues..................',&
!!           & (ivalues(ii),ii=1,size(values))
!!        end subroutine reportit
!!      end program demo_s2vs
!!
!!   Expected output
!!
!!     S2VS:
!!     input string............. 10 20e3;3.45 -400.3e-2;1234; 5678
!!     number of values found...           6
!!     values...................   10.0000000 20000.0000 3.45000005
!!     -4.00299978 1234.00000 5678.00000
!!    ivalues.................. 10 20000 3 -4 1234 5678
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function s2vs(string,delim) result(darray)

! ident_57="@(#)M_strings::s2vs(3f): function returns array of values from a string"

character(len=*),intent(in)        :: string                       ! keyword to retrieve value for from dictionary
character(len=*),optional          :: delim                        ! delimiter characters
character(len=:),allocatable       :: delim_local
doubleprecision,allocatable        :: darray(:)                    ! function type

character(len=:),allocatable       :: carray(:)                    ! convert value to an array using split(3f)
integer                            :: i
integer                            :: ier
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(delim))then
      delim_local=delim
   else
      delim_local=' ;,'
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call split(string,carray,delimiters=delim_local)         ! split string into an array
   allocate(darray(size(carray)))                           ! create the output array
   do i=1,size(carray)
      call string_to_value(carray(i), darray(i), ier)       ! convert the string to a numeric value
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end function s2vs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isprint(3f) - [M_strings:COMPARE] returns .true. if character is an
!!     ASCII printable character
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isprint(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isprint
!!
!!##DESCRIPTION
!!     isprint(3f) returns .true. if character is an ASCII printable character
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    isprint  logical value returns true if character is a
!!             printable ASCII character else false.
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_isprint
!!    use M_strings, only : isprint
!!    implicit none
!!    integer                    :: i
!!    character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!       write(*,'(40(a))')'ISPRINT: ',pack( string, isprint(string) )
!!    end program demo_isprint
!!
!!   Results:
!!
!!    ISPRINT:  !"#$%&'()*+,-./0123456789:;<=>?@ABCDEF
!!    GHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmn
!!    opqrstuvwxyz{|}~
!!
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isprint(onechar)

! ident_58="@(#)M_strings::isprint(3f): indicates if input character is a printable ASCII character"

character,intent(in) :: onechar
logical              :: isprint
   select case (onechar)
      case (' ':'~')   ; isprint=.TRUE.
      case default     ; isprint=.FALSE.
   end select
end function isprint
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isgraph(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     printable character except a space is considered non-printable
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isgraph(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isgraph
!!
!!##DESCRIPTION
!!    isgraph(3f) returns .true. if character is a printable character
!!    except a space is considered non-printable
!!
!!##OPTIONS
!!    onechar   character to test
!!
!!##RETURNS
!!    isgraph   logical value returns true if character is a printable
!!              non-space character
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_isgraph
!!    use M_strings, only : isgraph
!!    implicit none
!!    integer                    :: i
!!    character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!       write(*,'(40(a))')'ISGRAPH: ',pack( string, isgraph(string) )
!!    end program demo_isgraph
!!
!!   Results:
!!
!!    ISGRAPH: !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFG
!!    HIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmno
!!    pqrstuvwxyz{|}~
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isgraph(onechar)

! ident_59="@(#)M_strings::isgraph(3f) :indicates if character is printable ASCII character excluding space"

character,intent(in) :: onechar
logical              :: isgraph
   select case (iachar(onechar))
   case (33:126)
     isgraph=.TRUE.
   case default
     isgraph=.FALSE.
   end select
end function isgraph
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    isalpha(3f) - [M_strings:COMPARE] returns .true. if character is a
!!    letter and .false. otherwise
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!   elemental function isalpha(onechar)
!!
!!    character,intent(in) :: onechar
!!    logical              :: isalpha
!!
!!##DESCRIPTION
!!    isalpha(3f) returns .true. if character is a letter and
!!    .false. otherwise
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    isalpha  logical value returns .true. if character is a ASCII letter
!!             or false otherwise.
!!##EXAMPLE
!!
!!
!!   Sample program
!!
!!     program demo_isalpha
!!     use M_strings, only : isalpha
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(40(a))')'ISGRAPH: ',pack( string, isalpha(string) )
!!     end program demo_isalpha
!!
!!   Results:
!!
!!    ISGRAPH: ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklm
!!    nopqrstuvwxyz
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental function isalpha(ch) result(res)

! ident_60="@(#)M_strings::isalpha(3f): Return .true. if character is a letter and .false. otherwise"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('A':'Z','a':'z')
     res=.true.
   case default
     res=.false.
   end select
end function isalpha
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isxdigit(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     hexadecimal digit (0-9, a-f, or A-F).
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isxdigit(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isxdigit
!!
!!##DESCRIPTION
!!     isxdigit(3f) returns .true. if character is a hexadecimal digit (0-9,
!!     a-f, or A-F).
!!
!!##OPTIONS
!!    onechar   character to test
!!
!!##RETURNS
!!    isxdigit  logical value returns true if character is a hexadecimal digit
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!     program demo_isxdigit
!!     use M_strings, only : isxdigit
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(40(a))')'ISXDIGIT: ',pack( string, isxdigit(string) )
!!     end program demo_isxdigit
!!
!!   Results:
!!
!!    ISXDIGIT: 0123456789ABCDEFabcdef
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isxdigit(ch) result(res)

! ident_61="@(#)M_strings::isxdigit(3f): returns .true. if c is a hexadecimal digit (0-9,a-f, or A-F)"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('A':'F','a':'f','0':'9')
     res=.true.
   case default
     res=.false.
   end select
end function isxdigit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isdigit(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     digit (0,1,...,9) and .false. otherwise
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isdigit(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isdigit
!!
!!##DESCRIPTION
!!     isdigit(3f) returns .true. if character is a digit (0,1,...,9)
!!     and .false. otherwise
!!
!!##EXAMPLES
!!
!!
!!  Sample Program:
!!
!!     program demo_isdigit
!!     use M_strings, only : isdigit, isspace, switch
!!     implicit none
!!     character(len=10),allocatable :: string(:)
!!     integer                       :: i
!!        string=[&
!!        & '1 2 3 4 5 ' ,&
!!        & 'letters   ' ,&
!!        & '1234567890' ,&
!!        & 'both 8787 ' ]
!!        ! if string is nothing but digits and whitespace return .true.
!!        do i=1,size(string)
!!           write(*,'(a)',advance='no')'For string['//string(i)//']'
!!           write(*,*) &
!!            & all(isdigit(switch(string(i))).or.&
!!            & isspace(switch(string(i))))
!!        enddo
!!     end program demo_isdigit
!!
!!  Expected output:
!!
!!        For string[1 2 3 4 5 ] T
!!        For string[letters   ] F
!!        For string[1234567890] T
!!        For string[both 8787 ] F
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isdigit(ch) result(res)

! ident_62="@(#)M_strings::isdigit(3f): Returns .true. if ch is a digit (0-9) and .false. otherwise"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('0':'9')
     res=.true.
   case default
     res=.false.
   end select
end function isdigit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isblank(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     blank character (space or horizontal tab).
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isblank(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isblank
!!
!!##DESCRIPTION
!!     isblank(3f) returns .true. if character is a blank character (space
!!     or horizontal tab).
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    isblank  logical value returns true if character is a "blank"
!!             ( an ASCII  space or horizontal tab character).
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_isblank
!!     use M_strings, only : isblank
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(*(g0,1x))')'ISXBLANK: ',&
!!        & ichar(pack( string, isblank(string) ))
!!     end program demo_isblank
!!
!!   Results:
!!
!!    ISXBLANK:  9 32
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isblank(ch) result(res)

! ident_63="@(#)M_strings::isblank(3f): returns .true. if character is a blank (space or horizontal tab)"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case(' ',char(9))
     res=.true.
   case default
     res=.false.
   end select
end function isblank
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isascii(3f) - [M_strings:COMPARE] returns .true. if the character is
!!     in the range char(0) to char(256)
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isascii(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isascii
!!
!!##DESCRIPTION
!!     isascii(3f) returns .true. if the character is in the range char(0)
!!     to char(127)
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    isupper  logical value returns true if character is an ASCII
!!             character.
!!##EXAMPLE
!!
!!  Sample program
!!
!!     program demo_isascii
!!     use M_strings, only : isascii
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,255)]
!!        write(*,'(10(g0,1x))')'ISASCII: ', &
!!        & ichar(pack( string, isascii(string) ))
!!     end program demo_isascii
!!
!!  Results:
!!
!!    ISASCII:  0 1 2 3 4 5 6 7 8
!!    9 10 11 12 13 14 15 16 17 18
!!    19 20 21 22 23 24 25 26 27 28
!!    29 30 31 32 33 34 35 36 37 38
!!    39 40 41 42 43 44 45 46 47 48
!!    49 50 51 52 53 54 55 56 57 58
!!    59 60 61 62 63 64 65 66 67 68
!!    69 70 71 72 73 74 75 76 77 78
!!    79 80 81 82 83 84 85 86 87 88
!!    89 90 91 92 93 94 95 96 97 98
!!    99 100 101 102 103 104 105 106 107 108
!!    109 110 111 112 113 114 115 116 117 118
!!    119 120 121 122 123 124 125 126 127
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isascii(ch) result(res)

! ident_64="@(#)M_strings::isascii(3f): returns .true. if character is in the range char(0) to char(127)"

character,intent(in) :: ch
logical              :: res
   select case(ichar(ch))
   case(0:127)
     res=.true.
   case default
     res=.false.
   end select
end function isascii
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isspace(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     null, space, tab, carriage return, new line, vertical tab, or formfeed
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isspace(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isspace
!!
!!##DESCRIPTION
!!     isspace(3f) returns .true. if character is a null, space, tab,
!!     carriage return, new line, vertical tab, or formfeed
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    isspace  returns true if character is ASCII white space
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program demo_isspace
!!     use M_strings, only : isspace
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(20(g0,1x))')'ISSPACE: ', &
!!        & ichar(pack( string, isspace(string) ))
!!     end program demo_isspace
!!
!!   Results:
!!
!!    ISSPACE:  0 9 10 11 12 13 32
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isspace(ch) result(res)

! ident_65="@(#)M_strings::isspace(3f): true if null,space,tab,return,new line,vertical tab, or formfeed"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case(' ')                 ! space(32)
     res=.true.
   case(char(0))             ! null(0)
     res=.true.
   case(char(9):char(13))    ! tab(9), new line(10), vertical tab(11), formfeed(12), carriage return(13),
     res=.true.
   case default
     res=.false.
   end select
end function isspace
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     iscntrl(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     delete character or ordinary control character
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function iscntrl(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: iscntrl
!!
!!##DESCRIPTION
!!     iscntrl(3f) returns .true. if character is a delete character or
!!     ordinary control character
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    iscntrl  logical value returns true if character is a control character
!!
!!##EXAMPLE
!!
!!  Sample program
!!
!!     program demo_iscntrl
!!     use M_strings, only : iscntrl
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(20(g0,1x))')'ISCNTRL: ', &
!!        & ichar(pack( string, iscntrl(string) ))
!!     end program demo_iscntrl
!!
!!   Results:
!!
!!    ISCNTRL:  0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
!!    20 21 22 23 24 25 26 27 28 29 30 31 127
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function iscntrl(ch) result(res)

! ident_66="@(#)M_strings::iscntrl(3f): true if a delete or ordinary control character(0x7F or 0x00-0x1F)"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case(char(127),char(0):char(31))
     res=.true.
   case default
     res=.false.
   end select
end function iscntrl
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     ispunct(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     printable punctuation character
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function ispunct(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: ispunct
!!
!!##DESCRIPTION
!!     ispunct(3f) returns .true. if character is a printable punctuation
!!     character
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    ispunct  logical value returns true if character is a printable
!!             punctuation character.
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program demo_ispunct
!!     use M_strings, only : ispunct
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(20(g0,1x))')'ISPUNCT: ', &
!!        & ichar(pack( string, ispunct(string) ))
!!        write(*,'(20(g0,1x))')'ISPUNCT: ', &
!!        & pack( string, ispunct(string) )
!!     end program demo_ispunct
!!   Results:
!!
!!    ISPUNCT:  33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 58 59 60 61
!!    62 63 64 91 92 93 94 95 96 123 124 125 126
!!    ISPUNCT:  ! " # $ % & ' ( ) * + , - . / : ; < =
!!    > ? @ [ \ ] ^ _ ` { | } ~
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function ispunct(ch) result(res)

! ident_67="@(#)M_strings::ispunct(3f): true if a printable punctuation character (isgraph(c)&&"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case (char(33):char(47), char(58):char(64), char(91):char(96), char(123):char(126))
     res=.true.
!  case(' ','0':'9','A':'Z','a':'z',char(128):)
!    res=.true.
!  case(char(0):char(31),char(127))
!    res=.true.
   case default
     res=.false.
   end select
end function ispunct
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     fortran_name(3f) - [M_strings:COMPARE] test if string meets criteria
!!     for being a fortran name
!!
!!##SYNOPSIS
!!
!!
!!     elemental function fortran_name(line) result (lout)
!!
!!      character(len=*),intent(in)  :: line
!!      logical                      :: lout
!!
!!##DESCRIPTION
!!     Determines if a string is an allowed Fortran name. To pass the input
!!     string must be composed of 1 to 63 ASCII characters and start with a
!!     letter and be composed entirely of alphanumeric characters [a-zA-Z0-9]
!!     and underscores.
!!
!!##OPTIONS
!!     LINE   input string to test. Leading spaces are significant but
!!            trailing spaces are ignored.
!!
!!##RETURNS
!!     LOUT   a logical value indicating if the input string passed or failed
!!            the test to see if it is a valid Fortran name or not.
!!
!!##EXAMPLE
!!
!!    Sample program
!!
!!      program demo_fortran_name
!!      use M_strings, only : fortran_name
!!      implicit none
!!      character(len=*),parameter :: names(*)=[character(len=20) ::  &
!!       & '_name',         'long_variable_name', 'name_',         &
!!       & '12L',           'a__b__c  ',          'PropertyOfGas', &
!!       & '3%3',           '$NAME',              ' ',             &
!!       & 'Variable-name', 'A',                  'x@x' ]
!!      integer :: i
!!         write(*,'(i3,1x,a20,1x,l1)')&
!!         & (i,names(i),fortran_name(names(i)),i=1,size(names))
!!      end program demo_fortran_name
!!
!!    Results:
!!
!!       1 _name                F
!!       2 long_variable_name   T
!!       3 name_                T
!!       4 12L                  F
!!       5 a__b__c              T
!!       6 PropertyOfGas        T
!!       7 3%3                  F
!!       8 $NAME                F
!!       9                      F
!!      10 Variable-name        F
!!      11 A                    T
!!      12 x@x                  F
elemental function fortran_name(line) result (lout)

! ident_68="@(#)M_strings::fortran_name(3f): Return .true. if name is a valid Fortran name"

! determine if a string is a valid Fortran name ignoring trailing spaces (but not leading spaces)
character(len=*),parameter   :: int='0123456789'
character(len=*),parameter   :: lower='abcdefghijklmnopqrstuvwxyz'
character(len=*),parameter   :: upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
character(len=*),parameter   :: allowed=upper//lower//int//'_'

character(len=*),intent(in)  :: line
character(len=:),allocatable :: name
logical                      :: lout
   name=trim(line)
   if(len(name).ne.0)then
      lout = verify(name(1:1), lower//upper) == 0  &
       & .and. verify(name,allowed) == 0           &
       & .and. len(name) <= 63
   else
      lout = .false.
   endif
end function fortran_name
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isupper(3f) - [M_strings:COMPARE] returns .true. if character is an
!!     uppercase letter (A-Z)
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isupper(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isupper
!!
!!##DESCRIPTION
!!     isupper(3f) returns .true. if character is an uppercase letter (A-Z)
!!
!!##OPTIONS
!!    onechar  character to test
!!##RETURNS
!!    isupper  logical value returns true if character is an uppercase
!!             ASCII character else false.
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program demo_isupper
!!     use M_strings, only : isupper
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(10(g0,1x))')'ISUPPER: ', &
!!        & ichar(pack( string, isupper(string) ))
!!        write(*,'(10(g0,1x))')'ISUPPER: ', &
!!        & pack( string, isupper(string) )
!!     end program demo_isupper
!!
!!  Results:
!!
!!    ISUPPER:  65 66 67 68 69 70 71 72 73
!!    74 75 76 77 78 79 80 81 82 83
!!    84 85 86 87 88 89 90
!!    ISUPPER:  A B C D E F G H I
!!    J K L M N O P Q R S
!!    T U V W X Y Z
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
pure elemental function isupper(ch) result(res)

! ident_69="@(#)M_strings::isupper(3f): returns true if character is an uppercase letter (A-Z)"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('A':'Z')
     res=.true.
   case default
     res=.false.
   end select
end function isupper
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     islower(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     miniscule letter (a-z)
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function islower(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: islower
!!
!!##DESCRIPTION
!!     islower(3f) returns .true. if character is a miniscule letter (a-z)
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    islowe  logical value returns true if character is a lowercase
!!             ASCII character else false.
!!##EXAMPLE
!!
!!  Sample program
!!
!!     program demo_islower
!!     use M_strings, only : islower
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(15(g0,1x))')'ISLOWER: ', &
!!        & ichar(pack( string, islower(string) ))
!!        write(*,'(15(g0,1x))')'ISLOWER: ', &
!!        & pack( string, islower(string) )
!!     end program demo_islower
!!   Results:
!!
!!    ISLOWER:  97 98 99 100 101 102 103 104 105 106 107 108 109 110
!!    111 112 113 114 115 116 117 118 119 120 121 122
!!    ISLOWER:  a b c d e f g h i j k l m n
!!    o p q r s t u v w x y z
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function islower(ch) result(res)

! ident_70="@(#)M_strings::islower(3f): returns true if character is a miniscule letter (a-z)"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('a':'z')
     res=.true.
   case default
     res=.false.
   end select
end function islower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    isalnum,isalpha,iscntrl,isdigit,isgraph,islower,
!!    isprint,ispunct,isspace,isupper,
!!    isascii,isblank,isxdigit(3f) - [M_strings:COMPARE] test membership in
!!    subsets of ASCII set
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    Where "FUNCNAME" is one of the function names in the group, the
!!    functions are defined by
!!
!!     elemental function FUNCNAME(onechar)
!!     character,intent(in) :: onechar
!!     logical              :: FUNC_NAME
!!##DESCRIPTION
!!
!!       These elemental functions test if a character belongs to various
!!       subsets of the ASCII character set.
!!
!!       isalnum    returns .true. if character is a letter (a-z,A-Z)
!!                  or digit (0-9)
!!       isalpha    returns .true. if character is a letter and
!!                  .false. otherwise
!!       isascii    returns .true. if character is in the range char(0)
!!                  to char(127)
!!       isblank    returns .true. if character is a blank (space or
!!                  horizontal tab).
!!       iscntrl    returns .true. if character is a delete character or
!!                  ordinary control character (0x7F or 0x00-0x1F).
!!       isdigit    returns .true. if character is a digit (0,1,...,9)
!!                  and .false. otherwise
!!       isgraph    returns .true. if character is a printable ASCII
!!                  character excluding space
!!       islower    returns .true. if character is a miniscule letter (a-z)
!!       isprint    returns .true. if character is a printable ASCII character
!!       ispunct    returns .true. if character is a printable punctuation
!!                  character (isgraph(c) && !isalnum(c)).
!!       isspace    returns .true. if character is a null, space, tab,
!!                  carriage return, new line, vertical tab, or formfeed
!!       isupper    returns .true. if character is an uppercase letter (A-Z)
!!       isxdigit   returns .true. if character is a hexadecimal digit
!!                  (0-9, a-f, or A-F).
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_isdigit
!!
!!     use M_strings, only : isdigit, isspace, switch
!!     implicit none
!!     character(len=10),allocatable :: string(:)
!!     integer                       :: i
!!        string=[&
!!        & '1 2 3 4 5 ' ,&
!!        & 'letters   ' ,&
!!        & '1234567890' ,&
!!        & 'both 8787 ' ]
!!        ! if string is nothing but digits and whitespace return .true.
!!        do i=1,size(string)
!!           write(*,'(a)',advance='no')'For string['//string(i)//']'
!!           write(*,*) &
!!           all(isdigit(switch(string(i))) .or. &
!!           & isspace(switch(string(i))))
!!        enddo
!!
!!     end program demo_isdigit
!!
!!   Expected output:
!!
!!    For string[1 2 3 4 5 ] T
!!    For string[letters   ] F
!!    For string[1234567890] T
!!    For string[both 8787 ] F
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
elemental function isalnum(ch) result(res)

! ident_71="@(#)M_strings::isalnum(3f): returns true if character is a letter (a-z,A-Z) or digit(0-9)"

character,intent(in)       :: ch
logical                    :: res
   select case(ch)
   case('a':'z','A':'Z','0':'9')
     res=.true.
   case default
     res=.false.
   end select
end function isalnum
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    base(3f) - [M_strings:BASE] convert whole number string in base [2-36]
!!    to string in alternate base [2-36]
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function base(x,b,y,a)
!!
!!    character(len=*),intent(in)  :: x
!!    character(len=*),intent(out) :: y
!!    integer,intent(in)           :: b,a
!!##DESCRIPTION
!!
!!    Convert a numeric string from base B to base A. The function returns
!!    FALSE if B is not in the range [2..36] or if string X contains invalid
!!    characters in base B or if result Y is too big
!!
!!    The letters A,B,...,Z represent 10,11,...,36 in the base > 10.
!!
!!##OPTIONS
!!    x   input string representing numeric whole value
!!    b   assumed base of input string
!!    y   output string
!!    a   base specified for output string
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_base
!!    use M_strings, only : base
!!    implicit none
!!    integer           :: ba,bd
!!    character(len=40) :: x,y
!!
!!    print *,' BASE CONVERSION'
!!    write(*,'("Start   Base (2 to 36): ")',advance='no'); read *, bd
!!    write(*,'("Arrival Base (2 to 36): ")',advance='no'); read *, ba
!!    INFINITE: do
!!       write(*,'("Enter number in start base (0 to quit): ")',advance='no')
!!       read *, x
!!       if(x.eq.'0') exit INFINITE
!!       if(base(x,bd,y,ba))then
!!            write(*,'("In base ",I2,": ",A20)')  ba, y
!!        else
!!          print *,'Error in decoding/encoding number.'
!!        endif
!!     enddo INFINITE
!!
!!     end program demo_base
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
logical function base(x,b,y,a)
implicit none
character(len=*),intent(in)  :: x
character(len=*),intent(out) :: y
integer,intent(in)           :: b,a
integer                      :: temp

! ident_72="@(#)M_strings::base(3f): convert whole number string in base [2-36] to string in alternate base [2-36]"

base=.true.
if(decodebase(x,b,temp)) then
   if(codebase(temp,a,y)) then
   else
      print *,'Error in coding number.'
      base=.false.
   endif
else
   print *,'Error in decoding number.'
   base=.false.
endif

end function base
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    base2(3f) - [M_strings:BASE] convert whole number to string in base 2
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function base2(int)
!!
!!    integer,intent(in)           :: int
!!    character(len=:),allocatable :: base2
!!##DESCRIPTION
!!
!!    Convert a whole number to a string in base 2.
!!
!!    The letters A,B,...,Z represent 10,11,...,36 in the base > 10.
!!
!!##OPTIONS
!!    int   input string representing numeric whole value
!!##RETURNS
!!    base2   string representing input value in base 2
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_base2
!!    use M_strings, only : base2
!!    implicit none
!!    integer                      :: i
!!    character(len=:),allocatable :: string
!!       write(*,'(a)') base2(huge(0))
!!       write(*,'(a)') base2(0)
!!       write(*,'(a)') base2(1-huge(0))
!!    end program demo_base2
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
! 0 in binary: 0
! 42 in binary: 101010
! huge(int) in binary: 1111111111111111111111111111111
! 032 in binary is 100000
! itimes=10000000
!      G_TRICK=base2_f(32)   <BASE2_F  >Processor Time =  0.766 seconds.
!      G_TRICK=base2_fdo(32) <BASE2_FDO>Processor Time =  0.958 seconds.
!      G_TRICK=base2_a(32)   <BASE2_A  >Processor Time =  1.022 seconds.
!      G_TRICK=base2_c(32)   <BASE2_C  >Processor Time =  7.208 seconds.
!      G_TRICK=empty(32)     <EMPTY    >Processor Time =  0.132 seconds.
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function base2(x) result(str)
!  return string representing number as a binary number.  Fixed-length string:
integer, intent(in) :: x
integer           :: i
character(len=max(1,bit_size(x)-leadz(x))) :: str
    associate(n => len(str))
      str = repeat('0',n)
      do i = 0,n-1
        if (btest(x,i)) str(n-i:n-i) = '1'
      end do
    end associate
end function base2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function base2_fdo(x) result(str)
!  return string representing number as a binary number.  Fixed-length string: do concurrent
integer, intent(in) :: x
character(len=max(1,bit_size(x)-leadz(x))) :: str

integer :: n, i

    if (x == 0) then
      str(1:1) = '0'
      return
    endif
    n = len(str)
    str = repeat('0',n)
    do concurrent (i = 0:n-1, btest(x,i))
      str(n-i:n-i) = '1'
    end do
end function base2_fdo
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function base2_a(x) result(str)
!  return string representing number as a binary number. Allocatable-length string:
integer, intent(in) :: x
character(len=:), allocatable :: str

integer :: n, i

    n = max(1,bit_size(x)-leadz(x))
    allocate(character(len=n) :: str)
    if (x == 0) then
      str(1:1) = '0'
      return
    endif

    str = repeat('0',n)
    do concurrent (i = 0:n-1, btest(x,i))
      str(n-i:n-i) = '1'
    end do
end function base2_a
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function base2_c(x) result(str)
! internal write
integer, intent(in) :: x
character(len=max(1,bit_size(x)-leadz(x))) :: str
    write( str, fmt="(b0)" ) x
end function base2_c
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!
!!    decodebase(3f) - [M_strings:BASE] convert whole number string in base
!!    [2-36] to base 10 number
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function decodebase(string,basein,out10)
!!
!!    character(len=*),intent(in)  :: string
!!    integer,intent(in)           :: basein
!!    integer,intent(out)          :: out10
!!##DESCRIPTION
!!
!!    Convert a numeric string representing a whole number in base BASEIN
!!    to base 10. The function returns FALSE if BASEIN is not in the range
!!    [2..36] or if string STRING contains invalid characters in base BASEIN
!!    or if result OUT10 is too big
!!
!!    The letters A,B,...,Z represent 10,11,...,36 in the base > 10.
!!
!!##OPTIONS
!!    string   input string. It represents a whole number in
!!             the base specified by BASEIN unless BASEIN is set
!!             to zero. When BASEIN is zero STRING is assumed to
!!             be of the form BASE#VALUE where BASE represents
!!             the function normally provided by BASEIN.
!!    basein   base of input string; either 0 or from 2 to 36.
!!    out10    output value in base 10
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_decodebase
!!    use M_strings, only : codebase, decodebase
!!    implicit none
!!    integer           :: ba,bd
!!    character(len=40) :: x,y
!!    integer           :: r
!!
!!    print *,' BASE CONVERSION'
!!    write(*,'("Start   Base (2 to 36): ")',advance='no'); read *, bd
!!    write(*,'("Arrival Base (2 to 36): ")',advance='no'); read *, ba
!!    INFINITE: do
!!       print *,''
!!       write(*,'("Enter number in start base: ")',advance='no'); read *, x
!!       if(x.eq.'0') exit INFINITE
!!       if(decodebase(x,bd,r)) then
!!          if(codebase(r,ba,y)) then
!!            write(*,'("In base ",I2,": ",A20)')  ba, y
!!          else
!!            print *,'Error in coding number.'
!!          endif
!!       else
!!          print *,'Error in decoding number.'
!!       endif
!!    enddo INFINITE
!!
!!    end program demo_decodebase
!!
!!##AUTHOR
!!    John S. Urban
!!
!!       Ref.: "Math matiques en Turbo-Pascal by
!!              M. Ducamp and A. Reverchon (2),
!!              Eyrolles, Paris, 1988".
!!
!!    based on a F90 Version By J-P Moreau (www.jpmoreau.fr)
!!
!!##LICENSE
!!    Public Domain
logical function decodebase(string,basein,out_baseten)
implicit none

! ident_73="@(#)M_strings::decodebase(3f): convert whole number string in base [2-36] to base 10 number"

character(len=*),intent(in)  :: string
integer,intent(in)           :: basein
integer,intent(out)          :: out_baseten

character(len=len(string))   :: string_local
integer           :: long, i, j, k
real              :: y
real              :: mult
character(len=1)  :: ch
real,parameter    :: XMAXREAL=real(huge(1))
integer           :: out_sign
integer           :: basein_local
integer           :: ipound
integer           :: ierr

  string_local=upper(trim(adjustl(string)))
  decodebase=.false.

  ipound=index(string_local,'#')                                       ! determine if in form [-]base#whole
  if(basein.eq.0.and.ipound.gt.1)then                                  ! split string into two values
     call string_to_value(string_local(:ipound-1),basein_local,ierr)   ! get the decimal value of the base
     string_local=string_local(ipound+1:)                              ! now that base is known make string just the value
     if(basein_local.ge.0)then                                         ! allow for a negative sign prefix
        out_sign=1
     else
        out_sign=-1
     endif
     basein_local=abs(basein_local)
  else                                                                 ! assume string is a simple positive value
     basein_local=abs(basein)
     out_sign=1
  endif

  out_baseten=0
  y=0.0
  ALL: if(basein_local<2.or.basein_local>36) then
    print *,'(*decodebase* ERROR: Base must be between 2 and 36. base=',basein_local
  else ALL
     out_baseten=0;y=0.0; mult=1.0
     long=LEN_TRIM(string_local)
     do i=1, long
        k=long+1-i
        ch=string_local(k:k)
        if(ch.eq.'-'.and.k.eq.1)then
           out_sign=-1
           cycle
        endif
        if(ch<'0'.or.ch>'Z'.or.(ch>'9'.and.ch<'A'))then
           write(*,*)'*decodebase* ERROR: invalid character ',ch
           exit ALL
        endif
        if(ch<='9') then
              j=IACHAR(ch)-IACHAR('0')
        else
              j=IACHAR(ch)-IACHAR('A')+10
        endif
        if(j>=basein_local)then
           exit ALL
        endif
        y=y+mult*j
        if(mult>XMAXREAL/basein_local)then
           exit ALL
        endif
        mult=mult*basein_local
     enddo
     decodebase=.true.
     out_baseten=nint(out_sign*y)*sign(1,basein)
  endif ALL
end function decodebase
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    codebase(3f) - [M_strings:BASE] convert whole number in base 10 to
!!    string in base [2-36]
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function codebase(in_base10,out_base,answer)
!!
!!    integer,intent(in)           :: in_base10
!!    integer,intent(in)           :: out_base
!!    character(len=*),intent(out) :: answer
!!
!!##DESCRIPTION
!!    Convert a number from base 10 to base OUT_BASE. The function returns
!!    .FALSE. if OUT_BASE is not in [2..36] or if number IN_BASE10 is
!!    too big.
!!
!!    The letters A,B,...,Z represent 10,11,...,36 in the base > 10.
!!
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_codebase
!!    use M_strings, only : codebase
!!    implicit none
!!    character(len=20) :: answer
!!    integer           :: i, j
!!    logical           :: ierr
!!    do j=1,100
!!       do i=2,36
!!          ierr=codebase(j,i,answer)
!!          write(*,*)'VALUE=',j,' BASE=',i,' ANSWER=',answer
!!       enddo
!!    enddo
!!    end program demo_codebase
!!
!!##AUTHOR
!!    John S. Urban
!!
!!     Ref.: "Math matiques en Turbo-Pascal by
!!            M. Ducamp and A. Reverchon (2),
!!            Eyrolles, Paris, 1988".
!!
!!    based on a F90 Version By J-P Moreau (www.jpmoreau.fr)
!!
!!##LICENSE
!!    Public Domain
logical function codebase(inval10,outbase,answer)
implicit none

! ident_74="@(#)M_strings::codebase(3f): convert whole number in base 10 to string in base [2-36]"

integer,intent(in)           :: inval10
integer,intent(in)           :: outbase
character(len=*),intent(out) :: answer
integer                      :: n
real                         :: inval10_local
integer                      :: outbase_local
integer                      :: in_sign
  answer=''
  in_sign=sign(1,inval10)*sign(1,outbase)
  inval10_local=abs(inval10)
  outbase_local=abs(outbase)
  if(outbase_local<2.or.outbase_local>36) then
    print *,'*codebase* ERROR: base must be between 2 and 36. base was',outbase_local
    codebase=.false.
  else
     do while(inval10_local>0.0 )
        n=INT(inval10_local-outbase_local*INT(inval10_local/outbase_local))
        if(n<10) then
           answer=ACHAR(IACHAR('0')+n)//answer
        else
           answer=ACHAR(IACHAR('A')+n-10)//answer
        endif
        inval10_local=INT(inval10_local/outbase_local)
     enddo
     codebase=.true.
  endif
  if(in_sign.eq.-1)then
     answer='-'//trim(answer)
  endif
  if(answer.eq.'')then
     answer='0'
  endif
end function codebase
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function todecimal(base, instr)

! ident_75="@(#)M_strings::todecimal(3f): given string and base return decimal integer"

! based on an example at rosetta code.
character(len=36),parameter  :: alphanum = "0123456789abcdefghijklmnopqrstuvwxyz"
integer,intent(in)           :: base
character(*),intent(in)      :: instr
character(len=:),allocatable :: instr_local
integer                      :: todecimal
integer                      :: length, i, n

   instr_local=trim(lower(instr))
   todecimal = 0
   length = len(instr_local)
   do i = 1, length
      n = index(alphanum, instr_local(i:i)) - 1
      n = n * base**(length-i)
      todecimal = todecimal + n
   enddo
end function todecimal
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function tobase(base, number)

! ident_76="@(#)M_strings::todecimal(3f): given integer and base return string"

! based on an example at rosetta code.
character(len=36),parameter  :: alphanum = "0123456789abcdefghijklmnopqrstuvwxyz"
integer,intent(in)           :: base
integer,intent(in)           :: number
character(len=:),allocatable :: tobase
character(len=31)            :: holdit
integer                      :: number_local, i, rem
   number_local=number

   holdit = "                               "
   do i = 31, 1, -1
      if(number_local < base) then
         holdit(i:i) = alphanum(number_local+1:number_local+1)
         exit
      endif
      rem = mod(number_local, base)
      holdit(i:i) = alphanum(rem+1:rem+1)
      number_local = number_local / base
   enddo
   tobase = adjustl(holdit)
end function tobase

!SUBROUTINE DectoBase(decimal, string, base)
! CHARACTER string
!    string = '0'
!    temp = decimal
!    length = CEILING( LOG(decimal+1, base) )   !<<<<<<<< INTERESTING
!    DO i = length, 1, -1
!      n = MOD( temp, base )
!      string(i) = "0123456789abcdefghijklmnopqrstuvwxyz"(n+1)
!      temp = INT(temp / base)
!    ENDDO
! END
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    fmt(3f) - [M_strings:TOKENS] break a long line into a paragraph
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function fmt(source_string,length)
!!
!!    character(len=*),intent(in)       :: source_string
!!    integer,intent(in)                :: length
!!    character(allocatable(len=length)    :: fmt(:)
!!
!!##DESCRIPTION
!!    fmt(3f) breaks a long line into a simple paragraph of specified
!!    line length.
!!
!!    Given a long string break it on spaces into an array such that no
!!    variable is longer than the specified length. Individual words longer
!!    than LENGTH will be placed in variables by themselves.
!!
!!##OPTIONS
!!     SOURCE_STRING  input string to break into an array of shorter strings
!!                    on blank delimiters
!!     LENGTH         length of lines to break the string into.
!!
!!##RETURNS
!!     FMT  character array filled with data from source_string broken at
!!          spaces into variables of length LENGTH.
!!
!!##EXAMPLE
!!
!!  sample program
!!
!!    program demo_fmt
!!    use M_strings, only : fmt
!!    implicit none
!!    character(len=:),allocatable :: paragraph(:)
!!    character(len=*),parameter    :: string= '&
!!     &one two three four five &
!!     &six seven eight &
!!     &nine ten eleven twelve &
!!     &thirteen fourteen fifteen sixteen &
!!     &seventeen'
!!
!!    write(*,*)'LEN=',len(string)
!!    write(*,*)'INPUT:'
!!    write(*,*)string
!!
!!    paragraph=fmt(string,40)
!!    write(*,*)'LEN=',len(paragraph),' SIZE=',size(paragraph)
!!    write(*,*)'OUTPUT:'
!!    write(*,'(a)')paragraph
!!
!!    write(*,'(a)')fmt(string,0)
!!    write(*,'(3x,a)')fmt(string,47)
!!
!!    end program demo_fmt
!!
!!  Results:
!!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function fmt(source_string,length)

! ident_77="@(#)M_strings::fmt(3f): wrap a long string into a paragraph"

character(len=*),intent(in)       :: source_string
integer,intent(in)                :: length
integer                           :: itoken
integer                           :: istart
integer                           :: iend
character(len=*),parameter        :: delimiters=' '
character(len=:),allocatable      :: fmt(:)
integer                           :: ilines
integer                           :: ilength
integer                           :: iword, iword_max
integer                           :: i
!-----------------------------------------------------------------------------------------------------------------------------------
!  parse string once to find out how big to make the returned array, then redo everything but store the data
!  could store array of endpoints and leave original whitespace alone or many other options
   do i=1,2
      iword_max=0                                  ! length of longest token
      ilines=1                                     ! number of output line output will go on
      ilength=0                                    ! length of output line so far
      itoken=0                                     ! must set ITOKEN=0 before looping on strtok(3f) on a new string.
      do while ( strtok(source_string,itoken,istart,iend,delimiters) )
         iword=iend-istart+1
         iword_max=max(iword_max,iword)
         if(iword.gt.length)then                   ! this token is longer than the desired line length so put it on a line by itself
            if(ilength.ne.0)then
               ilines=ilines+1
            endif
            if(i.eq.2)then                 ! if fmt has been allocated store data, else just gathering data to determine size of fmt
               fmt(ilines)=source_string(istart:iend)//' '
            endif
            ilength=iword+1
         elseif(ilength+iword.le.length)then       ! this word will fit on current line
            if(i.eq.2)then
               fmt(ilines)=fmt(ilines)(:ilength)//source_string(istart:iend)
            endif
            ilength=ilength+iword+1
         else                                      ! adding this word would make line too long so start new line
            ilines=ilines+1
            ilength=0
            if(i.eq.2)then
               fmt(ilines)=fmt(ilines)(:ilength)//source_string(istart:iend)
            endif
            ilength=iword+1
         endif
      enddo
      if(i==1)then                                 ! determined number of lines needed so allocate output array
         allocate(character(len=max(length,iword_max)) :: fmt(ilines))
         fmt=' '
      endif
   enddo
   fmt=fmt(:ilines)
end function fmt
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function setbits8(string) result(answer)
use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
implicit none
integer(kind=int8)          :: answer
character(len=8),intent(in) :: string
integer                     :: pos
integer                     :: ilen
   answer=0_int8
   ilen=len(string)
   if(ilen.ne.bit_size(answer))then
      write(stderr,*)'*setbits8* wrong string length =',ilen
      ilen=min(ilen,int(bit_size(answer)))
   endif
   do pos=1,ilen
      select case(string(pos:pos))
       case('1')
         answer = ibset(answer, pos-1)
       case('0')
         answer = ibclr(answer, pos-1)
       case default
         write(stderr,*)'*setbits8* unknown value. must be 0 or 1. found [',string(pos:pos),'] at position ',pos,' in ',string
      end select
   enddo
end function setbits8
!-----------------------------------------------------------------------------------------------------------------------------------
function setbits16(string) result(answer)
use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
implicit none
integer(kind=int16)          :: answer
character(len=16),intent(in) :: string
integer                      :: pos
integer                      :: ilen
   answer=0_int16
   ilen=len(string)
   if(ilen.ne.bit_size(answer))then
      write(stderr,*)'*setbits16* wrong string length =',ilen
      ilen=min(ilen,int(bit_size(answer)))
   endif
   do pos=1,len(string)
      select case(string(pos:pos))
       case('1')
         answer = ibset(answer, pos-1)
       case('0')
         answer = ibclr(answer, pos-1)
       case default
         write(stderr,*)'*setbits16* unknown value. must be 0 or 1. found [',string(pos:pos),'] at position ',pos,' in ',string
      end select
   enddo
end function setbits16
!-----------------------------------------------------------------------------------------------------------------------------------
function setbits32(string) result(answer)
use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
implicit none
integer(kind=int32)          :: answer
character(len=32),intent(in) :: string
integer                      :: pos
integer                      :: ilen
   answer=0_int32
   ilen=len(string)
   if(ilen.ne.bit_size(answer))then
      write(stderr,*)'*setbits32* wrong string length =',ilen
      ilen=min(ilen,int(bit_size(answer)))
   endif
   do pos=1,len(string)
      select case(string(pos:pos))
       case('1')
         answer = ibset(answer, pos-1)
       case('0')
         answer = ibclr(answer, pos-1)
       case default
         write(stderr,*)'*setbits32* unknown value. must be 0 or 1. found [',string(pos:pos),'] at position ',pos,' in ',string
      end select
   enddo
end function setbits32
!-----------------------------------------------------------------------------------------------------------------------------------
function setbits64(string) result(answer)
use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
implicit none
integer(kind=int64)          :: answer
character(len=64),intent(in) :: string
integer                      :: pos
integer                      :: ilen
   answer=0_int64
   ilen=len(string)
   if(ilen.ne.bit_size(answer))then
      write(stderr,*)'*setbits64* wrong string length =',ilen
      ilen=min(ilen,int(bit_size(answer)))
   endif
   do pos=1,len(string)
      select case(string(pos:pos))
       case('1')
         answer = ibset(answer, pos-1)
       case('0')
         answer = ibclr(answer, pos-1)
       case default
         write(stderr,*)'*setbits64* unknown value. must be 0 or 1. found [',string(pos:pos),'] at position ',pos,' in ',string
      end select
   enddo
end function setbits64
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     msg(3f) - [M_strings] converts any standard scalar type to a string
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!     function msg(g1,g2g3,g4,g5,g6,g7,g8,g9,sep)
!!
!!      class(*),intent(in),optional  :: g1,g2,g3,g4,g5,g6,g7,g8,g9
!!      character(len=*),intent(in),optional :: sep
!!      character(len=:),allocatable :: msg
!!
!!##DESCRIPTION
!!     msg(3f) builds a space-separated string from up to nine scalar values.
!!
!!##OPTIONS
!!     g[1-9]  optional value to print the value of after the message. May
!!             be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!             or CHARACTER.
!!     sep     separator between values. Defaults to a space
!!
!!##RETURNS
!!     msg     description to print
!!
!!##EXAMPLES
!!
!!
!!   Sample program:
!!
!!        program demo_msg
!!        use M_strings, only : msg
!!        implicit none
!!        character(len=:),allocatable :: pr
!!        character(len=:),allocatable :: frmt
!!        integer                      :: biggest
!!
!!        pr=msg('HUGE(3f) integers',huge(0),&
!!        & 'and real',huge(0.0),'and double',huge(0.0d0))
!!        write(*,'(a)')pr
!!        pr=msg('real            :',&
!!         & huge(0.0),0.0,12345.6789,tiny(0.0) )
!!        write(*,'(a)')pr
!!        pr=msg('doubleprecision :',&
!!         & huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
!!        write(*,'(a)')pr
!!        pr=msg('complex         :',&
!!         & cmplx(huge(0.0),tiny(0.0)) )
!!        write(*,'(a)')pr
!!
!!        ! create a format on the fly
!!        biggest=huge(0)
!!        frmt=msg('(*(i',int(log10(real(biggest))),':,1x))',sep='')
!!        write(*,*)'format=',frmt
!!
!!        ! although it will often work, using msg(3f) in an I/O statement
!!        ! is not recommended
!!        write(*,*)msg('program will now stop')
!!
!!        end program demo_msg
!!
!!   Output
!!
!!       HUGE(3f) integers 2147483647 and real 3.40282347E+38
!!       and double 1.7976931348623157E+308
!!       real            : 3.40282347E+38 0.00000000
!!       12345.6787 1.17549435E-38
!!       doubleprecision : 1.7976931348623157E+308 0.0000000000000000
!!       12345.678900000001 2.2250738585072014E-308
!!       complex         : (3.40282347E+38,1.17549435E-38)
!!        format=(*(i9:,1x))
!!        program will now stop
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
function msg_scalar(generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9,sep)
implicit none

! ident_78="@(#)M_strings::msg_scalar(3fp): writes a message to a string composed of any standard scalar types"

class(*),intent(in),optional  :: generic1 ,generic2 ,generic3 ,generic4 ,generic5
class(*),intent(in),optional  :: generic6 ,generic7 ,generic8 ,generic9
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
character(len=:), allocatable :: msg_scalar
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(present(sep))then
      sep_local=sep
      increment=len(sep)+1
   else
      sep_local=' '
      increment=2
   endif

   istart=1
   line=' '
   if(present(generic1))call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   msg_scalar=trim(line)
contains
!===================================================================================================================================
subroutine print_generic(generic)
!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, biggest=>int64, real32, real64, dp=>real128
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in) :: generic
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'(i0)') generic
      type is (integer(kind=int16));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int32));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int64));    write(line(istart:),'(i0)') generic
      type is (real(kind=real32));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real64));      write(line(istart:),'(1pg0)') generic
      !*!type is (real(kind=real128));     write(line(istart:),'(1pg0)') generic
      type is (logical);                write(line(istart:),'(l1)') generic
      type is (character(len=*));       write(line(istart:),'(a)') trim(generic)
      type is (complex);                write(line(istart:),'("(",1pg0,",",1pg0,")")') generic
   end select
   istart=len_trim(line)+increment
   line=trim(line)//sep_local
end subroutine print_generic
!===================================================================================================================================
end function msg_scalar
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function msg_one(generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9,sep)
implicit none

! ident_79="@(#)M_strings::msg_one(3fp): writes a message to a string composed of any standard one dimensional types"

class(*),intent(in)           :: generic1(:)
class(*),intent(in),optional  :: generic2(:), generic3(:), generic4(:), generic5(:)
class(*),intent(in),optional  :: generic6(:), generic7(:), generic8(:), generic9(:)
character(len=*),intent(in),optional :: sep
character(len=:),allocatable   :: sep_local
character(len=:), allocatable :: msg_one
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(present(sep))then
      sep_local=sep
      increment=len(sep)+1
   else
      sep_local=' '
      increment=2
   endif

   istart=1
   line=' '
   call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   msg_one=trim(line)
contains
!===================================================================================================================================
subroutine print_generic(generic)
!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, biggest=>int64, real32, real64, dp=>real128
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in),optional :: generic(:)
integer :: i
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int16));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int32));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int64));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (real(kind=real32));      write(line(istart:),'("[",*(1pg0,1x))') generic
      type is (real(kind=real64));      write(line(istart:),'("[",*(1pg0,1x))') generic
      !*!type is (real(kind=real128));     write(line(istart:),'("[",*(1pg0,1x))') generic
      type is (logical);                write(line(istart:),'("[",*(l1,1x))') generic
      type is (character(len=*));       write(line(istart:),'("[",:*("""",a,"""",1x))') (trim(generic(i)),i=1,size(generic))
      type is (complex);                write(line(istart:),'("[",*("(",1pg0,",",1pg0,")",1x))') generic
   end select
   istart=len_trim(line)+increment
   line=trim(line)//"]"//sep_local
end subroutine print_generic
!===================================================================================================================================
end function msg_one
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    split2020(3f) - parse a string into tokens
!!
!!##SYNOPSIS
!!
!!   TOKEN form
!!
!!    subroutine split2020 (string, set, tokens, separator)
!!    character(len=*),intent(in) :: string
!!    character(len=*),intent(in) :: set
!!    character(len=:),allocatable,intent(out) :: tokens(:)
!!    character(len=1),allocatable,intent(out),optional :: separator(:)
!!
!!   BOUNDS ARRAY form
!!
!!    subroutine split2020 (string, set, first, last)
!!    character(len=*),intent(in) :: string
!!    character(len=*),intent(in) :: set
!!    integer,allocatable,intent(out) :: first(:)
!!    integer,allocatable,intent(out) :: last(:)
!!
!!   STEP THROUGH BY POSITION form
!!
!!    subroutine split2020 (string, set, pos [, back])
!!    character(len=*),intent(in) :: string
!!    character(len=*),intent(in) :: set
!!    integer,intent(inout)       :: pos
!!    logical,intent(in),optional :: back
!!
!!##DESCRIPTION
!!    Parse a string into tokens. STRING, SET, TOKENS and SEPARATOR must
!!    all be of the same CHARACTER kind type parameter.
!!
!!##OPTIONS
!!    STRING      string to break into tokens
!!
!!    SET         Each character in SET is a token delimiter. A
!!                sequence of zero or more characters in STRING delimited by
!!                any token delimiter, or the beginning or end of STRING,
!!                comprise a token. Thus, two consecutive token delimiters
!!                in STRING, or a token delimiter in the first or last
!!                character of STRING, indicate a token with zero length.
!!
!!                ??? how about if null defaults to all whitespace characters
!!
!!    TOKENS      It is allocated with the lower bound equal to
!!                one and the upper bound equal to the number of tokens in
!!                STRING, and with character length equal to the length of
!!                the longest token. The tokens in STRING are assigned by
!!                intrinsic assignment, in the order found, to the elements
!!                of TOKENS, in array element order.
!!
!!                ???If input is null it still must be of size 1?
!!
!!    SEPARATOR   Each element in SEPARATOR(i) is assigned the value of
!!                the ith token delimiter in STRING.
!!                It is allocated with the lower bound equal to
!!                one and the upper bound equal to one less than the number
!!                of tokens in STRING, and with character length equal to
!!                one.
!!
!!                ???one less than? '' ' '
!!
!!    FIRST     It is allocated with the lower bound equal to one and the
!!              upper bound equal to the number of tokens in STRING. Each
!!              element is assigned, in array element order, the starting
!!              position of each token in STRING, in the order found. If a
!!              token has zero length, the starting position is equal to one
!!              if the token is at the beginning of STRING, and one greater
!!              than the position of the preceding delimiter otherwise.
!!
!!    LAST      It is allocated with the lower bound equal to one and the
!!              upper bound equal to the number of tokens in STRING. Each
!!              element is assigned, in array element order, the ending
!!              position of each token in STRING, in the order found. If
!!              a token has zero length, the ending position is one less
!!              than the starting position.
!!
!!    POS       If BACK is present with the value .TRUE., the value
!!              of POS shall be in the range 0 < POS     LEN (STRING)+1;
!!              otherwise it shall be in the range 0     POS LEN (STRING).
!!
!!              If BACK is absent or is present with the value .FALSE., POS
!!              is assigned the position of the leftmost token delimiter in
!!              STRING whose position is greater than POS, or if there is
!!              no such character, it is assigned a value one greater than
!!              the length of STRING. This identifies a token with starting
!!              position one greater than the value of POS on invocation,
!!              and ending position one less than the value of POS on return.
!!
!!              If BACK is present with the value true, POS is assigned the
!!              position of the rightmost token delimiter in STRING whose
!!              position is less than POS, or if there is no such character,
!!              it is assigned the value zero. This identifies a token with
!!              ending position one less than the value of POS on invocation,
!!              and starting position one greater than the value of POS
!!              on return.
!!
!!              When SPLIT is invoked with a value for POS of
!!              1 <= POS <= LEN(STRING) and STRING(POS:POS) is not a
!!              token delimiter present in SET, the token identified by
!!              SPLIT does not comprise a complete token as described in the
!!              description of the SET argument, but rather a partial token.
!!
!!    BACK      shall be a logical scalar. It is an INTENT (IN) argument. If
!!              POS does not appear and BACK is present with the value true,
!!              STRING is scanned backwards for tokens starting from the
!!              end. If POS does not appear and BACK is absent or present
!!              with the value false, STRING is scanned forwards for tokens
!!              starting from the beginning.
!!
!!##EXAMPLES
!!
!! Sample of uses
!!
!!    program demo_sort2020
!!    use M_strings, only : split2020
!!    implicit none
!!    character(len=*),parameter :: gen='(*("[",g0,"]":,","))'
!!
!!     ! Execution of TOKEN form
!!     block
!!       character (len=:), allocatable :: string
!!       character (len=:), allocatable :: tokens(:)
!!       character (len=*),parameter :: set = " ,"
!!       string = 'first,second,third'
!!       call split2020(string, set, tokens )
!!       write(*,gen)tokens
!!
!!     ! assigns the value ['first ','second','third ' ]
!!     ! to TOKENS.
!!     endblock
!!
!!     ! Execution of BOUNDS form
!!
!!     block
!!       character (len=:), allocatable :: string
!!       character (len=*),parameter :: set = " ,"
!!       integer, allocatable        :: first(:), last(:)
!!       string =    'first,second,,forth'
!!       call split2020 (string, set, first, last)
!!       write(*,gen)first
!!       write(*,gen)last
!!
!!     ! will assign the value [ 1, 7, 14, 15 ] to FIRST,
!!     ! and the value [ 5, 12, 13, 19 ] to LAST.
!!     endblock
!!
!!     ! Execution of STEP form
!!     block
!!       character (len=:), allocatable :: string
!!       character (len=*),parameter :: set = " ,"
!!       integer :: p, istart, iend
!!       string = " one,   last  example  "
!!       do while (p < len(string))
!!         istart = p + 1
!!         call split2020 (string, set, p)
!!         iend=p-1
!!         if(iend.gt.istart)then
!!            print '(t3,a,1x,i0,1x,i0)', string (istart:iend),istart,iend
!!         endif
!!       enddo
!!     endblock
!!    end program demo_sort2020
!!
!!   Results:
!!
!!    [first ],[second],[third ]
!!    [1],[7],[14],[15]
!!    [5],[12],[13],[19]
!!      one 2 4
!!      last 9 12
!!      example 15 21
!!
!!      > ??? option to skip adjacent delimiters (not return null tokens) common with whitespace
!!      > ??? quoted strings, especially CSV both " and ', Fortran adjacent is insert versus other rules
!!      > ??? escape character like \!!      > ??? multi-character delimiters like \\n, \\t,
!!      > ??? regular expression separator
!!
!!##AUTHOR
!!    Milan Curcic, "milancurcic@hey.com"
!!
!!##LICENSE
!!    MIT
!!
!!##VERSION
!!    version 0.1.0, copyright 2020, Milan Curcic
  pure subroutine split_tokens(string, set, tokens, separator)
    !! Splits a string into tokens using characters in set as token delimiters.
    !! If present, separator contains the array of token delimiters.
    character(*), intent(in) :: string
    character(*), intent(in) :: set
    character(:), allocatable, intent(out) :: tokens(:)
    character, allocatable, intent(out), optional :: separator(:)

    integer, allocatable :: first(:), last(:)
    integer :: n

    call split2020(string, set, first, last)
    allocate(character(len=maxval(last - first) + 1) :: tokens(size(first)))

    do concurrent (n = 1:size(tokens))
      tokens(n) = string(first(n):last(n))
    enddo

    if (present(separator)) then
      allocate(separator(size(tokens) - 1))
      do concurrent (n = 1:size(tokens) - 1)
        separator(n) = string(first(n+1)-1:first(n+1)-1)
      enddo
    endif

  end subroutine split_tokens
!===================================================================================================================================
  pure subroutine split_first_last(string, set, first, last)
    !! Computes the first and last indices of tokens in input string, delimited
    !! by the characters in set, and stores them into first and last output
    !! arrays.
    character(*), intent(in) :: string
    character(*), intent(in) :: set
    integer, allocatable, intent(out) :: first(:)
    integer, allocatable, intent(out) :: last(:)

    character :: set_array(len(set))
    logical, dimension(len(string)) :: is_first, is_last, is_separator
    integer :: n, slen

    slen = len(string)

    do concurrent (n = 1:len(set))
      set_array(n) = set(n:n)
    enddo

    do concurrent (n = 1:slen)
      is_separator(n) = any(string(n:n) == set_array)
    enddo

    is_first = .false.
    is_last = .false.

    if (.not. is_separator(1)) is_first(1) = .true.

    do concurrent (n = 2:slen-1)
      if (.not. is_separator(n)) then
        if (is_separator(n - 1)) is_first(n) = .true.
        if (is_separator(n + 1)) is_last(n) = .true.
      else
        if (is_separator(n - 1)) then
          is_first(n) = .true.
          is_last(n-1) = .true.
        endif
      endif
    enddo

    if (.not. is_separator(slen)) is_last(slen) = .true.

    first = pack([(n, n = 1, slen)], is_first)
    last = pack([(n, n = 1, slen)], is_last)

  end subroutine split_first_last
!===================================================================================================================================
  pure subroutine split_pos(string, set, pos, back)
    !! If back is absent, computes the leftmost token delimiter in string whose
    !! position is > pos. If back is present and true, computes the rightmost
    !! token delimiter in string whose position is < pos. The result is stored
    !! in pos.
    character(*), intent(in) :: string
    character(*), intent(in) :: set
    integer, intent(in out) :: pos
    logical, intent(in), optional :: back

    logical :: backward
    character :: set_array(len(set))
    integer :: n, result_pos

    !TODO use optval when implemented in stdlib
    !backward = optval(back, .false.)
    backward = .false.
    if (present(back)) backward = back

    do concurrent (n = 1:len(set))
      set_array(n) = set(n:n)
    enddo

    if (backward) then
      result_pos = 0
      do n = pos - 1, 1, -1
        if (any(string(n:n) == set_array)) then
          result_pos = n
          exit
        endif
      enddo
    else
      result_pos = len(string) + 1
      do n = pos + 1, len(string)
        if (any(string(n:n) == set_array)) then
          result_pos = n
          exit
        endif
      enddo
    endif

    pos = result_pos

  end subroutine split_pos
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
  pure function string_tokens(string, set) result(tokens)
    !! Splits a string into tokens using characters in set as token delimiters.
    character(*), intent(in) :: string
    character(*), intent(in) :: set
    character(:), allocatable :: tokens(:)
    call split_tokens(string, set, tokens)
  end function string_tokens
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function lowercase(str) result(lcstr)

! convert string to lower case leaving quoted strings as is

character (len=*):: str
character (len=len_trim(str)):: lcstr
integer :: ilen
integer :: ioffset
integer :: iquote
integer :: i
integer :: iav
integer :: iqc

ilen=len_trim(str)
ioffset=iachar('A')-iachar('a')
iquote=0
lcstr=str
do i=1,ilen
  iav=iachar(str(i:i))
  if(iquote==0 .and. (iav==34 .or.iav==39)) then
    iquote=1
    iqc=iav
    cycle
  endif
  if(iquote==1 .and. iav==iqc) then
    iquote=0
    cycle
  endif
  if (iquote==1) cycle
  if(iav >= iachar('A') .and. iav <= iachar('Z')) then
    lcstr(i:i)=achar(iav-ioffset)
  else
    lcstr(i:i)=str(i:i)
  endif
enddo

end function lowercase
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function uppercase(str) result(ucstr)

! convert string to upper case leaving quoted strings as is

character (len=*):: str
character (len=len_trim(str)):: ucstr
integer :: ilen
integer :: ioffset
integer :: iquote
integer :: i
integer :: iav
integer :: iqc

ilen=len_trim(str)
ioffset=iachar('A')-iachar('a')
iquote=0
ucstr=str
do i=1,ilen
  iav=iachar(str(i:i))
  if(iquote==0 .and. (iav==34 .or.iav==39)) then
    iquote=1
    iqc=iav
    cycle
  endif
  if(iquote==1 .and. iav==iqc) then
    iquote=0
    cycle
  endif
  if (iquote==1) cycle
  if(iav >= iachar('a') .and. iav <= iachar('z')) then
    ucstr(i:i)=achar(iav+ioffset)
  else
    ucstr(i:i)=str(i:i)
  endif
enddo

end function uppercase
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine matching_delimiter(str,ipos,imatch)

! Sets imatch to the position in string of the delimiter matching the delimiter
! in position ipos. Allowable delimiters are (), [], {}, <>.

character(len=*) :: str
character :: delim1,delim2,ch
integer :: ipos
integer :: imatch
integer :: lenstr
integer :: idelim2
integer :: istart, iend
integer :: inc
integer :: isum
integer :: i

lenstr=len_trim(str)
delim1=str(ipos:ipos)
select case(delim1)
   case('(')
      idelim2=iachar(delim1)+1
      istart=ipos+1
      iend=lenstr
      inc=1
   case(')')
      idelim2=iachar(delim1)-1
      istart=ipos-1
      iend=1
      inc=-1
   case('[','{','<')
      idelim2=iachar(delim1)+2
      istart=ipos+1
      iend=lenstr
      inc=1
   case(']','}','>')
      idelim2=iachar(delim1)-2
      istart=ipos-1
      iend=1
      inc=-1
   case default
      write(*,*) delim1,' is not a valid delimiter'
      return
end select
if(istart < 1 .or. istart > lenstr) then
   write(*,*) delim1,' has no matching delimiter'
   return
endif
delim2=achar(idelim2) ! matching delimiter

isum=1
do i=istart,iend,inc
   ch=str(i:i)
   if(ch /= delim1 .and. ch /= delim2) cycle
   if(ch == delim1) isum=isum+1
   if(ch == delim2) isum=isum-1
   if(isum == 0) exit
enddo
if(isum /= 0) then
   write(*,*) delim1,' has no matching delimiter'
   return
endif
imatch=i

end subroutine matching_delimiter
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_strings











!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_time
use M_strings, only : upper, lower,  substitute, split, adjustc
use M_strings, only : string_to_values, s2v, v2s
use M_strings, only : compact, transliterate
use M_verify, only : stderr
use, intrinsic :: iso_fortran_env, only : int64
implicit none
! ident_1="@(#)M_time::M_time(3f): date and time function module"
private
!-----------------------------------------------------------------------------------------------------------------------------------
! UNIT TESTS

! EPOCH TIME (UT starts at 0000 on 1 Jan. 1970)
   public date_to_unix   !(dat,UNIXTIME,IERR)                 ! Convert date array to Unix Time
   public unix_to_date   !(unixtime,DAT,IERR)                 ! Convert Unix Time to date array
   public d2u            !(dat) result (UNIXTIME)             ! Convert date array to Unix Time
   public u2d            !(unixtime) result (DAT)             ! Convert Unix Time to date array
! JULIAN
   public julian_to_date !(julian,DAT,IERR)                   ! Convert Julian Date to date array
   public date_to_julian !(dat,JULIAN,IERR)                   ! Convert date array to Julian Date
   public d2j            !(dat) result (JULIAN)               ! Convert date array to Julian Date
   public j2d            !(julian) result (DAT)               ! Convert Julian Date to date array
! DAY OF WEEK
   public dow            !(dat,[WEEKDAY],[DAY],IERR)          ! Convert date array to day of the week as number(Mon=1) and name
! WEEK OF YEAR
   public d2w  !(dat,ISO_YEAR,ISO_WEEK,ISO_WEEKDAY,ISO_NAME)  ! Calculate iso-8601 Week-numbering year date yyyy-Www-d
   public w2d  !(iso_year,iso_week,iso_weekday,DAT)           ! given iso-8601 Week-numbering year date yyyy-Www-d calculate date
! ORDINAL DAY
   public d2o            !(dat) result(ORDINAL)               ! given date array return ordinal day of year, Jan 1st=1
   public o2d            !(ordinal) result(DAT)               ! given ordinal day of year return date array, Jan 1st=1
   public ordinal_to_date!(year,ordinal_day,DAT)              ! given ordinal day of year return date array, Jan 1st=1
   public ordinal_seconds!()                                  ! seconds since the beginning of current year
! PRINTING DATES
   public fmtdate        !(dat,format) result (TIMESTR)       ! Convert date array to string using format
   public fmtdate_usage  !(indent)                            ! display macros recognized by fmtdate(3f)
   public now            !(format) result (NOW)               ! return string representing current time given format
   public box_month      !(dat,CALEN)                         ! print specified month into character array
! PRINTING DURATIONS
   public sec2days       !(seconds) result (dhms)             ! converts seconds to string D-HH:MM:SS
   public days2sec       !(str) result (seconds)              ! converts string D-HH:MM:SS to seconds from small to large
! MONTH NAME
   public mo2v           !(month_name) result (MONTH_NUMBER)  ! given month name return month number
   public v2mo           !(month_number) result (MONTH_NAME)  ! given month number return month name
   public mo2d           !(month_name) result (DAT)           ! given month name and year return date array for 1st day of month
! ASTROLOGICAL
   public easter         !(year,dat)                          ! calculate month and day Easter falls on for given year
   public moon_fullness  !(datin) result(FULLNESS)            ! percentage of moon phase from new to full
   public phase_of_moon  !(datin) result(PHASE)               ! return name for phase of moon for given date
!x! public ephemeris      !(dat,planet,DD,DM,DC,AH,AM)         ! ephemeris position of planets for adjusting an equatorial telescope
! READING DATES
   public guessdate      !(anot,dat)                          ! Converts a date string to a date array, in various formats
! C INTERFACE
   public system_sleep   !(wait_seconds)                      ! Call sleep(3c)
   private call_sleep
   private call_usleep
!-----------------------------------------------------------------------------------------------------------------------------------
integer,parameter,public   :: realtime=kind(0.0d0)            ! type for 1 epoch time and julian days
!-----------------------------------------------------------------------------------------------------------------------------------
! INTERNAL
real(kind=realtime),parameter,private :: SECDAY=86400.0d0     ! 24:00:00 hours as seconds
!-----------------------------------------------------------------------------------------------------------------------------------
!  integer,parameter       :: igreg_1582=15+31*(10+12*1582)   ! ASSUMES: Gregorian Calendar was adopted 15 Oct. 1582 (588829)
!  integer,parameter       :: igreg_1752=03+31*( 9+12*1752)   ! ASSUMES: Gregorian Calendar was adopted 3 Sep. 1752 (652026)
!  integer,save            :: igreg=igreg_1582
!-----------------------------------------------------------------------------------------------------------------------------------
! CONVENIENT CONSTANTS FOR USE WITH + AND - OPERATORS
real(kind=realtime),public,parameter :: dt_minute=60.0d0      ! one minute in seconds
real(kind=realtime),public,parameter :: dt_hour=3600.0d0      ! one hour in seconds
real(kind=realtime),public,parameter :: dt_day=86400.0d0      ! 24:00:00 hours in seconds
real(kind=realtime),public,parameter :: dt_week=dt_day*7.0d0  ! one week in seconds
!-----------------------------------------------------------------------------------------------------------------------------------
 contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    date_to_julian(3f) - [M_time:JULIAN] converts DAT date-time array to
!!    Julian Date
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine date_to_julian(dat,juliandate,ierr)
!!
!!     integer,intent(in)               :: dat(8)
!!     real(kind=realtime),intent(out)  :: juliandate
!!     integer,intent(out)              :: ierr
!!
!!##DESCRIPTION
!!   Converts a DAT date-time array to a Unix Epoch Time (UET) value.
!!   UET is the number of seconds since 00:00 on January 1st, 1970, UTC.
!!
!!##OPTIONS
!!    dat   Integer array holding a "DAT" array, similar in structure
!!          to the array returned by the intrinsic DATE_AND_TIME(3f):
!!
!!           dat=[ year,month,day,timezone,hour,&
!!               & minutes,seconds,milliseconds]
!!
!!##RETURNS
!!    juliandate  A Julian Ephemeris Date (JED) is the number of days since
!!                noon (not midnight) on January 1st, 4713 BC.
!!    ierr        Error code. If 0 no error occurred.
!!
!!##EXAMPLE
!!
!!    Sample Program:
!!
!!     program demo_date_to_julian
!!     use M_time, only : date_to_julian,realtime
!!     implicit none
!!     integer             :: dat(8)
!!     real(kind=realtime) :: juliandate
!!     integer             :: ierr
!!        ! generate DAT array
!!        call date_and_time(values=dat)
!!        ! show DAT array
!!        write(*,'(" Today is:",*(i0:,":"))')dat
!!        ! convert DAT to Julian Date
!!        call date_to_julian(dat,juliandate,ierr)
!!        write(*,*)'Julian Date is ',juliandate
!!        write(*,*)'ierr is ',ierr
!!     end program demo_date_to_julian
!!
!!    results:
!!
!!     Today is:2016:7:19:-240:11:3:13:821
!!     Julian Date is    2457589.1272432986
!!     ierr is            0
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!##LICENSE
!!    Public Domain
subroutine date_to_julian(dat,julian,ierr)
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!! AUTHOR:    John S. Urban
!!##VERSION:   1.0 2015-12-21
!! REFERENCE: From Wikipedia, the free encyclopedia 2015-12-19
! * There is no year zero
! * Julian Date must be non-negative
! * Julian Date starts at noon; while Civil Calendar date starts at midnight
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_2="@(#)M_time::date_to_julian(3f): Converts proleptic Gregorian DAT date-time array to Julian Date"

integer,intent(in)               :: dat(8)   ! array like returned by DATE_AND_TIME(3f)
real(kind=realtime),intent(out)  :: julian   ! Julian Date (non-negative, but may be non-integer)
integer,intent(out)              :: ierr     ! Error return: 0 =successful execution,-1=invalid year,-2=invalid month,-3=invalid day
                                             ! -4=invalid date (29th Feb, non leap-year)
integer                          :: year, month, day, utc, hour, minute
real(kind=realtime)              :: second
integer                          :: A, Y, M, JDN
!-----------------------------------------------------------------------------------------------------------------------------------
   year   = dat(1)                        ! Year
   month  = dat(2)                        ! Month
   day    = dat(3)                        ! Day
   utc    = dat(4)*60                     ! Delta from UTC, convert from minutes to seconds
   hour   = dat(5)                        ! Hour
   minute = dat(6)                        ! Minute
   second = dat(7)-utc+dat(8)/1000.0d0    ! Second   ! correction for time zone and milliseconds
!-----------------------------------------------------------------------------------------------------------------------------------
   julian = -HUGE(99999)                  ! this is the date if an error occurs and IERR is < 0
!-----------------------------------------------------------------------------------------------------------------------------------
   if(year==0 .or. year .lt. -4713) then
      ierr=-1
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  You must compute first the number of years (Y) and months (M) since March 1st -4800 (March 1, 4801 BC)
   A=(14-month)/12 ! A will be 1 for January or February, and 0 for other months, with integer truncation
   Y=year+4800-A
   M=month+12*A-3  ! M will be 0 for March and 11 for February
!  All years in the BC era must be converted to astronomical years, so that 1BC is year 0, 2 BC is year "-1", etc.
!  Convert to a negative number, then increment towards zero
!  Staring from a Gregorian calendar date
   JDN=day + (153*M+2)/5 + 365*Y + Y/4 - Y/100 + Y/400 - 32045  !  with integer truncation
!  Finding the Julian Calendar date given the JDN (Julian day number) and time of day
   julian=JDN + dble(hour-12)/24.0d0 + dble(minute)/1440.0d0 + second/86400.0d0
!-----------------------------------------------------------------------------------------------------------------------------------
   if(julian.lt.0.d0) then                  ! Julian Day must be non-negative
      ierr=1
   else
      ierr=0
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine date_to_julian
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    julian_to_date(3f) - [M_time:JULIAN] converts a JED(Julian Ephemeris
!!    Date) to a DAT date-time array.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine julian_to_date(julian,dat,ierr)
!!
!!     real(kind=realtime),intent(in) :: julian
!!     integer,intent(out)            :: dat(8)
!!     integer,intent(out)            :: ierr
!!
!!##DESCRIPTION
!!   Converts a Unix Epoch Time (UET) value to a DAT date-time array.
!!   UET is the number of seconds since 00:00 on January 1st, 1970, UTC.
!!
!!##OPTIONS
!!     julian  Julian Date (days)
!!     dat     Integer array holding a "DAT" array, similar in structure
!!             to the array returned by the intrinsic DATE_AND_TIME(3f):
!!
!!              dat=[ year,month,day,timezone,hour,&
!!               & minutes,seconds,milliseconds]
!!
!!##RETURNS
!!    unixtime  The "Unix Epoch" time, or the number of seconds since 00:00:00 on
!!              January 1st, 1970, UTC.
!!    ierr      Error code. If 0 no error occurred.
!!
!!##EXAMPLE
!!
!!     Sample program:
!!
!!      program demo_julian_to_date
!!      use M_time, only : julian_to_date, fmtdate, realtime
!!      implicit none
!!      real(kind=realtime)     :: juliandate
!!      integer                 :: dat(8)
!!      integer                 :: ierr
!!         ! set sample Julian Date
!!         juliandate=2457589.129d0
!!         ! create DAT array for this date
!!         call julian_to_date(juliandate,dat,ierr)
!!         write(*,*)'Sample Date=',fmtdate(dat)
!!         ! go back one day
!!         call julian_to_date(juliandate-1.0d0,dat,ierr)
!!         write(*,*)'Day Before =',fmtdate(dat)
!!         ! go forward one day
!!         call julian_to_date(juliandate+1.0d0,dat,ierr)
!!         write(*,*)'Day After  =',fmtdate(dat)
!!      end program demo_julian_to_date
!!
!!     Results:
!!
!!      Sample Date=Tuesday, July 19th, 2016 11:05:45 AM UTC-04:00
!!      Day Before =Monday, July 18th, 2016 11:05:45 AM UTC-04:00
!!      Day After  =Wednesday, July 20th, 2016 11:05:45 AM UTC-04:00
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!##LICENSE
!!    Public Domain
subroutine julian_to_date(julian,dat,ierr)

! ident_3="@(#)M_time::julian_to_date(3f): Converts Julian Date to DAT date-time array"

real(kind=realtime),intent(in)   :: julian            ! Julian Date (non-negative)
integer,intent(out)              :: dat(8)
integer,intent(out)              :: ierr              ! 0 for successful execution, otherwise 1
integer                          :: tz
real(kind=realtime)              :: second
integer                          :: year
integer                          :: month
integer                          :: day
integer                          :: hour
integer                          :: minute
integer                          :: jalpha,ja,jb,jc,jd,je,ijul

   if(julian.lt.0.d0) then                      ! Negative Julian Date not allowed
      ierr=1
      return
   else
      ierr=0
   endif
   tz=get_timezone()

   ijul=idint(julian)                           ! Integral Julian Date
   second=sngl((julian-dble(ijul))*secday)      ! Seconds from beginning of Jul. Day
   second=second+(tz*60)

   if(second.ge.(secday/2.0d0)) then            ! In next calendar day
      ijul=ijul+1
      second=second-(secday/2.0d0)              ! Adjust from noon to midnight
   else                                         ! In same calendar day
      second=second+(secday/2.0d0)              ! Adjust from noon to midnight
   endif

   if(second.ge.secday) then                    ! Final check to prevent time 24:00:00
      ijul=ijul+1
      second=second-secday
   endif

   minute=int(second/60.0d0)                    ! Integral minutes from beginning of day
   second=second-dble(minute*60)                ! Seconds from beginning of minute
   hour=minute/60                               ! Integral hours from beginning of day
   minute=minute-hour*60                        ! Integral minutes from beginning of hour

   !---------------------------------------------
   jalpha=idint((dble(ijul-1867216)-0.25d0)/36524.25d0) ! Correction for Gregorian Calendar
   ja=ijul+1+jalpha-idint(0.25d0*dble(jalpha))
   !---------------------------------------------

   jb=ja+1524
   jc=idint(6680.d0+(dble(jb-2439870)-122.1d0)/365.25d0)
   jd=365*jc+idint(0.25d0*dble(jc))
   je=idint(dble(jb-jd)/30.6001d0)
   day=jb-jd-idint(30.6001d0*dble(je))
   month=je-1

   if(month.gt.12)then
      month=month-12
   endif

   year=jc-4715
   if(month.gt.2)then
      year=year-1
   endif

   if(year.le.0)then
      year=year-1
   endif

   dat=[ year, month, day, tz, hour, minute, int(second), int((second-int(second))*1000.0)]
   ierr=0

end subroutine julian_to_date
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    date_to_unix(3f) - [M_time:UNIX_EPOCH] converts DAT date-time array to Unix
!!    Epoch Time
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine date_to_unix(dat,unixtime,ierr)
!!
!!     integer,intent(in)               :: dat(8)
!!     real(kind=realtime),intent(out)  :: unixtime
!!     integer,intent(out)              :: ierr
!!
!!##DESCRIPTION
!!   Converts a DAT date-time array to a UET (Unix Epoch Time).
!!
!!##OPTIONS
!!    dat   Integer array holding a "DAT" array, similar in structure
!!          to the array returned by the intrinsic DATE_AND_TIME(3f):
!!
!!              dat=[ year,month,day,timezone,hour,&
!!               & minutes,seconds,milliseconds]
!!##RETURNS
!!    unixtime  The "Unix Epoch" time, or the number of seconds since
!!              00:00:00 on January 1st, 1970, UTC.
!!    ierr      Error code. If 0 no error occurred.
!!
!!##EXAMPLE
!!
!!     Sample program:
!!
!!      program demo_date_to_unix
!!      use M_time, only : date_to_unix, realtime
!!      implicit none
!!      integer             :: dat(8)
!!      real(kind=realtime) :: unixtime
!!      integer             :: ierr
!!         call date_and_time(values=dat)
!!         write(*,'(" Today is:",*(i0:,":"))')dat
!!         call date_to_unix(dat,unixtime,ierr)
!!         write(*,*)'Unix Epoch time is ',unixtime
!!         write(*,*)'ierr is ',ierr
!!      end program demo_date_to_unix
!!
!!     results:
!!
!!      Today is:2016:7:18:-240:23:44:20:434
!!      Unix Epoch time is    1468899860.4340105
!!      ierr is            0
!!##AUTHOR
!!    John S. Urban, 2015
!!##LICENSE
!!    Public Domain
subroutine date_to_unix(dat,unixtime,ierr)

! ident_4="@(#)M_time::date_to_unix(3f): Convert DAT date-time array to Unix Epoch Time"

integer,intent(in)              :: dat(8)       ! date time array similar to that returned by DATE_AND_TIME
real(kind=realtime),intent(out) :: unixtime     ! Unix time (seconds)
integer,intent(out)             :: ierr         ! return 0 on success, otherwise 1
real(kind=realtime)             :: julian
real(kind=realtime),save        :: julian_at_epoch
logical,save                    :: first=.true.
!-----------------------------------------------------------------------------------------------------------------------------------
if(first) then                                        ! Convert zero of Unix Epoch Time to Julian Date and save
   call date_to_julian([1970,1,1,0,0,0,0,0],julian_at_epoch,ierr)
   if(ierr.ne.0) return                               ! Error
   first=.false.
endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call date_to_julian(dat,julian,ierr)
   if(ierr.ne.0) return                               ! Error
   unixtime=(julian-julian_at_epoch)*secday
end subroutine date_to_unix
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    unix_to_date(3f) - [M_time:UNIX_EPOCH] converts Unix Epoch Time to
!!    DAT date-time
!!    array
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unix_to_date(unixtime,dat,ierr)
!!
!!     real(kind=realtime),intent(in) :: unixtime
!!     integer,intent(out)            :: dat(8)
!!     integer,intent(out)            :: ierr
!!
!!##DESCRIPTION
!!   Converts a Unix Epoch Time (UET) to a DAT date-time array.
!!
!!##OPTIONS
!!
!!    unixtime  The "Unix Epoch" time, or the number of seconds since
!!              00:00:00 on January 1st, 1970, UTC; of type
!!              real(kind=realtime).
!!
!!##RETURNS
!!     dat      Integer array holding a "DAT" array, similar in structure
!!              to the array returned by the intrinsic DATE_AND_TIME(3f):
!!
!!               dat=[ year,month,day,timezone,hour,&
!!               & minutes,seconds,milliseconds]
!!
!!    ierr      Error code. If 0 no error occurred.
!!
!!##EXAMPLE
!!
!!     Sample program:
!!
!!      program demo_unix_to_date
!!      use M_time, only : unix_to_date, u2d, fmtdate, realtime
!!      implicit none
!!      real(kind=realtime)           :: unixtime
!!      ! seconds in a day
!!      real(kind=realtime),parameter :: DAY=86400.0d0
!!      integer                       :: dat(8)
!!      integer                       :: ierr
!!         ! sample Unix Epoch time
!!         unixtime=1468939038.4639933d0
!!         ! create DAT array for today
!!         call unix_to_date(unixtime,dat,ierr)
!!         write(*,*)'Sample Date=',fmtdate(dat)
!!         ! go back one day
!!         call unix_to_date(unixtime-DAY,dat,ierr)
!!         ! subtract day and print
!!         write(*,*)'Day Before =',fmtdate(dat)
!!         ! go forward one day
!!         call unix_to_date(unixtime+DAY,dat,ierr)
!!         ! add day print
!!         write(*,*)'Day After  =',fmtdate(dat)
!!      end program demo_unix_to_date
!!
!!    Results:
!!
!!     Sample Date=Tuesday, July 19th, 2016 10:37:18 AM
!!     Day Before =Monday, July 18th, 2016 10:37:18 AM
!!     Day After  =Wednesday, July 20th, 2016 10:37:18 AM
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!##LICENSE
!!    Public Domain
subroutine unix_to_date(unixtime,dat,ierr)

! ident_5="@(#)M_time::unix_to_date(3f): Converts Unix Time to DAT date-time array"

class(*),intent(in)              :: unixtime                            ! Unix time (seconds)
integer,intent(out)              :: dat(8)                              ! date and time array
integer,intent(out)              :: ierr                                ! 0 for successful execution, otherwise 1
real(kind=realtime)              :: julian                              ! Unix time converted to a Julian Date
real(kind=realtime)              :: local_unixtime
real(kind=realtime),save         :: Unix_Origin_as_Julian               ! start of Unix Time as Julian Date
logical,save                     :: first=.TRUE.
!  Notice that the value UNIXTIME can be any of several types ( INTEGER,REAL,REAL(KIND=REALTIME))
   select type(unixtime)
   type is (integer);             local_unixtime=dble(unixtime)
   type is (real);                local_unixtime=dble(unixtime)  ! typically not precise enough for UET values.
   type is (real(kind=realtime)); local_unixtime=unixtime
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   if(first)then                                                             ! Initialize calculated constants on first call
      call date_to_julian([1970,1,1,0,0,0,0,0],Unix_Origin_as_Julian,ierr)   ! Compute start of Unix Time as a Julian Date
      if(ierr.ne.0) return                                                   ! Error
      first=.FALSE.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   julian=(local_unixtime/secday)+Unix_Origin_as_Julian           ! convert seconds from Unix Epoch to Julian Date
   call julian_to_date(julian,dat,ierr)                           ! calculate date-time array from Julian Date
   !dat(4)=get_timezone()                                          ! need to get time zone
end subroutine unix_to_date
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    d2o(3f) - [M_time:ORDINAL_DAY] converts DAT date-time array to Ordinal day
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function d2o(dat) result (ordinal)
!!
!!     integer,intent(in),optional :: dat(8)
!!     integer                     :: ordinal
!!
!!##DESCRIPTION
!!   Given a date in the form of a "DAT" array return the Ordinal Day,
!!   (ie. "the day of the year").
!!
!!##OPTIONS
!!     dat  Integer array holding a "DAT" array, similar in structure
!!          to the array returned by the intrinsic DATE_AND_TIME(3f):
!!
!!              dat=[ year,month,day,timezone,hour,&
!!               & minutes,seconds,milliseconds]
!!##RETURNS
!!     ordinal  The day of the year calculated for the given input date,
!!              where Jan 1st=1.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_d2o
!!     use M_time, only : d2o
!!     implicit none
!!     integer :: dat(8)
!!        call date_and_time(values=dat)
!!        write(*,'(" Today is:",*(i0:,":"))')dat
!!        write(*,*)'Day of year is:',d2o(dat)
!!
!!        ! year,month,day,timezone,hour,minute,seconds,milliseconds
!!        dat=[2020,12,31,-240,12,0,0,0]
!!        write(*,*)dat(1),' Days in year is:',d2o(dat)
!!
!!        dat=[2021,12,31,-240,12,0,0,0]
!!        write(*,*)dat(1),' Days in year is:',d2o(dat)
!!
!!        dat=[2022,12,31,-240,12,0,0,0]
!!        write(*,*)dat(1),' Days in year is:',d2o(dat)
!!
!!        dat=[2023,12,31,-240,12,0,0,0]
!!        write(*,*)dat(1),' Days in year is:',d2o(dat)
!!
!!        dat=[2024,12,31,-240,12,0,0,0]
!!        write(*,*)dat(1),' Days in year is:',d2o(dat)
!!
!!     end program demo_d2o
!!
!!    results:
!!
!!     Today is:2016:7:19:-240:20:1:19:829
!!     Day of year is:         201
!!            2020  Days in year is:         366
!!            2021  Days in year is:         365
!!            2022  Days in year is:         365
!!            2023  Days in year is:         365
!!            2024  Days in year is:         366
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!##LICENSE
!!    Public Domain
function d2o(dat) result (ordinal)

! ident_6="@(#)M_time::d2o(3f): Converts DAT date-time array to Ordinal day"

! JSU 2015-12-13
integer,intent(in),optional :: dat(8)                 ! date time array similar to that returned by DATE_AND_TIME
integer                     :: dat_local(8)
integer                     :: ordinal                ! the returned number of days
real(kind=realtime)         :: unixtime               ! Unix time (seconds)
real(kind=realtime)         :: unix_first_day
integer                     :: ierr                   ! return 0 on success, otherwise 1 from date_to_unix(3f)
   if(present(dat))then
     dat_local=dat
   else
     call date_and_time(values=dat_local)
   endif
   call date_to_unix(dat_local,unixtime,ierr)         ! convert date to Unix Epoch Time
   if(ierr.ne.0)then
      call stderr('*d2o* bad date array')
      ordinal=-1                                      ! initialize to bad value
   else
      call date_to_unix([dat_local(1),1,1,dat_local(4),0,0,0,0],unix_first_day,ierr)
      ordinal=int((unixtime-unix_first_day)/secday)+1
   endif
end function d2o
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    ordinal_seconds(3f) - [M_time:ORDINAL_DAY] seconds since beginning of year
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function ordinal_seconds()
!!
!!     integer :: ordinal_seconds
!!##DESCRIPTION
!!   Return number of seconds since beginning of current year.
!!
!!   Before using this routine consider the consequences if the application
!!   is running at the moment a new year begins.
!!
!!##EXAMPLE
!!
!!    sample program
!!
!!     program demo_ordinal_seconds
!!     use M_time, only : ordinal_seconds
!!     implicit none
!!     character(len=1) :: paws
!!     integer          :: ios
!!     integer          :: istart, iend
!!     istart=ordinal_seconds()
!!     write(*,'(a)',advance='no')'now pause. Enter return to continue ...'
!!     read(*,'(a)',iostat=ios) paws
!!     iend=ordinal_seconds()
!!     write(*,*)'that took ',iend-istart,'seconds'
!!     write(*,*)istart,iend
!!     end program demo_ordinal_seconds
!!##AUTHOR
!!    John S. Urban, 2015
!!##LICENSE
!!    Public Domain
integer function ordinal_seconds()

! ident_7="@(#)M_time::ordinal_seconds(3f): seconds since beginning of year"

integer                     :: vtime(8)
integer                     :: year, month, day, hour, minutes, seconds, timezone, milliseconds
integer                     :: ordinal_day_of_year
equivalence(vtime(1),year)
equivalence(vtime(2),month)
equivalence(vtime(3),day)
equivalence(vtime(4),timezone)
equivalence(vtime(5),hour)
equivalence(vtime(6),minutes)
equivalence(vtime(7),seconds)
equivalence(vtime(8),milliseconds)
   call date_and_time(values=vtime)
   ordinal_day_of_year=d2o(vtime)
   ordinal_seconds=ordinal_day_of_year*24*60*60 +hour*60*60 +minutes*60 +seconds
end function ordinal_seconds
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    ordinal_to_date(3f) - [M_time:ORDINAL_DAY] when given a valid year and
!!    day of the year returns the DAT array for the date
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!      subroutine ordinal_to_date(yyyy, ddd, dat)
!!
!!       integer, intent(in)   :: yyyy
!!       integer, intent(in)   :: ddd
!!       integer, intent(out)  :: dat
!!##DESCRIPTION
!!   When given a valid year, YYYY, and day of the year, DDD, returns the
!!   date as a DAT date array
!!##OPTIONS
!!       yyyy  known year
!!       ddd   known ordinal day of the year
!!##RETURNS
!!       dat   DAT array describing the date
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_ordinal_to_date
!!     use M_time, only : ordinal_to_date
!!     implicit none
!!     INTEGER            :: yyyy, ddd, mm, dd, yy
!!     integer            :: dat(8)
!!     integer            :: ios
!!       INFINITE: do
!!          write(*,'(a)',advance='no')&
!!          & 'Enter year YYYY and ordinal day of year DD '
!!          read(*,*,iostat=ios)yyyy,ddd
!!          if(ios.ne.0)exit INFINITE
!!          ! recover month and day from year and day number.
!!          call ordinal_to_date(yyyy, ddd, dat)
!!          yy=dat(1)
!!          mm=dat(2)
!!          dd=dat(3)
!!          write(*,'(*(g0))')'For Year ',yyyy,' and Ordinal day ',ddd,  &
!!          &         ' Month is ',mm,' and Day of Month is ',dd, &
!!          &         ' and Year is ',yy
!!        enddo INFINITE
!!     end program demo_ordinal_to_date
subroutine ordinal_to_date(yyyy,ddd,dat)
!x!use M_time, only : d2j,j2d, realtime

! ident_8="@(#)M_time::ordinal_to_date(3f): given a valid year and day of the year returns the DAT array for the date"

integer :: yyyy
integer :: ddd
integer :: dat(8)
   !dat=[year,month,day,timezone,hour,minutes,seconds,milliseconds]
   ! find Julian day for first day of given year and add ordinal day -1 and convert back to a DAT
   dat=j2d(d2j( [yyyy,1,1,0,12,0,0,0])+real(ddd-1,kind=realtime))
end subroutine ordinal_to_date
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    o2d(3f) - [M_time:ORDINAL_DAY] converts Ordinal day to DAT date-time array
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function o2d(ordinal,[year]) result (dat)
!!
!!     integer,intent(in) :: ordinal  ! the day of the year
!!     integer,optional   :: year     ! year
!!     integer            :: dat(8)   ! date time array
!!
!!##DESCRIPTION
!!   Given an Ordinal day of the year return a date in the form of a
!!   "DAT" array.
!!
!!##OPTIONS
!!     ordinal  The day of the year for the given year, where Jan 1st=1.
!!
!!     year     An optional year for the ordinal day. If not present the
!!              current year is assumed.
!!
!!##RETURNS
!!     dat   Integer array holding a "DAT" array, similar in structure
!!           to the array returned by the intrinsic DATE_AND_TIME(3f):
!!
!!              dat=[ year,month,day,timezone,hour,&
!!               & minutes,seconds,milliseconds]
!!
!!           The timezone value is from the current time on the current
!!           platform.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_o2d
!!     use M_time, only : o2d,fmtdate
!!     implicit none
!!     integer :: year
!!        do year=2004,2008
!!           write(*,*)&
!!           & '100th day of ',year,' is ',fmtdate(o2d(100,year))
!!        enddo
!!        write(*,*)'100th day of this year is ',fmtdate(o2d(100))
!!     end program demo_o2d
!!
!!    results:
!!
!!     100th day of 2004 is Friday, April 9th, 2004 ...
!!     00:00:00 PM UTC-02:40
!!     100th day of 2005 is Sunday, April 10th, 2005 ...
!!     00:00:00 PM UTC-02:40
!!     100th day of 2006 is Monday, April 10th, 2006 ...
!!     00:00:00 PM UTC-02:40
!!     100th day of 2007 is Tuesday, April 10th, 2007 ...
!!     00:00:00 PM UTC-02:40
!!     100th day of 2008 is Wednesday, April 9th, 2008 ...
!!     00:00:00 PM UTC-02:40
!!     100th day of this year is Saturday, April 9th, 2016 ...
!!     00:00:00 PM UTC-02:40
!!##AUTHOR
!!    John S. Urban, 2015
!!##LICENSE
!!    Public Domain
function o2d(ordinal,year) result (dat)

! ident_9="@(#)M_time::o2d(3f): Converts ordinal day to DAT date-time array"

integer                    :: dat(8)                  ! date time array similar to that returned by DATE_AND_TIME
integer,intent(in)         :: ordinal                 ! the returned number of days
integer,optional           :: year
real(kind=realtime)        :: unixtime                ! Unix time (seconds)
integer                    :: ierr                    ! return 0 on success, otherwise 1 from date_to_unix(3f)
   if(present(year))then
      dat=[year,1,ordinal,get_timezone(),0,0,0,0]     ! initialize DAT with parameters and set timezone, set HH:MM:SS.XX to zero
   else
      call date_and_time(values=dat)                  ! set year and timezone to current values
      dat=[dat(1),1,ordinal,dat(4),0,0,0,0]           ! apply ordinal parameter to January of current year, set HH:MM:SS.XX to zero
   endif
   ierr=0
   call date_to_unix(dat,unixtime,ierr)               ! convert date to Unix Epoch Time
   if(ierr.ne.0)then
      call stderr('*o2d* bad date array')
   else
      dat=u2d(unixtime)
   endif
end function o2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    v2mo(3f) - [M_time:MONTH_NAME] returns the month name of a Common
!!    month number
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function v2mo(imonth) result(month_name)
!!
!!     integer,intent(in)           :: imonth      ! month number (1-12)
!!     character(len=:),allocatable :: month_name  ! month name
!!
!!##DESCRIPTION
!!   Given a Common Calendar month number, return the name of the month
!!   as a string.
!!
!!##OPTIONS
!!    imonth      Common month number (1-12). If out of the allowable range
!!                the month name returned will be 'UNKNOWN'.
!!##RETURNS
!!    month_name  A string representing a month name or the word 'UNKNOWN'
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_v2mo
!!     use M_time, only : v2mo
!!     implicit none
!!     integer :: i
!!        write(*,*)(v2mo(i),i=1,13)
!!     end program demo_v2mo
!!
!!    results:
!!
!!     January
!!     February
!!     March
!!     April
!!     May
!!     June
!!     July
!!     August
!!     September
!!     October
!!     November
!!     December
!!     UNKNOWN.
!!##AUTHOR
!!    John S. Urban, 2015
!!##LICENSE
!!    Public Domain
function v2mo(imonth) result(month_name)

! ident_10="@(#)M_time::v2mo(3f): returns the month name of a Common month number"

! JSU 2015-12-13
character(len=:),allocatable :: month_name                                        ! string containing month name or abbreviation.
integer,intent(in)           :: imonth                                            ! the number of the month(1-12)
character(len=*),parameter   :: names(12)=[                                    &
   &'January  ', 'February ', 'March    ', 'April    ', 'May      ', 'June     ', &
   &'July     ', 'August   ', 'September', 'October  ', 'November ', 'December ']

   select case(imonth)
   case (1:12);        month_name=trim(names(imonth))
   case default;       month_name='UNKNOWN'
   end select

end function v2mo
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    mo2d(3f) - [M_time:MONTH_NAME] given month name return DAT date-time
!!    array for beginning of that month in specified year
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       function mo2d(month_name,year) result (dat)
!!
!!        character(len=*),intent(in) :: month_name
!!        integer,intent(in),optional :: year
!!        integer                     :: dat(8)
!!
!!##DESCRIPTION
!!   Given a Common Calendar month name, return the date as a "DAT" array
!!   for the 1st day of the month. An optional year may be specified. The
!!   year defaults to the current year.
!!
!!##OPTIONS
!!    month_name  A string representing a Common Calendar month name.
!!    year        Optional year. Defaults to current year
!!##RETURNS
!!    dat         An integer array that has the same structure as the array
!!                returned by the Fortran intrinsic DATE_AND_TIME(3f):
!!
!!                 dat=[ year,month,day,timezone,hour,&
!!                  & minutes,seconds,milliseconds]
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_mo2d
!!     use M_time, only : mo2d
!!     implicit none
!!        write(*,'(*(i0:,":"))')mo2d('March')
!!     end program demo_mo2d
!!
!!    results:
!!
!!       2016:3:1:-240:0:0:0:0
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!##LICENSE
!!    Public Domain
function mo2d(month_name,year) result (dat)

! ident_11="@(#)M_time::mo2d(3f): month name to DAT date-time array for 1st of that month in specified year"

character(len=*),intent(in) :: month_name
integer,intent(in),optional :: year
integer                     :: dat(8)
   call date_and_time(values=dat)
   if(present(year))then
      dat(1)=year
   endif
   dat(2)=mo2v(month_name) ! convert given month name to a number
   if(dat(2).le.0)then
      call stderr('*mo2d* bad month name '//trim(month_name))
      dat(2)=1
   endif
   dat(3)=1  ! set day to first of month
   dat(5)=0  ! set hour to zero
   dat(6)=0  ! set minutes to zero
   dat(7)=0  ! set seconds to zero
   dat(8)=0  ! set milliseconds to zero
end function mo2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    mo2v(3f) - [M_time:MONTH_NAME] given month name return month number
!!    (1-12) of that month
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function mo2v(month_name) result(imonth)
!!
!!      character(len=*),intent(in):: month_name ! month name
!!      integer                    :: imonth     ! month number
!!
!!##DESCRIPTION
!!   Given a string representing the name or abbreviation of a Gregorian
!!   Calendar month return a number representing the position of the
!!   month in the calendar starting with 1 for January and ending with
!!   12 for December.
!!
!!##OPTIONS
!!    month_name  name or abbreviation of month. Case is ignored
!!                Once enough characters are found to uniquely identify a
!!                month the rest of the name is ignored.
!!##RETURNS
!!    imonth      month number returned. If the name is not recognized a -1
!!                is returned.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_mo2v
!!     use M_time, only : mo2v
!!     implicit none
!!        write(*,*)mo2v("April")
!!        write(*,*)mo2v('Apr')
!!        ! NOTE: still matches September, as "SE" was enough
!!        write(*,*)mo2v('sexember')
!!        write(*,*)mo2v('unknown')  ! returns -1
!!     end program demo_mo2v
!!
!!    results:
!!
!!       >  4
!!       >  4
!!       >  9
!!       > -1
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!##LICENSE
!!    Public Domain
function mo2v(month_name) result(imonth)

! ident_12="@(#)M_time::mo2v(3f): given month name return month number (1-12) of that month"

! JSU 2015-12-13
character(len=*),intent(in):: month_name   ! string containing month name or abbreviation.
integer                    :: imonth       ! the number of the month(1-12), or -1 if the name could not be recognized.
character(len=3)           :: string
  string = upper(month_name)     ! Case is ignored; test string now guaranteed to have three characters
  imonth = 0
  FIND: select case(string(1:1)) ! The month name has to match up to the unique beginning of a month name, and the rest is ignored.
  case('F'); imonth=2      ! February
  case('S'); imonth=9      ! September
  case('O'); imonth=10     ! October
  case('N'); imonth=11     ! November
  case('D'); imonth=12     ! December
  case default
     select case(string(1:2))
     case('JA'); imonth=1    ! JAnuary
     case('AP'); imonth=4    ! APril
     case('AU'); imonth=8    ! AUgust
     case default
        select case(string(1:3))
        case('MAR'); imonth=3 ! MARch
        case('MAY'); imonth=5 ! MAY
        case('JUN'); imonth=6 ! JUNe
        case('JUL'); imonth=7 ! JULy
        case default
           imonth=-1
        end select
     end select
  end select FIND
end function mo2v
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    now(3f) - [M_time:DATE_PRINTING] return string representing current
!!    time given format
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function now(format) RESULT (timestr)
!!
!!     character(len=*),intent(in)     :: format  ! input format string
!!     character(len=:),allocatable    :: timestr ! formatted date
!!
!!##DESCRIPTION
!!   The now(3f) function is a call to the fmtdate(3f) function using the
!!   current date and time. That is, it is a convenient way to print the
!!   current date and time.
!!
!!##OPTIONS
!!     format      string describing how to format the current date and time.
!!                 For a complete description of the formatting macros
!!                 supported see fmtdate_usage(3f).
!!##RETURNS
!!     timestr     formatted output string representing date
!!
!!##EXAMPLE
!!
!!    Sample Program:
!!
!!     program demo_now
!!     use M_time, only : now
!!     implicit none
!!        write(*,*)now("The current date is %w, %l %d, %Y %H:%m:%s %N")
!!        call showme()
!!     contains
!!     subroutine showme() ! see all formatting options
!!     use M_time, only : fmtdate_usage
!!        call fmtdate_usage() ! see all formatting options
!!     end subroutine
!!     end program demo_now
!!
!!    results:
!!
!!       The current date is Sun, Jul 17th, 2016 01:21:35 PM
!!        ::
!!        :: description of all formatting options will appear here
!!        ::
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!##LICENSE
!!    Public Domain
function now(format)

! ident_13="@(#)M_time::now(3f): return string representing current time given format"

! JSU 2015-10-24
character(len=*),intent(in),optional :: format
character(len=:),allocatable         :: now
integer                              :: values(8)
!-----------------------------------------------------------------------------------------------------------------------------------
   call date_and_time(values=values)
   if(present(format))then
      now=fmtdate(values,format)
   else
      now=trim(fmtdate(values))
   endif
end function now
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    fmtdate(3f) - [M_time:DATE_PRINTING] given DAT date-time array return
!!    date as string using specified format
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function fmtdate(values,format) RESULT (timestr)
!!
!!     integer,dimension(8),intent(in)      :: values
!!     character(len=*),intent(in),optional :: format
!!     character(len=:),allocatable         :: timestr
!!
!!##DESCRIPTION
!!   The fmtdate(3f) procedure lets you reformat a DAT array in
!!   many common formats using a special string containing macro names
!!   beginning with '%'. To see the allowable macros call or see the
!!   fmtdate_usage(3f) routine.
!!
!!##OPTIONS
!!     values   date in a "DAT" array, which is the same format as
!!              the values returned by the intrinsic DATE_AND_TIME(3f):
!!
!!                 dat=[ year,month,day,timezone,hour,&
!!                  & minutes,seconds,milliseconds]
!!
!!     format   string describing how to format the "DAT" array.
!!              For a complete description of the formatting macros
!!              supported see fmtdate_usage(3f).
!!##RETURNS
!!     timestr  formatted output string representing date
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_fmtdate
!!     use M_time, only : fmtdate
!!     implicit none
!!     integer :: dat(8)
!!        call date_and_time(values=dat)
!!        write(*,*)fmtdate(dat,"current date: %w, %l %d, %Y %H:%m:%s %N")
!!        call showme()
!!     contains
!!     subroutine showme()
!!        use M_time, only : fmtdate_usage
!!        call fmtdate_usage() ! see all formatting options
!!     end subroutine showme
!!     end program demo_fmtdate
!!
!!    results:
!!
!!       The current date is Sun, Jul 17th, 2016 01:21:35 PM
!!        ::
!!        :: An up-to-date description of all the
!!        :: formatting options will appear here
!!        ::
!!
!!##AUTHOR
!!    John S. Urban, 2015-12-19
!!##LICENSE
!!    Public Domain
function fmtdate(values,format) RESULT (timestr)

! ident_14="@(#)M_time::fmtdate(3f): given DAT date-time array return date as string using format"

! JSU 2015-10-24
integer,dimension(8),intent(in)      :: values    ! numeric time values as DATE_AND_TIME(3f) intrinsic returns
character(len=*),intent(in),optional :: format    ! input format string
character(len=:),allocatable         :: timestr
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
integer,dimension(8)                 :: valloc    ! numeric time values as DATE_AND_TIME(3f) intrinsic returns
integer,parameter                    :: longest=4096
character(len=1)                     :: chara     ! character being looked at in format string
character(len=10)                    :: iso_name
character(len=2)                     :: dayend
character(len=9)                     :: day       ! day of week
character(len=:),allocatable         :: local_format
character(len=longest)               :: text      ! character array
character(len=longest)               :: xxxx
integer                              :: i,ii,i10
integer                              :: ierr
integer                              :: iout
integer                              :: iso_year, iso_week, iso_weekday
integer                              :: systemclock, countrate
integer                              :: weekday
integer,save                         :: called=0
logical                              :: keyword   ! flag that previous character was a % character
logical,save                         :: since=.FALSE.
real(kind=realtime)                  :: cputime
real(kind=realtime)                  :: julian
real(kind=realtime)                  :: unixtime
real(kind=realtime),save             :: unixtime_last

   valloc=values
   if(present(format))then
      local_format=format
   else
      local_format=' '
   endif

   select case(local_format)
   case('iso-8601W','isoweek') ; local_format='%I'                    ! 2016-W24-5 (yyyy-Www-d)
   case('iso-8601','iso')      ; local_format='%Y-%M-%DT%h:%m:%s%z'   ! 2006-08-14T02:34:56-0600
   case('sql')       ; local_format='"%Y-%M-%D %h:%m:%s.%x"'          !
   case('sqlday')    ; local_format='"%Y-%M-%D"'                      !
   case('sqltime')   ; local_format='"%h:%m:%s.%x"'                   !
   case('rfc-2822')  ; local_format='%w, %D %l %Y %h:%m:%s %T'        ! Mon, 14 Aug 2006 02:34:56 -0600
   case('rfc-3339')  ; local_format='%Y-%M-%DT%h:%m:%s%z'             ! 2006-08-14 02:34:56-06:00
   case('suffix')    ; local_format='%Y%D%M%h%m%s'                    ! 20170122210327
   case('date')      ; local_format='%w %l %D %h:%m:%s UTC%z %Y'      ! Mon Jul 25 03:19:21 UTC-4:00 2016
   case('short')     ; local_format='%w, %l %d, %Y %H:%m:%s %N UTC%z' ! Fri, Jun 17th, 2016 06:31:00 PM UTC-04:00
   case('long')      ; local_format='%W, %L %d, %Y %H:%m:%s %N UTC%z' ! Friday, June 17th, 2016 06:31:00 PM UTC-04:00
   case(' ')         ; local_format='%W, %L %d, %Y %H:%m:%s %N UTC%z' ! Friday, June 17th, 2016 06:31:00 PM UTC-04:00
   case('formal')    ; local_format='The %d of %L %Y'                 ! The 9th of November 2014
   case('lord')  ; local_format='the %d day of %L in the year of our Lord %Y' ! the 9th day of November in the year of our Lord 2014
   case('easter')
      call easter(values(1), valloc)                                  ! given year get month and day Easter falls on
      local_format="Easter day: the %d day of %L in the year of our Lord %Y"
   case('all')
     local_format='&
     & Civil Calendar:%t%W %L %d%n&
     & Civil Date:%t%t%Y-%M-%D %h:%m:%s %z%n&
     & Julian Date:%t%t%J%n&
     & Unix Epoch Time:%t%E%n&
     & Day Of Year:%t%t%O%n&
     & ISO-8601 week:%t%t%I&
     &'
   case default
      xxxx=local_format
      if(index(xxxx,'%').eq.0)then               ! if no % characters try to guess what macros are present

         call substitute(xxxx,'year','%Y')
         call substitute(xxxx,'month','%M')
         call substitute(xxxx,'MONTH','%L')
         call substitute(xxxx,'Month','%l')

         call substitute(xxxx,'weekday','%u')
         call substitute(xxxx,'WEEKDAY','%W')
         call substitute(xxxx,'Weekday','%w')
         call substitute(xxxx,'today','%Y%M%D')
         call substitute(xxxx,'day','%D')
         call substitute(xxxx,'DAY','%d')

         call substitute(xxxx,'GOOD','%N')
         call substitute(xxxx,'HOUR','%H')

         call substitute(xxxx,'hour','%h')
         call substitute(xxxx,'minute','%m')
         call substitute(xxxx,'timezone','%T')
         call substitute(xxxx,'TIMEZONE','%z')
         call substitute(xxxx,'Timezone','%Z')

         call substitute(xxxx,'millisecond','%x')
         call substitute(xxxx,'second','%s')

         call substitute(xxxx,'epoch','%e')
         call substitute(xxxx,'julian','%j')
         call substitute(xxxx,'ordinal','%O')

         if(index(xxxx,'%').eq.0)then            ! if no % characters change every char to %char if a format macro letter
            do i=65,122
             select case(char(i))
             case('B':'E','H':'J','L':'Q','S','T','U','W','Y','Z','b':'e','h':'m','n','o':'q','s':'u','w','x','z')
                 call substitute(xxxx,char(i),'%'//char(i))
             end select
            enddo
         endif
      endif
      local_format=trim(xxxx)
   end select

   text=' '
!  write string, when encounter a percent character do a substitution
   keyword=.FALSE.
   iout=1

   do i10=1,len(local_format)                  ! Read the FORMAT string and replace the "%" strings per the following rules:
      chara=local_format(i10:i10)
      if(chara.eq.'%'.and..not.keyword)then
            keyword=.TRUE.
            cycle
      endif
      if(keyword)then
         keyword=.FALSE.
         select case(chara)
         !=====================================================================================
         case('%'); write(text(iout:),'(A1)')chara                        ! literal percent character
         !=====================================================================================
         case('b'); write(text(iout:),'(A1)')' '                          ! space character
         !=====================================================================================
         case('B'); write(text(iout:),'(A1)')'!'                          ! exclamation (bang) character
         !=====================================================================================
         case('c'); call cpu_time(cputime)                                ! CPU_TIME()
                    write(text(iout:),'(G0)')cputime
         !=====================================================================================
         case('C'); called = called + 1                                   ! number of times this routine called
                    write(text(iout:),'(I0)')called
         !=====================================================================================
         case('d');                                                       ! the day of the month 1st..31st
                    dayend='  '
                    select case(valloc(3))
                    case(1,21,31); dayend='st'
                    case(2,22); dayend='nd'
                    case(3,23); dayend='rd'
                    case(4:20,24:30); dayend='th'
                    case default
                    end select
                    write(text(iout:),'(I0,a)')valloc(3),dayend
         !=====================================================================================
         case('D'); write(text(iout:),'(I2.2)')valloc(3)                  ! the day of the month 1..31
         !=====================================================================================
         case('e'); call date_to_unix(valloc,unixtime,ierr)               ! integer Unix Epoch time in seconds
                    write(text(iout:),'(G0)')nint(unixtime)
         !=====================================================================================
         case('E'); call date_to_unix(valloc,unixtime,ierr)               ! Unix Epoch time in seconds
                    write(text(iout:),'(G0)')unixtime
         !=====================================================================================
         case('h'); write(text(iout:),'(I2.2)')valloc(5)                  ! the hour of the day, in the range of 0 to 23
         !=====================================================================================
         case('H'); ii=mod(valloc(5),12)                                  ! hour of day in range 1..12
                    if(ii.eq.0)then
                       ii=12
                    endif
                    write(text(iout:),'(I0)')ii
         !=====================================================================================
         case('i'); call d2w(valloc,iso_year,iso_week,iso_weekday,iso_name) ! ISO week of year
                    write(text(iout:),'(I0)')iso_week
         !=====================================================================================
         case('I'); call d2w(valloc,iso_year,iso_week,iso_weekday,iso_name) ! iso-8601 Week-numbering year date
                    write(text(iout:),'(a)')iso_name
         !=====================================================================================
         case('j'); call date_to_julian(valloc,julian,ierr)               ! integer Julian Day (truncated to integer)
                    write(text(iout:),'(I0)')int(julian)
         !=====================================================================================
         case('J'); call date_to_julian(valloc,julian,ierr)               ! Julian Date out to milliseconds
                    !write(text(iout:),'(I0,".",i3.3)')int(julian),nint((julian-int(julian))*1000.0)
                    write(text(iout:),'(g0)')julian
         !=====================================================================================
         case('k'); call system_clock(count=systemclock,count_rate=countrate)  ! systemclock/countrate
                    write(text(iout:),'(G0)')real(systemclock)/countrate
         !=====================================================================================
         case('K'); call system_clock(count=systemclock,count_rate=countrate)  ! system clock count
                    write(text(iout:),'(I0)') systemclock
         !=====================================================================================
         case('l'); write(text(iout:),'(A3)')v2mo(valloc(2))              ! three characters of the name of the month of the year
         !=====================================================================================
         case('L'); write(text(iout:),'(A)')v2mo(valloc(2))               ! name of the month of the year
         !=====================================================================================
         case('m'); write(text(iout:),'(I2.2)')valloc(6)                  ! the minutes of the hour, in the range 0 to 59
         !=====================================================================================
         case('M'); write(text(iout:),'(I2.2)')valloc(2)                  ! month of year (1..12)
         !=====================================================================================
         case('N'); if( valloc(5).ge.12)then                              ! AM||PM
                       write(text(iout:),'("PM")')
                    else
                       write(text(iout:),'("AM")')
                    endif
         !=====================================================================================
         case('n');
                    write(text(iout:),'(a)')new_line("A")
         !=====================================================================================
         case('O'); write(text(iout:),'(I3.3)')d2o(valloc)                ! Ordinal day of year
         !=====================================================================================
         case('o'); call date_to_unix(valloc,unixtime,ierr)               ! integer Unix Epoch time in seconds
                    write(text(iout:),'(G0)')floor(unixtime/86400)        ! number of whole days since Epoch time
         !=====================================================================================
         !=====================================================================================
         case('p'); write(text(iout:),'(A)')phase_of_moon(valloc)         ! phase of moon
         !=====================================================================================
         case('P'); write(text(iout:),'(i0,"%")')moon_fullness(valloc)    ! percent of fullness
         !=====================================================================================
         case('q'); write(text(iout:),'("''")')                           ! single quote (apostrophe)
         !=====================================================================================
         case('Q'); write(text(iout:),'(''"'')')                          ! double quote
         !=====================================================================================
         case('s'); write(text(iout:),'(I2.2)')valloc(7)                  ! the seconds of the minute, in the range 0 to 59
         !=====================================================================================
         case('S'); if(.not.since)then                                    ! seconds since last called
                       since=.TRUE.
                       call date_to_unix(valloc,unixtime_last,ierr)
                    endif
                    call date_to_unix(valloc,unixtime,ierr)
                    write(text(iout:),'(G0)')unixtime-unixtime_last
                    unixtime_last=unixtime
         !=====================================================================================
         case('t'); write(text(iout:),'(A1)')CHAR(9)                      ! tab character
         !=====================================================================================
         case('T'); write(text(iout:),'(SP,I3.2,SS,I2.2)')int(valloc(4)/60),abs(mod(valloc(4),60)) ! time from UTC as +-hhmm
         !=====================================================================================
         case('U'); call dow(valloc,weekday,day,ierr)
                    write(text(iout:),'(I1)')mod(weekday+7,7)+1           ! Return the day of the week, 1..7 Sunday=1
         !=====================================================================================
         case('u'); call dow(valloc,weekday,day,ierr)                     ! Return the day of the week, 1..7 Monday=1
                    write(text(iout:),'(I1)')weekday
         !=====================================================================================
         case('W'); call dow(valloc,weekday,day,ierr)                     ! Return the name of the day of the week
                    write(text(iout:),'(a)')day
         !=====================================================================================
         case('w'); call dow(valloc,weekday,day,ierr)                     ! Return the first three characters of the day of the week
                    write(text(iout:),'(A3)')day(1:3)
         !=====================================================================================
         case('x'); write(text(iout:),'(I3.3)')valloc(8)                  ! the milliseconds of the second, in the range 0 to 999
         !=====================================================================================
         case('Y'); write(text(iout:),'(I0.4)')valloc(1)                  ! the year, including the century (for example, 1990)
         !=====================================================================================
         case('Z'); write(text(iout:),'(SP,I5.4)')valloc(4)               ! time difference with respect to UTC in minutes
         !=====================================================================================
         case('z'); write(text(iout:),'(SP,I3.2,":",SS,I2.2)')int(valloc(4)/60),abs(mod(valloc(4),60)) ! time from UTC as +-hh:mm
         !=====================================================================================
         case default
            write(text(iout:),'(A1)')chara
         !=====================================================================================
         end select
         !=====================================================================================
         iout=len_trim(text)+1
         if(iout.ge.longest)exit
      else
         write(text(iout:),'(A1)')chara;iout=iout+1
      endif
   enddo
   timestr=trim(text)
end function fmtdate
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    fmtdate_usage(3f) - [M_time:DATE_PRINTING] display macros recognized
!!    by fmtdate(3f) and now(3f)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine fmtdate_usage(indent)
!!
!!     integer,intent(in),optional      :: indent
!!
!!##DESCRIPTION
!!
!!   The fmtdate_usage(3f) subroutine displays the formatting options
!!   available for use in procedures such as fmtdate(3f) and now(3f).
!!   It is typically used to produce up-to-date help text in commands
!!   that use the M_time(3fm) module, so that the formatting information
!!   only needs maintained in one place (this routine) and is easily
!!   displayed so users can quickly obtain a description of the formatting
!!   macros.
!!
!!##OPTIONS
!!     indent      how many spaces to prefix the output with, so that
!!                 calling programs can position the output. Default
!!                 for this optional parameter is three (3).
!!
!!##EXAMPLE
!!
!!    Sample Program:
!!
!!     program demo_fmtdate_usage
!!     use M_time, only : fmtdate_usage
!!     implicit none
!!        call fmtdate_usage() ! see all formatting options
!!     end program demo_fmtdate_usage
!!
!!    results (actually call the routine to ensure this is up to date):
!!
!!     Description                                        Example
!!
!!     Base time array:
!!     (1) %Y -- year, yyyy                                2016
!!     (2) %M -- month of year, 01 to 12                   07
!!     (3) %D -- day of month, 01 to 31                    29
!!         %d -- day of month, with suffix (1st, 2nd,...)  29th
!!     (4) %Z -- minutes from UTC                          -0240
!!         %z -- -+hh:mm from UTC                          -04:00
!!         %T -- -+hhmm  from UTC                          -0400
!!     (5) %h -- hours, 00 to 23                           10
!!         %H -- hour (1 to 12, or twelve-hour clock)      10
!!         %N -- midnight< AM <=noon; noon<= PM <midnight  AM
!!     (6) %m -- minutes, 00 to 59                         54
!!     (7) %s -- sec, 00 to 59                             08
!!     (8) %x -- milliseconds 000 to 999                   521
!!     Conversions:
!!         %E -- Unix Epoch time                           1469804048.5220029
!!         %e -- integer value of Unix Epoch time          1469804049
!!         %J -- Julian  date                              2457599.121
!!         %j -- integer value of Julian Date(Julian Day)  2457599
!!         %O -- Ordinal day (day of year)                 211
!!         %o -- Whole days since Unix Epoch date          17011
!!         %U -- day of week, 1..7 Sunday=1                6
!!         %u -- day of week, 1..7 Monday=1                5
!!         %i -- ISO week of year 1..53                    30
!!         %I -- iso-8601 week-numbering date(yyyy-Www-d)  2016-W30-5
!!      Names:
!!         %l -- abbreviated month name                    Jul
!!         %L -- full month name                           July
!!         %w -- first three characters of weekday         Fri
!!         %W -- weekday name                              Friday
!!         %p -- phase of moon                             New
!!         %P -- percent of way from new to full moon      -1%
!!      Literals:
!!         %% -- a literal %                               %
!!         %t -- tab character
!!         %b -- blank character
!!         %B -- exclamation(bang) character
!!         %n -- new line (system dependent)
!!         %q -- single quote (apostrophe)
!!         %Q -- double quote
!!      Program timing:
!!         %c -- CPU_TIME(3f) output                     .21875000000000000
!!         %C -- number of times this routine is used    1
!!         %S -- seconds since last use of this format   .0000000000000000
!!         %k -- time in seconds from SYSTEM_CLOCK(3f)   723258.812
!!         %K -- time in clicks from SYSTEM_CLOCK(3f)    723258812
!!
!!    If no percent (%) is found in the format one of several
!!    alternate substitutions occurs.
!!
!!    If the format is composed entirely of one of the following
!!    keywords the following substitutions occur:
!!
!!      "iso-8601",
!!      "iso"        ==> %Y-%M-%DT%h:%m:%s%z
!!      "iso-8601W",
!!      "isoweek"    ==> %I 2016-W30-5
!!      "sql"        ==> "%Y-%M-%D %h:%m:%s.%x"
!!      "sqlday"     ==> "%Y-%M-%D"
!!      "sqltime"    ==> "%h:%m:%s.%x"
!!      "rfc-2822"   ==> %w, %D %l %Y %h:%m:%s %T
!!      "rfc-3339"   ==> %Y-%M-%DT%h:%m:%s%z
!!      "date"       ==> %w %l %D %h:%m:%s UTC%z %Y
!!      "short"      ==> %w, %l %d, %Y %H:%m:%s %N UTC%z
!!      "long"," "   ==> %W, %L %d, %Y %H:%m:%s %N UTC%z
!!      "suffix"     ==> %Y%D%M%h%m%s
!!      "formal"     ==> The %d of %L %Y
!!      "lord"       ==> the %d day of %L in the year of our Lord %Y
!!      "easter"     ==> FOR THE YEAR OF THE CURRENT DATE:
!!                       Easter day: the %d day of %L in the year of our Lord %Y
!!      "all"        ==> A SAMPLE OF DATE FORMATS
!!
!!    otherwise the following words are replaced with the most
!!    common macros:
!!
!!    numeric values:
!!
!!       year     %Y  2016
!!       month    %M  07
!!       day      %D  29
!!       hour     %h  10
!!       minute   %m  54
!!       second   %s  08
!!       timezone %T  0400
!!
!!       epoch    %e  1469804049
!!       julian   %j  2457599
!!       ordinal  %O  211
!!       weekday  %u  5
!!
!!    string values:
!!
!!       MONTH    %L  July
!!       Month    %l  Jul
!!       WEEKDAY  %W  Thursday
!!       Weekday  %w  Thu
!!       DAY      %d  7th
!!       TIMEZONE %z  -04:00
!!       Timezone %Z  -240
!!       GOOD     %N  AM
!!       HOUR     %H  10
!!
!!    if none of these keywords are found then every letter that
!!    is a macro is assumed to have an implied percent in front
!!    of it. For example:
!!
!!       YMDhms ==> %Y%M%D%h%m%s ==> 20160729105408
!!##AUTHOR
!!    John S. Urban, 2015-10-24
!!##LICENSE
!!    Public Domain
subroutine fmtdate_usage(indent)

! ident_15="@(#)M_time::fmtdate_usage(3f): display macros recognized by fmtdate(3f)"

integer,intent(in),optional    :: indent
character(len=128),allocatable :: usage(:)
integer                        :: i,ii
character(len=:),allocatable   :: blanks
   if(present(indent))then ! set indent to passed value or, if value is not present, set indent to 3
      ii=indent
   else
      ii=3
   endif
   blanks=repeat(' ',ii)   ! define a prefix string to create specified indent
usage=[ CHARACTER(LEN=128) :: &
! 123456789 123456789 123456789 123456789 123456789 123456789 123456789 1234567890 123456789 123456789 123456789 123456789 12345678
&'Description                                        Example%b ',&
&'%b                                                           ',&
&'%bBase time array:                                           ',&
&' (1) %%Y -- year, yyyy                                %Y     ',&
&' (2) %%M -- month of year, 01 to 12                   %M     ',&
&' (3) %%D -- day of month, 01 to 31                    %D     ',&
&'     %%d -- day of month, with suffix (1st, 2nd,...)  %d     ',&
&' (4) %%Z -- minutes from UTC                          %Z     ',&
&'     %%z -- -+hh:mm from UTC                          %z     ',&
&'     %%T -- -+hhmm  from UTC                          %T     ',&
&' (5) %%h -- hours, 00 to 23                           %h     ',&
&'     %%H -- hour (1 to 12, or twelve-hour clock)      %H     ',&
&'     %%N -- midnight< AM <=noon; noon<= PM <midnight  %N     ',&
&' (6) %%m -- minutes, 00 to 59                         %m     ',&
&' (7) %%s -- sec, 00 to 59                             %s     ',&
&' (8) %%x -- milliseconds 000 to 999                   %x     ',&
&'%bConversions:                                               ',&
&'     %%E -- Unix Epoch time                           %E     ',&
&'     %%e -- integer value of Unix Epoch time          %e     ',&
&'     %%J -- Julian  date                              %J     ',&
&'     %%j -- integer value of Julian Date(Julian Day)  %j     ',&
&'     %%O -- Ordinal day (day of year)                 %O     ',&
&'     %%o -- Whole days since Unix Epoch date          %o     ',&
&'     %%U -- day of week, 1..7 Sunday=1                %U     ',&
&'     %%u -- day of week, 1..7 Monday=1                %u     ',&
&'     %%i -- ISO week of year 1..53                    %i     ',&
&'     %%I -- iso-8601 week-numbering date(yyyy-Www-d)  %I     ',&
&'%b Names:                                                    ',&
&'     %%l -- abbreviated month name                    %l     ',&
&'     %%L -- full month name                           %L     ',&
&'     %%w -- first three characters of weekday         %w     ',&
&'     %%W -- weekday name                              %W     ',&
&'     %%p -- phase of moon                             %p     ',&
&'     %%P -- percent of way from new to full moon      %P     ',&
&'%b Literals:                                                 ',&
&'     %%%% -- a literal %%                               %%   ',&
&'     %%t -- tab character                             %t     ',&
&'     %%b -- blank character                           %b     ',&
&'     %%B -- exclamation(bang) character               %B     ',&
&'     %%n -- new line (system dependent)               %n     ',&
&'     %%q -- single quote (apostrophe)                 %q     ',&
&'     %%Q -- double quote                              %Q     ',&
&'%b Program timing:                                           ',&
&'     %%c -- CPU_TIME(3f) output                       %c     ',&
&'     %%C -- number of times this routine is used      %C     ',&
&'     %%S -- seconds since last use of this format     %S     ',&
&'     %%k -- time in seconds from SYSTEM_CLOCK(3f)     %k     ',&
&'     %%K -- time in clicks from SYSTEM_CLOCK(3f)      %K     ',&
&'%b                                                           ',&
&'%bIf no percent (%%) is found in the format one of several   ',&
&'%balternate substitutions occurs.                            ',&
&'%b                                                           ',&
&'%bIf the format is composed entirely of one of the following ',&
&'%bkeywords the following substitutions occur:                ',&
&'%b  "iso-8601",                                              ',&
&'%b  "iso"        ==> %%Y-%%M-%%DT%%h:%%m:%%s%%z             %Y-%M-%DT%h:%m:%s%z     ',&
&'%b  "iso-8601W",                                                                    ',&
&'%b  "isoweek"    ==> %%I                              %I                            ',&
&'%b  "sql"        ==> "%%Y-%%M-%%D %%h:%%m:%%s.%%x"          "%Y-%M-%D %h:%m:%s.%x"  ',&
&'%b  "sqlday"     ==> "%%Y-%%M-%%D"                      "%Y-%M-%D"                  ',&
&'%b  "sqltime"    ==> "%%h:%%m:%%s.%%x"                   "%h:%m:%s.%x"              ',&
&'%b  "rfc-2822"   ==> %%w, %%D %%l %%Y %%h:%%m:%%s %%T        ',&
&'%b                   %w, %D %l %Y %h:%m:%s %T                ',&
&'%b  "rfc-3339"   ==> %%Y-%%M-%%DT%%h:%%m:%%s%%z             %Y-%M-%DT%h:%m:%s%z     ',&
&'%b  "date"       ==> %%w %%l %%D %%h:%%m:%%s UTC%%z %%Y      ',&
&'%b                   %w %l %D %h:%m:%s UTC%z %Y              ',&
&'%b  "short"      ==> %%w, %%l %%d, %%Y %%H:%%m:%%s %%N UTC%%z',&
&'%b                   %w, %l %d, %Y %H:%m:%s %N UTC%z         ',&
&'%b  "long"," "   ==> %%W, %%L %%d, %%Y %%H:%%m:%%s %%N UTC%%z',&
&'%b                   %W, %L %d, %Y %H:%m:%s %N UTC%z         ',&
&'%b  "suffix"     ==> %%Y%%D%%M%%h%%m%%s                    %Y%D%M%h%m%s             ',&
&'%b  "formal"     ==> The %%d of %%L %%Y                 The %d of %L %Y             ',&
&'%b  "lord"       ==> the %%d day of %%L in the year of our Lord %%Y                 ',&
&'%b                   the %d day of %L in the year of our Lord %Y                    ',&
&'%b  "easter"     ==> FOR THE YEAR OF THE CURRENT DATE:       ',&
&'%b                     Easter day: the %%d day of %%L in the year of our Lord %%Y   ',&
&'%b  "all"        ==> A SAMPLE OF DATE FORMATS                ',&
&'%botherwise the following words are replaced with the most   ',&
&'%bcommon macros:                                             ',&
&'%b   year          %%Y  %Y                                   ',&
&'%b   month         %%M  %M                                   ',&
&'%b   day           %%D  %D                                   ',&
&'%b   timezone      %%z  %z                                   ',&
&'%b   hour          %%h  %h                                   ',&
&'%b   minute        %%m  %m                                   ',&
&'%b   second        %%s  %s                                   ',&
&'%b   millisecond   %%x  %x                                   ',&
&'%b   epoch         %%e  %e                                   ',&
&'%b   julian        %%j  %j                                   ',&
&'%b   ordinal       %%O  %O                                   ',&
&'%b   weekday       %%u  %u                                   ',&
&'%b   MONTH         %%L  July                                 ',&
&'%b   Month         %%l  Jul                                  ',&
&'%b   DAY           %%d  7th                                  ',&
&'%b   HOUR          %%H  10                                   ',&
&'%b   GOOD          %%N  AM                                   ',&
&'%b   Weekday       %%w  Thu                                  ',&
&'%b   WEEKDAY       %%W  Thursday                             ',&
&'%b   Timezone      %%Z  -240                                 ',&
&'%b   TIMEZONE      %%z  -04:00                               ',&
&'%bif none of these keywords are found then every letter that ',&
&'%bis a macro is assumed to have an implied percent in front  ',&
&'%bof it. For example:                                        ',&
&'%b   YMDhms ==> %%Y%%M%%D%%h%%m%%s ==> %Y%M%D%h%m%s          ',&
&'%b                                                           ']
   write(*,'(a,a)')(blanks,(trim(now(usage(i)))),i=1,size(usage))
end subroutine fmtdate_usage
!-----------------------------------------------------------------------------------------------------------------------------------
! C for reference
! %U     week number of year, with Sunday as first day of week (00..53)
! %Z     alphabetic time zone abbreviation (e.g., EDT)
!        By default, date pads numeric fields with zeroes. The following optional flags may follow '%':
!        -      (hyphen) do not pad the field
!        _      (underscore) pad with spaces
!        0      (zero) pad with zeros
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    guessdate(3f) - [M_time:READING_DATES] reads in a date, in various formats
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine guessdate(anot,dat)
!!
!!     character(len=*),intent(in) :: anot
!!     integer,intent(out)         :: dat(8)
!!
!!##DESCRIPTION
!!
!!   Read in strings and except for looking for month names remove
!!   non-numeric characters and try to convert a string assumed to represent
!!   a date to a date-time array.
!!
!!   Years should always be expressed as four-digit numbers, and except for
!!   the special format yyyy-mm-dd the day should come after the year. Named
!!   months are preferred. If ambiguous the order is assumed to be day -
!!   month - year. Times are assumed to be of the form HH:MM:SS
!!
!!   It is planned that this routine will be superseded. As an alternative,
!!   a C routine exists in the standard C libraries that allows for
!!   expansive features when reading dates that can be called via the
!!   ISO_C_BINDING interface.
!!
!!##OPTIONS
!!    anot  A string assumed to represent a date including a year, month and day.
!!
!!    dat   Integer array holding a "DAT" array, similar in structure
!!          to the array returned by the intrinsic DATE_AND_TIME(3f):
!!
!!                 dat=[ year,month,day,timezone,hour,&
!!                  & minutes,seconds,milliseconds]
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_guessdate
!!     use M_time, only : guessdate, fmtdate
!!     implicit none
!!     character(len=20),allocatable :: datestrings(:)
!!     character(len=:),allocatable  :: answer
!!     integer                       :: dat(8)
!!     integer                       :: i
!!        datestrings=[ &
!!        & 'January 9th, 2001   ',&
!!        & ' Tue Jul 19 2016    ',&
!!        & ' 21/12/2016         ',&
!!        & ' 4th of Jul 2004    ' ]
!!        do i=1,size(datestrings)
!!           write(*,'(a)')repeat('-',80)
!!           write(*,*)'TRYING ',datestrings(i)
!!           call guessdate(datestrings(i),dat)
!!           write(*,*)'DAT ARRAY ',dat
!!           answer=fmtdate(dat)
!!           write(*,*)'FOR '//datestrings(i)//' GOT '//trim(answer)
!!        enddo
!!     end program demo_guessdate
!!
!!    results:
!!
!!     ---------------------------------------------------------------------
!!     TRYING January 9th, 2001
!!     DAT ARRAY         2001  1  9   -240    0   0   0    0
!!     FOR January 9th, 2001  GOT Tuesday, January 9th, 2001 12:00:00 AM
!!     ---------------------------------------------------------------------
!!     TRYING  Tue Jul 19 2016
!!     DAT ARRAY         2016  7  19  -240    0   0   0    0
!!     FOR  Tue Jul 19 2016   GOT Tuesday, July 19th, 2016 12:00:00 AM
!!     ---------------------------------------------------------------------
!!     TRYING  21/12/2016
!!     DAT ARRAY         2016  12 21  -240    0   0   0    0
!!     FOR  21/12/2016        GOT Wednesday, December 21st, 2016 12:00:00 AM
!!     ---------------------------------------------------------------------
!!     TRYING  4th of Jul 2004
!!     DAT ARRAY         2004  7  4   -240    0   0   0    0
!!     FOR  4th of Jul 2004   GOT Sunday, July 4th, 2004 12:00:00 AM
!!
!!##LICENSE
!!    Public Domain
subroutine guessdate(datestring,dat,ier)

! ident_16="@(#)M_time::guessdate(3f): Guess format of a date string to create a DAT date-time array"

! partially based on a concept from JRH 1991-03-19
! JSU, 20160729
!
! makes an odd number of assumptions trying to guess what date format is being used. If you know the format of your date
! values READ(3f) and parse them directly instead of using this procedure, even though it does a good job with common USA formats.
!
!x! REDO more rigorously with regular expressions and recognize standard formats directly


! NOTE : Main constraint is that day is input BEFORE year unless use YYYY-MM-DD and a : implies HH:MM:SS, no timezone names
!        Not rigorous. Gets most common formats but can easily make errors in all but simple unambiguous common date formats
character(len=*),intent(in)       :: datestring ! Date in string format
character(len=:),allocatable      :: datestring_local ! Date in string format
character(len=:),allocatable      :: temp
integer,intent(out)               :: dat(8)
integer,optional                  :: ier
integer                           :: ier_local
integer                           :: iye,mon,idy  ! Year, Month, Day
integer                           :: ihr,imi,ise  ! Hour, Minute, Second
integer                           :: itz, imill   ! Timezone, Milliseconds
character(len=len(datestring)*2)  :: buff
integer                           :: i,idum,ind
logical                           :: alpha
integer                           :: ios
integer                           :: itries
character(len=3),parameter        :: amon(12)=['JAN','FEB','MAR','APR','MAY','JUN', 'JUL','AUG','SEP','OCT','NOV','DEC']
integer,parameter                 :: idmon(12)=[31 , 28  , 31  , 30  , 31  , 30 , 31 , 31  , 30  , 31  , 30  , 31]
character(len=:),allocatable      :: scratch(:)
integer,parameter                 :: isize=40
real                              :: rvalues(isize)
character(len=2)                  :: ampm
integer                           :: iend, inums, ierr
logical                           :: number
logical                           :: verbose
integer                           :: loops

   call date_and_time(values=dat)                           ! get time zone of current process and set defaults
   iye=dat(1)
   mon=dat(2)
   idy=dat(3)
   itz=dat(4)                                               ! default is to use current timezone
   ihr=0
   imi=0
   ise=0
   imill=0

   ier_local=0
   rvalues=0.0
   datestring_local=''
   verbose=.false.

!-----------------------------------------------------------------------------------------------------------------------------------
   temp=' '//trim(upper(datestring))
   if(len(temp).ge.2)then
      if(temp(2:2).eq.'?')then
         verbose=.true.
         temp=temp(3:)
      endif
   endif
   if(verbose)write(*,*)'*guessdate* a ',temp,'::',iye,mon,idy,itz,ihr,imi,ise,imill
!-----------------------------------------------------------------------------------------------------------------------------------
   number=.false.                                        ! when transition from letter to number add a space
   do i=1,len(temp)
      select case(temp(i:i))
      case('A':'Z','/')
         if(number)then
            datestring_local=datestring_local//' '
         endif
         number=.false.
      case default
         number=.true.
      end select
      datestring_local=datestring_local//temp(i:i)
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------

   if(verbose)write(*,*)'*guessdate* b ',datestring_local,'::',iye,mon,idy,itz,ihr,imi,ise,imill
   datestring_local=datestring_local//'                 '  ! pad string so substitute will fit if old string shorter than new string
   !make sure spaces are around month names
   call substitute(datestring_local,'JANUARY',' JAN ')
   call substitute(datestring_local,'FEBRUARY',' FEB ')
   call substitute(datestring_local,'MARCH',' MAR ')
   call substitute(datestring_local,'APRIL',' APR ')
   call substitute(datestring_local,'MAY',' MAY ')
   call substitute(datestring_local,'JUNE',' JUN ')
   call substitute(datestring_local,'JULY',' JUL ')
   call substitute(datestring_local,'AUGUST',' AUG ')
   call substitute(datestring_local,'SEPTEMBER',' SEP ')
   call substitute(datestring_local,'OCTOBER',' OCT ')
   call substitute(datestring_local,'NOVEMBER',' NOV ')
   call substitute(datestring_local,'DECEMBER',' DEC ')
   call substitute(datestring_local,'SEPT',' SEP ')

   call substitute(datestring_local,'JAN',' JAN ')
   call substitute(datestring_local,'FEB',' FEB ')
   call substitute(datestring_local,'MAR',' MAR ')
   call substitute(datestring_local,'APR',' APR ')
   call substitute(datestring_local,'MAY',' MAY ')
   call substitute(datestring_local,'JUN',' JUN ')
   call substitute(datestring_local,'JUL',' JUL ')
   call substitute(datestring_local,'AUG',' AUG ')
   call substitute(datestring_local,'SEP',' SEP ')
   call substitute(datestring_local,'OCT',' OCT ')
   call substitute(datestring_local,'NOV',' NOV ')
   call substitute(datestring_local,'DEC',' DEC ')


   ! assume T[0=9] is from yyyyy-mm-ddThh:mm:ss.xx ISO-8601 format (or SEPTnn,OCTnn AUGUSTnn, where space was added or name changed)
   call substitute(datestring_local,'T0',' 0')
   call substitute(datestring_local,'T1',' 1')
   call substitute(datestring_local,'T2',' 2')
   call substitute(datestring_local,'T3',' 3')
   call substitute(datestring_local,'T4',' 4')
   call substitute(datestring_local,'T5',' 5')
   call substitute(datestring_local,'T6',' 6')
   call substitute(datestring_local,'T7',' 7')
   call substitute(datestring_local,'T8',' 8')
   call substitute(datestring_local,'T9',' 9')

   call substitute(datestring_local,': ',':')
   call substitute(datestring_local,' :',':')

   if(verbose)write(*,*)'*guessdate* A ',datestring_local,'::',iye,mon,idy,itz,ihr,imi,ise,imill
!-----------------------------------------------------------------------------------------------------------------------------------
   call substitute(datestring_local,'UTC',' ')
!-----------------------------------------------------------------------------------------------------------------------------------
   call split(datestring_local,scratch,' ;,"''')
   if(verbose)write(*,*)'*guessdate* B ',(trim(scratch(i)),'|',i=1,size(scratch)),'::',iye,mon,idy,itz,ihr,imi,ise,imill
!-----------------------------------------------------------------------------------------------------------------------------------
   do i=1,size(scratch)                                                       ! a leading +/- is assumed to be a timezone
      if( index("+-",scratch(i)(1:1)) .ne. 0)then
         if(index(scratch(i),':').ne.0)then                                   ! assumed to be +-hh:mm
            call string_to_values(scratch(i),isize,rvalues,inums,':',ierr)
            if(inums.ge.2)then
               itz=60*nint(rvalues(1))+nint(rvalues(2))
            elseif(inums.eq.1)then
               itz=60*nint(rvalues(1))
            endif
         else                                                                ! assumed to be +-mm
            itz=nint(s2v(scratch(i)))
         endif
         scratch(i)=' '
      endif
   enddo
   if(verbose)write(*,*)'*guessdate* C ',(trim(scratch(i)),'|',i=1,size(scratch)),'::',iye,mon,idy,itz,ihr,imi,ise,imill
!-----------------------------------------------------------------------------------------------------------------------------------
   do i=1,size(scratch)                      ! AM and PM are assumed to only occur significantly (not end of day or month name, ...)
      if(len_trim(scratch(i)).ge.2)then
         iend=len_trim(scratch(i))
         ampm=scratch(i)(iend-1:iend)
         select case (ampm)
         case('AM')
            call substitute(scratch(i),'AM',':')
         case('PM')
            ihr=ihr+12
            call substitute(scratch(i),'PM',':')
         end select
      endif
   enddo
   if(verbose)write(*,*)'*guessdate* E ',(trim(scratch(i)),'|',i=1,size(scratch)),'::',iye,mon,idy,itz,ihr,imi,ise,imill
!-----------------------------------------------------------------------------------------------------------------------------------
   do i=1,size(scratch)                                                      ! look for HH:MM:SS
      if(index(scratch(i),':').ne.0)then
         buff=scratch(i)
         call substitute(buff,'-',' -')
         call substitute(buff,'+',' +')
         call string_to_values(buff,isize,rvalues,inums,':/',ierr)
         if(inums.ge.1) ihr=ihr+nint(rvalues(1))
         if(inums.ge.2) imi=nint(rvalues(2))
         if(inums.ge.3) ise=nint(rvalues(3))
         if(inums.ge.4) itz=nint(rvalues(4))
         scratch(i)=' '
      endif
   enddo
   if(verbose)write(*,*)'*guessdate* F ',(trim(scratch(i)),'|',i=1,size(scratch)),'::',iye,mon,idy,itz,ihr,imi,ise,imill
!-----------------------------------------------------------------------------------------------------------------------------------
   do i=1,size(scratch)                                                       ! assume yyyy-mm-dd if found a dash
      if(index(scratch(i),"-").ne.0)then
            call string_to_values(scratch(i),isize,rvalues,inums,'-',ierr)
            select case(inums)
            case(3)
               iye=nint(rvalues(1))
               mon=nint(rvalues(2))
               idy=nint(rvalues(3))
               scratch(i)=v2s(nint(rvalues(3)))//' '//v2s(nint(rvalues(2)))//' '//v2s(nint(rvalues(1)))
            case(2)
               iye=nint(rvalues(1))
               mon=nint(rvalues(2))
               scratch(i)=v2s(nint(rvalues(2)))//' '//v2s(nint(rvalues(1)))
            case default
            end select
      endif
   enddo
   if(verbose)write(*,*)'*guessdate* D ',(trim(scratch(i)),'|',i=1,size(scratch)),'::',iye,mon,idy,itz,ihr,imi,ise,imill
!-----------------------------------------------------------------------------------------------------------------------------------
   datestring_local=''
   do i=1,size(scratch)
      datestring_local=datestring_local//' '//adjustl(trim(scratch(i)))
   enddo
   if(verbose)write(*,*)'*guessdate* G ',(trim(scratch(i)),'|',i=1,size(scratch)),'::',iye,mon,idy,itz,ihr,imi,ise,imill
!-----------------------------------------------------------------------------------------------------------------------------------
   if(datestring_local.eq.' ')then
     loops=0
   else
     loops=1000
   endif
   if(verbose)write(*,*)'*guessdate* Ga',datestring_local,'::',iye,mon,idy,itz,ihr,imi,ise,imill,loops
   INFINITE: do itries=1,loops                              ! give up after 1000 passes
      buff=datestring_local                                 ! copy to buffer
      alpha=.false.

      do i=1,12
         ind=index(buff,amon(i))
         if(ind.ne.0) then                                  ! Found a matching month
            mon=i
            buff(ind:ind+2)='   '                           ! Delete month
            alpha=.true.                                    ! Alphabetic month
            exit
         endif
      enddo

      do i=1,len(buff)                                      ! First remove all non-numeric characters
         idum=ichar(buff(i:i))
         if(idum.lt.48.or.idum.gt.57)then
            buff(i:i)=' '
         endif
      enddo

      if(alpha) then                                        ! Alphabetic month
         read(buff,*,iostat=ios) idy,iye
         if(ios.ne.0)cycle INFINITE
      else
         read(buff,*,iostat=ios) idy,mon,iye
         if(ios.ne.0)cycle INFINITE
      endif
      !x!if(iye.le.99)then
      !x!   iye=iye+2000                                       ! Cope with two digit year (assume 21st century.)
      !x!endif
      if(mon.lt.1.or.mon.gt.12) cycle INFINITE              ! Check range of months
      if(mon.eq.2) then                                     ! Special check for Feb.
         if((iye/4)*4.eq.iye) then                          ! Leap year
            if(idy.lt.1.or.idy.gt.29) cycle INFINITE
         else                                               ! Non-leap year
            if(idy.lt.1.or.idy.gt.28) cycle INFINITE
         endif
      else
         if(idy.lt.1.or.idy.gt.idmon(mon)) cycle INFINITE   ! Error ..... re-input
      endif
      exit
   enddo INFINITE
   if(verbose)write(*,*)'*guessdate* H ',datestring_local,'::',iye,mon,idy,itz,ihr,imi,ise,imill
   if(itries.ge.1000)then
      write(*,*)'*guessdate* ERROR: could not extract date for '//trim(datestring)
   endif
   dat(1)=iye
   dat(2)=mon
   dat(3)=idy
   dat(4)=itz
   dat(5)=ihr
   dat(6)=imi
   dat(7)=ise
   dat(8)=imill
   if(present(ier))ier=ier_local
end subroutine guessdate
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    dow(3f) - [M_time:DAY_OF_WEEK] given a date-time array DAT return
!!    the day of the week
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine dow(values, weekday, day, ierr)
!!
!!     integer,intent(in) :: values(8)
!!     integer,intent(out),optional :: weekday
!!     character(len=*),intent(out),optional :: day
!!     integer,intent(out),optional :: ierr
!!
!!##DESCRIPTION
!!   Given a date array DAT
!!   return the day of the week as a number and a name, Mon=1.
!!
!!##OPTIONS
!!    values   "DAT" array (an integer array of the same format as
!!             the array returned by the intrinsic DATE_AND_TIME(3f))
!!             describing the date to be used to calculate the day
!!             of the week.
!!##RETURNS
!!    weekday  The numeric day of the week, starting with Monday=1.
!!             Optional.
!!    day      The name of the day of the week.
!!             Optional.
!!    ierr     Error code
!!
!!             o [ 0] correct
!!             o [-1] invalid input date
!!             o [-2] neither day nor weekday
!!               return values were requested.
!!
!!             If the error code is not returned and an error occurs,
!!             the program is stopped.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_dow
!!     use M_time, only : dow
!!     implicit none
!!     integer          :: dat(8)     ! input date array
!!     integer          :: weekday
!!     character(len=9) :: day
!!     integer          :: ierr
!!       call date_and_time(values=dat)
!!       call dow(dat, weekday, day, ierr)
!!       write(*,'(a,i0)')'weekday=',weekday
!!       write(*,'(a,a)')'day=',trim(day)
!!       write(*,'(a,i0)')'ierr=',ierr
!!     end program demo_dow
!!
!!    results:
!!
!!     weekday=1
!!     day=Monday
!!     ierr=0
!!##AUTHOR
!!    John S. Urban, 2015-12-19
!!##LICENSE
!!    Public Domain
subroutine dow(values, weekday, day, ierr)

! ident_17="@(#)M_time::dow(3f): Given DAT date-time array return the day of the week"

integer,intent(in)                    :: values(8) ! date and time array used to get time zone
integer,intent(out),optional          :: weekday   ! The day of the week, 1 = Monday, 7 = Sunday
character(len=*),intent(out),optional :: day       ! The name of the day of the week, e.g. 'Sunday'. Minimum length = 9
integer,intent(out),optional          :: ierr      ! Error code,0=correct,-1=invalid input date,-2=neither day nor weekday specified
real(kind=realtime)                   :: julian    ! the Julian Date for which the weekday is required,
integer                               :: iweekday
integer                               :: ierr_local

   call date_to_julian(values,julian,ierr_local)   ! need Julian Date to calculate day of week for first day of month
   ierr_local = 0
   iweekday=0  ! bad value.

   if(julian < 0) then
      ierr_local = -1
   elseif(.not.present(day).and. .not.present(weekday)) then
      ierr_local=-2
   else
      ! Julian Day is in Z time zone and starts at noon so add 1/2 day; and add time zone
      iweekday = mod(int((julian+dble(values(4)/60.0d0/24.0d0)+0.5d0)+1.0d0), 7)
      iweekday = iweekday +1  ! change range from 0 to 6 to 1 to 7
      iweekday = mod(iweekday+5,7)+1  ! change from Sunday=1 to Monday=1

      if(present(day)) then
         select case(iweekday)
         case(1)     ;day = 'Monday'
         case(2)     ;day = 'Tuesday'
         case(3)     ;day = 'Wednesday'
         case(4)     ;day = 'Thursday'
         case(5)     ;day = 'Friday'
         case(6)     ;day = 'Saturday'
         case(7)     ;day = 'Sunday'
         case default;day = 'error'
         end select
      endif

   endif

   if(present(ierr))then
      ierr=ierr_local
   elseif(ierr_local.ne.0)then
      write(*,*)'*dow* Unprocessed Error ',ierr_local,' stopping.'
      stop 2
   endif

   if(present(weekday))then
      weekday=iweekday
   endif

end subroutine dow
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    d2w(3f) - [M_time:WEEK_OF_YEAR] calculate iso-8601 Week-numbering
!!    year date yyyy-Www-d given DAT date-time array
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine d2w(dat,iso_year,iso_week,iso_weekday,iso_name)
!!
!!     integer,intent(in)              :: dat(8)     ! input date array
!!     integer,intent(out)             :: iso_year, iso_week, iso_weekday
!!     character(len=10),intent(out)   :: iso_name
!!
!!##DESCRIPTION
!!   Given a "DAT" array defining a date and time, return the ISO-8601
!!   Week in two formats -- as three integer values defining the ISO year,
!!   week of year and weekday; and as a string of the form "yyyy-Www-d".
!!
!!##OPTIONS
!!    dat          "DAT" array (an integer array of the same format as
!!                 the array returned by the intrinsic DATE_AND_TIME(3f))
!!                 describing the date, which is the basic time description
!!                 used by the other M_time(3fm) module procedures.
!!##RETURNS
!!    iso_year     ISO-8601 year number for the given date
!!    iso_week     ISO-8601 week number for the given date
!!    iso_weekday  ISO-8601 weekday number for the given date
!!    iso_name     ISO-8601 Week string for the data in the form "yyyy-Www-d".
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_d2w
!!     use M_time, only : d2w
!!     implicit none
!!     integer           :: dat(8)     ! input date array
!!     integer           :: iso_year, iso_week, iso_weekday
!!     character(len=10) :: iso_name
!!        call date_and_time(values=dat)
!!        call d2w(dat,iso_year,iso_week,iso_weekday,iso_name)
!!        write(*,'("ISO-8601 Week:   ",a)')iso_name
!!        write(*,'(a,i0)')'ISO-8601 year    ',iso_year
!!        write(*,'(a,i0)')'ISO-8601 week    ',iso_week
!!        write(*,'(a,i0)')'ISO-8601 weekday ',iso_weekday
!!     end program demo_d2w
!!
!!    results:
!!
!!     ISO-8601 Week:   2016-W29-1
!!     ISO-8601 year    2016
!!     ISO-8601 week    29
!!     ISO-8601 weekday 1
!!
!!##DEFINITION
!!   The ISO-8601 date and time standard was issued by the International Organization for Standardization (ISO).
!!   It is used (mainly) in government and business for fiscal years, as well as in timekeeping.
!!   The system specifies a week year atop the Gregorian calendar by defining a notation for ordinal weeks of the year.
!!
!!   An ISO week-numbering year (also called ISO year informally) has 52 or 53 full weeks.
!!   That is 364 or 371 days instead of the usual 365 or 366 days.
!!   The extra week is referred to here as a leap week, although ISO-8601 does not use this term.
!!   Weeks start with Monday.
!!   The first week of a year is the week that contains the first Thursday of the year (and, hence, always contains 4 January).
!!   ISO week year numbering therefore slightly deviates from the Gregorian for some days close to January 1st.
!!
!!##CALCULATION
!!   The ISO-8601 week number of any date can be calculated, given its ordinal date (i.e. position within the year)
!!   and its day of the week.
!!
!!##METHOD
!!     Using ISO weekday numbers (running from 1 for Monday to 7 for Sunday),
!!     subtract the weekday from the ordinal date, then add 10. Divide the result
!!     by 7. Ignore the remainder; the quotient equals the week number. If
!!     the week number thus obtained equals 0, it means that the given date
!!     belongs to the preceding (week-based) year. If a week number of 53 is
!!     obtained, one must check that the date is not actually in week 1 of the
!!     following year.
!!
!!     These two statements are assumed true when correcting the dates around January 1st:
!!
!!     o The number of weeks in a given year is equal to the corresponding week number of 28 December.
!!     o January 4th is always in the first week.
!!
!!##ISO_NAME
!!   Week date representations are in the format YYYYWww-D.
!!
!!     o [YYYY] indicates the ISO week-numbering year which is slightly different from the traditional Gregorian calendar year.
!!     o [Www] is the week number prefixed by the letter W, from W01 through W53.
!!     o [D] is the weekday number, from 1 through 7, beginning with Monday and ending with Sunday.
!!
!!   For example, the Gregorian date 31 December 2006 corresponds to the Sunday of the 52nd week of 2006, and is written
!!
!!     2006-W52-7 (extended form)
!!     or
!!     2006W527 (compact form).
!!
!!##REFERENCE
!!    From Wikipedia, the free encyclopedia 2015-12-19
!!
!!##AUTHOR
!!    John S. Urban, 2015-12-19
!!##LICENSE
!!    Public Domain
subroutine d2w(dat,iso_year,iso_week,iso_weekday,iso_name)

! ident_18="@(#)M_time::d2w(3f): DAT date-time array to iso-8601 Week-numbering year date yyyy-Www-d"

integer,intent(in)              :: dat(8)     ! input date array
integer,intent(out)             :: iso_year, iso_week, iso_weekday
character(len=10),intent(out)   :: iso_name
integer                         :: shared_weekday
integer                         :: last_week_this_year
integer                         :: dec28_lastyear(8)   ! December 28th is always in last week
integer                         :: dec28_thisyear(8)   ! December 28th is always in last week
character(len=9)                :: day
integer                         :: ierr
   iso_year=dat(1)                                               ! initially assume the iso_year is the same as the data array year
   iso_week=uncorrected_week_of_year(dat)                        ! this is the week number unless around January 1st
   iso_weekday=shared_weekday                                    ! this is the number of the day of the week assuming Monday=1
   dec28_thisyear=[dat(1),12,28,dat(4),0,0,0,0]                  ! Dec 28th is always in last week; use this to get number of weeks
   last_week_this_year=uncorrected_week_of_year(dec28_thisyear)  ! get the number of the last week of the year (52 or 53)

   ! correct dates around January 1st
   if(iso_week  < 1)then                                         ! if week < 1 then week = lastWeek(year -1)
      dec28_lastyear=[dat(1)-1,12,28,dat(4),0,0,0,0]             ! Dec 28th is always in last week, we want its week number
      iso_week=uncorrected_week_of_year(dec28_lastyear)          ! got the week number for the last week of last year (52 or 53)
      iso_year=dat(1)-1                                          ! our date belongs to last year
   elseif(iso_week >last_week_this_year)then                     ! if week > lastweek(year) then week = 1
      iso_week=iso_week-last_week_this_year                      ! our date belongs to next year
      iso_year=dat(1)+1
   endif

   write(iso_name,'(i4.4,"-W",i2.2,"-",i1)')iso_year,iso_week,iso_weekday ! create ISO string designation for our date

contains
   function uncorrected_week_of_year(datin)
   implicit none
   integer            :: uncorrected_week_of_year
   integer,intent(in) :: datin(8)
   integer            :: ordinal
      call dow(datin,shared_weekday,day,ierr)                 ! formula needs day of week 1..7 where Monday=1
      ordinal=d2o(datin)                                      ! formula needs ordinal day of year where Jan 1st=1
      uncorrected_week_of_year=(ordinal-shared_weekday+10)/7
   end function uncorrected_week_of_year

end subroutine d2w
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    w2d(3f) - [M_time:WEEK_OF_YEAR] calculate DAT date-time array from iso-8601
!!    Week-numbering year date yyyy-Www-d
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine w2d(iso_year,iso_week,iso_weekday,dat)
!!
!!     integer,intent(in)      :: iso_year, iso_week, iso_weekday
!!     integer,intent(out)     :: dat(8)     ! output date array
!!
!!##DESCRIPTION
!!   Given an ISO-8601 week return a "DAT" array defining a date and time,
!!   The ISO-8601 is supplied as three integer values defining the ISO
!!   year, week of year and weekday.
!!
!!##OPTIONS
!!    iso_year     ISO-8601 year number for the given date
!!    iso_week     ISO-8601 week number for the given date
!!    iso_weekday  ISO-8601 weekday number for the given date
!!    iso_name     ISO-8601 Week string for the data in the form "yyyy-Www-d".
!!
!!##RETURNS
!!    dat          "DAT" array (an integer array of the same format as
!!                 the array returned by the intrinsic DATE_AND_TIME(3f))
!!                 describing the date to be used, which is the basic
!!                 time description used by the other M_time(3fm) module
!!                 procedures.
!!
!!##EXAMPLE
!!
!!
!!  Sample program:
!!
!!     program demo_w2d
!!     use M_time, only : w2d, fmtdate
!!     implicit none
!!       write(*,'(a)')&
!!       & 'Given Monday 29 December 2008 is written "2009-W01-1"'
!!       call printit(2009,1,1)
!!       write(*,'(a)')&
!!       & 'Given Sunday 3 January 2010 is written "2009-W53-7"'
!!       call printit(2009,53,7)
!!       write(*,'(a)')&
!!       & 'Given the Gregorian date Sun 31 December 2006 &
!!       &is written 2006-W52-7'
!!       call printit(2006,52,7)
!!       write(*,'(a)')&
!!       & 'Given 27 September 2008 is 2008-W39-6'
!!       call printit(2008,39,6)
!!     contains
!!     subroutine printit(iso_year,iso_week,iso_weekday)
!!     ! ISO-8601 Week: 2016-W29-1
!!     integer  :: iso_year, iso_week, iso_weekday
!!     ! input date array
!!     integer  :: dat(8)
!!      call w2d(iso_year,iso_week,iso_weekday,dat)
!!      write(*,'(a,i0)')'GIVEN:           '
!!      write(*,'(a,i0)')'ISO-8601 year    ',iso_year
!!      write(*,'(a,i0)')'ISO-8601 week    ',iso_week
!!      write(*,'(a,i0)')'ISO-8601 weekday ',iso_weekday
!!      write(*,'(a,i0)')'RESULT:          '
!!      write(*,'(a,*(i0:,","))')'   DAT array        ',dat
!!      write(*,'(a,/,67("="))')'    '//fmtdate(dat,'long')
!!     end subroutine printit
!!    end program demo_w2d
!!
!!  Results:
!!
!!     Given Monday 29 December 2008 is written "2009-W01-1"
!!     GIVEN:
!!     ISO-8601 year    2009
!!     ISO-8601 week    1
!!     ISO-8601 weekday 1
!!     RESULT:
!!        DAT array        2008,12,29,-240,0,0,0,0
!!         Monday, December 29th, 2008 12:00:00 AM UTC-04:00
!!     =========================================================
!!     Given Sunday 3 January 2010 is written "2009-W53-7"
!!     GIVEN:
!!     ISO-8601 year    2009
!!     ISO-8601 week    53
!!     ISO-8601 weekday 7
!!     RESULT:
!!        DAT array        2010,1,3,-240,0,0,0,0
!!         Sunday, January 3rd, 2010 12:00:00 AM UTC-04:00
!!     =========================================================
!!     Given the Gregorian date Sun 31 December 2006 is written 2006-W52-7
!!     GIVEN:
!!     ISO-8601 year    2006
!!     ISO-8601 week    52
!!     ISO-8601 weekday 7
!!     RESULT:
!!        DAT array        2006,12,31,-240,0,0,0,0
!!         Sunday, December 31st, 2006 12:00:00 AM UTC-04:00
!!     =========================================================
!!     Given 27 September 2008 is 2008-W39-6
!!     GIVEN:
!!     ISO-8601 year    2008
!!     ISO-8601 week    39
!!     ISO-8601 weekday 6
!!     RESULT:
!!        DAT array        2008,9,27,-240,0,0,0,0
!!         Saturday, September 27th, 2008 12:00:00 AM UTC-04:00
!!     =========================================================
!!
!!##DEFINITION
!!   The ISO-8601 date and time standard was issued by the International
!!   Organization for Standardization (ISO). It is used (mainly) in
!!   government and business for fiscal years, as well as in timekeeping.
!!   The system specifies a week year atop the Gregorian calendar by
!!   defining a notation for ordinal weeks of the year.
!!
!!   An ISO week-numbering year (also called ISO year informally) has
!!   52 or 53 full weeks. That is 364 or 371 days instead of the usual
!!   365 or 366 days. The extra week is referred to here as a leap week,
!!   although ISO-8601 does not use this term. Weeks start with Monday.
!!   The first week of a year is the week that contains the first Thursday
!!   of the year (and, hence, always contains 4 January). ISO week year
!!   numbering therefore slightly deviates from the Gregorian for some
!!   days close to January 1st.
!!
!!##METHOD
!!     Calculating a date given the year, week number and weekday
!!
!!     This method requires that one know the weekday of 4 January of the year
!!     in question. Add 3 to the number of this weekday, giving a correction
!!     to be used for dates within this year.
!!
!!     Method: Multiply the week number by 7, then add the weekday. From this
!!     sum subtract the correction for the year. The result is the ordinal
!!     date, which can be converted into a calendar date.
!!     If the ordinal date thus obtained is zero or negative,
!!     the date belongs to the previous calendar year; if greater than the
!!     number of days in the year, to the following year.
!!
!!     Example: year 2008, week 39, Saturday (day 6)
!!     Correction for 2008: 5 + 3 = 8
!!     (39 x 7) + 6 = 279
!!     279 - 8 = 271
!!     Ordinal day 271 of a leap year is day 271 - 244 = 27 September
!!     Result: 27 September 2008
!!
!!##ISO_NAME
!!   Week date representations are in the format YYYYWww-D.
!!
!!     o [YYYY] indicates the ISO week-numbering year which is slightly
!!       different from the traditional Gregorian calendar year.
!!     o [Www] is the week number prefixed by the letter W, from W01 through W53.
!!     o [D] is the weekday number, from 1 through 7, beginning with Monday
!!       and ending with Sunday.
!!
!!   For example, the Gregorian date 31 December 2006 corresponds to the
!!   Sunday of the 52nd week of 2006, and is written
!!
!!     2006-W52-7 (extended form)
!!     or
!!     2006W527 (compact form).
!!
!!##REFERENCE
!!    From Wikipedia, the free encyclopedia 2016-08-08
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!##LICENSE
!!    Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine w2d(iso_year,iso_week,iso_weekday,dat)

! ident_19="@(#)M_time::w2d(3f): convert iso-8601 Week-numbering year date yyyy-Www-d to DAT date-time array"

integer,intent(in)              :: iso_year, iso_week, iso_weekday
integer,intent(out)             :: dat(8)     ! output date array
integer                         :: jan4weekday
integer                         :: correction
integer                         :: ordinal
integer                         :: ierr
   call dow( [iso_year,1,4,0,12,0,0,0], jan4weekday, ierr=ierr) ! get day of week for January 4th where Sun=1
   correction=jan4weekday+3                      ! calculate correction
   ordinal=iso_week*7+iso_weekday-correction     ! calculate ordinal day
   dat=o2d(ordinal,iso_year)                     ! convert ordinal to DAT (routine works with negative values or days past year end)
end subroutine w2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    box_month(3f) - [M_time:DATE_PRINTING] create specified month in a
!!    character array
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine box_month(dat,calen)
!!
!!     integer,intent(in)    :: dat(8)
!!     character(len=21)     :: calen(8)
!!
!!##DESCRIPTION
!!   box_month(3f) uses a year and month from a date array to populate
!!   a small character array with a calendar representing the month.
!!
!!##OPTIONS
!!    dat  "DAT" array (an integer array of the same format as
!!          the array returned by the intrinsic DATE_AND_TIME(3f))
!!          describing the date to be used to specify what calendar
!!          month to produce.
!!
!!                 dat=[ year,month,day,timezone,hour,&
!!                  & minutes,seconds,milliseconds]
!!##RETURNS
!!    calen  returned character array holding a display of the
!!           specified month
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_box_month
!!     use M_time, only : box_month
!!     implicit none
!!     integer           :: dat(8)
!!     character(len=21) :: calendar(8)
!!        call date_and_time(values=dat)
!!        call box_month(dat,calendar)
!!        write(*,'(a)')calendar
!!     end program demo_box_month
!!
!!    results:
!!
!!      >     July 2016
!!      >Mo Tu We Th Fr Sa Su
!!      >             1  2  3
!!      > 4  5  6  7  8  9 10
!!      >11 12 13 14 15 16 17
!!      >18 19 20 21 22 23 24
!!      >25 26 27 28 29 30 31
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!##LICENSE
!!    Public Domain
subroutine box_month(dat,calen)

! ident_20="@(#)M_time::box_month(3f): generate month specified by DAT date-time array in character array"

integer,parameter             :: wklen=3*7
!-----------------------------------------------------------------------------------------------------------------------------------
! uses year and month from date array DAT to populate a small character array with a calendar representing the month
integer,intent(in)            :: dat(8)
character(len=wklen)          :: calen(8)
!-----------------------------------------------------------------------------------------------------------------------------------
real(kind=realtime)           :: julian
integer                       :: weekday
integer                       :: dat_1st(8)
integer                       :: dat_nextday(8)
integer                       :: location,ierr,i
!-----------------------------------------------------------------------------------------------------------------------------------
   calen(:)='                    '                                 ! initialize output array to spaces
   dat_1st=[dat(1),dat(2),1,dat(4),0,0,0,0]                        ! create date array for first day in month specified
   call dow(dat_1st, weekday, ierr=ierr)                           ! return the day of the week for first of month
!-----------------------------------------------------------------------------------------------------------------------------------
   calen(1)=adjustc(v2mo(dat(2))//' '//v2s(dat(1)),len(calen(1)))  ! build first line with month and year centered
   calen(2)='Mo Tu We Th Fr Sa Su'                                 ! build second line with days of week
!-----------------------------------------------------------------------------------------------------------------------------------
   location=1+((weekday-1)*3)                                      ! if data were one row where would 3-character day value start?
   call date_to_julian(dat_1st,julian,ierr)                        ! get Julian Date for 1st day of month
   MNTH: do i=1,31                                                 ! put dates into rest of array starting at third line
      write(calen(location/wklen+3)(mod(location,wklen):),'(i2)')i
      if(i.ge.28)then                                              ! is tomorrow in another month?
         call julian_to_date(julian+i,dat_nextday,ierr)
         if(dat_nextday(2).ne.dat(2))then
            exit MNTH
         endif
      endif
      location=location+3
   enddo MNTH
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine box_month
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    d2j(3f) - [M_time:JULIAN] given DAT date-time array returns Julian Date
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function d2j(dat) result (julian)
!!
!!     integer,intent(in)  :: dat(8)
!!     real(kind=realtime) :: julian
!!
!!##DESCRIPTION
!!   Given DAT date-time array returns Julian Date
!!
!!##OPTIONS
!!    dat       Integer array holding a "DAT" array, similar in structure
!!              to the array returned by the intrinsic DATE_AND_TIME(3f):
!!
!!                 dat=[ year,month,day,timezone,hour,&
!!                  & minutes,seconds,milliseconds]
!!
!!              If not present, use current time.
!!##RETURNS
!!    julian    The Julian Date.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_d2j
!!     use M_time, only : d2j
!!     implicit none
!!     integer :: dat(8)
!!        call date_and_time(values=dat)
!!        write(*,'(" Today is:",*(i0:,":"))')dat
!!        write(*,*)'Julian Date is ',d2j(dat)
!!     end program demo_d2j
!!
!!    results:
!!
!!     Today is:2016:7:19:-240:2:11:50:885
!!     Julian Date is    2457588.7582278359
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!##LICENSE
!!    Public Domain
function d2j(dat) result (julian)

! ident_21="@(#)M_time::d2j(3f): Given DAT date-time array returns Julian Date"

integer,intent(in),optional :: dat(8)
real(kind=realtime)         :: julian
integer                     :: ierr
integer                     :: dat_local(8)

   if(present(dat))then                      ! if dat array is present use value contained in it
      call date_to_julian(dat,julian,ierr)
   else                                      ! if dat array is not present create one containing current time
      call date_and_time(values=dat_local)
      call date_to_julian(dat_local,julian,ierr)
   endif

end function d2j
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    j2d(3f) - [M_time:JULIAN] given a JED (Julian Ephemeris Date) returns a
!!    date-time array DAT.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function j2d(julian) result (dat)
!!
!!     real(kind=realtime),intent(in),optional :: julian
!!     integer                                 :: dat(8)
!!
!!##DESCRIPTION
!!   Converts a Julian Ephemeris Date to a DAT date-time array.
!!
!!##OPTIONS
!!    julian  A Julian Ephemeris Date (JED) is the number of days since
!!            noon (not midnight) on January 1st, 4713 BC.
!!            If not present, use current time.
!!
!!##RETURNS
!!    dat   Integer array holding a "DAT" array, similar in structure
!!          to the array returned by the intrinsic DATE_AND_TIME(3f):
!!
!!                 dat=[ year,month,day,timezone,hour,&
!!                  & minutes,seconds,milliseconds]
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_j2d
!!     use M_time, only : j2d, d2j, fmtdate, realtime
!!     implicit none
!!     real(kind=realtime) :: today
!!     integer :: dat(8)
!!        call date_and_time(values=dat) ! get the date using intrinsic
!!        today=d2j(dat)                  ! convert today to Julian Date
!!        write(*,*)'Today=',fmtdate(j2d(today))
!!        ! math is easy with Julian Days and Julian Dates
!!        write(*,*)'Yesterday=',fmtdate(j2d(today-1.0d0))
!!        write(*,*)'Tomorrow=',fmtdate(j2d(today+1.0d0))
!!     end program demo_j2d
!!
!!    results:
!!
!!     Today=Tuesday, July 19th, 2016 08:48:20 AM
!!     Yesterday=Monday, July 18th, 2016 08:48:20 AM
!!     Tomorrow=Wednesday, July 20th, 2016 08:48:20 AM
!!##AUTHOR
!!    John S. Urban, 2015
!!##LICENSE
!!    Public Domain
function j2d(julian) result (dat)

! ident_22="@(#)M_time::j2d(3f): Given Julian Date returns DAT date-time array"

real(kind=realtime),intent(in)   :: julian
integer                          :: dat(8)
integer                          :: ierr
   call julian_to_date(julian,dat,ierr)
end function j2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    d2u(3f) - [M_time:UNIX_EPOCH] given DAT date-time array returns Unix
!!    Epoch Time (UET starts at 0000 on 1 Jan. 1970, UTC)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function d2u(dat) result (unixtime)
!!
!!       integer,intent(in),optional :: dat(8)
!!       real(kind=realtime)         :: unixtime
!!
!!##DESCRIPTION
!!   Converts a DAT date-time array to a Unix Epoch Time value. Typically
!!   mathematical operations such as sums, sorting and comparison are
!!   performed with simple UET numeric values, and then they are converted
!!   back.
!!
!!##OPTIONS
!!    dat   Integer array holding a "DAT" array, similar in structure
!!          to the array returned by the intrinsic DATE_AND_TIME(3f):
!!
!!                 dat=[ year,month,day,timezone,hour,&
!!                  & minutes,seconds,milliseconds]
!!
!!          If not present the current time is used
!!
!!##RETURNS
!!    unixtime  The "Unix Epoch" time, or the number of seconds since 00:00:00 on
!!              January 1st, 1970, UTC.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_d2u
!!     use M_time, only : d2u
!!     implicit none
!!     integer           :: dat(8)
!!        call date_and_time(values=dat)
!!        write(*,'(" Today is:",*(i0:,":"))')dat
!!        write(*,*)'Unix Epoch time is ',d2u(dat)
!!     end program demo_d2u
!!
!!    results:
!!
!!     Today is:2016:7:19:-240:2:0:48:561
!!     Unix Epoch time is    1468908048.5610321
!!##AUTHOR
!!    John S. Urban, 2015
!!##LICENSE
!!    Public Domain
function d2u(dat) result (unixtime)

! ident_23="@(#)M_time::d2u(3f): Given DAT date-time array returns Unix Epoch time"

real(kind=realtime)           :: unixtime
integer,intent(in),optional   :: dat(8)
   integer                    :: datlocal(8)
   integer                    :: ierr
   if(present(dat))then
      datlocal=dat
   else
      call date_and_time(values=datlocal)  ! current time is placed in array
   endif
   call date_to_unix(datlocal,unixtime,ierr)
end function d2u
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    u2d(3f) - [M_time:UNIX_EPOCH] given Unix Epoch Time returns DAT
!!    date-time array
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function u2d(unixtime) result (dat)
!!
!!     class(*),intent(in),optional      :: unixtime
!!     ! integer
!!     ! real
!!     ! real(kind=realtime)
!!
!!     integer                           :: dat(8)
!!
!!##DESCRIPTION
!!   Given Unix Epoch Time returns DAT date-time array
!!
!!##OPTIONS
!!    unixtime  The "Unix Epoch" time, or the number of seconds since
!!              00:00:00 on January 1st, 1970, UTC. If not present, use
!!              current time.
!!
!!##RETURNS
!!    dat       Integer array holding a "DAT" array, similar in structure
!!              to the array returned by the intrinsic DATE_AND_TIME(3f):
!!
!!                 dat=[ year,month,day,timezone,hour,&
!!                  & minutes,seconds,milliseconds]
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_u2d
!!     use M_time, only : u2d, d2u, fmtdate, realtime
!!     implicit none
!!     real(kind=realtime) :: today
!!     integer :: dat(8)
!!        ! get the date using intrinsic
!!        call date_and_time(values=dat)
!!        ! convert today to Julian Date
!!        today=d2u(dat)
!!        write(*,*)'Today=',fmtdate(u2d(today))
!!        ! subtract day
!!        write(*,*)'Yesterday=',fmtdate(u2d(today-86400.0d0))
!!        ! add day
!!        write(*,*)'Tomorrow=',fmtdate(u2d(today+86400.0d0))
!!     end program demo_u2d
!!
!!    results:
!!
!!     Today=Tuesday, July 19th, 2016 11:10:08 AM
!!     Yesterday=Monday, July 18th, 2016 11:10:08 AM
!!     Tomorrow=Wednesday, July 20th, 2016 11:10:08 AM
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!##LICENSE
!!    Public Domain
function u2d(unixtime) result (dat)

! ident_24="@(#)M_time::u2d(3f): Given Unix Epoch Time returns DAT date-time array"

class(*),intent(in),optional   :: unixtime
integer                        :: dat(8)
real(kind=realtime)            :: local_unixtime
integer                        :: ierr

   if(present(unixtime))then
      select type(unixtime)
      type is (integer);             local_unixtime=unixtime
      type is (integer(kind=int64)); local_unixtime=unixtime
      type is (real);                local_unixtime=unixtime
      type is (real(kind=realtime)); local_unixtime=unixtime
      end select
   else
      local_unixtime=d2u()
   endif

   call unix_to_date(local_unixtime,dat,ierr)

end function u2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function get_timezone() result(tz)
implicit none
integer :: tz
integer :: timezone(8)
   call date_and_time(values=timezone)
   tz=timezone(4)
   if(tz.gt.0)then  ! gfortran bug on new-years
      write(*,*)'<ERROR>*get_timezone*TZ=',tz
      tz=mod(tz,1440)-1440
   endif
end function get_timezone
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    sec2days(3f) - [M_time:DURATION] convert seconds to string of form
!!    dd-hh:mm:ss
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function sec2days(seconds,crop) result(dhms)
!!
!!     real(kind=realtime),intent(in) :: seconds
!!       or
!!     integer,intent(in)             :: seconds
!!       or
!!     real,intent(in)                :: seconds
!!       or
!!     character(len=*)               :: seconds
!!
!!     logical,intent(in),optional    :: crop
!!     character(len=:),allocatable   :: dhms
!!
!!##DESCRIPTION
!!   Given a number of seconds convert it to a string of the form
!!
!!       dd-hh:mm:ss
!!
!!   where dd is days, hh hours, mm minutes and ss seconds.
!!
!!##OPTIONS
!!    seconds    number of seconds to convert to string of form dd-hh:mm:ss. May
!!               be of type INTEGER, REAL, REAL(KIND=REALTIME), or CHARACTER.
!!
!!               CHARACTER strings may be of the form
!!               [NNd][NNh][NNm][NNs][NNw]. Case,spaces and underscores are
!!               ignored. Allowed aliases for d,h,m, and s units are
!!
!!                   d -  days,day
!!                   m -  minutes,minute,min
!!                   h -  hours,hour,hrs,hr
!!                   s -  seconds,second,sec
!!
!!               The numeric values may represent floating point numbers.
!!
!!    crop       if .true., remove leading zero day values or day and hour values.
!!               Optional, defaults to .false. .
!!##RETURNS
!!    dmhs       the returned string of form [d:h:]m:s
!!
!!##EXAMPLE
!!
!!    Sample Program:
!!
!!     program demo_sec2days
!!     use M_time, only : sec2days
!!     implicit none
!!        write(*,*)sec2days(129860)
!!        write(*,*)sec2days(80000.0d0)
!!        write(*,*)sec2days(80000.0,crop=.true.)
!!        write(*,*)sec2days('1 day 2.0hr 100 min 300.0seconds')
!!     end program demo_sec2days
!!
!!    results:
!!
!!     1-12:04:20
!!     0-22:13:20
!!     22:13:20
!!     1-03:45:00
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!##LICENSE
!!    Public Domain
function sec2days(seconds,crop) result(dhms)
use, intrinsic :: iso_fortran_env, only : int64

! ident_25="@(#)M_time::sec2days(3f): converts seconds or string of form IId JJh KKm LLs to string showing days of form D-HH:MM:SS"

! on this platform, (select_int_kind(i),i=1,100) returns
! 1:2=1 ,3:4=2 ,5:9=4 ,10:18= 8 ,19:38=16 ,39:=-1
!integer,parameter        :: k(38)=[(selected_int_kind(i),i=1,38)]
integer                  :: i
class(*),intent(in)               :: seconds
logical,intent(in),optional       :: crop
character(len=:),allocatable      :: dhms
real(kind=realtime), parameter    :: units_hl(4)=[ 86400.0d0, 3600.0d0, 60.0d0, 1.0d0 ]
character(len=40)                 :: scratch
integer(kind=int64)               :: days, hours, minutes, secsleft
integer,parameter                 :: one_day=86400
integer,parameter                 :: one_hour=3600
integer,parameter                 :: one_minute=60
logical                           :: crop_local
integer                           :: iprint
logical                           :: negative
integer                           :: ilast
character(len=:),allocatable      :: strlocal
character(len=:),allocatable      :: array(:)
doubleprecision                   :: dtime

   !  Convert input value to nearest integer
   !  Notice that the value SECONDS can be any of several types ( INTEGER,REAL,REAL(KIND=REALTIME))
   select type(seconds)
   type is (integer);               secsleft=seconds
   type is (real);                  secsleft=nint(seconds)
   type is (real(kind=realtime));   secsleft=nint(seconds)
   type is (character(len=*))

      ! note _ is removed from input strings to allow use of _ every three digits in a number as sometimes seen in Java, perl, ...
      strlocal=compact(lower(transliterate(seconds," _',",'')),'')//'                '   ! add whitespace to make room for spaces

      call substitute(strlocal,'days','d')                      ! from long names to short names substitute common aliases for units
      call substitute(strlocal,'day','d')
      call substitute(strlocal,'hours','h')
      call substitute(strlocal,'hour','h')
      call substitute(strlocal,'hrs','h')
      call substitute(strlocal,'hr','h')
      call substitute(strlocal,'minutes','m')
      call substitute(strlocal,'minute','m')
      call substitute(strlocal,'min','m')
      call substitute(strlocal,'seconds','s')
      call substitute(strlocal,'second','s')
      call substitute(strlocal,'secs','s')
      call substitute(strlocal,'sec','s')
      call substitute(strlocal,'weeks','w')
      call substitute(strlocal,'week','w')
      call substitute(strlocal,'wks','w')
      call substitute(strlocal,'wk','w')

      call substitute(strlocal,'s','s ')          ! assuming only one suffix character and not too many to exceed length of strlocal
      call substitute(strlocal,'m','m ')
      call substitute(strlocal,'h','h ')
      call substitute(strlocal,'d','d ')
      call substitute(strlocal,'w','w ')

      dtime=0.0d0
      call split(strlocal,array,' ')

      do i=1,size(array)
         ilast=len_trim(array(i))
         select case(array(i)(ilast:ilast))
         case('w'); dtime=dtime+s2v(array(i)(:ilast-1))*units_hl(1)*7
         case('d'); dtime=dtime+s2v(array(i)(:ilast-1))*units_hl(1)
         case('h'); dtime=dtime+s2v(array(i)(:ilast-1))*units_hl(2)
         case('m'); dtime=dtime+s2v(array(i)(:ilast-1))*units_hl(3)
         case('s'); dtime=dtime+s2v(array(i)(:ilast-1))*units_hl(4)
         case default
            dtime=dtime+s2v(array(i))
         end select
      enddo
      secsleft=int(dtime,kind=int64)
   end select

   if(present(crop))then    ! whether to trim cases where(days=0) and (hours=0 when days=0) from output or always show dd-hh:mm:ss
      crop_local=crop
   else
      crop_local=.false.
   endif

   if(secsleft.lt.0)then
      secsleft=-secsleft
      negative=.true.
   else
      negative=.false.
   endif

   iprint=4

   days=secsleft/one_day                  ! get whole number of days
   if(days.eq.0) iprint=3
   secsleft=secsleft-days*one_day         ! calculate remainder

   hours=secsleft/one_hour                ! get whole number of hours
   if(days.eq.0.and.hours.eq.0) iprint=2
   secsleft=secsleft-hours*one_hour

   minutes=secsleft/one_minute            ! get whole number of minutes
   secsleft=secsleft-minutes*one_minute

   if(.not.crop_local)then
      iprint=4
   endif

   select case(iprint)                    ! select format if cropping is on and leading zero values are present
   case(2)
      write(scratch,'(i2.2,":",i2.2)')minutes,secsleft
   case(3)
      write(scratch,'(i2.2,":",i2.2,":",i2.2)')hours,minutes,secsleft
   case default
      write(scratch,'(i0,"-",i2.2,":",i2.2,":",i2.2)')days,hours,minutes,secsleft
   end select

   if(negative)then
      dhms='-'//trim(scratch)
   else
      dhms=trim(scratch)
   endif

end function sec2days
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    days2sec(3f) - [M_time:DURATION] convert string of form
!!    [[-]dd-]hh:mm:ss.nn to seconds
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function days2sec(str) result(time)
!!
!!     character(len=*),intent(in)       :: str
!!     real(kind=realtime)               :: time
!!
!!##DESCRIPTION
!!   Given a string representing a duration of the form
!!   "[-][[[dd-]hh:]mm:]ss"  or [NNd][NNh][NNm[]NNs][NNw]
!!   return a value representing seconds.
!!
!!   If "dd-" is present, units for the numbers are assumed to
!!   proceed from day to hour to minute to second. But if no
!!   day is present, the units are assumed to proceed from second
!!   to minutes to hour from left to right. That is ...
!!
!!         [-]dd-hh:mm:ss
!!         [-]dd-hh:mm
!!         [-]dd-hh
!!
!!         hh:mm:ss
!!         mm:ss
!!         ss
!!
!!         Where dd is days, hh hours, mm minutes and ss seconds.
!!
!!   A decimal fraction is supported on the seconds (Actually,
!!   any of the numeric values may represent positive floating
!!   point numbers). Spaces are ignored.
!!
!!
!!   Simple numeric values may also be used with unit suffixes; where
!!   s,m,h, or d represents seconds, minutes, hours or days and w
!!   represents a week. Allowed aliases for w,d,h,m, and s units are
!!
!!        [NNd][NNh][NNm][NNs][NNw]
!!
!!          d -  days,day
!!          m -  minutes,minute,min,mins
!!          h -  hours,hour,hr,hrs
!!          s -  seconds,second,sec,secs
!!          w -  week, weeks, wk, wks
!!
!!   The numeric values may represent floating point numbers.
!!
!!   Spaces, commas and case are ignored.
!!
!!##OPTIONS
!!       str   string of the general form dd-hh:mm:ss.nn
!!##RETURNS
!!       time  the number of seconds represented by the input string
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_days2sec
!!     use M_time, only : days2sec
!!     implicit none
!!        write(*,*)days2sec('1-12:04:20')
!!        write(*,*)'one second ',days2sec('1')
!!        write(*,*)'one minute ',days2sec('1:00')
!!        write(*,*)'one hour ',days2sec('1:00:00')
!!        write(*,*)'one day ',days2sec('1-00:00:00')
!!        write(*,*)nint(days2sec(' 1-12:04:20              ')) .eq. 129860
!!        write(*,*)nint(days2sec(' 1.5 days                ')) .eq. 129600
!!        write(*,*)nint(days2sec(' 1.5 days 4hrs 30minutes ')) .eq. 145800
!!        write(*,*)nint(days2sec(' 1.5d                    ')) .eq. 129600
!!        write(*,*)nint(days2sec(' 1d2h3m4s                ')) .eq. 93784
!!        ! duplicates
!!        write(*,*)nint(days2sec(' 1d1d1d                  ')) .eq. 259200
!!        ! negative values
!!        write(*,*)nint(days2sec(' 4d-12h                  ')) .eq. 302400
!!     end program demo_days2sec
!!
!!    Results:
!!
!!     > 129860.00000000000
!!     > one second    1.0000000000000000
!!     > one minute    60.000000000000000
!!     > one hour    3600.0000000000000
!!     > one day    86400.000000000000
!!     > T
!!     > T
!!     > T
!!     > T
!!     > T
!!     > T
!!     > T
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!##LICENSE
!!    Public Domain
function days2sec(str) result(time)
implicit none

! ident_26="@(#)M_time::days2sec(3f): convert string [[-]dd-]hh:mm:ss.nn to seconds or string IId JJh KKm LLs to seconds"

character(len=*),intent(in)       :: str
real(kind=realtime)               :: time
! Supported input syntax:
!    [-]dd-hh:mm:ss
!          hh:mm:ss
!          mm:ss
!          ss
!
character(len=:),allocatable      :: strlocal
character(len=:),allocatable      :: array(:)
real(kind=realtime), parameter    :: units_lh(4)=[ 1.0d0, 60.0d0, 3600.0d0, 86400.0d0 ]
real(kind=realtime), parameter    :: units_hl(4)=[ 86400.0d0, 3600.0d0, 60.0d0, 1.0d0 ]
integer                           :: i, icount, iwords, ilast
logical                           :: negative

   time=0.0d0
   strlocal=compact(str,'')                              ! remove whitespace
   strlocal=transliterate(strlocal,"_',",'')             ! remove single quotes,underscores sometimes used in numbers
   strlocal=lower(strlocal)//repeat(' ',len(strlocal))   ! change to lowercase and add whitespace to make room for spaces

   if(len(strlocal).eq.0)then
      time=0.0d0
   elseif(scan(strlocal,'smhdw').ne.0)then               ! unit code values not DD-HH:MM:SS either plain number or unit numbers
      call substitute(strlocal,'days','d')               ! from long names to short names substitute common aliases for units
      call substitute(strlocal,'day','d')
      call substitute(strlocal,'hours','h')
      call substitute(strlocal,'hour','h')
      call substitute(strlocal,'hrs','h')
      call substitute(strlocal,'hr','h')
      call substitute(strlocal,'minutes','m')
      call substitute(strlocal,'minute','m')
      call substitute(strlocal,'mins','m')
      call substitute(strlocal,'min','m')
      call substitute(strlocal,'seconds','s')
      call substitute(strlocal,'second','s')
      call substitute(strlocal,'secs','s')
      call substitute(strlocal,'sec','s')
      call substitute(strlocal,'weeks','w')
      call substitute(strlocal,'week','w')
      call substitute(strlocal,'wks','w')
      call substitute(strlocal,'wk','w')

      call substitute(strlocal,'s','s ')          ! assuming only one suffix character and not too many to exceed length of strlocal
      call substitute(strlocal,'m','m ')
      call substitute(strlocal,'h','h ')
      call substitute(strlocal,'d','d ')
      call substitute(strlocal,'w','w ')
      call split(strlocal,array,' ')
      iwords=size(array)
      icount=0
      do i=iwords,1,-1
         icount=icount+1
         ilast=len_trim(array(i))
         select case(array(i)(ilast:ilast))
         case('w'); time=time+s2v(array(i)(:ilast-1))*units_hl(1)*7
         case('d'); time=time+s2v(array(i)(:ilast-1))*units_hl(1)
         case('h'); time=time+s2v(array(i)(:ilast-1))*units_hl(2)
         case('m'); time=time+s2v(array(i)(:ilast-1))*units_hl(3)
         case('s'); time=time+s2v(array(i)(:ilast-1))*units_hl(4)
         case default
            time=time+s2v(array(i))
         end select
      enddo
   else

      if(strlocal(1:1).eq.'-')then          ! allow negative prefix as first character but remove it and change sign of value at end
         negative=.true.
         strlocal(1:1)=' '
      else
         negative=.false.
      endif

      call split(trim(strlocal),array,' -:')
      iwords=size(array)

      if(iwords.gt.4)then
         write(*,*)'*days2sec* error: too many values in '//trim(strlocal)
         iwords=4
      endif

      if(index(strlocal,'-').gt.0)then                ! found a dash, assume has days and form DD-HH:MM:SS, DD-, DD-HH, DD-HH:MM
         do i=1,iwords
            time=time+s2v(array(i))*units_hl(i)
         enddo
      else                                            ! no dash, assume no days, either HH:MM:SS or MM:SS, SS
         icount=0
         do i=iwords,1,-1
            icount=icount+1
            ilast=len_trim(array(i))
            time=time+s2v(array(i))*units_lh(icount)
         enddo
      endif

      if(negative)time=-time

   endif

end function days2sec
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     phase_of_moon(3f) - [M_time:ASTROLOGICAL] return name for phase of
!!     moon for given date
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!   function phase_of_moon(datin)
!!
!!    integer,intent(in)            :: datin(8)
!!    character(len=:),allocatable  :: phase_of_moon
!!
!!##DESCRIPTION
!!   Phases Of The Moon
!!
!!   This procedure is used to support the %p field descriptor for the
!!   fmtdate(3f) routine.
!!
!!   The moon circles the earth every 29.530588853 days on average, so pick a
!!   starting point and count. A new moon occurred at Julian date 2451550.1
!!   (January 6, 2000, 18:14 UTC). Then it is easy to count the number of
!!   days since the last new moon. This is an approximate calculation.
!!
!!   There are eight generally recognized phases of the moon in common use
!!
!!    o new or dark
!!    o waxing crescent
!!    o first quarter
!!    o waxing gibbous
!!    o full
!!    o waning gibbous
!!    o laster quarter
!!    o waning crescent
!!
!!   To calculate the phase of the moon simply divide the days since the
!!   last new moon by eight and select the appropriate phase.
!!
!!   Note that technically the four states (new, first quarter, full, third
!!   quarter) are events not phases. That is to say, the moon is technically
!!   only new for an instant.
!!
!!##EXAMPLES
!!
!!  Sample:
!!
!!   program demo_phase_of_moon
!!   use M_time, only : now
!!   use M_time, only : phase_of_moon
!!   use M_time, only : moon_fullness
!!   implicit none
!!   integer             :: dat(8)
!!    ! generate DAT array
!!    call date_and_time(values=dat)
!!    ! show DAT array
!!    write(*,'(" Today is:",*(i0:,":"))')dat
!!    ! the %p and %P fields are supported by fmtdate(3f)
!!    write(*,*)&
!!    & now('The phase of the moon is %p, with a fullness of %P')
!!    write(*,'(1x,*(a))',advance='no')&
!!    & 'The phase of the moon is ',trim( phase_of_moon(dat)),','
!!    write(*,'(1x,a,i0,a)')'with a fullness of ',moon_fullness(dat),'%'
!!   end program demo_phase_of_moon
!!
!!  Sample output:
!!
!!     Today is:2018:11:3:-240:20:18:44:245
!!     The phase of the moon is Waning crescent, with a fullness of -30%
!!     The phase of the moon is Waning crescent, with a fullness of -30%
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!##LICENSE
!!    Public Domain
function phase_of_moon(datin)
implicit none

! ident_27="@(#)M_time::phase_of_moon(3f): return name for phase of moon for given date"

integer,intent(in)            :: datin(8)
character(len=:),allocatable  :: phase_of_moon

real(kind=realtime),parameter :: syndonic_month=29.530588853_realtime ! average period of a lunar cycle, or days per lunation
integer,parameter             :: reference(*)= [2000,1,6,0,18,14,0,0] ! new moon of January 2000 was January 6, 18:14 UTC.
character(len=20),parameter   :: phase_names(*)=[ "New            ", "Waxing crescent", &
                                                  "First quarter  ", "Waxing gibbous ", &
                                                  "Full           ", "Waning gibbous ", &
                                                  "Last quarter   ", "Waning crescent"  ]
real(kind=realtime),parameter :: phase_length=syndonic_month/8_realtime  ! days per phase
integer                       :: phase
real(kind=realtime)           :: days

days= d2j(datin)-d2j(reference)                               ! days between reference date and input date
days = mod(days + phase_length/2.0d0, syndonic_month)         ! modulo calculation of which phase rounding up
if(days.lt.0)days=days+syndonic_month                         ! correct for days before reference date
phase = int( days * ( size(phase_names) / syndonic_month ))+1 ! index into phase names
phase_of_moon=phase_names(phase)

end function phase_of_moon
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     moon_fullness(3f) - [M_time:ASTROLOGICAL] return percentage of moon phase
!!     from new to full
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!   function moon_fullness(datin)
!!
!!    integer,intent(in)            :: datin(8)
!!    integer                       :: moon_fullness
!!
!!##DESCRIPTION
!!
!!   This procedure is used to support the %P field descriptor for the
!!   fmtdate(3f) routine.
!!
!!   The moon circles the earth every 29.530588853 days on average, so pick
!!   a starting point and count. A new moon occurred at January 6, 2000,
!!   18:14 UTC. Then it is easy to count the number of days since the last
!!   new moon. This is an approximate calculation.
!!
!!##OPTIONS
!!
!!  datin      DAT Date array describing input date
!!
!!##RESULTS
!!
!!     moon_fullness  0 is a new or dark moon, 100 is a full moon, + for waxing
!!                    and - for waning.
!!
!!##EXAMPLES
!!
!!    Sample:
!!
!!     program demo_moon_fullness
!!     use M_time, only : now
!!     use M_time, only : phase_of_moon
!!     use M_time, only : moon_fullness
!!     implicit none
!!     integer             :: dat(8)
!!        ! generate DAT array
!!        call date_and_time(values=dat)
!!        ! show DAT array
!!        write(*,'(" Today is:",*(i0:,":"))')dat
!!        ! the %p and %P fields are supported by fmtdate(3f)
!!        write(*,*)&
!!        &now('The phase of the moon is %p, with a fullness of %P')
!!        write(*,'(1x,*(a))',advance='no')&
!!        &'The phase of the moon is ',trim( phase_of_moon(dat)),','
!!        write(*,'(1x,a,i0,a)')&
!!        &'with a fullness of ', moon_fullness(dat),'%'
!!     end program demo_moon_fullness
!!
!!    Sample output:
!!
!!      Today is:2018:11:3:-240:20:18:44:245
!!      The phase of the moon is Waning crescent, with a fullness of -30%
!!      The phase of the moon is Waning crescent, with a fullness of -30%
!!##AUTHOR
!!    John S. Urban, 2015
!!##LICENSE
!!    Public Domain
function moon_fullness(datin)
implicit none

! ident_28="@(#)M_time::moon_fullness(3f): return percentage of moon phase from new to full"

integer,intent(in)            :: datin(8)
integer                       :: moon_fullness

real(kind=realtime),parameter :: syndonic_month=29.530588853_realtime  ! average period of a lunar cycle, or days per lunation
integer,parameter             :: reference(*)= [2000,1,6,0,18,14,0,0]  ! new moon of January 2000 was January 6, 18:14 UTC.
real(kind=realtime)           :: days_into_cycle

days_into_cycle = mod(d2j(datin)-d2j(reference) , syndonic_month)      ! number of days into lunar cycle
if(days_into_cycle.lt.0)days_into_cycle=days_into_cycle+syndonic_month ! correct for input date being before reference date

if(days_into_cycle.le.syndonic_month/2.0_realtime)then                 ! if waxing from new to full report as 0% to 100%
   moon_fullness=int((days_into_cycle/syndonic_month)*200.0_realtime+0.5_realtime)
else                                                                   ! if waning from full to new report as -99% to -1%
   moon_fullness=-(200-int((days_into_cycle/syndonic_month)*200.0_realtime))
endif

end function moon_fullness
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    easter(3f) - [M_time:ASTROLOGICAL] calculate date for Easter given a year
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine easter(year,dat)
!!
!!     integer, intent(in)   :: year
!!     integer, intent(out)  :: dat
!!
!!##DESCRIPTION
!!   The Date of Easter (Sunday)
!!
!!   The algorithm is due to J.-M. Oudin (1940) and is reprinted
!!   in the Explanatory Supplement to the Astronomical Almanac,
!!   ed. P. K. Seidelmann (1992). See Chapter 12, "Calendars", by
!!   L. E. Doggett.
!!
!!   The following are dates of Easter from 1980 to 2024:
!!
!!        1980  April  6        1995  April 16        2010  April  4
!!        1981  April 19        1996  April  7        2011  April 24
!!        1982  April 11        1997  March 30        2012  April  8
!!        1983  April  3        1998  April 12        2013  March 31
!!        1984  April 22        1999  April  4        2014  April 20
!!        1985  April  7        2000  April 23        2015  April  5
!!        1986  March 30        2001  April 15        2016  March 27
!!        1987  April 19        2002  March 31        2017  April 16
!!        1988  April  3        2003  April 20        2018  April  1
!!        1989  March 26        2004  April 11        2019  April 21
!!        1990  April 15        2005  March 27        2020  April 12
!!        1991  March 31        2006  April 16        2021  April  4
!!        1992  April 19        2007  April  8        2022  April 17
!!        1993  April 11        2008  March 23        2023  April  9
!!        1994  April  3        2009  April 12        2024  March 31
!!
!!   N.B. The date of Easter for the Eastern Orthodox Church may be different.
!!
!!##OPTIONS
!!      year    Year for which to calculate day that Easter falls on
!!##RESULTS
!!      dat     Date array for noon on Easter for the specified year
!!
!!##EXAMPLE
!!
!!
!!    Sample program:
!!
!!     program demo_easter
!!     use M_time, only : easter, fmtdate
!!     implicit none
!!     integer :: year
!!     integer :: dat(8) ! year,month,day,tz,hour,minute,second,millisecond
!!       call date_and_time(values=dat)  ! get current year
!!       year=dat(1)
!!       call easter(year, dat)
!!       write(*,*)fmtdate(dat,&
!!       "Easter day: the %d day of %L in the year of our Lord %Y")
!!     end program demo_easter
!!
!!    Sample output:
!!
!!     Easter day: the 16th day of April in the year of our Lord 2017
!>
!!
!!   U.S. Naval Observatory Astronomical Applications Department
!!
!!   This code assembled by Alan Miller
!!   Reference web site:
!!   http://aa.usno.navy.mil/faq/docs/easter.html
!!   Latest revision 8 April 2002
SUBROUTINE Easter(year, dat)
implicit none

! ident_29="@(#)M_time::easter(3f): calculate date for Easter given a year"

integer,intent(in)    :: year
integer,intent(out)   :: dat(8) ! year,month,day,tz,hour,minute,second,millisecond
integer               :: day, month
integer               :: c, i, j, k, l, n

   c = year / 100
   n = year - 19 * ( year / 19 )
   k = ( c - 17 ) / 25
   i = c - c / 4 - ( c - k ) / 3 + 19 * n + 15
   i = i - 30 * ( i / 30 )
   i = i - (i / 28) * (1 - (i / 28) * (29 / (i + 1 )) * ( (21 - n) / 11) )
   j = year + year / 4 + i + 2 - c + c / 4
   j = j - 7 * ( j / 7 )
   l = i - j
   month = 3 + ( l + 40 ) / 44
   day = l + 28 - 31 * ( month / 4 )

   ! fill out a date_and_time array
   call date_and_time(values=dat)  ! get current year
   dat=[year,month,day,dat(4),12,0,0,0]

end subroutine Easter
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!
!   XXXX
!  X    X
! X
! X
! X
! X
! X
!  X    X
!   XXXX
!
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_sleep(3f) - [M_time:C_INTERFACE] call C sleep(3c) or usleep(3c)
!!    procedure
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine system_sleep(wait_seconds)
!!
!!       integer,intent(in)  :: wait_seconds
!!          or
!!       real,intent(in)  :: wait_seconds
!!
!!##DESCRIPTION
!!   The system_sleep(3f) routine uses the intrinsic ISO_C_BINDING
!!   interface to call the C sleep(3c) procedure or usleep(3c)
!!   routine.
!!
!!##OPTIONS
!!    wait_seconds  integer,real or doubleprecision number of seconds for
!!                  process to sleep.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_system_sleep
!!     use M_time, only : system_sleep, now
!!     implicit none
!!     integer :: i
!!        !
!!        write(*,'(a)')"Time before integer call is: ",now()
!!        call system_sleep(4)
!!        write(*,'(a)')"Time after  integer call is: ",now()
!!        !
!!        write(*,'(a)')"Time before real call is: ",now()
!!        call system_sleep(4.0)
!!        write(*,'(a)')"Time after  real call is: ",now()
!!        !
!!        write(*,'(a)')"Time before loop is: ",now()
!!        do i=1,1000
!!           call system_sleep(4.0/1000.0)
!!        enddo
!!        write(*,'(a)')"Time after loop  is: ",now()
!!        !
!!     end program demo_system_sleep
!!
!!  results
!!
!!      Time before integer call is:
!!      Sunday, July 17th, 2016 2:29:45 AM UTC-0240
!!      Time after integer call is:
!!      Sunday, July 17th, 2016 2:29:49 AM UTC-0240
!!      Time before real call is:
!!      Sunday, July 17th, 2016 2:29:49 AM UTC-0240
!!      Time after  real call is:
!!      Sunday, July 17th, 2016 2:29:53 AM UTC-0240
!!      Time before loop is:
!!      Sunday, July 17th, 2016 2:29:53 AM UTC-0240
!!      Time after loop  is:
!!      Sunday, July 17th, 2016 2:30:09 AM UTC-0240
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine system_sleep(seconds)
use,intrinsic                 :: iso_c_binding, only: c_int

! ident_30="@(#)M_time::system_sleep(3f): call sleep(3c) or usleep(3c)"

class(*),intent(in)           :: seconds
integer(kind=c_int)           :: cint
   select type(seconds)
   type is (integer);             cint=seconds                    ; call call_sleep(cint)
   type is (real);                cint=nint(seconds*1000000.0d0)  ; call call_usleep(cint)
   type is (real(kind=realtime)); cint=nint(seconds*1000000.0d0)  ; call call_usleep(cint)
   end select
end SUBROUTINE system_sleep
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine call_sleep(wait_seconds)
use,intrinsic                   :: iso_c_binding, only: c_int

! ident_31="@(#)M_time::call_sleep(3fp): call sleep(3c)"

integer(kind=c_int),intent(in)  :: wait_seconds
integer(kind=c_int)             :: how_long
interface
   function c_sleep(seconds) bind (C,name="sleep")
      import
      integer(c_int)       :: c_sleep ! should be unsigned int (not available in Fortran). OK until highest bit gets set.
      integer(c_int), intent(in), VALUE :: seconds
   end function c_sleep
end interface
   if(wait_seconds.gt.0)then
      how_long=c_sleep(wait_seconds)
   endif
end subroutine call_sleep
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine call_usleep(wait_seconds)
use,intrinsic                   :: iso_c_binding, only: c_int

! ident_32="@(#)M_time::call_usleep(3fp): call usleep(3c)"

integer(kind=c_int),intent(in)  :: wait_seconds
integer(kind=c_int)             :: how_long
interface
   function c_usleep(seconds) bind (C,name="usleep")
      import
      integer(c_int)       :: c_usleep ! should be unsigned int (not available in Fortran). OK until highest bit gets set.
      integer(c_int), intent(in), VALUE :: seconds
   end function c_usleep
end interface
   if(wait_seconds.gt.0)then
      how_long=c_usleep(wait_seconds)
   endif
end subroutine call_usleep
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_time
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! M_time calls now_ex as a regular procedure to prevent a dependency loop
function now_ex(format)
use M_time, only: now

! ident_33="@(#)M_time::now_ex(3f): use of now(3f) outside of a module"

character(len=*),intent(in),optional :: format
character(len=:),allocatable         :: now_ex
   now_ex=now(format)
end function now_ex
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!











!>
!!##NAME
!!    M_list(3f) - [M_list] maintain simple lists
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    use M_list, only : insert, replace, remove
!!    use M_list, only : dictionary
!!
!!##DESCRIPTION
!!
!!    The M_list(3fm) module allows for maintaining an array as a sorted
!!    list. An example is given that creates a keyword-value dictionary
!!    using the lists.
!!
!!    The lists are maintained as simple allocatable arrays. Each time an
!!    entry is added or deleted the array is re-allocated. Because of the
!!    expense of reallocating the data these routines are best suited for
!!    maintaining small lists that do not change size frequently.
!!
!!    The advantage is that the dictionary components are simple arrays of
!!    intrinsic types which can be easily accessed with standard routines.
!!
!!    BASIC LIST
!!
!!    subroutine locate(list,value,place,ier,errmsg)  finds the index where a
!!                                                    value is found or should
!!                                                    be in a sorted array and
!!                                                    flag if the value exists
!!                                                    already
!!    subroutine insert(list,value,place)     insert entry into an allocatable
!!                                            array at specified position
!!    subroutine replace(list,value,place)    replace entry in an allocatable
!!                                            array at specified position
!!    subroutine remove(list,place)           remove entry from an allocatable
!!                                            array at specified position
!!
!!    BASIC DICTIONARY
!!
!!    Due to bugs in gfortran up to at least 7.4.0, this next section
!!    does not work.
!!
!!    type dictionary
!!
!!       character(len=:),allocatable :: key(:)
!!       character(len=:),allocatable :: value(:)
!!       integer,allocatable          :: count(:)
!!
!!    %get      get value from type(dictionary) given an existing key
!!    %set      set or replace value for type(dictionary) given a key
!!    %del      delete an existing key from type(dictionary)
!!    %clr      empty a type(dictionary)
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_M_list
!!    use M_list, only : insert, locate, replace, remove
!!    ! create a dictionary with character keywords, values, and value lengths
!!    ! using the routines for maintaining a list
!!
!!     use M_list, only : locate, insert, replace
!!     implicit none
!!     character(len=:),allocatable   :: keywords(:)
!!     character(len=:),allocatable   :: values(:)
!!     integer,allocatable            :: counts(:)
!!     integer                        :: i
!!     ! insert and replace entries
!!     call update('b','value of b')
!!     call update('a','value of a')
!!     call update('c','value of c')
!!     call update('c','value of c again')
!!     call update('d','value of d')
!!     call update('a','value of a again')
!!     ! show array
!!     write(*,'(*(a,"==>","[",a,"]",/))')(trim(keywords(i)),values(i)(:counts(i)),i=1,size(keywords))
!!     ! remove some entries
!!     call update('a')
!!     call update('c')
!!     write(*,'(*(a,"==>","[",a,"]",/))')(trim(keywords(i)),values(i)(:counts(i)),i=1,size(keywords))
!!     ! get some values
!!     write(*,*)'get b=>',get('b')
!!     write(*,*)'get d=>',get('d')
!!     write(*,*)'get notthere=>',get('notthere')
!!
!!     contains
!!     subroutine update(key,valin)
!!     character(len=*),intent(in)           :: key
!!     character(len=*),intent(in),optional  :: valin
!!     integer                               :: place
!!     integer                               :: ilen
!!     character(len=:),allocatable          :: val
!!     if(present(valin))then
!!        val=valin
!!        ilen=len_trim(val)
!!        ! find where string is or should be
!!        call locate(keywords,key,place)
!!        ! if string was not found insert it
!!        if(place.lt.1)then
!!           call insert(keywords,key,iabs(place))
!!           call insert(values,val,iabs(place))
!!           call insert(counts,ilen,iabs(place))
!!        else
!!           call replace(values,val,place)
!!           call replace(counts,ilen,place)
!!        endif
!!     else
!!        call locate(keywords,key,place)
!!        if(place.gt.0)then
!!           call remove(keywords,place)
!!           call remove(values,place)
!!           call remove(counts,place)
!!        endif
!!     endif
!!     end subroutine update
!!     function get(key) result(valout)
!!     character(len=*),intent(in)   :: key
!!     character(len=:),allocatable  :: valout
!!     integer                       :: place
!!        ! find where string is or should be
!!        call locate(keywords,key,place)
!!        if(place.lt.1)then
!!           valout=''
!!        else
!!           valout=values(place)(:counts(place))
!!        endif
!!     end function get
!!    end program demo_M_list
!!
!!   Results:
!!
!!    d==>[value of d]
!!    c==>[value of c again]
!!    b==>[value of b]
!!    a==>[value of a again]
!!
!!    d==>[value of d]
!!    b==>[value of b]
!!
!!     get b=>value of b
!!     get d=>value of d
!!     get notthere=>
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
module M_list
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT,stdout=>OUTPUT_UNIT    ! access computing environment
implicit none
private

public locate        ! [M_list] find PLACE in sorted character array where value can be found or should be placed
   private locate_c
   private locate_d
   private locate_r
   private locate_i
public insert        ! [M_list] insert entry into a sorted allocatable array at specified position
   private insert_c
   private insert_d
   private insert_r
   private insert_i
   private insert_l
public replace       ! [M_list] replace entry by index from a sorted allocatable array if it is present
   private replace_c
   private replace_d
   private replace_r
   private replace_i
   private replace_l
public remove        ! [M_list] delete entry by index from a sorted allocatable array if it is present
   private remove_c
   private remove_d
   private remove_r
   private remove_i
   private remove_l

! ident_1="@(#)M_list::locate(3f): Generic subroutine locates where element is or should be in sorted allocatable array"
interface locate
   module procedure locate_c, locate_d, locate_r, locate_i
end interface

! ident_2="@(#)M_list::insert(3f): Generic subroutine inserts element into allocatable array at specified position"
interface insert
   module procedure insert_c, insert_d, insert_r, insert_i, insert_l
end interface

! ident_3="@(#)M_list::replace(3f): Generic subroutine replaces element from allocatable array at specified position"
interface replace
   module procedure replace_c, replace_d, replace_r, replace_i, replace_l
end interface

! ident_4="@(#)M_list::remove(3f): Generic subroutine deletes element from allocatable array at specified position"
interface remove
   module procedure remove_c, remove_d, remove_r, remove_i, remove_l
end interface

!-----------------------------------------------------------------------------------------------------------------------------------
public dictionary

type dictionary
   character(len=:),allocatable :: key(:)
   character(len=:),allocatable :: value(:)
   integer,allocatable          :: count(:)
   contains
      procedure,public :: get => dict_get
      procedure,public :: set => dict_add    ! insert entry by name into a sorted allocatable character array if it is not present
      procedure,public :: del => dict_delete ! delete entry by name from a sorted allocatable character array if it is present
end type dictionary

logical,save :: debug=.false.
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    locate(3f) - [M_list] finds the index where a string is found or should be in a sorted array
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine locate(list,value,place,ier,errmsg)
!!
!!    character(len=:)|doubleprecision|real|integer,allocatable :: list(:)
!!    character(len=*)|doubleprecision|real|integer,intent(in)  :: value
!!    integer, intent(out)                  :: PLACE
!!
!!    integer, intent(out),optional         :: IER
!!    character(len=*),intent(out),optional :: ERRMSG
!!
!!##DESCRIPTION
!!
!!    LOCATE(3f) finds the index where the VALUE is found or should
!!    be found in an array. The array must be sorted in descending
!!    order (highest at top). If VALUE is not found it returns the index
!!    where the name should be placed at with a negative sign.
!!
!!    The array and list must be of the same type (CHARACTER, DOUBLEPRECISION,
!!    REAL,INTEGER)
!!
!!##OPTIONS
!!
!!    VALUE         the value to locate in the list.
!!    LIST          is the list array.
!!
!!##RETURNS
!!    PLACE         is the subscript that the entry was found at if it is
!!                  greater than zero(0).
!!
!!                  If PLACE is negative, the absolute value of
!!                  PLACE indicates the subscript value where the
!!                  new entry should be placed in order to keep the
!!                  list alphabetized.
!!
!!    IER           is zero(0) if no error occurs.
!!                  If an error occurs and IER is not
!!                  present, the program is stopped.
!!
!!    ERRMSG        description of any error
!!
!!##EXAMPLES
!!
!!
!!    Find if a string is in a sorted array, and insert the string into
!!    the list if it is not present ...
!!
!!     program demo_locate
!!     use M_sort, only : sort_shell
!!     use M_list, only : locate
!!     implicit none
!!     character(len=:),allocatable  :: arr(:)
!!     integer                       :: i
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'aaa', 'b', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!
!!     call update(arr,'b')
!!     call update(arr,'[')
!!     call update(arr,'c')
!!     call update(arr,'ZZ')
!!     call update(arr,'ZZZZ')
!!     call update(arr,'z')
!!
!!     contains
!!     subroutine update(arr,string)
!!     character(len=:),allocatable :: arr(:)
!!     character(len=*)             :: string
!!     integer                      :: place, plus, ii, end
!!     ! find where string is or should be
!!     call locate(arr,string,place)
!!     write(*,*)'for "'//string//'" index is ',place, size(arr)
!!     ! if string was not found insert it
!!     if(place.lt.1)then
!!        plus=abs(place)
!!        ii=len(arr)
!!        end=size(arr)
!!        ! empty array
!!        if(end.eq.0)then
!!           arr=[character(len=ii) :: string ]
!!        ! put in front of array
!!        elseif(plus.eq.1)then
!!           arr=[character(len=ii) :: string, arr]
!!        ! put at end of array
!!        elseif(plus.eq.end)then
!!           arr=[character(len=ii) :: arr, string ]
!!        ! put in middle of array
!!        else
!!           arr=[character(len=ii) :: arr(:plus-1), string,arr(plus:) ]
!!        endif
!!        ! show array
!!        write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     endif
!!     end subroutine update
!!     end program demo_locate
!!
!!   Results:
!!
!!     for "b" index is            2           5
!!     for "[" index is           -4           5
!!    SIZE=5 xxx,b,aaa,[,ZZZ,
!!     for "c" index is           -2           6
!!    SIZE=6 xxx,c,b,aaa,[,ZZZ,
!!     for "ZZ" index is           -7           7
!!    SIZE=7 xxx,c,b,aaa,[,ZZZ,,
!!     for "ZZZZ" index is           -6           8
!!    SIZE=8 xxx,c,b,aaa,[,ZZZZ,ZZZ,,
!!     for "z" index is           -1           9
!!    SIZE=9 z,xxx,c,b,aaa,[,ZZZZ,ZZZ,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine locate_c(list,value,place,ier,errmsg)

! ident_5="@(#)M_list::locate_c(3f): find PLACE in sorted character array where VALUE can be found or should be placed"

character(len=*),intent(in)             :: value
integer,intent(out)                     :: place
character(len=:),allocatable            :: list(:)
integer,intent(out),optional            :: ier
character(len=*),intent(out),optional   :: errmsg
integer                                 :: i
character(len=:),allocatable            :: message
integer                                 :: arraysize
integer                                 :: maxtry
integer                                 :: imin, imax
integer                                 :: error
   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif
   arraysize=size(list)
   if(debug)write(stderr,*)'*locate_c* START ARRAYSIZE=',size(list)

   error=0
   if(arraysize.eq.0)then
      maxtry=0
      place=-1
   else
      maxtry=int(log(float(arraysize))/log(2.0)+1.0)
      place=(arraysize+1)/2
   endif
   imin=1
   imax=arraysize
   message=''

   LOOP: block
   do i=1,maxtry

      if(value.eq.list(PLACE))then
         exit LOOP
      else if(value.gt.list(place))then
         imax=place-1
      else
         imin=place+1
      endif

      if(imin.gt.imax)then
         place=-imin
         if(iabs(place).gt.arraysize)then ! ran off end of list. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif

      place=(imax+imin)/2

      if(place.gt.arraysize.or.place.le.0)then
         message='*locate* error: search is out of bounds of list. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif

   enddo
   message='*locate* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   else if(error.ne.0)then
      write(stderr,*)message//' VALUE=',trim(value)//' PLACE=',place
      stop 1
   endif
   if(present(errmsg))then
      errmsg=message
   endif
   if(debug)write(stderr,*)'*locate_c* END PLACE=',place,' ARRAYSIZE=',size(list),' LENGTH=',len(list)
end subroutine locate_c
subroutine locate_d(list,value,place,ier,errmsg)

! ident_6="@(#)M_list::locate_d(3f): find PLACE in sorted doubleprecision array where VALUE can be found or should be placed"

! Assuming an array sorted in descending order
!
!  1. If it is not found report where it should be placed as a NEGATIVE index number.

doubleprecision,allocatable            :: list(:)
doubleprecision,intent(in)             :: value
integer,intent(out)                    :: place
integer,intent(out),optional           :: ier
character(len=*),intent(out),optional  :: errmsg

integer                                :: i
character(len=:),allocatable           :: message
integer                                :: arraysize
integer                                :: maxtry
integer                                :: imin, imax
integer                                :: error

   if(.not.allocated(list))then
      list=[doubleprecision :: ]
   endif
   arraysize=size(list)
   if(debug)write(stderr,*)'*locate_d* START ARRAYSIZE=',size(list)

   error=0
   if(arraysize.eq.0)then
      maxtry=0
      place=-1
   else
      maxtry=int(log(float(arraysize))/log(2.0)+1.0)
      place=(arraysize+1)/2
   endif
   imin=1
   imax=arraysize
   message=''

   LOOP: block
   do i=1,maxtry

      if(value.eq.list(PLACE))then
         exit LOOP
      else if(value.gt.list(place))then
         imax=place-1
      else
         imin=place+1
      endif

      if(imin.gt.imax)then
         place=-imin
         if(iabs(place).gt.arraysize)then ! ran off end of list. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif

      place=(imax+imin)/2

      if(place.gt.arraysize.or.place.le.0)then
         message='*locate* error: search is out of bounds of list. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif

   enddo
   message='*locate* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   else if(error.ne.0)then
      write(stderr,*)message//' VALUE=',value,' PLACE=',place
      stop 1
   endif
   if(present(errmsg))then
      errmsg=message
   endif
   if(debug)write(stderr,*)'*locate_d* END PLACE=',place,' ARRAYSIZE=',size(list)
end subroutine locate_d
subroutine locate_r(list,value,place,ier,errmsg)

! ident_7="@(#)M_list::locate_r(3f): find PLACE in sorted real array where VALUE can be found or should be placed"

! Assuming an array sorted in descending order
!
!  1. If it is not found report where it should be placed as a NEGATIVE index number.

real,allocatable                       :: list(:)
real,intent(in)                        :: value
integer,intent(out)                    :: place
integer,intent(out),optional           :: ier
character(len=*),intent(out),optional  :: errmsg

integer                                :: i
character(len=:),allocatable           :: message
integer                                :: arraysize
integer                                :: maxtry
integer                                :: imin, imax
integer                                :: error

   if(.not.allocated(list))then
      list=[real :: ]
   endif
   arraysize=size(list)
   if(debug)write(stderr,*)'*locate_r* START ARRAYSIZE=',size(list)

   error=0
   if(arraysize.eq.0)then
      maxtry=0
      place=-1
   else
      maxtry=int(log(float(arraysize))/log(2.0)+1.0)
      place=(arraysize+1)/2
   endif
   imin=1
   imax=arraysize
   message=''

   LOOP: block
   do i=1,maxtry

      if(value.eq.list(PLACE))then
         exit LOOP
      else if(value.gt.list(place))then
         imax=place-1
      else
         imin=place+1
      endif

      if(imin.gt.imax)then
         place=-imin
         if(iabs(place).gt.arraysize)then ! ran off end of list. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif

      place=(imax+imin)/2

      if(place.gt.arraysize.or.place.le.0)then
         message='*locate* error: search is out of bounds of list. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif

   enddo
   message='*locate* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   else if(error.ne.0)then
      write(stderr,*)message//' VALUE=',value,' PLACE=',place
      stop 1
   endif
   if(present(errmsg))then
      errmsg=message
   endif
   if(debug)write(stderr,*)'*locate_r* END PLACE=',place,' ARRAYSIZE=',size(list)
end subroutine locate_r
subroutine locate_i(list,value,place,ier,errmsg)

! ident_8="@(#)M_list::locate_i(3f): find PLACE in sorted integer array where VALUE can be found or should be placed"

! Assuming an array sorted in descending order
!
!  1. If it is not found report where it should be placed as a NEGATIVE index number.

integer,allocatable                    :: list(:)
integer,intent(in)                     :: value
integer,intent(out)                    :: place
integer,intent(out),optional           :: ier
character(len=*),intent(out),optional  :: errmsg

integer                                :: i
character(len=:),allocatable           :: message
integer                                :: arraysize
integer                                :: maxtry
integer                                :: imin, imax
integer                                :: error

   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   arraysize=size(list)
   if(debug)write(stderr,*)'*locate_i* START ARRAYSIZE=',size(list)

   error=0
   if(arraysize.eq.0)then
      maxtry=0
      place=-1
   else
      maxtry=int(log(float(arraysize))/log(2.0)+1.0)
      place=(arraysize+1)/2
   endif
   imin=1
   imax=arraysize
   message=''

   LOOP: block
   do i=1,maxtry

      if(value.eq.list(PLACE))then
         exit LOOP
      else if(value.gt.list(place))then
         imax=place-1
      else
         imin=place+1
      endif

      if(imin.gt.imax)then
         place=-imin
         if(iabs(place).gt.arraysize)then ! ran off end of list. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif

      place=(imax+imin)/2

      if(place.gt.arraysize.or.place.le.0)then
         message='*locate* error: search is out of bounds of list. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif

   enddo
   message='*locate* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   else if(error.ne.0)then
      write(stderr,*)message//' VALUE=',value,' PLACE=',place
      stop 1
   endif
   if(present(errmsg))then
      errmsg=message
   endif
   if(debug)write(stderr,*)'*locate_i* END PLACE=',place,' ARRAYSIZE=',size(list)
end subroutine locate_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    remove(3f) - [M_list] remove entry from an allocatable array at specified position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine remove(list,place)
!!
!!    character(len=:)|doubleprecision|real|integer,intent(inout) :: list(:)
!!    integer, intent(out) :: PLACE
!!
!!##DESCRIPTION
!!
!!    Remove a value from an allocatable array at the specified index.
!!    The array is assumed to be sorted in descending order. It may be of
!!    type CHARACTER, DOUBLEPRECISION, REAL, or INTEGER.
!!
!!##OPTIONS
!!
!!    list    is the list array.
!!    PLACE   is the subscript for the entry that should be removed
!!
!!##EXAMPLES
!!
!!
!!    Sample program
!!
!!     program demo_remove
!!     use M_sort, only : sort_shell
!!     use M_list, only : locate, remove
!!     implicit none
!!     character(len=:),allocatable :: arr(:)
!!     integer                       :: i
!!     integer                       :: end
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'Z', 'aaa', 'b', 'b', 'ab', 'bb', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     call remove(arr,1)
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     call remove(arr,4)
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!
!!     end program demo_remove
!!
!!   Results:
!!
!!    Expected output
!!
!!     SIZE=9 xxx,bb,b,b,ab,aaa,ZZZ,Z,,
!!     SIZE=8 bb,b,b,ab,aaa,ZZZ,Z,,
!!     SIZE=7 bb,b,b,aaa,ZZZ,Z,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine remove_c(list,place)

! ident_9="@(#)M_list::remove_c(3fp): remove string from allocatable string array at specified position"

character(len=:),allocatable :: list(:)
integer,intent(in)           :: place
integer                      :: ii, end
   if(debug) write(stderr,*)'*remove_c* START PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[character(len=2) :: ]
   endif
   ii=len(list)
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[character(len=ii) :: list(:place-1) ]
   else
      list=[character(len=ii) :: list(:place-1), list(place+1:) ]
   endif
   if(debug)write(stderr,*)'*remove_c* END PLACE=',place,' NEWSIZE=',size(list),' LENGTH=',len(list)
end subroutine remove_c
subroutine remove_d(list,place)

! ident_10="@(#)M_list::remove_d(3fp): remove doubleprecision value from allocatable array at specified position"

doubleprecision,allocatable  :: list(:)
integer,intent(in)           :: place
integer                      :: end
   if(debug) write(stderr,*)'*remove_d* START PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
           list=[doubleprecision :: ]
   endif
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif
   if(debug)write(stderr,*)'*remove_d* END PLACE=',place,' NEWSIZE=',size(list)

end subroutine remove_d
subroutine remove_r(list,place)

! ident_11="@(#)M_list::remove_r(3fp): remove value from allocatable array at specified position"

real,allocatable    :: list(:)
integer,intent(in)  :: place
integer             :: end
   if(debug) write(stderr,*)'*remove_r* START PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[real :: ]
   endif
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif
   if(debug)write(stderr,*)'*remove_r* END PLACE=',place,' NEWSIZE=',size(list)

end subroutine remove_r
subroutine remove_l(list,place)

! ident_12="@(#)M_list::remove_l(3fp): remove value from allocatable array at specified position"

logical,allocatable    :: list(:)
integer,intent(in)     :: place
integer                :: end

   if(debug) write(stderr,*)'*remove_l* START PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[logical :: ]
   endif
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif
   if(debug)write(stderr,*)'*remove_l* END PLACE=',place,' NEWSIZE=',size(list)

end subroutine remove_l
subroutine remove_i(list,place)

! ident_13="@(#)M_list::remove_i(3fp): remove value from allocatable array at specified position"
integer,allocatable    :: list(:)
integer,intent(in)     :: place
integer                :: end

   if(debug) write(stderr,*)'*remove_i* START PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif
   if(debug)write(stderr,*)'*remove_i* END PLACE=',place,' NEWSIZE=',size(list)

end subroutine remove_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    replace(3f) - [M_list] replace entry in a string array at specified position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine replace(list,value,place)
!!
!!    character(len=*)|doubleprecision|real|integer,intent(in) :: value
!!    character(len=:)|doubleprecision|real|integer,intent(in) :: list(:)
!!    integer, intent(out)          :: PLACE
!!
!!##DESCRIPTION
!!
!!    replace a value in an allocatable array at the specified index. Unless the
!!    array needs the string length to increase this is merely an assign of a value
!!    to an array element.
!!
!!    The array may be of type CHARACTER, DOUBLEPRECISION, REAL, or INTEGER>
!!    It is assumed to be sorted in descending order without duplicate values.
!!
!!    The value and list must be of the same type.
!!
!!##OPTIONS
!!
!!    VALUE         the value to place in the array
!!    LIST          is the array.
!!    PLACE         is the subscript that the entry should be placed at
!!
!!##EXAMPLES
!!
!!
!!   Replace key-value pairs in a dictionary
!!
!!     program demo_replace
!!     use M_list, only  : insert, locate, replace
!!     ! Find if a key is in a list and insert it
!!     ! into the key list and value list if it is not present
!!     ! or replace the associated value if the key existed
!!     implicit none
!!     character(len=20)            :: key
!!     character(len=100)           :: val
!!     character(len=:),allocatable :: keywords(:)
!!     character(len=:),allocatable :: values(:)
!!     integer                      :: i
!!     integer                      :: place
!!     call update('b','value of b')
!!     call update('a','value of a')
!!     call update('c','value of c')
!!     call update('c','value of c again')
!!     call update('d','value of d')
!!     call update('a','value of a again')
!!     ! show array
!!     write(*,'(*(a,"==>",a,/))')(trim(keywords(i)),trim(values(i)),i=1,size(keywords))
!!
!!     call locate(keywords,'a',place)
!!     if(place.gt.0)then
!!        write(*,*)'The value of "a" is',trim(values(place))
!!     else
!!        write(*,*)'"a" not found'
!!     endif
!!
!!     contains
!!     subroutine update(key,val)
!!     character(len=*),intent(in)  :: key
!!     character(len=*),intent(in)  :: val
!!     integer                      :: place
!!
!!     ! find where string is or should be
!!     call locate(keywords,key,place)
!!     ! if string was not found insert it
!!     if(place.lt.1)then
!!        call insert(keywords,key,abs(place))
!!        call insert(values,val,abs(place))
!!     else ! replace
!!        call replace(values,val,place)
!!     endif
!!
!!     end subroutine update
!!    end program demo_replace
!!
!!   Expected output
!!
!!    d==>value of d
!!    c==>value of c again
!!    b==>value of b
!!    a==>value of a again
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine replace_c(list,value,place)

! ident_14="@(#)M_list::replace_c(3fp): replace string in allocatable string array at specified position"

character(len=*),intent(in)  :: value
character(len=:),allocatable :: list(:)
character(len=:),allocatable :: kludge(:)
integer,intent(in)           :: place
integer                      :: ii
integer                      :: tlen
integer                      :: end
   if(debug) write(stderr,*)'*replace_c* START VALUE=',trim(value),' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif
   tlen=len_trim(value)
   end=size(list)
   if(place.lt.0.or.place.gt.end)then
           write(stderr,*)'*replace_c* error: index out of range. end=',end,' index=',place
   elseif(len_trim(value).le.len(list))then
      list(place)=value
   else  ! increase length of variable
      ii=max(tlen,len(list))
      kludge=[character(len=ii) :: list ]
      list=kludge
      list(place)=value
   endif
   if(debug)write(stderr,*)'*replace_c* END VALUE=',trim(value),' PLACE=',place,' NEWSIZE=',size(list),' LENGTH=',len(list)
end subroutine replace_c
subroutine replace_d(list,value,place)

! ident_15="@(#)M_list::replace_d(3fp): place doubleprecision value into allocatable array at specified position"

doubleprecision,intent(in)   :: value
doubleprecision,allocatable  :: list(:)
integer,intent(in)           :: place
integer                      :: end
   if(debug) write(stderr,*)'*replace_d* START VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
           list=[doubleprecision :: ]
   endif
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.gt.0.and.place.le.end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(stderr,*)'*replace_d* error: index out of range. end=',end,' index=',place
   endif
   if(debug)write(stderr,*)'*replace_d* END VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine replace_d
subroutine replace_r(list,value,place)

! ident_16="@(#)M_list::replace_r(3fp): place value into allocatable array at specified position"

real,intent(in)       :: value
real,allocatable      :: list(:)
integer,intent(in)    :: place
integer               :: end
   if(debug) write(stderr,*)'*replace_r* START REPLACE_R VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[real :: ]
   endif
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.gt.0.and.place.le.end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(stderr,*)'*replace_r* error: index out of range. end=',end,' index=',place
   endif
   if(debug)write(stderr,*)'*replace_r* END REPLACE_R VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine replace_r
subroutine replace_l(list,value,place)

! ident_17="@(#)M_list::replace_l(3fp): place value into allocatable array at specified position"

logical,allocatable   :: list(:)
logical,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[logical :: ]
   endif
   end=size(list)
   if(debug)write(stderr,*)'*replace_l* START REPLACE_L VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.gt.0.and.place.le.end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(stderr,*)'*replace_l* error: index out of range. end=',end,' index=',place
   endif
   if(debug)write(stderr,*)'*replace_l* END REPLACE_L VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine replace_l
subroutine replace_i(list,value,place)

! ident_18="@(#)M_list::replace_i(3fp): place value into allocatable array at specified position"

integer,intent(in)    :: value
integer,allocatable   :: list(:)
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(debug)write(stderr,*)'*replace_i* START VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.gt.0.and.place.le.end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(stderr,*)'*replace_i* error: index out of range. end=',end,' index=',place
   endif
   if(debug)write(stderr,*)'*replace_i* END VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine replace_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    insert(3f) - [M_list] insert entry into a string array at specified position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine insert(list,value,place)
!!
!!    character(len=*)|doubleprecision|real|integer,intent(in) :: value
!!    character(len=:)|doubleprecision|real|integer,intent(in) :: list(:)
!!    integer,intent(in)    :: place
!!
!!##DESCRIPTION
!!
!!    Insert a value into an allocatable array at the specified index.
!!    The list and value must be of the same type (CHARACTER, DOUBLEPRECISION,
!!    REAL, or INTEGER)
!!
!!##OPTIONS
!!
!!    list    is the list array. Must be sorted in descending order.
!!    value   the value to place in the array
!!    PLACE   is the subscript that the entry should be placed at
!!
!!##EXAMPLES
!!
!!
!!    Find if a string is in a sorted array, and insert the string into
!!    the list if it is not present ...
!!
!!     program demo_insert
!!     use M_sort, only : sort_shell
!!     use M_list, only : locate, insert
!!     implicit none
!!     character(len=:),allocatable :: arr(:)
!!     integer                       :: i
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'aaa', 'b', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!     ! add or replace values
!!     call update(arr,'b')
!!     call update(arr,'[')
!!     call update(arr,'c')
!!     call update(arr,'ZZ')
!!     call update(arr,'ZZZ')
!!     call update(arr,'ZZZZ')
!!     call update(arr,'')
!!     call update(arr,'z')
!!
!!     contains
!!     subroutine update(arr,string)
!!     character(len=:),allocatable :: arr(:)
!!     character(len=*)             :: string
!!     integer                      :: place, end
!!
!!     end=size(arr)
!!     ! find where string is or should be
!!     call locate(arr,string,place)
!!     ! if string was not found insert it
!!     if(place.lt.1)then
!!        call insert(arr,string,abs(place))
!!     endif
!!     ! show array
!!     end=size(arr)
!!     write(*,'("array is now SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!
!!     end subroutine update
!!     end program demo_insert
!!
!!   Results:
!!
!!     array is now SIZE=5 xxx,b,aaa,ZZZ,,
!!     array is now SIZE=6 xxx,b,aaa,[,ZZZ,,
!!     array is now SIZE=7 xxx,c,b,aaa,[,ZZZ,,
!!     array is now SIZE=8 xxx,c,b,aaa,[,ZZZ,ZZ,,
!!     array is now SIZE=9 xxx,c,b,aaa,[,ZZZZ,ZZZ,ZZ,,
!!     array is now SIZE=10 z,xxx,c,b,aaa,[,ZZZZ,ZZZ,ZZ,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine insert_c(list,value,place)

! ident_19="@(#)M_list::insert_c(3fp): place string into allocatable string array at specified position"

character(len=*),intent(in)  :: value
character(len=:),allocatable :: list(:)
character(len=:),allocatable :: kludge(:)
integer,intent(in)           :: place
integer                      :: ii
integer                      :: end
   if(debug) write(stderr,*)'*insert_c* START VALUE=',trim(value),' PLACE=',place,' ORIGINALSIZE=',size(list)

   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif

   ii=max(len_trim(value),len(list),2)
   end=size(list)

   if(end.eq.0)then                                          ! empty array
      list=[character(len=ii) :: value ]
   elseif(place.eq.1)then                                    ! put in front of array
      kludge=[character(len=ii) :: value, list]
      list=kludge
   elseif(place.gt.end)then                                  ! put at end of array
      kludge=[character(len=ii) :: list, value ]
      list=kludge
   elseif(place.ge.2.and.place.le.end)then                 ! put in middle of array
      kludge=[character(len=ii) :: list(:place-1), value,list(place:) ]
      list=kludge
   else                                                      ! index out of range
      write(stderr,*)'*insert_c* error: index out of range. end=',end,' index=',place,' value=',value
   endif

   if(debug)write(stderr,*)'*insert_c* END VALUE=',trim(value),' PLACE=',place,' NEWSIZE=',size(list)
end subroutine insert_c
subroutine insert_r(list,value,place)

! ident_20="@(#)M_list::insert_r(3fp): place real value into allocatable array at specified position"

real,intent(in)       :: value
real,allocatable      :: list(:)
integer,intent(in)    :: place
integer               :: end

   if(debug) write(stderr,*)'*insert_r* START VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[real :: ]
   endif

   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.eq.1)then                                    ! put in front of array
      list=[value, list]
   elseif(place.gt.end)then                                  ! put at end of array
      list=[list, value ]
   elseif(place.ge.2.and.place.le.end)then                   ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(stderr,*)'*insert_r* error: index out of range. end=',end,' index=',place,' value=',value
   endif

   if(debug)write(stderr,*)'*insert_r* END VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine insert_r
subroutine insert_d(list,value,place)

! ident_21="@(#)M_list::insert_d(3fp): place doubleprecision value into allocatable array at specified position"

doubleprecision,intent(in)       :: value
doubleprecision,allocatable      :: list(:)
integer,intent(in)               :: place
integer                          :: end
   if(debug) write(stderr,*)'*insert_d* START VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[doubleprecision :: ]
   endif
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.eq.1)then                                    ! put in front of array
      list=[value, list]
   elseif(place.gt.end)then                                  ! put at end of array
      list=[list, value ]
   elseif(place.ge.2.and.place.le.end)then                 ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(stderr,*)'*insert_d* error: index out of range. end=',end,' index=',place,' value=',value
   endif
   if(debug)write(stderr,*)'*insert_d* END VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine insert_d
subroutine insert_l(list,value,place)

! ident_22="@(#)M_list::insert_l(3fp): place value into allocatable array at specified position"

logical,allocatable   :: list(:)
logical,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[logical :: ]
   endif
   end=size(list)
   if(debug)write(stderr,*)'*insert_l* START VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.eq.1)then                                    ! put in front of array
      list=[value, list]
   elseif(place.gt.end)then                                  ! put at end of array
      list=[list, value ]
   elseif(place.ge.2.and.place.le.end)then                 ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(stderr,*)'*insert_l* error: index out of range. end=',end,' index=',place,' value=',value
   endif

   if(debug)write(stderr,*)'*insert_l* END VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine insert_l
subroutine insert_i(list,value,place)

! ident_23="@(#)M_list::insert_i(3fp): place value into allocatable array at specified position"

integer,allocatable   :: list(:)
integer,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(debug)write(stderr,*)'*insert_i* START VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.eq.1)then                                    ! put in front of array
      list=[value, list]
   elseif(place.gt.end)then                                  ! put at end of array
      list=[list, value ]
   elseif(place.ge.2.and.place.le.end)then                 ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(stderr,*)'*insert_i* error: index out of range. end=',end,' index=',place,' value=',value
   endif

   if(debug)write(stderr,*)'*insert_i* END VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine insert_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    dict_delete(3f) - [M_list] delete entry by name from an allocatable sorted string array if it is present
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine dict_delete(key,dict)
!!
!!    character(len=*),intent(in) :: key
!!    type(dictionary)            :: dict
!!
!!##DESCRIPTION
!!
!!    Find if a string is in a sorted array, and delete the string
!!    from the dictionary if it is present.
!!
!!##OPTIONS
!!
!!    KEY           the key name to find and delete from the dictionary.
!!    DICTIONARY    the dictionary.
!!
!!##EXAMPLES
!!
!!
!!    delete a key from a dictionary by name.
!!
!!     program demo_dict_delete
!!     use M_list, only : dictionary
!!     implicit none
!!     type(dictionary) :: caps
!!     integer                       :: i
!!     call caps%set(caps,'A','aye')
!!     call caps%set(caps,'B','bee')
!!     call caps%set(caps,'C','see')
!!     call caps%set(caps,'D','dee')
!!
!!     write(*,101)(trim(arr(i)),i=1,size(caps%keys)) ! show array
!!     call  caps%del(caps,'A')
!!     call  caps%del(caps,'C')
!!     call  caps%del(caps,'z')
!!     write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
!!     101 format (1x,*("[",a,"]",:,","))
!!     end program demo_dict_delete
!!
!!    Results:
!!
!!     [z],[xxx],[c],[b],[b],[aaa],[ZZZ],[ZZ]
!!     [z],[xxx],[b],[b],[aaa],[ZZZ],[ZZ]
!!     [z],[xxx],[b],[b],[aaa],[ZZZ],[ZZ]
!!     [z],[xxx],[b],[b],[aaa],[ZZZ],[ZZ]
!!     [z],[xxx],[aaa],[ZZZ],[ZZ]
!!     [z],[xxx],[aaa],[ZZZ]
!!     [z],[xxx],[aaa],[ZZZ]
!!     [xxx],[aaa],[ZZZ]
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine dict_delete(self,key)

! ident_24="@(#)M_list::dict_delete(3f): remove string from sorted allocatable string array if present"

class(dictionary),intent(inout) :: self
character(len=*),intent(in)     :: key
integer                         :: place

   call locate(self%key,key,place)
   if(place.ge.1)then
      call remove(self%key,place)
      call remove(self%value,place)
      call remove(self%count,place)
   endif

end subroutine dict_delete
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    dict_get(3f) - [M_list] get value of key-value pair in a dictionary given key
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine dict_get(dict,key,value)
!!
!!    type(dictionary)            :: dict
!!    character(len=*),intent(in) :: key
!!    character(len=*),intent(in) :: VALUE
!!
!!##DESCRIPTION
!!
!!    get a value given a key from a key-value dictionary
!!
!!    If key is not found in dictionary , return a blank
!!
!!##OPTIONS
!!
!!    DICT     is the dictionary.
!!    KEY      key name
!!    VALUE    value associated with key
!!
!!##EXAMPLES
!!
!!
!!     program demo_locate
!!     use M_list, only : dictionary
!!     implicit none
!!     type(dictionary)             :: table
!!     character(len=:),allocatable :: val
!!     integer          :: i
!!
!!     call table%set('A','value for A')
!!     call table%set('B','value for B')
!!     call table%set('C','value for C')
!!     call table%set('D','value for D')
!!     call table%set('E','value for E')
!!     call table%set('F','value for F')
!!     call table%set('G','value for G')
!!     write(*,*)table%get('A')
!!     write(*,*)table%get('B')
!!     write(*,*)table%get('C')
!!     write(*,*)table%get('D')
!!     write(*,*)table%get('E')
!!     write(*,*)table%get('F')
!!     write(*,*)table%get('G')
!!     write(*,*)table%get('H')
!!     end program demo_locate
!!
!!    Results:
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function dict_get(self,key) result(value)

! ident_25="@(#)M_list::dict_get(3f): get value of key-value pair in dictionary, given key"

!!class(dictionary),intent(inout) :: self
class(dictionary)               :: self
character(len=*),intent(in)     :: key
character(len=:),allocatable    :: value
integer                         :: place
   call locate(self%key,key,place)
   if(place.lt.1)then
      value=''
   else
      value=self%value(place)(:self%count(place))
   endif
end function dict_get
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    dict_add(3f) - [M_list] add or replace a key-value pair in a dictionary
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine dict_add(dict,key,value)
!!
!!    type(dictionary)            :: dict
!!    character(len=*),intent(in) :: key
!!    character(len=*),intent(in) :: VALUE
!!
!!##DESCRIPTION
!!    Add or replace a key-value pair in a dictionary.
!!
!!##OPTIONS
!!    DICT     is the dictionary.
!!    key      key name
!!    VALUE    value associated with key
!!
!!##EXAMPLES
!!
!!    If string is not found in a sorted array, insert the string
!!
!!     program demo_add
!!     use M_list, only : dictionary
!!     implicit none
!!     type(dictionary) :: dict
!!     integer          :: i
!!
!!     call dict%set('A','b')
!!     call dict%set('B','^')
!!     call dict%set('C',' ')
!!     call dict%set('D','c')
!!     call dict%set('E','ZZ')
!!     call dict%set('F','ZZZZ')
!!     call dict%set('G','z')
!!     call dict%set('A','new value for A')
!!     write(*,'(*(a,"==>","[",a,"]",/))')(trim(dict%key(i)),dict%value(i)(:dict%count(i)),i=1,size(dict%key))
!!     end program demo_add
!!
!!    Results:
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine dict_add(self,key,value)

! ident_26="@(#)M_list::dict_add(3f): place key-value pair into dictionary, adding the key if required"

class(dictionary),intent(inout) :: self
character(len=*),intent(in)     :: key
character(len=*),intent(in)     :: value
integer                         :: place
integer                         :: place2
   call locate(self%key,key,place)
   if(place.lt.1)then
      place2=iabs(place)
      call insert( self%key,   key,             place2 )
      call insert( self%value, value,           place2 )
      call insert( self%count, len_trim(value), place2 )
   elseif(place.gt.0)then  ! replace instead of insert
      call insert( self%value, value,           place )
      call insert( self%count, len_trim(value), place )
   endif
end subroutine dict_add
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_list
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================











!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_kracken
use M_verify,   only: debug, io_debug
use M_journal, only: journal
use M_strings, only: upper, string_to_value, split, s2v, atleast
use M_list,    only: locate, insert, replace
use M_args,    only: get_command_arguments_string, longest_command_argument
implicit none

! ident_1="@(#)M_kracken(3fm): parse command line options of Fortran programs using Unix-like syntax"

!===================================================================================================================================
   private
!-----------------------------------------------------------------------------------------------------------------------------------
   public :: kracken                ! define command and default parameter values from command arguments
!-----------------------------------------------------------------------------------------------------------------------------------
   public :: rget                   ! fetch real    value of name VERB_NAME from the Language Dictionary
   public :: dget                   ! fetch double  value of name VERB_NAME from the Language Dictionary
   public :: iget                   ! fetch integer value of name VERB_NAME from the Language Dictionary
   public :: lget                   ! fetch logical value of name VERB_NAME from the Language Dictionary
   public :: sget                   ! fetch string  value of name VERB_NAME from the Language Dictionary.
!-----------------------------------------------------------------------------------------------------------------------------------
   public :: rgets                  ! fetch real    values of name VERB_NAME from the Language Dictionary
   public :: dgets                  ! fetch double  values of name VERB_NAME from the Language Dictionary
   public :: igets                  ! fetch integer values of name VERB_NAME from the Language Dictionary
   public :: lgets                  ! fetch logical values of name VERB_NAME from the Language Dictionary
   public :: sgets                  ! fetch string  values of name VERB_NAME from the Language Dictionary.
!-----------------------------------------------------------------------------------------------------------------------------------
   public :: retrev                 ! retrieve token value as string from Language Dictionary when given NAME
!-----------------------------------------------------------------------------------------------------------------------------------
!  SPECIAL-PURPOSE PUBLIC ROUTINES:
   public :: setprompts             ! define prompts for commands in interactive mode
!  Only needs to be public for building languages, not cracking command line arguments
   public :: dissect                ! for user-defined commands: define defaults, then process user input
   public :: parse                  ! parse user command and store tokens into Language Dictionary
   public :: store                  ! replace dictionary name's value (if allow=add add name if necessary)
   public :: show                   ! display dictionary contents for information
!-----------------------------------------------------------------------------------------------------------------------------------
   private :: subscript_            ! return the subscript value of a string when given its name
   private :: menu                  ! generate an interactive menu when -? option is used
!-----------------------------------------------------------------------------------------------------------------------------------
! length of verbs and entries in Language dictionary
! NOTE:   many parameters may be  reduced in size so as to just accommodate being used as a command line parser.
!         In particular, some might want to change:
   logical,public            :: stop_command=.false.               ! indication to return stop_command as false in interactive mode
   integer,parameter,public  :: IPvalue=4096*16                    ! length of keyword value
   integer,parameter,public  :: IPverb=20                          ! length of verb
!-----------------------------------------------------------------------------------------------------------------------------------
   integer, parameter        :: dp = kind(0.d0)
   integer, parameter        :: k_int = SELECTED_INT_KIND(9)       ! integer*4
   integer, parameter        :: k_dbl = SELECTED_REAL_KIND(15,300) ! real*8
!-----------------------------------------------------------------------------------------------------------------------------------
   ! dictionary for Language routines
   character(len=:),allocatable               :: dict_vals(:)      ! contains the values of string variables
   character(len=:),allocatable               :: dict_verbs(:)     ! string variable names
   integer(kind=k_int),allocatable            :: dict_lens(:)      ! significant lengths of string variable values
   integer(kind=k_int),allocatable            :: dict_calls(:)     ! number of times this keyword stored on a call to parse
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=1),save,public               :: kracken_comment='#'
   character(len=:),public,allocatable,save   :: leftover          ! remaining command(s) on line
   integer,public,save                        :: current_command_length=0 ! length of options for current command
!-----------------------------------------------------------------------------------------------------------------------------------
   public :: cmd_args_to_dictionary
   public :: print_kracken_dictionary
   public unnamed
   public kracken_method
   character(len=:),allocatable,save :: unnamed(:)
   character(len=10),save            :: kracken_method='kracken'
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    RETREV(3f) - [ARGUMENTS:M_kracken] get keyword value as a string from a command's argument list processed by kracken(3f)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   SUBROUTINE retrev(name, string, len, ier)
!!
!!    CHARACTER(len=*),intent(in)  :: name
!!    CHARACTER(len=*),intent(out) :: string
!!    INTEGER,intent(out)          :: len
!!    INTEGER,intent(out)          :: ier
!!
!!##DESCRIPTION
!!    When a command has had its command argument list parsed using the
!!    kracken(3f) routine the value associated with any keyword can be retrieved
!!    as a string.
!!
!!##OPTIONS
!!
!!     NAME    parameter name of form VERB_KEYWORD
!!     STRING  returned parameter value
!!     LEN     length of returned STRING
!!     IER     error flag. Any non-zero value means an error occurred
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_retrev
!!     use M_kracken, only : kracken, retrev
!!     use M_kracken, only : IPvalue ! length of keyword value
!!     implicit none
!!     character(len=IPvalue) :: val
!!     integer                :: len, ier
!!
!!     call kracken('demo', ' -value my default string')
!!     call retrev('demo_value',val,len,ier)
!!     write(*,'(a)')'VALUE IS '//trim(val)
!!
!!     end program demo_retrev
!!
!!   Example execution and output:
!!
!!     $ ./demo_retrev
!!     VALUE IS my default string
!!
!!     $ ./demo_retrev -value use this value instead
!!     VALUE IS use this value instead
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine retrev(name,val,len,ier)

! ident_2="@(#)M_kracken::retrev(3f): retrieve token value from Language Dictionary when given NAME"

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)     :: name        ! name of variable to retrieve value for in form VERB_NAME
character(len=*),intent(out)    :: val         ! value for requested variable
   integer,intent(out)          :: len         ! position of last non-blank character in requested variable
   integer,intent(out)          :: ier         ! error flag 0=found requested variable; -1=entry not found
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                      :: isub        ! subscript in dictionary where requested entry and corresponding value are found
!-----------------------------------------------------------------------------------------------------------------------------------
   isub=subscript_(name)                       ! get index entry is stored at
!-----------------------------------------------------------------------------------------------------------------------------------
   if(isub > 0)then                            ! entry was in dictionary
      val=dict_vals(isub)                      ! retrieve corresponding value for requested entry
      len=dict_lens(isub)                      ! get significant length of value
      ier=0                                    ! indicate requested entry name was successfully found
   else                                        ! entry was not in dictionary
      val=" "                                  ! set value to blank
      len=0                                    ! set length to zero
      ier=-1                                   ! set error flag to indicate requested entry was not found
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine retrev
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    dget(3f) - [ARGUMENTS:M_kracken] given keyword fetch doubleprecision value from command argument
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function dget(keyword) result(value)
!!
!!     character(len=*),intent(in)  :: keyword
!!     doubleprecision              :: value
!!
!!##DESCRIPTION
!!     The dget(3f) function returns a scalar doubleprecision value from a command line
!!     argument using the M_kracken(3fm) module.
!!
!!##OPTIONS
!!     KEYWORD    the dictionary keyword (in form VERB_KEYWORD) to retrieve.
!!                The VERB name comes from the first argument of the KRACKEN(3f)
!!                call. The KEYWORD is a keyword from the KRACKEN(3f) call that
!!                should be interpreted as a doubleprecision value.
!!
!!##RETURNS
!!     VALUE      doubleprecision value returned by function
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_dget
!!    use M_kracken, only: kracken, dget
!!    implicit none
!!    doubleprecision :: val
!!      ! define command arguments and parse user command
!!      call kracken('demo','-val 3.1416' )
!!      val=dget('demo_val') ! get any values specified on -val option
!!      write(*,*)val         ! print the value
!!    end program demo_dget
!!
!!   Example program runs:
!!
!!    $ demo_dget
!!       3.14159989
!!
!!    $ demo_dget -val 10
!!       10.0000000
!!
!!    $ demo_dget -val 3.000
!!       3.00000000
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function dget(keyword)

! ident_3="@(#)M_kracken::dget(3f): given keyword fetch dble value from Language Dictionary (zero on err)"

real(kind=dp)                :: dget              ! function type
character(len=*),intent(in)  :: keyword           ! keyword to retrieve value for from dictionary
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                      :: ier            ! error flag on call to retrieve value
!-----------------------------------------------------------------------------------------------------------------------------------
   call string_to_value(sget(keyword), dget, ier) ! convert the string to a numeric value
!-----------------------------------------------------------------------------------------------------------------------------------
end function dget
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    rget(3f) - [ARGUMENTS:M_kracken] given keyword fetch real value from command argument
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function rget(keyword) result(value)
!!
!!     character(len=*),intent(in)  :: keyword
!!     real                         :: value
!!
!!##DESCRIPTION
!!     The rget(3f) function returns a scalar real value from a command line
!!     argument using the M_kracken(3fm) module.
!!
!!##OPTIONS
!!     KEYWORD    the dictionary keyword (in form VERB_KEYWORD) to retrieve.
!!                The VERB name comes from the first argument of the KRACKEN(3f)
!!                call. The KEYWORD is a keyword from the KRACKEN(3f) call that
!!                should be interpreted as a REAL value.
!!
!!##RETURNS
!!     VALUE      real value returned by function
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_rget
!!    use M_kracken, only: kracken, rget
!!    implicit none
!!    real :: val
!!      ! define command arguments and parse user command
!!      call kracken('demo','-val 3.1416' )
!!      val=rget('demo_val') ! get any values specified on -val option
!!      write(*,*)val        ! print the value
!!    end program demo_rget
!!
!!   Example program runs:
!!
!!    $ demo_rget
!!       3.14159989
!!
!!    $ demo_rget -val 10
!!       10.0000000
!!
!!    $ demo_rget -val 3.000
!!       3.00000000
!!
!!##SEE ALSO
!!    M_kracken(3fm), kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse,dissect,store,setprompts,show
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function rget(keyword)

! ident_4="@(#)M_kracken::rget(3f): given keyword fetch real value from language dictionary (zero on err)"

!-----------------------------------------------------------------------------------------------------------------------------------
   real                        :: rget             ! function type
   character(len=*),intent(in) :: keyword          ! keyword to retrieve value for from dictionary
!-----------------------------------------------------------------------------------------------------------------------------------
   rget=real(dget(keyword))                        ! just call DGET(3f) but change returned value to type REAL
!-----------------------------------------------------------------------------------------------------------------------------------
end function rget
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    iget(3f) - [ARGUMENTS:M_kracken] given keyword fetch integer value from command argument
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function iget(keyword) result(value)
!!
!!     character(len=*),intent(in)  :: keyword
!!     integer              :: value
!!
!!##DESCRIPTION
!!     The iget(3f) function returns a scalar integer value from a command line
!!     argument using the M_kracken(3fm) module.
!!
!!##OPTIONS
!!     KEYWORD    the dictionary keyword (in form VERB_KEYWORD) to retrieve.
!!                The VERB name comes from the first argument of the KRACKEN(3f)
!!                call. The KEYWORD is a keyword from the KRACKEN(3f) call that
!!                should be interpreted as a integer value.
!!
!!##RETURNS
!!     VALUE      integer value returned by function
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_iget
!!    use M_kracken, only: kracken, iget
!!    implicit none
!!    integer :: val
!!      ! define command arguments and parse user command
!!      call kracken('demo','-val 31416' )
!!      val=iget('demo_val') ! get any values specified on -val option
!!      write(*,*)val        ! print the value
!!    end program demo_iget
!!
!!   Example program runs:
!!
!!    $ demo_iget
!!       31416
!!
!!    $ demo_iget -val 10
!!       10
!!
!!    $ demo_iget -val 3.000
!!       3
!!
!!##SEE ALSO
!!    M_kracken(3fm), kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function iget(keyword)

! ident_5="@(#)M_kracken::iget(3f): given keyword fetch integer value from Language Dictionary (0 on err)"

!-----------------------------------------------------------------------------------------------------------------------------------
   integer                      :: iget            ! function type
   character(len=*),intent(in)  :: keyword         ! keyword to retrieve value for from dictionary
!-----------------------------------------------------------------------------------------------------------------------------------
   !iget = int(dget(keyword)+0.5_dp)               ! just call DGET(3f) but change returned value to type INTEGER
   iget = int(dget(keyword))                       ! just call DGET(3f) but change returned value to type INTEGER
!-----------------------------------------------------------------------------------------------------------------------------------
end function iget
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    lget(3f) - [ARGUMENTS:M_kracken] given keyword fetch logical value from command arguments
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function lget(keyword) result(lval)
!!
!!     character(len=*),intent(in)  :: keyword
!!     logical                      :: lval
!!##DESCRIPTION
!!
!!     The lget(3f) function returns a scalar logical value from a command line
!!     argument using the M_kracken(3fm) module.
!!
!!##OPTIONS
!!     keyword    the dictionary keyword (in form VERB_KEYWORD) to retrieve.
!!                The VERB name comes from the first argument of the
!!                KRACKEN(3f) call. The KEYWORD is a keyword from the second
!!                argument to the KRACKEN(3f) call.
!!
!!##RETURNS
!!     lval       logical value returned by function. The input value should be
!!                from the case-insensitive list of the words "true, false,
!!                t, f, yes, no, y, n, .false., .true., .f., .t.,''". .TRUE. is returned
!!                if the corresponding string in the dictionary for KEYWORD is blank.
!!                .FALSE. is returned if a string not in the list is found.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_lget
!!    use M_kracken, only: kracken, lget
!!    implicit none
!!    logical  :: val
!!      ! define command arguments and parse user command
!!      call kracken('demo','-truth .F.' )
!!      ! get any values specified on command line for -truth
!!      val=lget('demo_truth')
!!      write(*,'("The truth is ",l1)')val
!!    end program demo_lget
!!
!!   Example program runs:
!!
!!      $ demo_lget             # uses the default
!!      The truth is F
!!      $ demo_lget -truth      # A BLANK VALUE IS TRUE
!!      The truth is T
!!      $ demo_lget -truth yes  # Y, yes, T, true, .T., .true. are all true
!!      The truth is T
!!      $ demo_lget -truth F    # N, no, F, false, .F., .FALSE. are all false
!!      The truth is F
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function lget(keyword)

! ident_6="@(#)M_kracken::lget(3f): given keyword fetch logical value from lang. dictionary (.f. on err)"

!-----------------------------------------------------------------------------------------------------------------------------------
logical                      :: lget                  ! procedure type
character(len=*),intent(in)  :: keyword               ! the dictionary keyword (in form VERB_KEYWORD) to retrieve
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=:),allocatable :: value              ! value corresponding to the requested keyword
!-----------------------------------------------------------------------------------------------------------------------------------
!  ignore a leading dot character. Then, any word starting in "T" or "Y" is true, any word starting in "F" or "N" is false.
   if(len(sget(keyword)).ne.0)then
      value=trim(adjustl(upper(sget(keyword))))//' '  ! get value as uppercase, spaces trimmed, then add trailing space
      lget=.false.                                    ! initialize return value to .false.
      if(value.ne."#N#".and.value.ne.'"#N#"')then
         select case(value(1:1))                      ! check first letter
         case('T','Y',' ') ; lget=.true.              ! anything starting with "T" or "Y" or a blank is TRUE (true,t,yes,y,...)
         case('F','N')     ; lget=.false.             ! assume this is false or no
         case('.')                                    ! looking for fortran logical syntax .STRING.
            select case(value(2:2))
            case('T')      ; lget=.true.              ! assume this is .t. or .true.
            case('F')      ; lget=.false.             ! assume this is .f. or .false.
            case default
               call journal("*lget* bad logical expression for "//trim(keyword)//'='//value)
            end select
         case default
               call journal("*lget* bad logical expression for "//trim(keyword)//'='//value)
         end select
      else                                            ! special value "#N#" is assumed FALSE
         lget=.false.
      endif
   else
      lget=.false.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function lget
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    sget(3f) - [ARGUMENTS:M_kracken] given keyword fetch string value and length from command arguments
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function sget(name,ilen) result(string)
!!
!!    character(len=*),intent(in)   :: name        ! name to look up in dictionary
!!    integer,intent(out),optional  :: ilen        ! length of returned output string
!!    character(len=:),allocatable  :: string      ! returned value
!!
!!##DESCRIPTION
!!     The sget(3f) function returns a scalar character value from a command line
!!     argument using the M_kracken(3fm) module.
!!
!!##OPTIONS
!!     name    the dictionary keyword (in form VERB_KEYWORD) to retrieve.
!!             The VERB name comes from the first argument of the
!!             KRACKEN(3f) call. The KEYWORD is a keyword from the second
!!             argument to the KRACKEN(3f) call.
!!             This routine trusts that the desired name exists.
!!
!!##RETURNS
!!     string  returned string. If LEN(STRING).EQ.0 an error occurred, such
!!             as NAME not being in the dictionary.
!!     ilen    optional length of returned output string
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_sget
!!    use M_kracken, only: kracken, sget
!!    implicit none
!!    character(len=:),allocatable :: string, a, b
!!      ! define command arguments and parse user command
!!      call kracken('demo','-string This is the default -a A default -b B default' )
!!      ! get any values specified on command line for -truth
!!      string=sget('demo_string')
!!      a=sget('demo_a')
!!      b=sget('demo_b')
!!      write(*,'("string is ",a)')trim(string)
!!      write(*,'("a is ",a)')trim(a)
!!      write(*,'("b is ",a)')trim(b)
!!    end program demo_sget
!!
!!   Example program runs:
!!
!!    $demo_sget
!!    string is This is the default
!!    a is A default
!!    b is B default
!!
!!    $ demo_sget -a A value for A -string new value for string -b BBBBBBB
!!    string is new value for string
!!    a is A value for A
!!    b is BBBBBBB
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function sget(name,ilen) result(string)

! ident_7="@(#)M_kracken::sget(3f): Fetch string value and length of specified NAME from lang. dictionary"

!  This routine trusts that the desired name exists. A blank is returned if the name is not in the dictionary
character(len=:),allocatable  :: string      ! returned value
character(len=*),intent(in)   :: name        ! name to look up in dictionary
integer,intent(out),optional  :: ilen        ! length of returned output string
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                    :: isub        ! index where verb_oo is stored or -1 if this is an unknown name
!-----------------------------------------------------------------------------------------------------------------------------------
   isub=subscript_(name)                     ! given name return index name is stored at
!-----------------------------------------------------------------------------------------------------------------------------------
   if(isub > 0)then                          ! if index is valid return string
      string=trim(dict_vals(isub))
      if(len(string).eq.0)string=" "
   else                                      ! if index is not valid return blank string
      string=""
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(ilen))then                     ! if ILEN is present on call, return the value
      ilen=dict_lens(isub)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function sget
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    dgets(3f) - [ARGUMENTS:M_kracken] given keyword fetch doubleprecision array from command arguments
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function dgets(keyword,ier) result(darray)
!!
!!     character(len=*),intent(in)   :: keyword
!!     doubleprecision,allocatable   :: DARRAY
!!     integer,optional,intent(iout) :: ier
!!
!!##DESCRIPTION
!!     The dgets(3f) function returns a dynamically allocated array of
!!     doubleprecision values from a string that is the value for a command
!!     line option. It is part of the M_kracken(3fm) module.
!!
!!     Values that cannot be read as a numeric value are returned as a NaN.
!!
!!##OPTIONS
!!     keyword  dictionary name to retrieve, of form VERB_NAME where VERB
!!              is taken from the first parameter of the call to KRACKEN(3f)
!!              or DISSECT(3f).
!!
!!##RETURNS
!!     darray   double precision numeric array returned by function. The array
!!              will have zero size if the parsed dictionary entry is blank.
!!     IER      If present and non-zero an error occurred in converting strings to a value
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_dgets
!!    use M_kracken, only: kracken, dgets
!!    implicit none
!!    doubleprecision,allocatable  :: vals(:)
!!    integer              :: i
!!    ! define command arguments and parse user command
!!    call kracken('demo','-nums 1 2 3 1000 100,000 11.11111 77.77777 -77.7777' )
!!    vals=dgets('demo_nums') ! get any values specified for -nums
!!    write(*,'(*(g0:,","))')( vals(i),i=1,size(vals)) ! print the values
!!    end program demo_dgets
!!
!!   Example program runs:
!!
!!    $ demo_dgets
!!     1.0000000000000000,2.0000000000000000,3.0000000000000000,
!!     1000.0000000000000,100000.00000000000,11.111110000000000,
!!     77.777770000000004,-77.777699999999996
!!
!!    $ demo_dgets -nums 89,123,456.789 10.9999999
!!     89123456.789000005,10.999999900000001
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function dgets(keyword,ier) result(darray)

! ident_8="@(#)M_kracken::dgets(3f): given keyword fetch dble value from Language Dictionary (0 on err)"

character(len=*),intent(in) :: keyword                      ! keyword to retrieve value for from dictionary
real(kind=dp),allocatable   :: darray(:)                    ! function type

   character(len=:),allocatable  :: carray(:)          ! convert value to an array using split(3f)
   integer                       :: i
   integer,optional              :: ier
   integer                       :: ier_local
!-----------------------------------------------------------------------------------------------------------------------------------
   if(sget(keyword).ne.' ')then
      call split(sget(keyword),carray)                      ! find value associated with keyword and split it into an array
   else
      allocate(character(len=0) :: carray(0))
   endif
   allocate(darray(size(carray)))                           ! create the output array
   ier_local=0
   if(present(ier))then
         ier=0
   endif
   do i=1,size(carray)
      call string_to_value(carray(i), darray(i), ier_local)       ! convert the string to a numeric value
      if(present(ier).and.ier_local.ne.0)then
         ier=ier_local
      endif
      !if(ier_local.ne.0)then
      !   !darray(i)=0.0d0
      !   deallocate(darray)
      !   allocate(darray(i-1))                              ! create the output array
      !   exit
      !endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end function dgets
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    igets(3f) - [ARGUMENTS:M_kracken] given keyword fetch integer array from command arguments
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function igets(keyword,ier) result(iarray)
!!
!!     character(len=*),intent(in)   :: keyword
!!     integer,allocatable           :: iarray(:)
!!     integer,optional,intent(iout) :: ier
!!
!!##DESCRIPTION
!!     The igets(3f) function returns a dynamically allocated array of integers
!!     from a string that is the value for a command line option. It is part of
!!     the M_kracken(3fm) module.
!!
!!     Values that cannot be read as an integer value are returned as a NaN.
!!
!!##OPTIONS
!!     KEYWORD    the dictionary keyword (in form VERB_KEYWORD) to retrieve.
!!                The VERB name comes from the first argument of the KRACKEN(3f)
!!                call. The KEYWORD is a keyword from the KRACKEN(3f) call that
!!                should be interpreted as a list of INTEGER values. Decimal values
!!                are allowed but truncated. Note that comma characters are ignored.
!!
!!##RETURNS
!!     IARRAY     INTEGER array returned by function
!!                The array will have zero size if the parsed dictionary
!!     IER        If present and non-zero an error occurred in converting strings to a value
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_igets
!!    use M_kracken, only: kracken, igets
!!    implicit none
!!    doubleprecision,allocatable  :: vals(:)
!!    integer              :: i
!!    ! define command arguments and parse user command
!!    call kracken('demo','-nums 1 2 3 100 1000 10000 100,000 11.11111 77.77777 -77.7777' )
!!    vals=igets('demo_nums') ! get any values specified for -nums
!!    write(*,'(*(g0:,","))')( vals(i),i=1,size(vals)) ! print the values
!!    end program demo_igets
!!
!!   Example program runs:
!!
!!    $ demo_igets
!!    1,2,3,100,1000,10000,100000,11,77,-77
!!    $ demo_igets -val 89,123,456 10.9999999
!!    89123456,10
!!
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function igets(keyword,ier) result(iarray)

! ident_9="@(#)M_kracken::igets(3f): given keyword fetch integer array from string in dictionary(0 on err)"

character(len=*),intent(in) :: keyword             ! keyword to retrieve value for from dictionary
integer,allocatable         :: iarray(:)           ! convert value to an array
doubleprecision,allocatable :: darray(:)           ! convert value to an array
integer,optional            :: ier
   if(present(ier))then
      darray=dgets(keyword,ier)
      if(ier.eq.0)then
         iarray=int(darray)                           ! just call DGETS(3f) but change returned value to type INTEGER
      else
         iarray=[integer ::]
      endif
   else
      iarray=int(dgets(keyword))                   ! just call DGETS(3f) but change returned value to type INTEGER
   endif
end function igets
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    rgets(3f) - [ARGUMENTS:M_kracken] given keyword fetch real array from command arguments
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function rgets(keyword,ier) result(rarray)
!!
!!     character(len=*),intent(in)   :: keyword
!!     real,allocatable              :: rarray(:)
!!     integer,optional,intent(iout) :: ier
!!
!!##DESCRIPTION
!!     The rgets(3f) function returns a dynamically allocated array of real values
!!     from a string that is the value for a command line option. It is part of
!!     the M_kracken(3fm) module.
!!
!!     Values that cannot be read as a numeric value are returned as a NaN.
!!
!!##OPTIONS
!!     KEYWORD    the dictionary keyword (in form VERB_KEYWORD) to retrieve.
!!                The VERB name comes from the first argument of the KRACKEN(3f)
!!                call. The KEYWORD is a keyword from the KRACKEN(3f) call that
!!                should be interpreted as a list of REAL values.
!!
!!##RETURNS
!!     RARRAY     real array returned by function.
!!                The array will have zero size if the parsed dictionary
!!                entry is blank.
!!     IER        If present and non-zero an error occurred in converting strings to a value
!!
!!##EXAMPLE
!!
!!   Sample program converts between Celcius and Fahrenheit
!!
!!    program demo_rgets
!!    use M_kracken, only: kracken, rgets
!!    implicit none
!!    real,allocatable  :: val(:)
!!    integer           :: i
!!      ! define command arguments and parse user command
!!      call kracken('fc','-F -C' )
!!
!!      ! get any values specified on -C option
!!      val=rgets('fc_C')
!!      ! test if have something to print in C ==> F table
!!      if(size(val).gt.0)then
!!         ! print the requested values
!!         write(*,'(a,t14,a)')'celsius','fahrenheit'
!!         write(*,'(f5.1,t14,f5.1)')( val(i),(val(i)+40.0)*9.0/5.0 - 40.0,i=1,size(val))
!!      endif
!!
!!      val=rgets('fc_F')
!!      ! check for values on -F
!!      if(size(val).gt.0)then
!!         write(*,'(a,t14,a)') 'fahrenheit', 'celsius'
!!         write(*,'(f5.1,t14,f5.1)')(val(i),(val(i)+40.0)*5.0/9.0 - 40.0,i=1,size(val))
!!      endif
!!    end program demo_rgets
!!
!!   Example program runs:
!!
!!    % demo_rgets -C -273.15 0 100 -40 37
!!    celsius      fahrenheit
!!     -273.15      -459.67
!!        0.0         32.0
!!      100.0        212.0
!!      -40.0        -40.0
!!       37.0         98.6
!!
!!    % demo_rgets -F -459.67 32 212 -40 98.6
!!    fahrenheit   celsius
!!     -459.67      -273.15
!!       32.00         0.00
!!      212.00       100.00
!!      -40.00       -40.00
!!       98.60        37.00
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function rgets(keyword,ier) result(rarray)

! ident_10="@(#)M_kracken::rgets(3f): given keyword fetch real array from string in dictionary (0 on err)"

character(len=*),intent(in) :: keyword             ! keyword to retrieve value for from dictionary
real,allocatable            :: rarray(:)           ! convert value to an array
doubleprecision,allocatable :: darray(:)           ! convert value to an array
integer,optional            :: ier
   if(present(ier))then
      darray=dgets(keyword,ier)
      if(ier.eq.0)then
         rarray=real(darray)              ! just call DGETS(3f) but change returned value to type REAL
      else
         rarray=[real ::]
      endif
   else
      rarray=real(dgets(keyword))                  ! just call DGETS(3f) but change returned value to type REAL
   endif
end function rgets
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    lget(3f) - [ARGUMENTS:M_kracken] given keyword fetch logical array from command argument
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function lgets(keyword) result(lvals)
!!
!!     character(len=*),intent(in)  :: keyword
!!     logical,allocatable          :: lvals(:)
!!
!!##DESCRIPTION
!!     The lgets(3f) function returns a dynamically allocated array of logical values
!!     from a string that is the value for a command line option. It is part of
!!     the M_kracken(3fm) module.
!!
!!     Values that cannot be read as a logical value are returned as a ".FALSE.".
!!
!!##OPTIONS
!!     keyword    the dictionary keyword (in form VERB_KEYWORD) to retrieve.
!!                The VERB name comes from the first argument of the
!!                KRACKEN(3f) call. The KEYWORD is a keyword from the second
!!                argument to the KRACKEN(3f) call.
!!
!!##RETURNS
!!     lvals      logical array returned by function. The input value should be
!!                from the case-insensitive list of the words "true, false,
!!                t, f, yes, no, y, n, .false., .true., .f., .t".
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_lgets
!!    use M_kracken, only: kracken, lgets
!!    implicit none
!!    logical,allocatable  :: vals(:)
!!      ! define command arguments and parse user command
!!      call kracken('demo','-truths .F. .T. .F. .F. .T. .T.' )
!!      ! get any values specified on command line for -truth
!!      vals=lgets('demo_truths')
!!      write(*,*)vals
!!    end program demo_lgets
!!
!!   Example program runs:
!!
!!    $ demo_lgets
!!     F T F F T T
!!
!!    $ demo_lgets -truths false F .f. no true .true. t T Yes No
!!     F F F F T T T T T T F
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function lgets(keyword) result(larray)

! ident_11="@(#)M_kracken::lgets(3f): given keyword fetch logical array from string in dictionary(F on err)"

character(len=*),intent(in)  :: keyword                    ! the dictionary keyword (in form VERB_KEYWORD) to retrieve
logical,allocatable          :: larray(:)                  ! convert value to an array
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=:),allocatable       :: carray(:)         ! convert value to an array
   integer                            :: i
   integer                            :: ichar             ! point to first character of word unless first character is "."
!-----------------------------------------------------------------------------------------------------------------------------------
   if(len(sget(keyword)).ne.0)then
   call split(adjustl(upper(sget(keyword))),carray)        ! convert value to uppercase, left spaces trimmed; then parse into array
   if(size(carray).gt.0)then                                  ! if not a null string
      allocate(larray(size(carray)))                          ! allocate output array
      do i=1,size(carray)
         larray(i)=.false.                                    ! initialize return value to .false.
         if(carray(i).ne."#N#".and.carray(i).ne.'"#N#"')then
            if(carray(i)(1:1).eq.'.')then                     ! looking for fortran logical syntax .STRING.
               ichar=2
            else
               ichar=1
            endif
            select case(carray(i)(ichar:ichar))               ! check word to see if true or false
            case('T','Y',' '); larray(i)=.true.               ! anything starting with "T" or "Y" or a blank is TRUE (true,yes,...)
            case('F','N');     larray(i)=.false.              ! assume this is false or no
            case default
                  call journal("*lgets* bad logical expression for "//trim(keyword)//'='//carray(i))
            end select
         else                                                 ! special value "#N#" is assumed FALSE
            larray(i)=.false.
         endif
      enddo
   else                                                       ! for a blank string return one T
      allocate(larray(1))                                     ! allocate output array
      larray(1)=.true.
   endif
   else
      allocate(larray(1))                                     ! allocate output array
      larray(1)=.false.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function lgets
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    sgets(3f) - [ARGUMENTS:M_kracken] given keyword fetch string value parsed on whitespace into an array
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function sgets(name,delim) result(strings)
!!
!!    character(len=*),intent(in) :: name
!!    character(len=*),intent(in),optional :: delim
!!    character(len=:),allocatable :: strings(:)
!!
!!##DESCRIPTION
!!     The sgets(3f) function returns a dynamically allocated array of character values
!!     from a string that is the value for a command line option. It is part of
!!     the M_kracken(3fm) module.
!!
!!##OPTIONS
!!     name     the dictionary keyword (in form VERB_KEYWORD) to retrieve.
!!              The VERB name comes from the first argument of the
!!              KRACKEN(3f) or DISSECT(3f) call. The KEYWORD is a keyword from the second
!!              argument to the KRACKEN(3f) or DISSECT(3f) call.
!!              This routine trusts that the desired name exists.
!!              If the name does not exist the array [char(0)] is returned.
!!              An array of zero size is returned if the string is blank.
!!     delim    characters to split the string at into elements
!!
!!##RETURNS
!!     strings  returned string array
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_sgets
!!    use M_kracken, only : kracken, sgets
!!    character(len=:),allocatable :: strings(:)
!!       call kracken('cmd',' -string    This   is  a sentence ')
!!       strings= sgets("cmd_string")            ! get -strings words
!!       print *, "string=",('['//trim(strings(i))//']',i=1,size(strings))
!!       print *, "len= ",len(strings)
!!       print *, "size=",size(strings)
!!    end program demo_sgets
!!
!!   Example program execution:
!!
!!    $ demo_sgets
!!     string=[This][is][a][sentence]
!!     len=            8
!!     size=           4
!!
!!    $ demo_sgets -string a b c d e f g
!!     string=[a][b][c][d][e][f][g]
!!     len=            1
!!     size=           7
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function sgets(name,delim) result(strings)

! ident_12="@(#)M_kracken::sgets(3f): Fetch strings value for specified NAME from the lang. dictionary"

! This routine trusts that the desired name exists. A blank is returned if the name is not in the dictionary
character(len=:),allocatable         :: strings(:)
character(len=*),intent(in)          :: name                       ! name to look up in dictionary
character(len=*),intent(in),optional :: delim

integer                              :: isub                      ! index where verb_oo is stored or -1 if this is an unknown name
!-----------------------------------------------------------------------------------------------------------------------------------
   isub=subscript_(name)                                          ! given name return index name is stored at
!-----------------------------------------------------------------------------------------------------------------------------------
   if(isub > 0)then                                               ! if index is valid return strings
      if(present(delim))then
         call split(dict_vals(isub),strings,delim)
      else
         call split(dict_vals(isub),strings)
      endif
   else                                                           ! if index is not valid return NULL string
      allocate(character(len=1) :: strings(1))
      strings(1)=char(0)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function sgets
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    kracken(3f) - [ARGUMENTS:M_kracken] crack command line options on Fortran programs, using "-KEYWORD VALUE" syntax
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     subroutine kracken(verb, string[,ierror][style])
!!
!!        character(len=*), intent(in) ::  verb
!!        character(len=*), intent(in) :: string
!!        integer, intent(out), optional :: ierror
!!        character(len=*), intent(in),optional :: style
!!
!!##DESCRIPTION
!!     This is the main public procedure in the M_kracken(3f) module.
!!     It is used to define the command line options, their default
!!     values, and to crack the command line options using a syntax
!!     that looks very much like an execution of the program.
!!
!!##OPTIONS
!!     VERB     arbitrary command name, usually 'cmd' or the name of the
!!              program calling the routine. This defines the
!!              variable prefix name used by the other functions to
!!              retrieve command option values.
!!
!!     STRING   prototype command to define keywords and defaults.
!!              This string is simply a list of all keywords and their
!!              default values exactly as you would type them on the
!!              command line, with default values explicitly set.
!!
!!     IERROR   If an error occurs such as an unknown keyword the
!!              calling program will be stopped unless the optional
!!              parameter IERROR is present. If present, it is up
!!              to the calling program to decide what to do if
!!              a non-zero value is returned.
!!     STYLE    parsing style. Either 'kracken' or 'args'. The default
!!              is 'kracken'.
!!
!!##EXAMPLE
!!
!!
!!   Sample program:
!!
!!       program demo_kracken
!!
!!       use M_kracken
!!       ! define command arguments, default values and crack command line
!!       call kracken('cmd',              &
!!          &   '-int 20                  &
!!          &   -real 10e3                &
!!          &   -file input               &
!!          &   -dble 4.11223344556677d0  &
!!          &   -help    .false.          &
!!          &   -version .false.         '&
!!          &   )
!!       ! that's it. You defined your command arguments and their default
!!       ! values and parsed the user-supplied command line arguments.
!!
!!       ! Now you can just retrieve the values as strings using
!!       ! names of the form VERB_SWITCHNAME anywhere in your program.
!!       ! Note that the special name "VERB_oo"  is for the string
!!       ! before any switch.
!!          if(lget('cmd_help'))then ! was -help specified?
!!             write(*,*)'The help text'
!!             stop
!!          endif
!!          if(lget('cmd_version'))then ! was -version specified?
!!             write(*,*)'version 1.0 20161030'
!!             stop
!!          endif
!!          ! convert all the remaining options to scalar values
!!          ! and call a procedure with the values
!!          call mymain(                  &
!!          & sget('cmd_file'),           &
!!          & rget('cmd_real'),           &
!!          & dget('cmd_dble'),           &
!!          & iget('cmd_int')             &
!!          & )
!!       contains
!!       subroutine mymain(filename,value1,value2,ivalue3)
!!       ! this routine is using conventional values and does
!!       ! not use M_kracken(3fm) module at all
!!       implicit none
!!       character(len=*),intent(in) :: filename
!!       real,intent(in)             :: value1
!!       doubleprecision,intent(in)  :: value2
!!       integer,intent(in)          :: ivalue3
!!          ! just to show the command arguments have
!!          ! been processed echo the values
!!          print *, 'filename=',trim(filename)
!!          print *, 'values=',value1,value2,ivalue3
!!       end subroutine mymain
!!       end program demo_kracken
!!
!!   expected output from : "./cmd"
!!
!!          filename=input
!!          values= 10000.0000  4.1122334455667700  20
!!
!!   expected output from : "./cmd -file myfile -int 1234"
!!
!!          filename=myfile
!!          values= 10000.0000  4.1122334455667700  1234
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine kracken(verb,string,error_return,style)

! ident_13="@(#)M_kracken::kracken(3f): define and parse command line options"

!  get the entire command line argument list and pass it and the prototype to dissect()
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)          :: string
character(len=*),intent(in)          :: verb
integer,intent(out),optional         :: error_return
character(len=*),intent(in),optional :: style
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=:),allocatable         :: command
integer                              :: ier
integer                              :: ibig
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(error_return))then
      error_return=0
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ier=0
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(style))then
      kracken_method=style
   else
      kracken_method='kracken'
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ! no matter what method make sure this is allocated so user can query it
   ! and so methods can use unnamed array without having to test it
   if(allocated(unnamed))then
      deallocate(unnamed)
   endif
   ibig=longest_command_argument() ! bug in gfortran. len=0 should be fine
   allocate(character(len=ibig) ::unnamed(0))
   unnamed=[character(len=ibig) ::]            ! kludge
!-----------------------------------------------------------------------------------------------------------------------------------
   select case(upper(kracken_method))
    case('ARGS')
      call parse(trim(verb),string,'add')                ! initialize command
      call cmd_args_to_dictionary(trim(verb))            ! process user command options
      if(lget(trim(verb)//'_?'))then                     ! if -? option was present prompt for values
         call menu(verb)
      endif
      ! if calling procedure is not testing error flag stop program on error
      if(.not.present(error_return).and.ier.ne.0)then
         call journal("*kracken* (V 20191018) STOPPING: error parsing arguments using ARGS method")
         stop
      endif
    case default
      call store(trim(verb)//'_?','.false.','add',ier)   ! all commands have the option -? to invoke prompt mode
      call get_command_arguments_string(command,ier)
      if(debug) call journal('sc','KRACKEN ',trim(command))
      if(ier.ne.0)then
         call journal("*kracken* could not get command line arguments")
         if(present(error_return))error_return=ier
      else
         call dissect(verb,string,command,ier)
         ! if calling procedure is not testing error flag stop program on error
         if(.not.present(error_return).and.ier.ne.0)then
            call journal("*kracken* (V 20191018) STOPPING: error parsing arguments using DEFAULT method")
            stop
         endif
      endif
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine kracken
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    setprompts(3f) - [ARGUMENTS:M_kracken] set explicit prompts for keywords in interactive mode
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine setprompts(verb,init)
!!
!!    character(len=*),intent(in):: verb
!!    character(len=*),intent(in):: init
!!
!!##DESCRIPTION
!!
!!    Optionally set prompts for interactive prompting mode.
!!    The syntax of the call is the same as for KRACKEN(3f)/DISSECT(3f) except that prompt
!!    strings are given instead of default values. It is called before a call to KRACKEN(3f)
!!    or DISSECT(3f).
!!
!!##OPTIONS
!!    verb    name to define prompts for
!!    string  to define prompts instead of values
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_setprompts
!!     use M_kracken, only : kracken,iget,rget,sget,setprompts
!!     implicit none
!!
!!     call setprompts('demo', ' -int My INTEGER value  -float My REAL value  -str My CHARACTER value')
!!     call kracken(   'demo', ' -int 100 -float 123.456 -str DEFAULT')
!!     write(*,'(a,i0)')'INTEGER IS ',iget('demo_int')
!!     write(*,'(a,g0)')'REAL IS ',rget('demo_float')
!!     write(*,'(a,a)')'STRING IS '//trim(sget('demo_str'))
!!
!!     end program demo_setprompts
!!
!!   Example execution and output:
!!
!!        $ demo_setprompts -?
!!        demo parameters are
!!        >   1)My CHARACTER value:[DEFAULT]
!!            3)My INTEGER value:[100]
!!            4)My REAL value:[123.456]
!!        Enter parameter number to change("RETURN" to finish):
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine setprompts(verb,init)

! ident_14="@(#)M_kracken::setprompts(3f): set explicit prompts for keywords in interactive mode"

character(len=*),intent(in):: verb   ! verb name to define prompts for
character(len=*),intent(in):: init   ! string to define prompts instead of values
      call parse('?'//trim(verb),init,'add') ! initialize command, prefixing verb with question mark character to designate prompts
end subroutine setprompts
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    dissect(3f) - [ARGUMENTS:M_kracken] convenient call to parse() -- define defaults, then process
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine dissect(verb,init,pars,error_return)
!!
!!    character(len=*),intent(in)  :: verb
!!    character(len=*),intent(in)  :: init
!!    character(len=*),intent(in)  :: pars
!!    integer,intent(out),optional :: error_return
!!##DESCRIPTION
!!
!!##OPTIONS
!!    VERB          the name of the command to be reset/defined
!!    INIT          used to define command options; usually hard-set in the program.
!!    PARS          defines the command options to be set, usually from user input
!!
!!##RETURNS
!!    ERROR_RETURN  error code. If zero no error occurred.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_dissect
!!     use M_kracken, only : kracken,iget,rget,sget,dissect
!!     implicit none
!!     integer :: ierr
!!
!!     call dissect('demo',' -int 1000 -float 1234.567 -str CHARACTER value','-int 456 -float 50.00 ',ierr)
!!     write(*,'(a,i0)')'INTEGER IS ',iget('demo_int')
!!     write(*,'(a,g0)')'REAL IS ',rget('demo_float')
!!     write(*,'(a,a)')'STRING IS '//trim(sget('demo_str'))
!!
!!     end program demo_dissect
!!
!!   Results:
!!
!!    INTEGER IS 456
!!    REAL IS 50.0000000
!!    STRING IS CHARACTER value
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine dissect(verb,init,pars,error_return)

! ident_15="@(#)M_kracken::dissect(3f): convenient call to parse() define defaults, then process"

character(len=*),intent(in)  :: verb                     ! the name of the command to be reset/defined  and then set
character(len=*),intent(in)  :: init                     ! used to define or reset command options; usually hard-set in the program.
character(len=*),intent(in)  :: pars                     ! defines the command options to be set, usually from a user input file
integer,intent(out),optional :: error_return
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                :: ier
!-----------------------------------------------------------------------------------------------------------------------------------
   if(debug) call journal('sc','START DISSECT ',trim(verb)//'::'//trim(init)//'::'//trim(pars))
!-----------------------------------------------------------------------------------------------------------------------------------
   call store(trim(verb)//'_?','.false.','add',ier)   ! all commands have the option -? to invoke prompt mode
   call parse(trim(verb),init,'add')                  ! initialize command
!-----------------------------------------------------------------------------------------------------------------------------------
   call parse(verb,pars,"no_add",ier)                 ! process user command options
   if(lget(trim(verb)//'_?'))then                     ! if -? option was present prompt for values
      call menu(verb)
   endif
   if(present(error_return))error_return=ier
!-----------------------------------------------------------------------------------------------------------------------------------
   if(debug) call journal('sc','END DISSECT ',trim(verb)//'::'//trim(init)//'::'//trim(pars))
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine dissect
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    parse(3f) - [ARGUMENTS:M_kracken] parse user command and store tokens into Language Dictionary
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!! recursive subroutine parse(verb,string,allow,error_return)
!!
!!    character(len=*),intent(in)     ::  verb
!!    character(len=*),intent(in)     ::  string
!!    character(len=*),intent(in)     ::  allow
!!    integer,optional,intent(out)    ::  error_return
!!
!!##DESCRIPTION
!!    given a string of form
!!
!!      value  -var value -var value
!!
!!    define variables of form
!!
!!      verb_var(i) = value
!!
!!    --var will become verb__var
!!
!!    o  values may be in double quotes if they contain alphameric characters
!!    o  a # signifies the rest of the line is a comment
!!    o  adjacent double quotes put one double quote into value
!!    o  processing ends when an unquoted semi-colon or end of string is encountered
!!    o  the variable name for the first value is verb_init (often verb_oo)
!!    o  leading and trailing blanks are removed from values
!!    o  call it once to give defaults
!!    o  call it again and vars without values are set to null strings
!!
!!##OPTIONS
!!
!!    VERB     command name to process
!!    STRING   string is character input string with first verb removed (options + other commands)
!!    ALLOW    flag to allow or disallow new VERB_KEYWORD name being added. Should be
!!              NEW VARIABLES ARE ALLOWED
!!               o 'define'  -  add or replace a new VERB_KEYWORD entry and value
!!               o 'add'     -  add or append to a new VERB_KEYWORD entry and value
!!              NO NEW VARIABLES ARE ALLOWED
!!               o 'append' or 'no_add' - append to an *EXISTING* entry value
!!               o 'replace' - replace an *EXISTING* entry
!!
!!             That is, ff 'add' or 'append' and the value is not blank
!!             it will be APPENDED to the current value. If 'define' or
!!             'replace' it will replace the value instead of appending
!!             to it.
!!##RETURNS
!!    ERROR_RETURN  error code. If zero, no error occurred
!!
!!##EXAMPLE
!!
!!   sample program:
!!
!!    program demo_parse
!!    use M_kracken, only : parse, sget, iget, rget
!!    use M_strings, only : chomp
!!    implicit none
!!    character(len=:),allocatable  :: verb
!!    character(len=*),parameter    :: delimiters=' ;,'
!!    integer     :: i
!!    integer     :: ierr
!!    character(len=132) :: line
!!    character(len=132), parameter :: commands(5)= [character(len=132) :: &
!!      'start -i 10 -message this is a message', &
!!      'end -i 20 -j 30 -k 55.55 ', &
!!      'list', &
!!      'help -oo', &
!!      'end -i 44.44 ']
!!      do i=1,size(commands)
!!         line=commands(i) ! need mutable line
!!         if(chomp(line,verb,delimiters).ge. 0)then
!!            call parse(verb,line,'add',ierr)
!!            write(*,*)'do whatever a '//verb//' command does'
!!            select case(verb)
!!            case('start')
!!               write(*,*)trim(sget('start_i'))
!!               write(*,*)trim(sget('start_message'))
!!            case('end')
!!               write(*,*)iget('end_i')
!!               write(*,*)iget('end_j')
!!               write(*,*)rget('end_k')
!!            case('list')
!!               write(*,*)'list things'
!!            case('help')
!!               write(*,*)'show help text'
!!            endselect
!!         endif
!!      enddo
!!      ! look at some of the values as strings or numbers
!!      write(*,*)trim(sget('start_i'))
!!      write(*,*)trim(sget('start_message'))
!!      write(*,*)iget('end_i')
!!      write(*,*)iget('end_j')
!!      write(*,*)rget('end_k')
!!    end program demo_parse
!!
!!   Results:
!!
!!     do whatever a start command does
!!     10
!!     this is a message
!!     do whatever a end command does
!!              20
!!              30
!!       55.5499992
!!     do whatever a list command does
!!     list things
!!     do whatever a help command does
!!     show help text
!!     do whatever a end command does
!!              44
!!              30
!!       55.5499992
!!     10
!!     this is a message
!!              44
!!              30
!!       55.5499992
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
recursive subroutine parse(verb,string,allow,error_return)

! ident_16="@(#)M_kracken::parse(3f): parse user command and store tokens into Language Dictionary"

!!!   set up odd for future expansion
!!!   need to handle a minus followed by a blank character
!-----------------------------------------------------------------------------------------------------------------------------------
!     SPECIAL FORM:
!        VERB="MODE"
!           then STRING is a special keyword used to identify a special mode to set
!           and ALLOW is a value used to set the mode
!-----------------------------------------------------------------------------------------------------------------------------------
!     for left-over command string for Language routines
!     optionally needed if you are going to allow multiple commands on a line
      ! number of characters left over,
      ! number of non-blank characters in actual parameter list
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)     ::  verb   ! command name to process
character(len=*),intent(in)     ::  string ! string is character input string with first verb removed (options + other commands)
character(len=*),intent(in)     ::  allow  ! keyword indicating whether commands may be added or only replaced
integer,optional,intent(out)    ::  error_return
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=:),allocatable         ::  dummy  ! working copy of string
character(len=IPvalue),dimension(2)  ::  var
character(len=3)                     ::  delmt
character(len=2)                     ::  init
character(len=1)                     ::  currnt  ! current character being processed
character(len=1)                     ::  prev    ! character to left of CURRNT
character(len=1)                     ::  forwrd  ! character to right of CURRNT
character(len=IPvalue)               ::  val
character(len=IPverb)                ::  name
integer,dimension(2)                 ::  ipnt
integer,save                         ::  ileave=1 ! if 0, leave " where you find them; else if 1 remove them. Normally removed
integer                              ::  ilist
integer                              ::  ier
integer                              ::  islen
integer                              ::  ipln
integer                              ::  ipoint
integer                              ::  itype
integer                              ::  ifwd
integer                              ::  ibegin
integer                              ::  iend
!-----------------------------------------------------------------------------------------------------------------------------------
   if(.not.allocated(dict_verbs)) call initd()
!-----------------------------------------------------------------------------------------------------------------------------------
   if(debug) call journal('sc','PARSE ',trim(verb)//'::'//trim(string)//'::'//trim(allow))
!-----------------------------------------------------------------------------------------------------------------------------------
   leftover=" "
   current_command_length=0
   ilist=1
   init="oo"
   ier=0
   if(present(error_return)) error_return=0
   islen=len_trim(string)                               ! find number of characters in input string
   if(islen  ==  0)then                                 ! if input string is blank, even default variable will not be changed
      return
   endif
   dummy=string//'  '
   ipln=len_trim(verb)             ! find number of characters in verb prefix string
   if(size(dict_verbs).ne.0)then
      dict_calls=0                 ! clear number of times this keyword stored on a call to parse
                                   ! should more efficiently only do this for current VERB instead of entire array in dictionary
   endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   if(subscript_(trim(verb)//'_?') .le. 0 )then         ! assuming if adding this is initial call
      call store(trim(verb)//'_?','.false.','add',ier)  ! all commands have the option -? to invoke prompt mode
   elseif(allow.eq.'add')then
      call store(trim(verb)//'_?','.false.','add',ier)  ! all commands have the option -? to invoke prompt mode
   endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   if(subscript_(trim(verb)//'_>') .le. 0 )then         ! assuming if adding this is initial call
      call store(trim(verb)//'_>','#N#','add',ier)      ! all commands have the option -> to write journal(3f) output
   elseif(allow.eq.'add')then
      call store(trim(verb)//'_>','#N#','add',ier)      ! all commands have the option -> to write journal(3f) output
   endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
!  Process special mode-setting calls
   if(verb(:ipln)=="MODE")then
      if(string=="LEAVEQUOTES")then
         if(allow=="YES")then
            ileave=0
         elseif(allow=="NO")then
            ileave=1
         else
            call journal("*parse* LEAVEQUOTES value bad")
            ileave=1
         endif
      else
         call journal("*parse* UNKNOWN MODE")
      endif
      return
   endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   var(2)=init         ! initial variable name
   var(1)=" "          ! initial value of a string
   ipoint=0            ! ipoint is the current character pointer for (dummy)
   ipnt(2)=2           ! pointer to position in parameter name
   ipnt(1)=1           ! pointer to position in parameter value
   itype=1             ! itype=1 for value, itype=2 for variable
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   delmt="off"
   prev=" "
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   do
      ipoint=ipoint+1               ! move current character pointer forward
      currnt=dummy(ipoint:ipoint)   ! store current character into currnt
      ifwd=min(ipoint+1,islen)
      forwrd=dummy(ifwd:ifwd)       ! next character (or duplicate if last)
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      if((currnt=="-".and.prev==" ".and.delmt == "off".and.index("0123456789.",forwrd) == 0).or.ipoint > islen)then
      ! beginning of a parameter name
         if(forwrd.eq.'-')then                      ! change --var to -var so "long" syntax is supported
            dummy(ifwd:ifwd)='_'
            ipoint=ipoint+1                         ! ignore second - instead
         endif
         if(ipnt(1)-1 >= 1)then
            ibegin=1
            iend=len_trim(var(1)(:ipnt(1)-1))
            do
               if(iend  ==  0)then                  ! len_trim returned 0, parameter value is blank
                  iend=ibegin
                  exit
               elseif(var(1)(ibegin:ibegin) == " ")then
                  ibegin=ibegin+1
               else
                  exit
               endif
            enddo
            name=verb(:ipln)//"_"//var(2)(:ipnt(2))
            ! #A# means append
            ! #R# means retain previous value if any
            val=var(1)(ibegin:iend)
            if(val.eq.'"#R#"')then               ! special value saying to retain previous value so commands can remember last value
               if(subscript_(name).le.0)then
                  call store(name,' ',allow,ier) ! store name and blank value
               endif
            else
               call store(name,val,allow,ier)    ! store name and its value
            endif
            if(present(error_return).and.ier.ne.0)error_return=ier
         else
            name=verb(:ipln)//"_"//var(2)(:ipnt(2))
            val=" "                                 ! store name and null value
            call store(name,val,allow,ier)
            if(present(error_return).and.ier.ne.0)error_return=ier
         endif
         ilist=ilist+ipln+1+ipnt(2)
         ilist=ilist+1
         itype=2                          ! change to filling a variable name
         var(1)=" "                       ! clear value for this variable
         var(2)=" "                       ! clear variable name
         ipnt(1)=1                        ! restart variable value
         ipnt(2)=1                        ! restart variable name
         if(currnt.ne.' ')current_command_length=ipoint
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      elseif(currnt == kracken_comment .and. delmt == "off")then   ! rest of line is comment
         islen=ipoint
         dummy(:)=" "
         prev=" "
         leftover=" "
         cycle
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      elseif(currnt.eq.';'.and.delmt.eq.'off')then ! rest of line is another command(s)
         if(islen-ipoint.gt.0)then
            leftover=dummy(ipoint+1:)
         else
            leftover=' '
         endif

         islen=ipoint
         dummy(:)=" "
         prev=" "
         cycle
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      else       ! currnt is not one of the special characters
         ! the space after a keyword before the value
         if(currnt == " ".and.itype  ==  2)then
            ! switch from building a keyword string to building a value string
            itype=1
         ! beginning of a delimited parameter value
         elseif(currnt  ==  """".and.itype  ==  1)then
            ! second of a double quote, put quote in
            if(prev  ==  """")then
                var(itype)(ipnt(itype):ipnt(itype))=currnt
                ipnt(itype)=ipnt(itype)+1
                current_command_length=ipoint
                delmt="on"
            elseif(delmt  ==  "on")then     ! first quote of a delimited string
                delmt="off"
            else
                delmt="on"
            endif
            if(ileave  ==  0.and.prev /= """")then  ! leave quotes where found them
               var(itype)(ipnt(itype):ipnt(itype))=currnt
               ipnt(itype)=ipnt(itype)+1
               current_command_length=ipoint
            endif
         else     ! add character to current parameter name or parameter value
            var(itype)(ipnt(itype):ipnt(itype))=currnt
            ipnt(itype)=ipnt(itype)+1
            if(currnt /= " ")then
               current_command_length=ipoint
            endif
         endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      prev=currnt
      if(ipoint <= islen)then
         cycle
      endif
      exit
   enddo
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   if(lget(trim(verb)//'_?'))then    ! if -? option was present prompt for values
      call menu(verb)
   endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
end subroutine parse
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    store(3f) - [ARGUMENTS:M_kracken] add or replace value for specified name in dictionary(if allow='add' add name if needed)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine store(name1,value1,allow1,ier)
!!
!!    character(len=*),intent(in) :: name1
!!    class(*),intent(in)         :: value1
!!    character(len=*),intent(in) :: allow1
!!    integer,intent(out)         :: ier
!!
!!##DESCRIPTION
!!    Normally a command string and the associated values are placed in
!!    the dictionary by a call to KRACKEN(3f) when parsing a command
!!    line, or DISSECT(3f) and PARSE(3f) when creating input file
!!    interpreters. Rarely there may be a need to place
!!    <NAME,VALUE> pairs directly into the command dictionary, so this
!!    routine is public in the M_kracken(3fm) module. However,
!!    *this routine is primarily assumed to be an internal procedure*.
!!
!!##OPTIONS
!!    NAME1    name in dictionary of form VERB_KEYWORD
!!    VALUE1   value to be associated to NAME1. Value may be of type INTEGER,
!!             REAL, DOUBLEPRECISION, LOGICAL or CHARACTER.
!!    ALLOW1   flag to allow new VERB_KEYWORD name being added. Should be
!!              'define'  add or replace a new VERB_KEYWORD entry and value
!!              'add'     add or append to a new VERB_KEYWORD entry and value
!!              'no_add' or 'append'  append to an *EXISTING* entry value
!!              'replace'             replace an *EXISTING* entry
!!
!!             If 'add' or 'append' and the value is not blank it will
!!             be APPENDED to the current value. If 'define' or 'replace'
!!             it will replace the value instead of appending to it.
!!
!!##RETURNS
!!    IER      flag if error occurs in adding or setting value
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_store
!!    use M_kracken, only : store, show
!!    implicit none
!!    integer :: ier
!!    ! The following should be equivalent to
!!    ! call kracken('MY',' &
!!    ! & -STRING My string value &
!!    ! & -INTEGER 1234 &
!!    ! & -INTEGER 0987654321 &
!!    ! & -REAL 1234.5678 &
!!    ! & -DOUBLE 123.4567d8 &
!!    ! & -LOGICAL T &
!!    ! & '
!!    call store('MY_STRING','My string value','add',ier)
!!    if(ier.ne.0)write(*,*)'ERROR: could not store MY_STRING ier=',ier
!!    ! now the verb MY is defined with the option -STRING so the
!!    ! dictionary has MY_STRING='My string value' defined
!!
!!    ! this will be an error because MY does not have the -INTEGER
!!    ! keyword defined
!!    call store('MY_INTEGER',12345678,'no_add',ier)
!!
!!    ! now define MY_INTEGER
!!    call store('MY_INTEGER',1234,'add',ier)
!!    ! if 'no_add' it will APPEND to current string
!!    call store('MY_INTEGER',987654321,'add',ier)
!!
!!    call store('MY_REAL',1234.5678,'add',ier)
!!    call store('MY_DOUBLE',123.4567d8,'add',ier)
!!    call store('MY_LOGICAL',.true.,'add',ier)
!!
!!    call show('MY',.false.,0)
!!    write(*,*)repeat('=',76)
!!
!!    ! if 'replace' is used REPLACE instead of APPEND to current value
!!    call store('MY_INTEGER',987654321,'replace',ier)
!!    call show('MY',.false.,0)
!!    write(*,*)repeat('=',76)
!!
!!    ! 'replace' can only replace an existing entry, not add one
!!    call store('MY_UNKNOWN',987654321,'replace',ier)
!!    call show('MY',.false.,0)
!!    write(*,*)repeat('=',76)
!!
!!    end program demo_store
!!
!!   Results:
!!
!!    >########################################################
!!    >error: UNKNOWN OPTION -INTEGER
!!    >MY parameters are
!!    > -STRING My string value
!!    >########################################################
!!    > MY_STRING            = My string value
!!    > MY_REAL              = 1234.5677
!!    > MY_LOGICAL           = T
!!    > MY_INTEGER           = 1234 987654321
!!    > MY_DOUBLE            = 12345670000.000000
!!    > =======================================================================
!!    > MY_STRING            = My string value
!!    > MY_REAL              = 1234.5677
!!    > MY_LOGICAL           = T
!!    > MY_INTEGER           = 987654321
!!    > MY_DOUBLE            = 12345670000.000000
!!    > =======================================================================
!!    >########################################################
!!    >error: UNKNOWN OPTION -UNKNOWN
!!    >MY parameters are
!!    > -STRING My string value
!!    > -REAL 1234.5677
!!    > -LOGICAL T
!!    > -INTEGER 987654321
!!    > -DOUBLE 12345670000.000000
!!    >########################################################
!!    > MY_STRING            = My string value
!!    > MY_REAL              = 1234.5677
!!    > MY_LOGICAL           = T
!!    > MY_INTEGER           = 987654321
!!    > MY_DOUBLE            = 12345670000.000000
!!    > =======================================================================
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine store(name1,value1,allow1,ier)

! ident_17="@(#)M_kracken::store(3f): replace or add dictionary entry name  and value (if allow='add' add name if necessary)"

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)        :: name1       ! name in dictionary of form VERB_KEYWORD
class(*),intent(in)                :: value1      ! value to be associated to NAME1
character(len=*),intent(in)        :: allow1      ! flag to allow new VERB_KEYWORD name being added
integer,intent(out)                :: ier         ! flag if error occurs in adding or setting value
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=:),allocatable       :: l_value1    ! value to be associated to NAME1
integer                            :: ilen
character(len=IPverb)              :: name
integer                            :: indx
character(len=10)                  :: allow
character(len=IPvalue)             :: value
character(len=IPvalue)             :: mssge       ! the  message/error/string  value
integer                            :: nlen
integer                            :: new
integer                            :: ii
integer                            :: i10
integer                            :: inew
!-----------------------------------------------------------------------------------------------------------------------------------
   select type(value1)                         ! convert non-character values to character string
   type is(integer)
      allocate(character(len=30):: l_value1)
      write(l_value1,'(i0)')value1
   type is(logical)
      l_value1=merge('T','F',value1)
   type is(real)
      allocate(character(len=30):: l_value1)
      write(l_value1,'(g0.8)')value1
   type is(doubleprecision)
      allocate(character(len=30):: l_value1)
      write(l_value1,'(g0)')value1
   type is(character(len=*))
      l_value1=value1
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   if(debug) write(*,*)'STORE ',trim(name1)//'::'//trim(l_value1)//'::'//trim(allow1)
!-----------------------------------------------------------------------------------------------------------------------------------
   value=" "
   name=" "                                          ! compiler bug. KLUDGE
   allow=" "
   name=name1                                        ! store into a standard size variable for this type

   ii=index(name,"_")                                ! -- is an alias for -oo
   ii=min(ii,IPverb-2)
   if(name(ii+1:).eq.' ')then
      name(ii+1:)='oo'
   endif

   value=l_value1                                    ! store into a standard size variable for this type
   allow=allow1                                      ! store into a standard size variable for this type
   nlen=len(name1)
!-----------------------------------------------------------------------------------------------------------------------------------
   if(.not.allocated(dict_verbs)) call initd()
   call locate(dict_verbs,name,indx,ier,mssge)       ! determine storage placement of the variable and whether it is new
   if(ier  ==  -1)then                               ! an error occurred in determining the storage location
      call journal("error occurred in *store*")
      call journal(mssge)
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(indx > 0)then                                  ! found the variable name
      new=1
   elseif(indx <= 0.and.(allow  ==  'add'.or. allow == 'define'))then        ! check if the name needs added and allow to add
      inew=iabs(indx)                                ! adding the new variable name in the variable name array
      call insert(dict_verbs,name,inew)              ! pull down the dictionary arrays to make room for new value
      call insert(dict_vals," ",inew)
      call insert(dict_calls,0,inew)
      call insert(dict_lens,0,inew)

      if(ier  ==  -1)then
         call journal("*store* could not add "//name(:nlen))
         call journal(mssge)
         return
      endif
      new=0
!-----------------------------------------------------------------------------------------------------------------------------------
   else                                              ! did not find variable name but not allowed to add it
      ii=index(name,"_")
      call journal("########################################################")
      call journal("*store* error: UNKNOWN OPTION -"//name(ii+1:))
      if(ii > 0)then
         call journal(name(:ii-1)//" parameters are")
         do i10=1,size(dict_verbs)
            if(name(:ii)  ==  dict_verbs(i10)(:ii))then
               if(dict_verbs(i10)(ii:ii+1).eq.'__')then
                  call journal(" --"//dict_verbs(i10)(ii+2:len_trim(dict_verbs(i10)))//" "//dict_vals(i10)(:dict_lens(i10)))
               else
                  call journal(" -"//dict_verbs(i10)(ii+1:len_trim(dict_verbs(i10)))//" "//dict_vals(i10)(:dict_lens(i10)))
               endif
            endif
         enddo
      endif
      call journal("########################################################")
      ier=-10
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ! ignore special value that means leave alone, used by 'set up' calls to leave a value alone
   ! note that this will prevent the keyword from being defined.
   indx=iabs(indx)  ! entry existed or was added
   if(indx.eq.0)then
      write(*,*)'*store* error: INDEX=0'
   elseif(value(1:4)  ==  "@LV@")then
      ! a leave-alone flag (for use by a 'defining' call)
      if(new  ==  0) then
         value=value(5:)                                                  ! trim off the leading @LV@
         if(dict_calls(INDX).eq.0.or.dict_vals(INDX).eq.' ')then
            call replace(dict_vals,value,INDX)
         else
            if(allow.eq.'define')then
               call replace(dict_vals,value,INDX)                             ! set a defined variable's value
            else
               call replace(dict_vals,trim(dict_vals(INDX))//' '//value,INDX) ! append a defined variable's value
            endif
         endif
         dict_lens(INDX)= len_trim(dict_vals(INDX))                       ! store length of string
         dict_calls(INDX)=dict_calls(INDX)+1                              ! detect duplicate use of a keyword
      endif
   else
      if(dict_calls(INDX).eq.0.or.dict_vals(INDX).eq.' ')then             ! if first time given a value or value blank
         call replace(dict_vals,value,indx)                               ! store a defined variable's value
      elseif(allow.eq.'replace'.or.allow.eq.'define')then
         call replace(dict_vals,value,indx)                               ! set a defined variable's value
      else
         call replace(dict_vals,trim(dict_vals(INDX))//' '//value,indx)   ! set a defined variable's value
      endif
      dict_lens(INDX)= len_trim(dict_vals(INDX))                          ! store length of string
      dict_calls(INDX)=dict_calls(INDX)+1                                 ! detect duplicate use of a keyword
   endif
   !---------------------------------------------------!
   !()()()()()()()()()()-                              !
   !---------------------                              !
   ! assume suffix _> is used to open file for journal()   !
   ! special-purpose just for USH.                     !
   ilen=len_trim(name)                                 !
      if(ilen.ge.2)then                                !
         if(name(ilen-1:ilen).eq.'_>')then             !
         if(value.ne."#N#")then                        !
            call journal('N',value)                    !
         endif                                         !
      endif                                            !
   endif                                               !
   !---------------------                              !
   !()()()()()()()()()()-                              !
   !---------------------------------------------------!
!-----------------------------------------------------------------------------------------------------------------------------------
   if(debug) write(*,*)'STORE END ',trim(name1)//'::'//trim(l_value1)//'::'//trim(allow1)
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine store
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    subscript_(3fp) - [ARGUMENTS:M_kracken] return the subscript value of a string when given its name
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function subscript_(chars0)
!!
!!    character(len=*),intent(in) :: chars0
!!
!!##DESCRIPTION
!!
!!##OPTIONS
!!
!!##RETURNS
!!
!!##EXAMPLE
!!
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function subscript_(chars0)

! ident_18="@(#)M_kracken::subscript_(3fp): return the subscript value of a string when given its name"

!  WARNING: only request value of names known to exist
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=*),intent(in)        :: chars0
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=IPverb)              :: chars
   character(len=IPvalue)             :: mssge
   integer                            :: ierr
   integer                            :: indx
   integer                            :: subscript_
!-----------------------------------------------------------------------------------------------------------------------------------
   chars=chars0
   indx=0
   ierr=0
   call locate(dict_verbs,chars,indx,ierr,mssge)                        ! look up position
!-----------------------------------------------------------------------------------------------------------------------------------
   if((ierr  ==  -1).or.(indx <= 0))then
      !call journal("*subscript_* variable "//trim(chars)//" undefined")
      subscript_=-1                                                       ! very unfriendly subscript value
   else
      subscript_=indx
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function subscript_
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    menu(3fp) - [ARGUMENTS:M_kracken] prompt for values using a menu interface
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine menu(verb)
!!
!!    character(len=*),intent(in)  :: verb
!!
!!##DESCRIPTION
!!
!!##OPTIONS
!!
!!##RETURNS
!!
!!##EXAMPLE
!!
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine menu(verb)

! ident_19="@(#)M_kracken::menu(3fp): prompt for values using a menu interface"

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: verb
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=IPvalue)    :: reply
   character(len=IPvalue)    :: prompt
   integer                   :: ii
   integer                   :: icount
   integer                   :: ios
   integer                   :: i10
   integer                   :: i20
   integer                   :: istart
   integer                   :: iend
   integer                   :: iend_OK   ! last open actually printed
   integer                   :: ifound
   integer                   :: ireply
   integer                   :: ivalu
   integer                   :: ierr
   integer                   :: indx
   character(len=IPvalue)    :: mssge     ! the message/error/string  value returned by BOUNCE(3f)
   character(len=1)          :: prefix
   integer                   :: icurrent  ! current menu item
   integer                   :: icmd
   integer                   :: imenu
   character(len=80),allocatable :: help_text(:)
   integer                       :: i
   integer                       :: cstat
   character(len=256)            :: sstat
!-----------------------------------------------------------------------------------------------------------------------------------
   stop_command=.false.
   ii=len_trim(verb)
   call journal(verb(:ii)//" parameters are")
   istart=1
   icurrent=1
!-----------------------------------------------------------------------------------------------------------------------------------
   if(.not.allocated(dict_verbs)) call initd()
   INFINITE: do
      icount=0                                                ! how many entries in the dictionary belong to this command
      iend=size(dict_verbs)                                   ! last dictionary entry to search for current command
      iend_OK=istart
      MAKEMENU: do i10=istart,iend                            ! search dictionary for keywords for current command
         if(verb(:ii)//'_'  ==  dict_verbs(i10)(:ii+1))then   ! found part of the desired command
            if(istart.eq.0)then
               istart=i10                                     ! store index to the beginning of this command
               icurrent=i10
            endif
            icount=icount+1                                   ! count keywords that start with VERB_
            if(dict_verbs(i10).eq.verb(:ii)//'_?')then        ! do not show the keyword VERB_?
               cycle MAKEMENU
            endif
            call locate(dict_verbs,'?'//dict_verbs(i10),indx,ierr,mssge) ! if ?VERB is defined assume it is a prompt
            if(indx.gt.0)then
               prompt=dict_vals(indx)
            else
               prompt=' '
            endif
            if(i10.eq.icurrent)then
              prefix='>'
            else
              prefix=' '
            endif
            imenu=i10-istart+1
            if(prompt.eq.'')then
               write(*,'(a,i4,")",a,a)') prefix,imenu,dict_verbs(i10)(ii+2:),trim(dict_vals(i10)(:dict_lens(i10)))
               iend_OK=i10
            elseif(prompt.eq.'#N#'.or.prompt.eq.'"#N#"')then                 ! special prompt value which means to skip prompting
            else
               write(*,'(a,i4,")",a,":[",a,"]")') prefix,imenu,trim(prompt),trim(dict_vals(i10))
               iend_OK=i10
            endif
         endif
      enddo MAKEMENU
      iend=icount+istart-1                                 ! no need to go thru entire dictionary on subsequent passes
!-----------------------------------------------------------------------------------------------------------------------------------
      write(*,'(a)',advance='no')'Enter parameter number to change("RETURN" to finish):'
      read(*,'(a)',iostat=ios)reply
      if(ios.ne.0)then
         reply=' '
      else
         reply=adjustl(reply)
      endif
      ivalu=-1
!-----------------------------------------------------------------------------------------------------------------------------------
      select case(REPLY(1:1))
!-----------------------------------------------------------------------------------------------------------------------------------
      case('-')  ! if it starts with a - assume it is a new specification of the arguments
         call parse(verb,trim(reply)//' -? .false.',"no_add")
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('@')                                        ! debug option to dump dictionary
         do i20=1,size(dict_verbs)
            if(dict_verbs(i20).ne.' ')then
                 write(*,*)i20,trim(dict_verbs(i20)),trim(dict_vals(i20)(:dict_lens(i20)))
            endif
         enddo
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('#')                                                                  ! ignore
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case(' ','q','e','0')                                                      ! exit menu changes
         exit INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('x','.')                                                              ! return value to indicate command has been stopped
         stop_command=.true.
         exit INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('!')                                                                      ! call system
         call execute_command_line(trim(reply(2:)), exitstat=icmd,cmdstat=cstat,cmdmsg=sstat)   ! execute system command
         if(icmd.ne.0)then                                                           ! if system command failed exit program
            call journal('sc','*M_kracken:menu* ERROR - SYSTEM COMMAND FAILED:',icmd)
         endif
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('?','h','i')                                                              ! get help information
      help_text=[ CHARACTER(LEN=80) ::                                                       &
!        &'12345678901234567890123456789012345678901234567890123456789012345678901234567890',&
         &'#------------------------------------------------------------------------------#',&
         &'| How to change parameter options by number:                                   |',&
         &'|  o  NNN              the number of the menu option to change the value of    |',&
         &'#------------------------------------------------------------------------------#',&
         &'| How to change parameter options by respecifying them:                        |',&
         &'|  o  -key1 value1 -key2 value2 ...                                            |',&
         &'|                      to respecify values using original specification style  |',&
         &'#------------------------------------------------------------------------------#',&
         &'# Working on the current keyword(identified by a ">" prefix in the menu):      |',&
         &'|  o  c                change current option value with command-line editor    |',&
         &'|  o  n                change "current" to next menu option                    |',&
         &'|  o  p                change "current" to previous menu option                |',&
!        &'|  o  /name            change "current" option to keyword "name"               |',&
         &'#------------------------------------------------------------------------------#',&
         &'| Exit menu mode:                                                              |',&
         &'|  o   |q|e|0          a RETURN on a blank line or the indicated characters    |',&
         &'|                      exits the menu and processes the command                |',&
         &'|  o  .                indicate to program to ignore command (may be ignored ) |',&
         &'#------------------------------------------------------------------------------#',&
         &'| Special functions:                                                           |',&
         &'|  o  !command         execute system command                                  |',&
         &'|  o  ?|i|h            display this help                                       |',&
         &'#------------------------------------------------------------------------------#']
         WRITE(*,'(a)')(help_text(i),i=1,size(help_text))
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('1','2','3','4','5','6','7','8','9')
         ivalu=nint(s2v(reply))
         ivalu=ivalu+istart-1
!-----------------------------------------------------------------------------------------------------------------------------------
      case('c')                                            ! change current menu item
         ivalu=icurrent
!-----------------------------------------------------------------------------------------------------------------------------------
      case('d')                                            ! turn on debug mode
         debug=.true.
         call journal('*menu* DEBUG: debug mode on')
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('D')                                            ! turn off debug mode
         debug=.false.
         call journal('*menu* DEBUG: debug mode off')
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('p')                                            ! change previous menu item
         icurrent=icurrent-1
         icurrent=max(icurrent,istart)
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('v')                                            ! show version
        !call journal('Version 20140403')
        !call journal('Version 20151229')
        !call journal('Version 20160414')
         call journal('Version 20191018')
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('n')                                            ! change next menu item
         icurrent=icurrent+1
         if(debug)then
             call journal('sc','*menu* DEBUG: ICURRENT=',icurrent)
             call journal('sc','*menu* DEBUG: ISTART=',istart)
             call journal('sc','*menu* DEBUG: IEND=',iend)
             call journal('sc','*menu* DEBUG: IEND_OK=',iend_ok)
         endif
         icurrent=min(icurrent,iend_OK)
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case default
         call journal(' Unrecognized selection (? for help)')
         cycle INFINITE
      end select
!-----------------------------------------------------------------------------------------------------------------------------------
      ireply=ivalu
!-----------------------------------------------------------------------------------------------------------------------------------
      if((ivalu.lt.istart).or.(ivalu.gt.iend))then
         write(*,*)'illegal menu choice ',istart,'<=',ivalu,'<=',iend, ' (enter "?" for help)'
!-----------------------------------------------------------------------------------------------------------------------------------
      else
         ifound=ireply                                                    ! index into dictionary for requested keyword and value
         if(dict_verbs(ifound).eq.verb(:ii)//'_?')then                    ! replaced this with FINISHED so exit
            exit INFINITE
         endif
         call locate(dict_verbs,'?'//dict_verbs(ifound),indx,ierr,mssge) ! if ?VERB is defined assume it is a prompt
         if(indx.gt.0)then
            prompt=dict_vals(indx)
         else
            prompt=' '
         endif
         if(prompt.eq.'')then
            write(*,'("Enter value for ",a,":")',advance='no') trim(dict_verbs(ifound)(ii+2:))
         elseif(prompt.eq.'#N#'.or.prompt.eq.'"#N#"')then                 ! special prompt value
         else
            write(*,'(a,":")',advance='no') trim(prompt)
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
         read(*,'(a)',iostat=ios)reply
         call store(dict_verbs(ifound),reply,"no_add",ierr)
!-----------------------------------------------------------------------------------------------------------------------------------
      endif
   enddo INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
   call store(trim(verb)//'_?','.false.','add',ierr)                      ! all commands have the option -? to invoke prompt mode
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine menu
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    show(3f) - [ARGUMENTS:M_kracken] dump dictionary entries
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine show(VERB_NAME0,VERBS_ONLY,IWIDE)
!!
!!    character(len=*),intent(in)   :: VERB_NAME0
!!    logical,intent(in)            :: VERBS_ONLY
!!    integer,intent(in)            :: iwide
!!
!!##DESCRIPTION
!!    Write information about a command from the command dictionary or list all the
!!    command verbs in the dictionary
!!
!!##OPTIONS
!!    VERB_NAME0   verb prefix to display. Default is all
!!    VERBS_ONLY   flag to show verbs only
!!    IWIDE        if .ge. zero, how many columns wide to show just verbs
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_show
!!     use M_kracken, only : kracken, show
!!     implicit none
!!
!!     call kracken('demo', ' default keyword -i 10 -j 20.20 -value my default string')
!!     call show('demo',.false.,0)
!!
!!     end program demo_show
!!   Results:
!!
!!     demo_value           = my default string
!!     demo_oo              = default keyword
!!     demo_j               = 20.20
!!     demo_i               = 10
!!     demo_?               = .false.
!!     demo_>               = #N#
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine show(VERB_NAME0,VERBS_ONLY,IWIDE)

! ident_20="@(#)M_kracken::show(3f): dump dictionary entries"

character(len=*),intent(in)   :: VERB_NAME0     ! verb prefix to display. Default is all
logical,intent(in)            :: VERBS_ONLY     ! flag to show verbs only
integer,intent(in)            :: iwide          ! if .ge. zero, how many columns wide to show just verbs
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=IPvalue)        :: VERB_NAME      ! verb prefix to display. Default is all
character(len=IPvalue)        :: message
integer                       :: i
integer                       :: j
integer                       :: ii
integer                       :: ich
integer                       :: istart
integer                       :: istep
integer                       :: iwide_local
integer                       :: verb_length
!-----------------------------------------------------------------------------------------------------------------------------------
   if(.not.allocated(dict_verbs)) call initd()
!-----------------------------------------------------------------------------------------------------------------------------------
   iwide_local=iwide
   if(iwide_local.le.0)iwide_local=80
   VERB_NAME=VERB_NAME0
!-----------------------------------------------------------------------------------------------------------------------------------
   if(VERBS_ONLY)then                                         ! show just verbs
      message=' '
      istart=1
      !!istep=len(DICT_VERBS)
      istep=1
      verb_length=1
      call journal('+c','')                                   ! start comment line
      do j=1,2                                                ! to make compact, make 1st pass to get length, 2nd pass to print
         do i=size(dict_verbs),1,-1                           ! loop thru entire dictionary
            if(DICT_VERBS(i)(1:1).eq.'?')cycle                ! remove prompts
            if(DICT_VERBS(i)(1:1).eq.'_')cycle                ! remove initial values
            verb_length=len_trim(DICT_VERBS(i))               ! find longest verb
            if(DICT_VERBS(i)(verb_length-2:).ne.'_oo') cycle  ! assume all commands have a VERB_oo value
            if(verb_length.lt.3)cycle                         ! looking for VERB_oo
            if(j.eq.1)then
               istep=max(istep,verb_length-3+1)
            elseif(istart+istep+1.gt.iwide_local)then
               call journal('ts','')                          ! end line
               call journal('+c','')                          ! start next comment line in trail so get pound character
               istart=1
               call journal('+st',adjustr(atleast(DICT_VERBS(i)(:verb_length-3),istep)))
               istart=istart+istep
            elseif(verb_length-3.gt.0.and.j.eq.2)then
               call journal('+st',adjustr(atleast(DICT_VERBS(i)(:verb_length-3),istep)))
               istart=istart+istep
            endif
         enddo
      enddo
      call journal('st','')
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(VERB_NAME.eq.' ')then                                   ! show all variables
      do i=1,size(dict_verbs)
         if(DICT_VERBS(i).ne.' ')then
            ii=max(1,dict_lens(i))                                 ! number of characters in corresponding dictionary VALUE
            call journal('sc',' ',atleast(DICT_VERBS(i),20)//'=',dict_vals(i))
         endif
      enddo
      call journal('sc',' dictionary size=',size(DICT_VERBS),'verb length=',len(DICT_VERBS),'value length=',len(DICT_VALS))
!-----------------------------------------------------------------------------------------------------------------------------------
   else                                                        ! show only verb_ variables
      ich=index(VERB_NAME,' ')                                 ! VERB_NAME assumed longer than any verb name, so at least one space
      VERB_NAME(ich:ich)='_'
      SCAN_DICTIONARY: do i=1,size(dict_verbs)
         if(DICT_VERBS(i).eq.' ')cycle SCAN_DICTIONARY
         if(VERB_NAME(:ich).eq.DICT_VERBS(i)(:ich))then
           ii=max(1,dict_lens(i))                          ! number of characters in corresponding dictionary VALUE
           call journal('+sc',' ',atleast(DICT_VERBS(i),20)//'=')
           call journal(dict_vals(i)(:ii))
         endif
      enddo SCAN_DICTIONARY
!-----------------------------------------------------------------------------------------------------------------------------------
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine show
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine initd()
   dict_verbs=[character(len=0) ::]  ! string variable names
   dict_vals=[character(len=0)  ::]  ! contains the values of string variables
   dict_calls=[integer ::]           ! number of times this keyword stored on a call to parse
   dict_lens=[integer ::]            ! significant lengths of string variable values
end subroutine initd
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine cmd_args_to_dictionary(verb)

! ident_21="@(#)M_kracken::cmd_args_to_dictionary(3f): convert command line arguments to dictionary entries using alternate style"

character(len=*),intent(in)  :: verb
integer                      :: pointer
character(len=:),allocatable :: lastkeyword
integer                      :: i
integer                      :: ilength, istatus, imax
integer                      :: ibig
character(len=:),allocatable :: current_argument
character(len=:),allocatable :: current_argument_padded
character(len=:),allocatable :: dummy
character(len=:),allocatable :: oldvalue
logical                      :: nomore
logical                      :: keyword_single
integer                      :: ierr
! revisit this. Assuming .false. and .true. can only occur as values for a logical switch is not valid but is a low risk.
! this could be particularly strange because .false. and .true. get converted to .true. to handle a duplicate logical switch
   if(allocated(unnamed))then
      deallocate(unnamed)
   endif
   ibig=longest_command_argument() ! bug in gfortran. len=0 should be fine
   allocate(character(len=ibig) ::unnamed(0))
   unnamed=[character(len=ibig) ::]            ! kludge

   nomore=.false.
   pointer=0
   lastkeyword=' '
   keyword_single=.true.
   GET_ARGS: do i=1, command_argument_count()                                                        ! insert and replace entries
      call get_command_argument(number=i,length=ilength,status=istatus)                              ! get next argument
      if(istatus /= 0) then                                                                          ! stop program on error
         call journal('sc','*prototype_and_cmd_args_to_nlist* error obtaining argument ',i,&
            &'status=',istatus,&
            &'length=',ilength)
         exit GET_ARGS
      else
         if(allocated(current_argument))deallocate(current_argument)
         ilength=max(ilength,1)
         allocate(character(len=ilength) :: current_argument)
         call get_command_argument(number=i,value=current_argument,length=ilength,status=istatus)    ! get next argument
         if(istatus /= 0) then                                                                       ! stop program on error
            call journal('sc','*prototype_and_cmd_args_to_nlist* error obtaining argument ',i,&
               &'status=',istatus,&
               &'length=',ilength,&
               &'target length=',len(current_argument))
            exit GET_ARGS
          endif
      endif

      if(current_argument.eq.'--')then ! everything after this goes into the unnamed array
         nomore=.true.
         pointer=0
         cycle
      endif
      dummy=current_argument//'   '
      current_argument_padded=current_argument//'   '
      if(.not.nomore.and.current_argument_padded(1:2).eq.'--'.and.index('0123456789.',dummy(3:3)).eq.0)then ! beginning of long word
         keyword_single=.false.
         if(lastkeyword.ne.'')then
            call ifnull()
         endif
         call locate(dict_verbs,verb//'_'//current_argument_padded(3:),pointer)
         if(pointer.le.0)then
            call journal('sc','*cmd_args_to_dictionary* UNKNOWN LONG KEYWORD: ',current_argument)
            call print_kracken_dictionary('OPTIONS:')
            stop 1
         endif
         lastkeyword=verb//'_'//trim(current_argument_padded(3:))
      elseif(.not.nomore .and. current_argument_padded(1:1).eq.'-' .and. index('0123456789.',dummy(2:2)).eq.0 .and. &
         & current_argument_padded.ne.'-')then  ! short word
         keyword_single=.true.
         if(lastkeyword.ne.'')then
            call ifnull()
         endif
         call locate(dict_verbs,verb//'_'//current_argument_padded(2:),pointer)
         if(pointer.le.0)then
            call journal('sc','*cmd_args_to_dictionary* UNKNOWN SHORT KEYWORD: ',current_argument)
            call print_kracken_dictionary('OPTIONS:')
            stop 2
         endif
         lastkeyword=verb//'_'//trim(current_argument_padded(2:))
      elseif(pointer.eq.0)then                                                                           ! unnamed arguments
         imax=max(len(unnamed),len(current_argument))
         !!write(*,*)'GOT HERE 4 UNNAMED:',current_argument,size(unnamed)
         unnamed=[character(len=imax) :: unnamed,current_argument]
      else
         if(debug)then
            call journal('sc','POINTER=',pointer,' KEYWORD=',dict_verbs(pointer),' VALUE=',current_argument,' LENGTH=',ilength)
         endif
         oldvalue=sget(dict_verbs(pointer))//'  '
         if(upper(oldvalue).eq.'.F'.or.upper(oldvalue).eq.'.T')then  ! assume boolean parameter
            if(current_argument.ne.' ')then
               imax=max(len(unnamed),len(current_argument))
               !!write(*,*)'GOT HERE 5',current_argument,size(unnamed)
               unnamed=[character(len=imax) :: unnamed,current_argument]
            endif
            current_argument='.true.'
         endif
         !!call journal('sc','GOT HERE D KEY=',dict_verbs(pointer),'VALUE=',current_argument,&
         !!   &'OLDVALUE=',oldvalue,'LASTKEYWORD=',lastkeyword)
         if(upper(oldvalue).eq.'.FALSE.'.or.upper(oldvalue).eq.'.TRUE.')then
            imax=max(len(unnamed),len(current_argument))
            unnamed=[character(len=imax) :: unnamed,current_argument]
            call store(dict_verbs(pointer),'.true.','replace',ierr)
         else
            call store(dict_verbs(pointer),current_argument,'replace',ierr)
         endif
         pointer=0
         lastkeyword=''
      endif
   enddo GET_ARGS
   if(lastkeyword.ne.'')then
      call ifnull()
   endif

contains
subroutine ifnull()
   oldvalue=sget(lastkeyword)//'  '
   if(upper(oldvalue).eq.'.F'.or.upper(oldvalue).eq.'.T')then
      !!call journal('sc','GOT HERE E','KEY=',dict_verbs(pointer),'VALUE ',oldvalue,'TO T',' LASTKEYWORD=',lastkeyword)
      call store(lastkeyword,'.true.','replace',ierr)
   else
      !!call journal('sc','GOT HERE F','KEY=',dict_verbs(pointer),'VALUE ',oldvalue,'TO BLANK',' LASTKEYWORD=',lastkeyword)
      if(upper(oldvalue).eq.'.FALSE.'.or.upper(oldvalue).eq.'.TRUE')then
         call store(lastkeyword,'.true.','replace',ierr)
      else
         call store(lastkeyword,' ','replace',ierr)
      endif
   endif
end subroutine ifnull

end subroutine cmd_args_to_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine print_kracken_dictionary(header)
character(len=*),intent(in) :: header
integer                     :: i
   if(allocated(dict_verbs))then
      if(size(dict_verbs).gt.0)then
         write(*,'(a,t21,1x,a5,a5,1x,a)')'OPTION','COUNT','LEN','VALUE'
         do i=1,size(dict_verbs)
            write(*,'(a,t21,i5,1x,i5,1x,"[",a,"]")') dict_verbs(i), dict_calls(i), dict_lens(i),trim(dict_vals(i))
         enddo
      endif
   endif
   if(allocated(unnamed))then
      if(size(unnamed).gt.0)then
         write(*,*)'UNNAMED:'
         do i=1,size(unnamed)
            write(*,'(i5.5,"[",a,"]")')i,unnamed(i)
         enddo
      endif
   endif
end subroutine print_kracken_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_kracken
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! HISTORY:
!-----------------------------------------------------------------------------------------------------------------------------------
! updated 20191018
! added 'kracken_method' and the 'args' method for users that prefer a more 1-like feel requiring quoted arguments on input
!-----------------------------------------------------------------------------------------------------------------------------------
! updated 20160414
! multiple uses of a keyword appends values together with a space in between rather than taking right-most definition
!-----------------------------------------------------------------------------------------------------------------------------------
! updated 20151228
! merged command-parsing module back into kracken. Makes kracken a little dirty and makes it require M_verify and M_journal
! but code is too similar in function to keep separate
!-----------------------------------------------------------------------------------------------------------------------------------
! updated 20151212
! allow cmd_oo to have default value
! requote token from the command line if it starts with - and has a space in it to make is possible to pass "-[^0-9-] values
!-----------------------------------------------------------------------------------------------------------------------------------
! updated 20131224
! minor cleanup
! updated 20131214
! added preliminary setprompts and menu as routines to explore prompting modes.
!-----------------------------------------------------------------------------------------------------------------------------------
! updated 20131206
! added optional error flag to KRACKEN(3f). If the error flag is not present, an error will cause the program to stop instead of
! always returning to the calling procedure.
!-----------------------------------------------------------------------------------------------------------------------------------
! updated 20131201
! create name CMD__NAME if --NAME is specified; so --version and --help are more easily used
! add dget(3f) function for returning doubleprecision values
! rename parse_two(3f) to dissect(3f) and make it public so input from sources other than
! command line arguments can be parsed easily.
!-----------------------------------------------------------------------------------------------------------------------------------
! updated 20131029
! read environment variable DEFAULT_CMD
! REMOVED
!-----------------------------------------------------------------------------------------------------------------------------------
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
