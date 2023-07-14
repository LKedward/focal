! -----------------------------------------------------------------------------
!  FOCAL
!
!   A modern Fortran abstraction layer for OpenCL
!   https://lkedward.github.io/focal-docs
!
! -----------------------------------------------------------------------------
!
! Copyright (c) 2020 Laurence Kedward
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
! -----------------------------------------------------------------------------

submodule (Focal) Focal_Utils
  !!  Implementation module for focal utility routines

  !! @note This is an implementation submodule: it contains the code implementing the subroutines defined in the
  !!  corresponding header module file. See header module file (Focal.f90) for interface definitions. @endnote

  implicit none

  contains


  module procedure fclGetKernelResource !(kernelString)
    use Focal, only: fclKernelStart, fclKernelEnd

    integer(c_intptr_t) :: a0, a1
    integer(c_intptr_t) :: i, length
    character(1), pointer :: text(:)

    type(c_ptr) :: aa

    aa = c_loc(fclKernelStart)

    a0 = transfer(c_loc(fclKernelStart),a0)
    a1 = transfer(c_loc(fclKernelEnd),a1)
    length = a1 - a0

    call c_f_pointer(aa,text,shape=[length])

    allocate(character(len=length) :: kernelString)
    do i=1,length
      kernelString(i:i) = text(i)
    end do

  end procedure fclGetKernelResource
  ! -----------------------------------------------------------------------------


  module procedure strStripNum
    !! Return copy of string with numerical characters removed

    integer :: i, n, ic, iOut

    n = len_trim(linei)

    strStripNum = ' '
    iOut = 1
    do i=1,n

      ic = ichar(linei(i:i))

      if (.not.(ic > 47 .and. ic < 58)) then   ! ASCII numbers are 48 to 57 inclusive
        strStripNum(iOut:iOut) = linei(i:i)
        iOut = iOut + 1
      end if

    end do

  end procedure strStripNum
  ! -----------------------------------------------------------------------------


  module procedure fclSourceFromFile !(filename,sourceString)
    !! Allocae and fill character string from file

    integer :: fh, iLen, ioStat, i
    character(1) :: char

    ! --- First pass: get kernel source length ---
    open(newunit=fh,file=filename,status='old', form='formatted', &
                   access='direct',recl=1)
    iLen = 1
    iostat = 0
    do while(iostat == 0)
      read(fh,'(A)',rec=iLen,iostat=iostat) char
      iLen = iLen + 1
    enddo
    iLen = iLen - 2
    close(fh)

    allocate(character(len=iLen) :: sourceString)

    ! --- Second pass: read kernel source into buffer ---
    open(newunit=fh,file=filename,status='old', form='formatted', &
                  access='direct',recl=1)
    do i=1,iLen
        read(fh,'(A)',rec=i) char
        sourceString(i:i) = char
    end do
    close(fh)

  end procedure fclSourceFromFile
  ! -----------------------------------------------------------------------------


  !> Convert string to uppercase
  !>  (For case-insensitive comparison)
  module procedure upperStr !(str) result (string)

    integer :: i
    integer,parameter :: diff = iachar('A')-iachar('a')

    string = str

    do i = 1,len_trim(str)
      select case (str(i:i))
      case ('a':'z')
        string(i:i) = achar(iachar(str(i:i))+diff)
      end select
    enddo

  end procedure upperStr
  ! -----------------------------------------------------------------------------


  module procedure str_noesc !(INSTR)
    integer                     :: ic,i10

    str_noesc=''                               ! initialize output string
    do i10=1,len_trim(INSTR(1:len(INSTR)))
        ic=ichar(INSTR(i10:i10))
        if(ic.le.31.or.ic.eq.127)then       ! find characters with ADE of 0-31, 127
          str_noesc(I10:I10)=' '               ! replace non-printable characters with a space
        else
          str_noesc(I10:I10)=INSTR(i10:i10)    ! copy other characters as-is from input string to output string
        endif
    enddo

  end procedure str_noesc
  ! -----------------------------------------------------------------------------


  !! SPLIT(3f) parses a string using specified delimiter characters and
  !!  store tokens into an allocatable array
  !! AUTHOR:  John S. Urban       LICENSE: Public Domain
  module procedure splitStr !(input_line,array,delimiters,order,nulls)
  
    intrinsic index, min, present, len

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

    if(present(order))then; ordr=upperStr(adjustl(order)); else; ordr='SEQUENTIAL'; endif ! decide on value for optional ORDER parameter
    if(present(nulls))then; nlls=upperStr(adjustl(nulls)); else; nlls='IGNORE'    ; endif ! optional parameter

    n=len(input_line)+1                        ! max number of strings INPUT_LINE could split into if all delimiter
    allocate(ibegin(n))                        ! allocate enough space to hold starting location of tokens if string all tokens
    allocate(iterm(n))                         ! allocate enough space to hold ending location of tokens if string all tokens
    ibegin(:)=1
    iterm(:)=1

    ilen=len(input_line)                                           ! ILEN is the column position of the last non-blank character
    icount=0                                                       ! how many tokens found
    inotnull=0                                                     ! how many tokens found not composed of delimiters
    imax=0                                                         ! length of longest token found

    select case (ilen)


    case default                                                   ! there is at least one non-delimiter in INPUT_LINE if get here
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

    end select

    select case (trim(adjustl(nlls)))
    case ('ignore','','ignoreend')
      ireturn=inotnull
    case default
      ireturn=icount
    end select
    allocate(character(len=imax) :: array(ireturn))                ! allocate the array to return

    select case (trim(adjustl(ordr)))                              ! decide which order to store tokens
    case ('reverse','right') ; ii=ireturn ; iiii=-1                ! last to first
    case default             ; ii=1       ; iiii=1                 ! first to last
    end select

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
  
  end procedure splitStr

end submodule Focal_Utils
