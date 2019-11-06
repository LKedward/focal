submodule (Focal) Focal_Utils
  !! FOCAL: openCL abstraction layer for fortran
  !!  Implementation module for focal utility routines

  !! @note This is an implementation submodule: it contains the code implementing the subroutines defined in the
  !!  corresponding header module file. See header module file (Focal.f90) for interface definitions. @endnote

  implicit none

  character(len=1,kind=c_char), target, bind(C,name="_binary_fclKernels_cl_start") :: i0
    !! c interoperable character for start of fclKernels binary resource
  character(len=1,kind=c_char), target, bind(C,name="_binary_fclKernels_cl_end") :: i1
    !! c interoperable character for sendtart of fclKernels binary resource

  contains

  
  module procedure fclGetKernelResource !(kernelString)

    integer(c_intptr_t) :: a0, a1
    integer(c_intptr_t) :: i, length
    character(1), pointer :: text(:)
    
    type(c_ptr) :: aa
    
    aa = c_loc(i0)

    a0 = transfer(c_loc(i0),a0)
    a1 = transfer(c_loc(i1),a1)
    length = a1 - a0
    
    call c_f_pointer(aa,text,shape=[length])
  
    allocate(character(len=length) :: kernelString)
    do i=1,length
      kernelString(i:i) = text(i)
    end do

  end procedure fclGetKernelResource
  ! -----------------------------------------------------------------------------


  module procedure upperstr ! character(len=len(linei)) function upperstr(linei)
    !! Return copy of string converted to uppercase
    !! Used for case-insensitive string comparison
    !! 1996, John S. Urban
    !! Public Domain Code, http://fortranwiki.org/fortran/show/ufpp

    intrinsic ichar, char, len
    integer :: inlen ! number of characters in trimmed input string
    integer :: i10 ! counter to increment through input and output string
    integer :: ilet ! current character being converted represented using ASCII Decimal Equivalent

    inlen=len_trim(linei) ! number of characters to convert to uppercase
    upperstr=' '  ! initialize output string to all blanks

    if(inlen.gt.len(upperstr))then ! make sure there is room to store the output characters
        write(*,'(a)')'*ufpp* FATAL - OUTPUT TOO LONG TO CONVERT TO UPPERCASE:'
    endif

    ! loop through each character in input string
    do i10=1,inlen,1
        ilet=ichar(linei(i10:i10))                ! current character in input to convert to output converted to ADE
        if( (ilet.ge.97) .and. (ilet.le.122))then ! lowercase a-z in ASCII is 97 to 122; uppercase A-Z in ASCII is 65 to 90
        upperstr(i10:i10)=char(ilet-32)        ! convert lowercase a-z to uppercase A-Z
        else
        upperstr(i10:i10)=linei(i10:i10)       ! character is not a lowercase a-z, just put it in output
        endif
    enddo

  end procedure upperstr
  ! -----------------------------------------------------------------------------


  module procedure fclSourceFromFile !(filename,sourceString)
    !! Allocae and fill character string from file
  
    integer :: fh, iLen, ioStat, i
    character(1) :: char
  
    ! --- First pass: get kernel source length ---
    open(newunit=fh,file=filename,status='old',access='direct',recl=1)
    iLen = 1
    iostat = 0
    do while(iostat == 0)
      read(fh,rec=iLen,iostat=iostat) char
      iLen = iLen + 1
    enddo
    iLen = iLen - 2
    close(fh)
  
    allocate(character(len=iLen) :: sourceString)
  
    ! --- Second pass: read kernel source into buffer ---
    open(newunit=fh,file=filename,status='old',access='direct',recl=1)
    do i=1,iLen
        read(fh,rec=i) char
        sourceString(i:i) = char
    end do
    close(fh)
  
  end procedure fclSourceFromFile
  ! -----------------------------------------------------------------------------


end submodule Focal_Utils