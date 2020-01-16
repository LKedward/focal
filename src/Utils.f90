submodule (Focal) Focal_Utils
  !! FOCAL: openCL abstraction layer for fortran
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
