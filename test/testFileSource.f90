program testFileSource
!! Focal test program
!!
!! This program tests the fclSourceFromFile utility

use Focal
use Focal_Test_Utils
use iso_fortran_env, only: sp=>real32, dp=>real64
implicit none

character(:), allocatable :: kernelSrc1, kernelSrc2              ! Kernel source string

integer :: i, fh
logical :: fExist

! --- Initialise ---
call fclTestInit()
call fclGetKernelResource(kernelSrc1)

! --- Write source to file ---
open(newunit=fh,file='testSource.cl',status='unknown')
write(fh,'(A)') kernelSrc1
close(fh)

INQUIRE(FILE='testSource.cl', EXIST=fExist)
call fclTestAssert(fExist,'Source file written')

allocate( character(len(kernelSrc1)) :: kernelSrc2 )
if (fExist) then
    call fclSourceFromFile('testSource.cl',kernelSrc2)
    open(newunit=fh,file='testSource.cl',status='unknown')
    close(fh,status='delete')
end if

call fclTestAssert(all([(kernelSrc1(i:i)==kernelSrc2(i:i),i=1,len(kernelSrc1))]),'kernelSrc1 == kernelSrc2')

call fclTestFinish()

end program testFileSource
! -----------------------------------------------------------------------------
