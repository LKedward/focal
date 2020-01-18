program testExample
!! Focal example program: calculate the sum of two arrays on an openCL device

use Focal
use Focal_Test_Utils
implicit none

call fclTestInit()

write(*,*) "This test will fail"

stop FCL_TEST_FAILED

end program testExample
! -----------------------------------------------------------------------------
