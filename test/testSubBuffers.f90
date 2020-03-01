program testSubBuffers
!! Focal test program
!!
!! Test the creation of sub-buffers from parent buffers and 
!!  using scalar fill on them.
!! 

use Focal
use Focal_Test_Utils
use iso_fortran_env, only: sp=>real32, dp=>real64
implicit none

character(1), target :: testChar1 = 'a'
character(1), target :: testChar2 = 'z'

real(sp), dimension(FCL_TEST_SIZE) :: hostReal32_1, hostReal32_2
real(dp), dimension(FCL_TEST_SIZE) :: hostReal64_1, hostReal64_2
integer, dimension(FCL_TEST_SIZE) :: hostInt32_1, hostInt32_2
character(len=1), dimension(FCL_TEST_SIZE), target :: hostChar_1, hostChar_2

type(fclDeviceFloat) :: deviceReal32_1,deviceReal32_2
type(fclDeviceDouble) :: deviceReal64_1, deviceReal64_2
type(fclDeviceInt32) :: deviceInt32_1, deviceInt32_2
type(fclDeviceBuffer) :: deviceBuffer_1, deviceBuffer_2

integer :: i
integer(c_size_t) :: nBytes

nBytes = FCL_TEST_SIZE*c_sizeof(testChar1)

! --- Initialise ---
call fclTestInit()

! --- Initialise device buffers ---
call fclInitBuffer(deviceInt32_1,FCL_TEST_SIZE)
call fclInitBuffer(deviceReal32_1,FCL_TEST_SIZE)
call fclInitBuffer(deviceReal64_1,FCL_TEST_SIZE)
call fclInitBuffer(deviceBuffer_1,nBytes)

! --- Initialise sub-buffers ---
!  Each sub-buffer points to the first half of the respective buffers
!   defined above.
call fclInitSubBuffer(deviceInt32_2,deviceInt32_1,0,FCL_TEST_SIZE/2)
call fclInitSubBuffer(deviceReal32_2,deviceReal32_1,0,FCL_TEST_SIZE/2)
call fclInitSubBuffer(deviceReal64_2,deviceReal64_1,0,FCL_TEST_SIZE/2)
call fclInitSubBuffer(deviceBuffer_2,deviceBuffer_1,int(0,c_size_t),int(nBytes/2,c_size_t))

! --- Fill main buffers with scalar ---
deviceInt32_1 = 10
deviceReal32_1 = 10.0
deviceReal64_1 = 10.0d0

call fclMemWriteScalar(deviceBuffer_1,c_loc(testChar1),c_sizeof(testChar1))

! --- Fill sub-buffers with different scalar ---
deviceInt32_2 = -1
deviceReal32_2 = -1.0
deviceReal64_2 = -1.0d0

call fclMemWriteScalar(deviceBuffer_2,c_loc(testChar2),c_sizeof(testChar2))


! --- Transfer main buffers back to host ---
hostInt32_1 = deviceInt32_1
hostReal32_1 = deviceReal32_1
hostReal64_1 = deviceReal64_1

call fclMemRead(c_loc(hostChar_1),deviceBuffer_1,nBytes)


! --- Fill host arrays with scalars for comparison ---
hostInt32_2(:) = 10
hostInt32_2(1:FCL_TEST_SIZE/2) = -1
hostReal32_2(:) = 10.0
hostReal32_2(1:FCL_TEST_SIZE/2) = -1.0
hostReal64_2(:) = 10.0d0
hostReal64_2(1:FCL_TEST_SIZE/2) = -1.0d0
hostChar_2(:) = 'a'
hostChar_2(1:FCL_TEST_SIZE/2) = 'z'

! --- Check arrays ---
call fclTestAssertEqual(hostReal32_1,hostReal32_2,'hostReal32_1 == hostReal32_2')
call fclTestAssertEqual(hostReal64_1,hostReal64_2,'hostReal64_1 == hostReal64_2')
call fclTestAssertEqual(hostInt32_1,hostInt32_2,'hostInt32_1 == hostInt32_2')
call fclTestAssertEqual(hostChar_1,hostChar_2,'hostChar_1 == hostChar_2')

call fclTestFinish()

end program testSubBuffers
! -----------------------------------------------------------------------------
