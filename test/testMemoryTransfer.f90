program testMemoryTransfer
!! Focal test program
!!
!! Tests scalar-fill, host-to-device, device-to-device and device-to-host transfers
!!  for all Focal abstracted data types as well as un-typed general deviceBuffers
!! 

use Focal
use Focal_Test_Utils
use iso_fortran_env, only: sp=>real32, dp=>real64
implicit none

character(*), parameter :: test_string = 'Hello world'

real(sp), dimension(FCL_TEST_SIZE) :: hostReal32_1, hostReal32_2
real(dp), dimension(FCL_TEST_SIZE) :: hostReal64_1, hostReal64_2
integer, dimension(FCL_TEST_SIZE) :: hostInt32_1, hostInt32_2
character(len=1), dimension(len(test_string)), target :: hostChar_1, hostChar_2

type(fclDeviceFloat) :: deviceReal32_1,deviceReal32_2
type(fclDeviceDouble) :: deviceReal64_1, deviceReal64_2
type(fclDeviceInt32) :: deviceInt32_1, deviceInt32_2
type(fclDeviceBuffer) :: deviceBuffer_1, deviceBuffer_2

integer :: i

! --- Initialise ---
call fclTestInit()

! --- Initialise device buffers ---
call fclInitBuffer(deviceInt32_1,FCL_TEST_SIZE,access='r')
call fclInitBuffer(deviceReal32_1,FCL_TEST_SIZE,access='r')
call fclInitBuffer(deviceReal64_1,FCL_TEST_SIZE,access='r')
call fclInitBuffer(deviceBuffer_1,c_sizeof(hostChar_1),access='r')

call fclInitBuffer(deviceInt32_2,FCL_TEST_SIZE,access='w')
call fclInitBuffer(deviceReal32_2,FCL_TEST_SIZE,access='w')
call fclInitBuffer(deviceReal64_2,FCL_TEST_SIZE,access='w')
call fclInitBuffer(deviceBuffer_2,c_sizeof(hostChar_1),access='w')

! --- Setup host arrays ---
do i=1,FCL_TEST_SIZE
  hostReal32_1(i) = i
  hostReal64_1(i) = 2*i
  hostInt32_1(i) = 3*i
end do

do i=1,len(test_string)
  hostChar_1(i) = test_string(i:i)
end do

! --- Perform host-to-device transfer ---
deviceReal32_1 = hostReal32_1
deviceReal64_1 = hostReal64_1
deviceInt32_1 = hostInt32_1
call fclMemWrite(deviceBuffer_1,c_loc(hostChar_1),deviceBuffer_1%nBytes)

! --- Perform device-to-device transfer ---
deviceReal32_2 = deviceReal32_1
deviceReal64_2 = deviceReal64_1
deviceInt32_2 = deviceInt32_1
call fclMemCopy(deviceBuffer_2,deviceBuffer_1)

! --- Perform device-to-host transfer ---
hostReal32_2 = deviceReal32_2
hostReal64_2 = deviceReal64_2
hostInt32_2 = deviceInt32_2
call fclMemRead(c_loc(hostChar_2),deviceBuffer_2,deviceBuffer_2%nBytes)

! --- Check arrays ---
call fclTestAssertEqual(hostReal32_1,hostReal32_2,'hostReal32_1 == hostReal32_2 (1)')
call fclTestAssertEqual(hostReal64_1,hostReal64_2,'hostReal64_1 == hostReal64_2 (1)')
call fclTestAssertEqual(hostInt32_1,hostInt32_2,'hostInt32_1 == hostInt32_2 (1)')
call fclTestAssertEqual(hostChar_1,hostChar_2,'hostChar_1 == hostChar_2 (1)')

! --- Fill host arrays with scalars for comparison ---
hostReal32_2(:) = 10.0
hostReal64_2(:) = 20.0d0
hostInt32_2(:) = 30
hostChar_2(:) = test_string(1:1)

! --- Perform scalar write to device ---
deviceReal32_1 = 10.0
deviceReal64_1 = 20.0d0
deviceInt32_1 = 30
call fclMemWriteScalar(deviceBuffer_1,c_loc(hostChar_1),c_sizeof(hostChar_1(1)))

! --- Perform device-to-host transfer ---
hostReal32_1 = deviceReal32_1
hostReal64_1 = deviceReal64_1
hostInt32_1 = deviceInt32_1
call fclMemRead(c_loc(hostChar_1),deviceBuffer_1,deviceBuffer_1%nBytes)

! --- Check arrays ---
call fclTestAssertEqual(hostReal32_1,hostReal32_2,'hostReal32_1 == hostReal32_2 (2)')
call fclTestAssertEqual(hostReal64_1,hostReal64_2,'hostReal64_1 == hostReal64_2 (2)')
call fclTestAssertEqual(hostInt32_1,hostInt32_2,'hostInt32_1 == hostInt32_2 (2)')
call fclTestAssertEqual(hostChar_1,hostChar_2,'hostChar_1 == hostChar_2 (2)')

call fclTestFinish()

end program testMemoryTransfer
! -----------------------------------------------------------------------------
