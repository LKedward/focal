program testMemoryTransfer
!! Focal test program
!!
!! Tests host-to-device, device-to-device and device-to-host transfers
!!  for all Focal abstracted data types
!! 

use Focal
use Focal_Test_Utils
use iso_fortran_env, only: sp=>real32, dp=>real64
implicit none

real(sp), dimension(FCL_TEST_SIZE) :: hostReal32_1, hostReal32_2
real(dp), dimension(FCL_TEST_SIZE) :: hostReal64_1, hostReal64_2
integer, dimension(FCL_TEST_SIZE) :: hostInt32_1, hostInt32_2

type(fclDeviceFloat) :: deviceReal32_1,deviceReal32_2
type(fclDeviceDouble) :: deviceReal64_1, deviceReal64_2
type(fclDeviceInt32) :: deviceInt32_1, deviceInt32_2

integer :: i

! --- Initialise ---
call fclTestInit()

! --- Initialise device buffers ---
deviceReal32_1 = fclBufferFloat(FCL_TEST_SIZE,read=.true.,write=.false.)
deviceReal64_1 = fclBufferDouble(FCL_TEST_SIZE,read=.true.,write=.false.)
deviceInt32_1 = fclBufferInt32(FCL_TEST_SIZE,read=.true.,write=.false.)

deviceReal32_2 = fclBufferFloat(FCL_TEST_SIZE,read=.true.,write=.false.)
deviceReal64_2 = fclBufferDouble(FCL_TEST_SIZE,read=.true.,write=.false.)
deviceInt32_2 = fclBufferInt32(FCL_TEST_SIZE,read=.true.,write=.false.)

! --- Setup host arrays ---
do i=1,FCL_TEST_SIZE
  hostReal32_1(i) = i
  hostReal64_1(i) = 2*i
  hostInt32_1(i) = 3*i
end do

! --- Perform host-to-device transfer ---
deviceReal32_1 = hostReal32_1
deviceReal64_1 = hostReal64_1
deviceInt32_1 = hostInt32_1

! --- Perform device-to-device transfer ---
deviceReal32_2 = deviceReal32_1
deviceReal64_2 = deviceReal64_1
deviceInt32_2 = deviceInt32_1

! --- Perform device-to-host transfer ---
hostReal32_2 = deviceReal32_2
hostReal64_2 = deviceReal64_2
hostInt32_2 = deviceInt32_2

! --- Check arrays ---
call fclTestAssertEqual(hostReal32_1,hostReal32_2,'hostReal32_1 == hostReal32_2')
call fclTestAssertEqual(hostReal64_1,hostReal64_2,'hostReal64_1 == hostReal64_2')
call fclTestAssertEqual(hostInt32_1,hostInt32_2,'hostInt32_1 == hostInt32_2')

call fclTestFinish()

end program testMemoryTransfer
! -----------------------------------------------------------------------------
