program test

use Focal
use clfortran
implicit none

integer :: i, j
integer :: iPlat, iDev
integer :: fh, iostat,iLen

real(c_float) :: testArray(1000000)

character(1) :: char
character(:), allocatable :: kernelSrc
type(fclPlatform), pointer :: platforms(:)
type(fclCommandQ) :: cmdq

type(fclContext) :: ctx
type(fclProgram) :: prog
type(fclKernel) :: sumKernel

type(fclDeviceFloat) :: test_d
type(fclDeviceFloat) :: test2_d
! integer :: platformDeviceIDs(1)

platforms => fclGetPlatforms();

! call fclGetPlatforms(platforms)

do i=1,size(platforms,1)

  write(*,*) ' --------- Platform ',i,'---------'
  write(*,*) 
  write(*,*) '  Profile: ',platforms(i)%profile
  write(*,*) '  Version: ',platforms(i)%version
  write(*,*) '  Name: ',platforms(i)%name
  write(*,*) '  Vendor: ',platforms(i)%vendor
  write(*,*) '  No. of devices: ', platforms(i)%numDevice
  write(*,*) 

  do j=1,platforms(i)%numDevice

    write(*,*) '  ---- Device ',j,'----'
    write(*,*) '   Name: ',platforms(i)%devices(j)%name
    write(*,*) '   Compute units: ',platforms(i)%devices(j)%nComputeUnits

    select case (platforms(i)%devices(j)%cl_device_type)
      case (CL_DEVICE_TYPE_CPU)
        write(*,*) '   Type: CPU'
      case (CL_DEVICE_TYPE_GPU)
        write(*,*) '   Type: GPU'
    end select
    write(*,*)

  end do

  write(*,*)

end do

iPlat = 1
iDev = 1

write(*,'(A,I4,A)') 'Creating context with platform ',iPlat,' ...'

ctx = fclCreateContext(platforms(iPlat))

write(*,'(A,I4,A)') 'Creating command queue with device ',iDev,' ...'

cmdq = fclCreateDeviceCommandQ(ctx,platforms(iPlat)%devices(iDev),enableProfiling=.true.)

write(*,*) 'Reading kernel...'
open(newunit=fh,file='kernel.cl',status='old',access='direct',recl=1)
iLen = 1
iostat = 0
do while(iostat == 0)
  read(fh,rec=iLen,iostat=iostat) char
  iLen = iLen + 1
enddo
iLen = iLen - 2

write(*,*) ' Kernel source length: ',iLen

allocate(character(len=iLen) :: kernelSrc)

close(fh)
open(newunit=fh,file='kernel.cl',status='old',access='direct',recl=1)
do i=1,iLen
    read(fh,rec=i) char
    kernelSrc(i:i) = char
end do

write(*,*)
write(*,*) ' Begin kernel source listing:'
write(*,*) kernelSrc
write(*,*)

write(*,*) 'Compiling kernel...'
prog = fclCompileProgram(ctx,kernelSrc)
sumKernel = fclGetProgramKernel(prog,'sum')

! read(*,*)
write(*,*) 'Allocating device memory...'
! test_d = fclBufferInt32(cmdq,size(testArray,1),read=.true.,write=.true.)
test_d = fclBufferFloat(cmdq,size(testArray,1),read=.true.,write=.true.)
test2_d = fclBufferFloat(cmdq,size(testArray,1),read=.true.,write=.true.)

testArray = 9.00

write(*,*) testArray(1), testArray(size(testArray,1))



test_d = testArray

testArray = 0.00

write(*,*) testArray(1), testArray(size(testArray,1))

testArray = test_d

write(*,*) testArray(1), testArray(size(testArray,1))

test_d = 10.00 !real(1,c_double)
testArray = test_d

write(*,*) testArray(1), testArray(size(testArray,1))

test_d = -1.00
test2_d = test_d

testArray = test2_d

write(*,*) testArray(1), testArray(size(testArray,1))

test_d = 1.00
test2_d = 2.00

write(*,*) 'Launching kernel...'
sumKernel%global_work_size(1) = size(testArray,1)
call sumKernel%launch(cmdq,size(testArray,1),test_d,test2_d)

testArray = test2_d
write(*,*) testArray(1), testArray(size(testArray,1))

end program test
