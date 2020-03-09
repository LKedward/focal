program testKernelSetup
!! Focal test program
!!
!! Based on testKernelSetup, this program tests the use of fclProfiler
!!  objects to capture kernel and buffer events and report on durations
!!

use Focal
use Focal_Test_Utils
use iso_fortran_env, only: sp=>real32, dp=>real64
implicit none


character(:), allocatable :: kernelSrc              ! Kernel source string
type(fclProgram) :: prog                            ! Focal program object
type(fclKernel) :: setInt_k, setFloat_k, setDouble_k, setChar_k
type(fclProfiler) :: profiler

real(sp), dimension(FCL_TEST_SIZE) :: hostReal32
real(dp), dimension(FCL_TEST_SIZE) :: hostReal64
integer, dimension(FCL_TEST_SIZE) :: hostInt32
character(1), dimension(FCL_TEST_SIZE), target :: hostChar

type(fclDeviceFloat) :: deviceReal32
type(fclDeviceDouble) :: deviceReal64
type(fclDeviceInt32) :: deviceInt32
type(fclDeviceBuffer) :: deviceBuffer

integer :: i, fh
logical :: fExist

! --- Initialise ---
call fclTestInit()

profiler%device = ocl_device

! --- Initialise device buffers ---
call fclInitBuffer(deviceInt32,FCL_TEST_SIZE)
call fclInitBuffer(deviceReal32,FCL_TEST_SIZE)
call fclInitBuffer(deviceReal64,FCL_TEST_SIZE)
call fclInitBuffer(deviceBuffer,c_sizeof(hostChar))

call fclProfilerAdd(profiler,1,deviceInt32,deviceReal32,deviceReal64,deviceBuffer)

call fclTestAssert(deviceInt32%profileSize == 1,'deviceInt32%profileSize == 1')
call fclTestAssert(associated(deviceInt32%profileEvents),'associated(deviceInt32%profileEvents)')
call fclTestAssert(size(deviceInt32%profileEvents,1)==1,'size(deviceInt32%profileEvents,1)==1')

call fclTestAssert(deviceReal32%profileSize == 1,'deviceReal32%profileSize == 1')
call fclTestAssert(associated(deviceReal32%profileEvents),'associated(deviceReal32%profileEvents)')
call fclTestAssert(size(deviceReal32%profileEvents,1)==1,'size(deviceReal32%profileEvents,1)==1')

call fclTestAssert(deviceReal64%profileSize == 1,'deviceReal64%profileSize == 1')
call fclTestAssert(associated(deviceReal64%profileEvents),'associated(deviceReal64%profileEvents)')
call fclTestAssert(size(deviceReal64%profileEvents,1)==1,'size(deviceReal64%profileEvents,1)==1')

call fclTestAssert(deviceBuffer%profileSize == 1,'deviceBuffer%profileSize == 1')
call fclTestAssert(associated(deviceBuffer%profileEvents),'associated(deviceBuffer%profileEvents)')
call fclTestAssert(size(deviceBuffer%profileEvents,1)==1,'size(deviceBuffer%profileEvents,1)==1')

! --- Initialise kernels ---
call fclGetKernelResource(kernelSrc)
prog = fclCompileProgram(kernelSrc)
setInt_k = fclGetProgramKernel(prog,'setInt32Test',[FCL_TEST_SIZE])
setFloat_k = fclGetProgramKernel(prog,'setFloatTest',[FCL_TEST_SIZE])
setDouble_k = fclGetProgramKernel(prog,'setDoubleTest',[FCL_TEST_SIZE])
setChar_k = fclGetProgramKernel(prog,'setCharTest',[FCL_TEST_SIZE])

call fclProfilerAdd(profiler,2,setInt_k,setFloat_k,setDouble_k,setChar_k)

call fclTestAssert(setInt_k%profileSize == 2,'setInt_k%profileSize == 2')
call fclTestAssert(associated(setInt_k%profileEvents),'associated(setInt_k%profileEvents)')
call fclTestAssert(size(setInt_k%profileEvents,1)==2,'size(setInt_k%profileEvents,1)==2')

call fclTestAssert(setFloat_k%profileSize == 2,'setFloat_k%profileSize == 2')
call fclTestAssert(associated(setFloat_k%profileEvents),'associated(setFloat_k%profileEvents)')
call fclTestAssert(size(setFloat_k%profileEvents,1)==2,'size(setFloat_k%profileEvents,1)==2')

call fclTestAssert(setDouble_k%profileSize == 2,'setDouble_k%profileSize == 2')
call fclTestAssert(associated(setDouble_k%profileEvents),'associated(setDouble_k%profileEvents)')
call fclTestAssert(size(setDouble_k%profileEvents,1)==2,'size(setDouble_k%profileEvents,1)==2')

call fclTestAssert(setChar_k%profileSize == 2,'setChar_k%profileSize == 2')
call fclTestAssert(associated(setChar_k%profileEvents),'associated(setChar_k%profileEvents)')
call fclTestAssert(size(setChar_k%profileEvents,1)==2,'size(setChar_k%profileEvents,1)==2')

! Launch kernels
call setInt_k%launch(FCL_TEST_SIZE,deviceInt32)
call setFloat_k%launch(FCL_TEST_SIZE,deviceReal32)
call setDouble_k%launch(FCL_TEST_SIZE,deviceReal64)
call setChar_k%launch(FCL_TEST_SIZE,deviceBuffer)

call fclTestAssert(setChar_k%nProfileEvent == 1,'setChar_k%nProfileEvent == 1')
call fclTestAssert(setFloat_k%nProfileEvent == 1,'setFloat_k%nProfileEvent == 1')
call fclTestAssert(setDouble_k%nProfileEvent == 1,'setDouble_k%nProfileEvent == 1')
call fclTestAssert(setInt_k%nProfileEvent == 1,'setInt_k%nProfileEvent == 1')

! --- Transfer device buffers to host ---

call fclTestAssert( deviceInt32%nProfileEvent == 0, 'deviceInt32%nProfileEvent == 0' )
call fclTestAssert( deviceReal32%nProfileEvent == 0, 'deviceReal32%nProfileEvent == 0' )
call fclTestAssert( deviceReal64%nProfileEvent == 0, 'deviceReal64%nProfileEvent == 0' )
call fclTestAssert( deviceBuffer%nProfileEvent == 0, 'deviceBuffer%nProfileEvent == 0' )

hostInt32 = deviceInt32

call fclTestAssert(deviceInt32%profileEvents(1)%cl_event == &
                   fclLastReadEvent%cl_event,'deviceInt32%profileEvents(1)%cl_event')

hostReal32 = deviceReal32

call fclTestAssert(deviceReal32%profileEvents(1)%cl_event == &
                   fclLastReadEvent%cl_event,'deviceReal32%profileEvents(1)%cl_event')

hostReal64 = deviceReal64

call fclTestAssert(deviceReal64%profileEvents(1)%cl_event == &
                   fclLastReadEvent%cl_event,'deviceReal64%profileEvents(1)%cl_event')

call fclMemRead(c_loc(hostChar),deviceBuffer,c_sizeof(hostChar))

call fclTestAssert(deviceBuffer%profileEvents(1)%cl_event == &
                   fclLastReadEvent%cl_event,'deviceBuffer%profileEvents(1)%cl_event')

call fclTestAssert( deviceInt32%nProfileEvent == 1, 'deviceInt32%nProfileEvent == 1' )
call fclTestAssert( deviceReal32%nProfileEvent == 1, 'deviceReal32%nProfileEvent == 1' )
call fclTestAssert( deviceReal64%nProfileEvent == 1, 'deviceReal64%nProfileEvent == 1' )
call fclTestAssert( deviceBuffer%nProfileEvent == 1, 'deviceBuffer%nProfileEvent == 1' )

! --- Wait for operations on default q to complete ---
call fclWait()

! --- Check arrays ---
call fclTestAssertEqual([sum(hostInt32)],[sum([(i,i=0,FCL_TEST_SIZE-1)])],'sum(hostInt32)')
call fclTestAssertEqual([sum(hostReal32)],[sum([(1.0*i,i=0,FCL_TEST_SIZE-1)])],'sum(hostReal32)')
call fclTestAssertEqual([sum(hostReal64)],[sum([(1.0d0*i,i=0,FCL_TEST_SIZE-1)])],'sum(hostReal64)')
call fclTestAssertEqual(hostChar,[('a',i=0,FCL_TEST_SIZE-1)],'hostChar')

call fclDumpProfileData(profiler)
call fclDumpTracingData(profiler,'testProfiling.trace')

INQUIRE(FILE='testProfiling.trace', EXIST=fExist)
open(newunit=fh,file='testProfiling.trace',status='old')
close(fh,status='delete')

call fclTestAssert(fExist,'trace file exists')

call fclFreeBuffer(deviceInt32)
call fclFreeBuffer(deviceReal32)
call fclFreeBuffer(deviceReal64)
call fclFreeBuffer(deviceBuffer)

call fclTestFinish()

end program testKernelSetup
! -----------------------------------------------------------------------------
