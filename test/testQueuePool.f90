program testQueuePool
!! Focal test program
!!
!! Based on testKernelSetup, this test runs on multiple queues
!!  using a queue pool object.
!!

use Focal
use Focal_Test_Utils
use iso_fortran_env, only: sp=>real32, dp=>real64
implicit none

character(:), allocatable :: kernelSrc              ! Kernel source string
type(fclDevice), allocatable :: devices(:)
type(fclCommandQPool) :: qPool
type(fclProfiler) :: profiler
type(fclProgram) :: prog                            ! Focal program object
type(fclKernel) :: setInt_k, setFloat_k, setDouble_k, setChar_k

real(sp), dimension(FCL_TEST_SIZE) :: hostReal32
real(dp), dimension(FCL_TEST_SIZE) :: hostReal64
integer, dimension(FCL_TEST_SIZE) :: hostInt32
character(1), dimension(FCL_TEST_SIZE), target :: hostChar

type(fclDeviceFloat) :: deviceReal32
type(fclDeviceDouble) :: deviceReal64
type(fclDeviceInt32) :: deviceInt32
type(fclDeviceBuffer) :: deviceBuffer

integer :: i

! --- Initialise ---
call fclTestInit()

! --- Create command queue pool ---
devices = fclFindDevices(sortBy='cores')
qPool = fclCreateCommandQPool(4,devices(1),enableProfiling=.true., &
                    blockingWrite=.false., blockingRead=.false.)
profiler%device = devices(1)

! --- Initialise device buffers ---
! (Using different queues in queue pool)
call fclInitBuffer(qPool%next(),deviceInt32,FCL_TEST_SIZE)
call fclInitBuffer(qPool%current(),deviceReal32,FCL_TEST_SIZE)
call fclInitBuffer(qPool%next(),deviceReal64,FCL_TEST_SIZE)
call fclInitBuffer(qPool%current(),deviceBuffer,c_sizeof(hostChar))

call fclProfilerAdd(profiler,1,deviceInt32,deviceReal32,deviceReal64,deviceBuffer)

! --- Initialise kernels ---
call fclGetKernelResource(kernelSrc)
prog = fclCompileProgram(kernelSrc)
setInt_k = fclGetProgramKernel(prog,'setInt32Test',[FCL_TEST_SIZE])
setFloat_k = fclGetProgramKernel(prog,'setFloatTest',[FCL_TEST_SIZE])
setDouble_k = fclGetProgramKernel(prog,'setDoubleTest',[FCL_TEST_SIZE])
setChar_k = fclGetProgramKernel(prog,'setCharTest',[FCL_TEST_SIZE])

call fclProfilerAdd(profiler,1,setInt_k, setFloat_k, setDouble_k, setChar_k)

! --- Call kernels ---
! (Using different queues in queue pool)
call setInt_k%launch(qPool%next(),FCL_TEST_SIZE,deviceInt32)
call setFloat_k%launch(qPool%next(),FCL_TEST_SIZE,deviceReal32)
call setDouble_k%launch(qPool%next(),FCL_TEST_SIZE,deviceReal64)
call setChar_k%launch(qPool%next(),FCL_TEST_SIZE,deviceBuffer)

! --- Transfer device buffers to host ---
hostInt32 = deviceInt32
hostReal32 = deviceReal32
hostReal64 = deviceReal64

call fclMemRead(c_loc(hostChar),deviceBuffer,c_sizeof(hostChar))

call fclWait()      ! Default queue
call fclWait(qPool) ! All queues in queue pool

! --- Check arrays ---
call fclTestAssertEqual([sum(hostInt32)],[sum([(i,i=0,FCL_TEST_SIZE-1)])],'sum(hostInt32)')
call fclTestAssertEqual([sum(hostReal32)],[sum([(1.0*i,i=0,FCL_TEST_SIZE-1)])],'sum(hostReal32)')
call fclTestAssertEqual([sum(hostReal64)],[sum([(1.0d0*i,i=0,FCL_TEST_SIZE-1)])],'sum(hostReal64)')
call fclTestAssertEqual(hostChar,[('a',i=0,FCL_TEST_SIZE-1)],'hostChar')

call fclFreeBuffer(deviceInt32)
call fclFreeBuffer(deviceReal32)
call fclFreeBuffer(deviceReal64)
call fclFreeBuffer(deviceBuffer)

call fclTestFinish()

end program testQueuePool
! -----------------------------------------------------------------------------
