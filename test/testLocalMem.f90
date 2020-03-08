program testKernelLocalMem
!! Focal test program
!!

use Focal
use Focal_Test_Utils
use iso_fortran_env, only: sp=>real32, dp=>real64
implicit none

integer, parameter :: blockSize = 10

character(:), allocatable :: kernelSrc              ! Kernel source string
type(fclProgram) :: prog                            ! Focal program object
type(fclKernel) :: sumSqInt_k, sumSqFloat_k, sumSqDouble_k

real(sp), dimension(FCL_TEST_SIZE) :: hostReal32
real(dp), dimension(FCL_TEST_SIZE) :: hostReal64
integer, dimension(FCL_TEST_SIZE) :: hostInt32

type(fclDeviceFloat) :: deviceReal32
type(fclDeviceDouble) :: deviceReal64
type(fclDeviceInt32) :: deviceInt32

integer :: i

! --- Initialise ---
call fclTestInit()

! --- Initialise typed device buffers ---
call fclInitBuffer(deviceInt32,FCL_TEST_SIZE)
call fclInitBuffer(deviceReal32,FCL_TEST_SIZE)
call fclInitBuffer(deviceReal64,FCL_TEST_SIZE)

! --- Initialise kernels ---
call fclGetKernelResource(kernelSrc)
prog = fclCompileProgram(kernelSrc)
sumSqInt_k = fclGetProgramKernel(prog,'sumSqInt32Test',[FCL_TEST_SIZE],[blockSize])
sumSqFloat_k = fclGetProgramKernel(prog,'sumSqFloatTest',[FCL_TEST_SIZE],[blockSize])
sumSqDouble_k = fclGetProgramKernel(prog,'sumSqDoubleTest',[FCL_TEST_SIZE],[blockSize])

! --- Initialise host arrays ---
hostInt32 = [(i,i=1,FCL_TEST_SIZE)]
hostReal32 = [(1.0*i,i=1,FCL_TEST_SIZE)]
hostReal64 = [(1.0d0*i,i=1,FCL_TEST_SIZE)]

! --- Transfer host to device ---
deviceInt32 = hostInt32
deviceReal32 = hostReal32
deviceReal64 = hostReal64

! --- Call kernels ---
call sumSqInt_k%launch(FCL_TEST_SIZE,blockSize,deviceInt32,fclLocalInt32(blockSize))
call sumSqFloat_k%launch(FCL_TEST_SIZE,blockSize,deviceReal32,fclLocalFloat(blockSize))
call sumSqDouble_k%launch(FCL_TEST_SIZE,blockSize,deviceReal64,fclLocalDouble(blockSize))

! --- Transfer device buffers to host ---
hostInt32 = deviceInt32
hostReal32 = deviceReal32
hostReal64 = deviceReal64

call fclWait()

! --- Check arrays ---
call fclTestAssert(sum(hostInt32(1:FCL_TEST_SIZE/blockSize))==sum([(i*i,i=1,FCL_TEST_SIZE)]),'sumSqInt32Test')
call fclTestAssert(abs(sum(hostReal32(1:FCL_TEST_SIZE/blockSize))-sum([(1.0*i*i,i=1,FCL_TEST_SIZE)])) < 1e-5,'sumSqReal32Test')
call fclTestAssert(abs(sum(hostReal64(1:FCL_TEST_SIZE/blockSize))-sum([(1.0d0*i*i,i=1,FCL_TEST_SIZE)])) < 1e-10,'sumSqReal64Test')

call fclFreeBuffer(deviceInt32)
call fclFreeBuffer(deviceReal32)
call fclFreeBuffer(deviceReal64)

call fclTestFinish()

end program testKernelLocalMem
! -----------------------------------------------------------------------------
