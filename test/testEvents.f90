program testKernelSetup
!! Focal test program
!!
!! Based on testKernelSetup, this test uses a command queue pool with
!!  4 queues to test setting event dependencies.
!!

use Focal
use Focal_Test_Utils
use iso_fortran_env, only: sp=>real32, dp=>real64
implicit none

type(fclCommandQPool) :: qPool
type(fclCommandQ), pointer :: cmdq
character(:), allocatable :: kernelSrc              ! Kernel source string
type(fclProgram) :: prog                            ! Focal program object
type(fclKernel) :: setInt_k, setFloat_k, setDouble_k, setChar_k
type(fclEvent) :: e(2)

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

qPool = fclCreateCommandQPool(4,ocl_device,enableProfiling=.true.,&
                            blockingRead=.false., blockingWrite=.false.)

! --- Initialise device buffers ---
call fclInitBuffer(deviceInt32,FCL_TEST_SIZE)
call fclInitBuffer(deviceReal32,FCL_TEST_SIZE)
call fclInitBuffer(deviceReal64,FCL_TEST_SIZE)
call fclInitBuffer(deviceBuffer,c_sizeof(hostChar))

! --- Initialise kernels ---
call fclGetKernelResource(kernelSrc)
prog = fclCompileProgram(kernelSrc)
setInt_k = fclGetProgramKernel(prog,'setInt32Test',[FCL_TEST_SIZE])
setFloat_k = fclGetProgramKernel(prog,'setFloatTest',[FCL_TEST_SIZE])
setDouble_k = fclGetProgramKernel(prog,'setDoubleTest',[FCL_TEST_SIZE])
setChar_k = fclGetProgramKernel(prog,'setCharTest',[FCL_TEST_SIZE])

! Launch first kernel
call setInt_k%launch(qPool%next(),FCL_TEST_SIZE,deviceInt32)
e(1) = fclLastKernelEvent

! Launch second kernel: dependency on first
call fclSetDependency(qPool%next(),fclLastKernelEvent)

cmdq => qPool%current()
call fclTestAssert(transfer(cmdq%dependencyListPtr,int(1,c_intptr_t)) == &
                     transfer(c_loc(cmdq%dependencyList),int(1,c_intptr_t)), &
                    'fclSetDependency_Event:dependencyListPtr:set')


call fclTestAssert(cmdq%dependencyList(1)==fclLastKernelEvent%cl_event, &
                      'fclSetDependency_Event:dependencyList')

call fclTestAssert(cmdq%nDependency==1, &
                      'fclSetDependency_Event:ndependency:set')

call setFloat_k%launch(qPool%current(),FCL_TEST_SIZE,deviceReal32)
call fclTestAssert(transfer(cmdq%dependencyListPtr,int(1,c_intptr_t)) == &
                     transfer(C_NULL_PTR,int(1,c_intptr_t)), &
                    'fclLaunchKernel:dependencyListPtr:cleared')

call fclTestAssert(cmdq%nDependency==0, &
                    'fclLaunchKernel:ndependency:unset')

e(2) = fclLastKernelEvent

! Launch third kernel: dependency on first and second
call fclSetDependency(qPool%next(),e)

cmdq => qPool%current()
call fclTestAssert(transfer(cmdq%dependencyListPtr,int(1,c_intptr_t)) == &
                     transfer(c_loc(cmdq%dependencyList),int(1,c_intptr_t)), &
                    'fclSetDependency_Eventlist:dependencyListPtr:set')


call fclTestAssert(all(cmdq%dependencyList(1:2)==e(1:2)%cl_event), &
                      'fclSetDependency_Eventlist:dependencyList')

call fclTestAssert(cmdq%nDependency==2, &
                      'fclSetDependency_Eventlist:ndependency:set')

call setDouble_k%launch(qPool%current(),FCL_TEST_SIZE,deviceReal64)


call fclTestAssert(transfer(cmdq%dependencyListPtr,int(1,c_intptr_t)) == &
                     transfer(C_NULL_PTR,int(1,c_intptr_t)), &
                    'fclLaunchKernel:dependencyListPtr:cleared (2)')

call fclTestAssert(cmdq%nDependency==0, &
                    'fclLaunchKernel:ndependency:unset (2)')

! Launch fourth kernel: no dependencies
call setChar_k%launch(qPool%next(),FCL_TEST_SIZE,deviceBuffer)

cmdq => qPool%current()
call fclTestAssert(transfer(cmdq%dependencyListPtr,int(1,c_intptr_t)) == &
                     transfer(C_NULL_PTR,int(1,c_intptr_t)), &
                    'dependencyListPtr:unset (3)')

call fclTestAssert(cmdq%nDependency==0, &
                    'ndependency:unset (3)')

call fclWait(qPool%queues(:)%lastKernelEvent)

! --- Transfer device buffers to host ---
call fclSetDependency(qPool%queues(:)%lastKernelEvent,hold=.true.)

call fclTestAssert(fclDefaultCmdQ%nDependency==4, &
                      'fclSetDependency_EventList:fclDefaultCmdQ:ndependency:set')

call fclTestAssert(transfer(fclDefaultCmdQ%dependencyListPtr,int(1,c_intptr_t)) == &
                      transfer(c_loc(fclDefaultCmdQ%dependencyList),int(1,c_intptr_t)), &
                     'fclSetDependency_EventList:fclDefaultCmdQ:dependencyListPtr:set')

hostInt32 = deviceInt32

call fclTestAssert(fclDefaultCmdQ%nDependency==4, &
                      'fclSetDependency_EventList:fclDefaultCmdQ:ndependency:set (2)')

hostReal32 = deviceReal32

call fclTestAssert(fclDefaultCmdQ%nDependency==4, &
                      'fclSetDependency_EventList:fclDefaultCmdQ:ndependency:set (3)')

hostReal64 = deviceReal64

call fclTestAssert(fclDefaultCmdQ%nDependency==4, &
                      'fclSetDependency_EventList:fclDefaultCmdQ:ndependency:set (4)')

call fclMemRead(c_loc(hostChar),deviceBuffer,c_sizeof(hostChar))

call fclTestAssert(fclDefaultCmdQ%nDependency==4, &
                      'fclSetDependency_EventList:fclDefaultCmdQ:ndependency:set (5)')

call fclClearDependencies()

call fclTestAssert(transfer(fclDefaultCmdQ%dependencyListPtr,int(1,c_intptr_t)) == &
                     transfer(C_NULL_PTR,int(1,c_intptr_t)), &
                    'fclClearDependencies:fclDefaultCmdQ:dependencyListPtr:unset')

call fclTestAssert(fclDefaultCmdQ%nDependency==0, &
                    'fclClearDependencies:fclDefaultCmdQ:nDependency:unset')


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

end program testKernelSetup
! -----------------------------------------------------------------------------
