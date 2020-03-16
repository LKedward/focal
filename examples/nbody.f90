program nbody
!! Focal example program: simple gravitational n-body simulator
!! Based on: https://github.com/ROCm-Developer-Tools/HIP-Examples/blob/master/mini-nbody/cuda/nbody-orig.cu
!!
!! This program demonstrates the following Focal capabilities
!!  - Creating a default context and selecting a device
!!  - Loading kernels from compiled resource
!!  - Transferring data to the device
!!  - Out-of-order command queue with barriers and event-dependencies
!!  - Profiling kernels and buffer transfers
!!
use Focal
implicit none

! --------- Program configuration ---------
integer, parameter :: N = 5000 !3E5                 ! No. of bodies
integer, parameter :: blockSize = 256               ! Local work group size
real, parameter :: dt = 1                           ! Global time-step
integer, parameter :: Niter = 1000                  ! Number of iterations to perform
character(*), parameter :: cl_vendor = 'nvidia,amd,intel'     ! Vendors for which to create OpenCL context in order of preference

! ---------Program variables  ---------
integer :: i
integer(c_size_t) :: kern1T, kern2T
real :: Tavg, perf
character(:), allocatable :: kernelSrc              ! Kernel source string
type(fclDevice) :: device                           ! OpenCL device on which to run
type(fclProgram) :: prog                            ! Focal program object 
type(fclKernel) :: kern1, kern2                     ! Focal kernel object
type(fclEvent) :: e
type(fclProfiler) :: profiler

real, dimension(N) :: px, py, pz, vx, vy, vz
type(fclDeviceFloat) :: pxd, pyd, pzd, vxd, vyd, vzd

! Write header
write(*,*) ('-',i=1,72)
write(*,*) '                Focal example program: nbody simulator'
write(*,'(A,I6,A,I6,A)') '                 ( NBody: ',N,'    NIteration: ',Niter,')'
write(*,*) ('-',i=1,72)
write(*,*)

! Initialise OpenCL context and select device with most cores 
device = fclInit(vendor=cl_vendor,sortBy='cores')

call fclSetDefaultCommandQ(fclCreateCommandQ(device,enableProfiling=.true., &
           outOfOrderExec=.true.,blockingWrite=.false.))

write(*,*) '  Created OpenCL command queue on device: "',device%name,'"'
write(*,'(A,I6,A,I6,A,I4,A,A,A)') '    (', device%nComputeUnits,' cores, ', &
    device%global_memory/1024/1024,'MB, ', &
    device%clock_freq, 'MHz, ',&
    device%version,')'
write(*,*) ''

! Set profiler device
profiler%device = device

! Load kernels from file and compile
call fclGetKernelResource(kernelSrc)
prog = fclCompileProgram(kernelSrc)

! Get kernel objects and set local/global work sizes
kern1 = fclGetProgramKernel(prog,'bodyForces',[N],[blockSize])
kern2 = fclGetProgramKernel(prog,'integrateBodies',[N],[blockSize])
                              
call fclProfilerAdd(profiler,Niter,kern1,kern2)

! Initialise host array data
call random_number(vx)
call random_number(vy)
call random_number(vz)

call random_number(px)
call random_number(py)
call random_number(pz)

! Initialise device arrays
call fclInitBuffer(pxd,N,profileName='pxd')
call fclInitBuffer(pyd,N,profileName='pyd')
call fclInitBuffer(pzd,N,profileName='pzd')
call fclInitBuffer(vxd,N,profileName='vxd')
call fclInitBuffer(vyd,N,profileName='vyd')
call fclInitBuffer(vzd,N,profileName='vzd')

call fclProfilerAdd(profiler,1,pxd,pyd,pzd,vxd,vyd,vzd)

! Copy data to device
pxd = px
pyd = py
pzd = pz
vxd = vx
vyd = vy
vzd = vz
call fclBarrier()
e = fclLastBarrierEvent

! Set kernel arguments once
call kern1%setArgs(N,dt,pxd,pyd,pzd,vxd,vyd,vzd)
call kern2%setArgs(N,dt,pxd,pyd,pzd,vxd,vyd,vzd)

! Main time-stepping loop
write(*,'(A)',advance='no') '  Time-stepping...'
do i=1,nIter

  call kern1%launchAfter(e)
  call kern2%launchAfter(fclLastKernelEvent)
  e = fclLastKernelEvent

end do

call fclWait()
write(*,*) ' done.'

call fclDumpProfileData(profiler)

call fclDumpTracingData(profiler,'nbody.trace')

! Calculate performance metric
kern1T = sum(fclGetEventDurations(kern1%profileEvents(1:Niter)))
kern2T = sum(fclGetEventDurations(kern2%profileEvents(1:Niter)))
Tavg = (kern1T+kern2T)/NIter

perf = N*N/Tavg

write(*,*) perf,' billion interactions per second'

end program nbody
! -----------------------------------------------------------------------------

