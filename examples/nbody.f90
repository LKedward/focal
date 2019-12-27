module nBodyUtils
  implicit none
  contains
  subroutine tecplotDump(fh,px,py,pz,t)
    integer, intent(in) :: fh
    real, intent(in), dimension(:) :: px, py, pz
    real, intent(in) :: t

    integer :: i, n
    n = size(px,1)

    write(fh,*) 'ZONE I=',n,' DATAPACKING=POINT SOLUTIONTIME=',t

    do i=1,n
      write(fh,*) px(i), py(i), pz(i)
    end do

  end subroutine tecplotDump
  ! -----------------------------------------------------------------------------

  subroutine tecplotStart(fh,filename)
    integer, intent(out) :: fh
    character(*), intent(in) :: filename

    open(newunit=fh, file=filename, status="replace")

    write(fh,*) 'TITLE="nBody"'
    write(fh,*) 'VARIABLES = "X" "Y" "Z"'

  end subroutine tecplotStart
  ! -----------------------------------------------------------------------------
end module nBodyUtils

program nbody
!! Focal example program: simple gravitational n-body simulator
!! Based on: https://github.com/ROCm-Developer-Tools/HIP-Examples/blob/master/mini-nbody/cuda/nbody-orig.cu
use nBodyUtils
use Focal
use clfortran
implicit none

integer :: N = 5000 !3E5                          ! No. of bodies
integer :: blockSize = 256
real :: dt = 1                           ! Global time-step
integer :: Niter = 5000                      ! Number of iterations to perform
integer :: saveFreq = 500

integer :: fh
integer :: i, nBlock, temp
character(:), allocatable :: kernelSrc      ! Kernel source string
type(fclDevice), allocatable :: devices(:)      ! List of focal devices
type(fclProgram) :: prog                    ! Focal program object 
type(fclKernel) :: kern1, kern2             ! Focal kernel object
type(fclEvent) :: e

real, allocatable, dimension(:) :: px, py, pz, vx, vy, vz
type(fclDeviceFloat) :: pxd, pyd, pzd, vxd, vyd, vzd

! Create context with nvidia platform
call fclSetDefaultContext(fclCreateContext(vendor='nvidia'))

! Select device with most cores and create command queue
devices = fclFindDevices(sortBy='cores')

call fclSetDefaultCommandQ(fclCreateCommandQ(devices(1),enableProfiling=.true., &
           outOfOrderExec=.true.,blockingWrite=.false.,blockingRead=.false.))

! Load kernel from file and compile
! call fclSourceFromFile('examples/sum.cl',kernelSrc)
call fclGetKernelResource(kernelSrc)
prog = fclCompileProgram(kernelSrc)
kern1 = fclGetProgramKernel(prog,'bodyForces')
kern2 = fclGetProgramKernel(prog,'integrateBodies')

! Initialise host array data
allocate(px(N))
allocate(py(N))
allocate(pz(N))
allocate(vx(N))
allocate(vy(N))
allocate(vz(N))

vx(:) = 0
vy(:) = 0
vz(:) = 0

call random_number(vx)
call random_number(vy)
call random_number(vz)

call random_number(px)
call random_number(py)
call random_number(pz)

px = px*1E8
py = py*1E8
pz = pz*1E8

vx = vx*1E2
vy = vy*1E2
vz = vz*1E2

! Initialise device arrays
pxd = fclBufferFloat(N,read=.true.,write=.true.)
pyd = fclBufferFloat(N,read=.true.,write=.true.)
pzd = fclBufferFloat(N,read=.true.,write=.true.)
vxd = fclBufferFloat(N,read=.true.,write=.true.)
vyd = fclBufferFloat(N,read=.true.,write=.true.)
vzd = fclBufferFloat(N,read=.true.,write=.true.)

! Copy data to device
pxd = px
pyd = py
pzd = pz
vxd = vx
vyd = vy
vzd = vz
call fclBarrier()
e = fclLastBarrierEvent

nBlock = (N/4+blockSize-1)/blockSize

! Set local and global work sizes
kern1%local_work_size(1) = blockSize
kern1%global_work_size(1) = nBlock*blockSize
kern2%local_work_size(1) = blockSize
kern2%global_work_size(1) = nBlock*blockSize

! call fclWait()

! read(*,*)

call tecplotStart(fh,'nBody.plt')

! Set kernel arguments once
call kern1%setArgs(N,dt,pxd,pyd,pzd,vxd,vyd,vzd)
call kern2%setArgs(N,dt,pxd,pyd,pzd,vxd,vyd,vzd)

do i=1,nIter

  call kern1%launchAfter(e)
  call kern2%launchAfter(fclLastKernelEvent)

  if (mod(i,saveFreq)==0) then
    write(*,*) 'iter ',i

    call fclSetDependency(fclLastKernelEvent,hold=.true.)
    px = pxd 
    py = pyd
    pz = pzd
    call fclClearDependencies()
    call fclBarrier()

    call fclWait(fclLastBarrierEvent)
    call tecplotDump(fh,px,py,pz,i*dt)
  end if

  ! call fclWait()

  call fclBarrier()
  e = fclLastBarrierEvent

end do

temp = clFinish(fclDefaultCmdQ%cl_command_queue)

end program nbody
! -----------------------------------------------------------------------------

