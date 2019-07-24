program sum
!! Focal example program: calculate the sum of two arrays
!!

use Focal
implicit none

integer, parameter :: Nelem = 1E6   ! No. of array elements
real, parameter :: sumVal = 10.0    ! Target value for array sum
integer, parameter :: iPlat = 1     ! OpenCL Platform index
integer, parameter :: iDev = 1      ! OpenCL device index

character(:), allocatable :: kernelSrc
integer :: i

interface
  subroutine loadSource(filename,sourceString)
    character(*), intent(in) :: filename
    character(:), intent(out), allocatable :: sourceString
  end subroutine loadSource
end interface

! Focal structures
type(fclPlatform), pointer :: platforms(:)  ! Platform list
type(fclContext) :: ctx                     ! Context
type(fclCommandQ) :: cmdq                   ! Command queue
type(fclProgram) :: prog                    ! Program
type(fclKernel) :: sumKernel                ! Kernel

! Host arrays
real(c_float) :: array1(Nelem)
real(c_float) :: array2(Nelem)

! Device arrays
type(fclDeviceFloat) :: array1_d
type(fclDeviceFloat) :: array2_d

platforms => fclGetPlatforms();

write(*,'(A,I4,A)') 'Creating context with platform ',iPlat,' ...'
ctx = fclCreateContext(platforms(iPlat))

write(*,'(A,I4,A)') 'Creating command queue with device ',iDev,' ...'
cmdq = fclCreateDeviceCommandQ(ctx,platforms(iPlat)%devices(iDev),enableProfiling=.true.)

fclDefaultCmdQ = cmdQ
fclDefaultCtx = ctx

write(*,*) 'Reading kernel...'
call loadSource('examples/sum.cl',kernelSrc)

write(*,*) ' Begin kernel source listing:'
write(*,*) kernelSrc
write(*,*)

write(*,*) 'Compiling kernel...'
prog = fclCompileProgram(kernelSrc)
sumKernel = fclGetProgramKernel(prog,'sum')

write(*,*) 'Initialising device memory...'
array1_d = fclBufferFloat(Nelem,read=.true.,write=.false.)
array2_d = fclBufferFloat(Nelem,read=.true.,write=.true.)

! Setup host array data
do i=1,Nelem
  array1(i) = i
  array2(i) = sumVal - i
end do

write(*,*) 'Copying host data to device memory...'
array1_d = array1
array2_d = array2

write(*,*) 'Launching kernel...'
sumKernel%global_work_size(1) = Nelem
call sumKernel%launch(Nelem,array1_d,array2_d)

write(*,*) 'Copying result from device to host...'
array2 = array2_d
write(*,*) array2(1), array2(size(array2,1))

end program sum
! -----------------------------------------------------------------------------


subroutine loadSource(filename,sourceString)
  !! Allocae and fill character string from file
  character(*), intent(in) :: filename
  character(:), intent(out), allocatable :: sourceString

  integer :: fh, iLen, ioStat, i
  character(1) :: char

  ! --- First pass: get kernel source length ---
  open(newunit=fh,file=filename,status='old',access='direct',recl=1)
  iLen = 1
  iostat = 0
  do while(iostat == 0)
    read(fh,rec=iLen,iostat=iostat) char
    iLen = iLen + 1
  enddo
  iLen = iLen - 2
  close(fh)

  allocate(character(len=iLen) :: sourceString)

  ! --- Second pass: read kernel source into buffer ---
  open(newunit=fh,file=filename,status='old',access='direct',recl=1)
  do i=1,iLen
      read(fh,rec=i) char
      sourceString(i:i) = char
  end do
  close(fh)

end subroutine loadSource
