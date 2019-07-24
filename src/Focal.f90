module Focal
  !! FOCAL: openCL abstraction layer for fortran
  !!  Header module for all focal parameters, types and interfaces

  !! @note This is a header module: it contains subroutine interface definitions only.
  !! Subroutine implementation (code) is found in the corresponding submodule files. @endnote


  use, intrinsic :: iso_fortran_env, only: real32, real64
  use, intrinsic :: iso_c_binding
  implicit none



  ! ---------------------------- CONSTANT PARAMETERS --------------------------

  integer, parameter :: errStringLen = 50


  ! ---------------------------- FOCAL TYPES ----------------------------------

  type :: fclContext
    !! Type wrapper for openCL context objects
    integer(c_intptr_t) :: cl_context                !! openCL context pointer
    type(fclPlatform), pointer :: platform           !! Focal platform object
  end type fclContext

  type :: fclPlatform
    !! Type wrapper for openCL platform objects
    integer(c_intptr_t) :: cl_platform_id            !! openCL platform pointer
    character, allocatable :: profile(:)
    character, allocatable :: version(:)
    character, allocatable :: name(:)
    character, allocatable :: vendor(:)
    character, allocatable :: extensions(:)
    type(fclContext) :: ctx                          !! Focal context object
    integer :: numDevice                             !! No. of devices
    type(fclDevice), pointer :: devices(:)           !! Focal device objects
    integer(c_intptr_t), allocatable :: cl_device_ids(:) !! openCL device pointers
  end type fclPlatform

  type :: fclDevice
    !! Type wrapper for openCL device objects
    integer(c_intptr_t) :: cl_device_id              !! openCL device pointer
    integer(c_int64_t) :: cl_device_type             !! openCL type
    character, allocatable :: name(:)
    integer(c_int32_t) :: nComputeUnits
  end type fclDevice

  type :: fclCommandQ
    !! Type wrapper for openCL command queue objects
    integer(c_intptr_t) :: cl_command_queue          !! openCL command Q pointer
  end type fclCommandQ

  type :: fclProgram
    !! Type wrapper for openCL program objects
    integer(c_intptr_t) :: cl_program                !! openCL program pointer
  end type fclProgram

  type :: fclKernel
    !! Type wrapper for openCL kernel objects
    integer(c_intptr_t) :: cl_kernel                 !! openCL kernel pointer
    character(:), allocatable :: name                !! Kernel name
    integer(c_int32_t) :: work_dim = 1               !! No. of dimensions
    integer(c_size_t) :: global_work_offset(3) = 0   !!
    integer(c_size_t) :: global_work_size(3) = 0
    integer(c_size_t) :: local_work_size(3) = 0
    contains
    procedure, pass :: launch => fclLaunchKernel
  end type fclKernel

  type :: fclDeviceBuffer
    !! Type wrapper for openCL memory objects
    integer(c_intptr_t) :: cl_mem                    !! openCL memory pointer
    type(fclCommandQ) :: cmdq                        !! Focal commandQ object
    integer(c_size_t) :: nBytes = -1                 !! Size of buffer in bytes
  end type fclDeviceBuffer

  type, extends(fclDeviceBuffer) :: fclDeviceInt32
    !! Type wrapper for memory objects representing int32
  end type fclDeviceInt32

  type, extends(fclDeviceBuffer) :: fclDeviceFloat
    !! Type wrapper for memory objects representing float
  end type fclDeviceFloat

  type, extends(fclDeviceBuffer) :: fclDeviceDouble
    !! Type wrapper for memory objects representing double
  end type fclDeviceDouble


  ! ---------------------------- GLOBAL PARAMETERS ----------------------------

  type(fclCommandQ) :: fclDefaultCmdQ
    !! Default command queue: used when command queue is omittetd in focal api calls

  type(fclContext) :: fclDefaultCtx
    !! Default context: used when context is omittetd in focal api calls

  logical :: fclBlockingWrite = .true.
    !! Enable/disable blocking writes when copying from host to device
  logical :: fclBlockingRead = .true.
    !! Enable/disable block reads when copying from device to host

  type(c_ptr) :: fclLastWriteEvent
    !! openCL pointer to the most recent write event (host-to-device) to be enqueued
  type(c_ptr) :: fclLastReadEvent
    !! openCL pointer to the most recent read event (device-to-host) to be enqueued
  type(c_ptr) :: fclLastCopyEvent
    !! openCL pointer to the most recent copy event (device-to-device) to be enqueued
  type(c_ptr) :: fclLastKernelEvent
    !! openCL pointer to the most recent kernel event to be enqueued




  ! ---------------------------- ERROR ROUTINES -------------------------------
  interface

    module subroutine fclHandleBuildError(builderrcode,prog,ctx)
      !! Check an openCL error code and print build log if necessary
      integer, intent(in) :: builderrcode
      type(fclProgram), intent(in) :: prog
      type(fclContext), intent(in) :: ctx
    end subroutine fclHandleBuildError

    module subroutine fclHandleErrorCode(errcode,descrip,stopnow)
      !! Check an openCL error code: stops and prints error if noT CL_SUCCESS
      integer(c_int32_t), intent(in) :: errcode
      character(*), intent(in), optional :: descrip
      logical, intent(in), optional :: stopnow
    end subroutine fclHandleErrorCode

    module function fclGetErrorString(errcode) result(errstr)
      !! Return the text representation for an openCL error code
      integer, intent(in) :: errcode
      character(errStringLen) :: errstr
    end function fclGetErrorString

    module subroutine fclRuntimeError(descrip)
      !! Stop and print message for Focal errors not caused by openCL API call
      character(*), intent(in), optional :: descrip
    end subroutine fclRuntimeError

  end interface




  ! ---------------------------- MEMORY ROUTINES -------------------------------
  interface assignment(=)
    !! Generic interface for assignment of fclBuffer objects
    procedure :: fclMemWriteScalarInt32
    procedure :: fclMemWriteScalarFloat
    procedure :: fclMemWriteScalarDouble
    procedure :: fclMemWriteInt32
    procedure :: fclMemWriteFloat
    procedure :: fclMemWriteDouble
    procedure :: fclMemReadInt32
    procedure :: fclMemReadFloat
    procedure :: fclMemReadDouble
    procedure :: fclMemCopyInt32
    procedure :: fclMemCopyFloat
    procedure :: fclMemCopyDouble
  end interface

  ! --------- Buffer Creation ---------

  interface fclBufferDouble
    !! Generic interface to initialise double array on device

    module function fclBufferDouble_1(cmdq,dim,read,write) result(mem)
      type(fclCommandQ), intent(in) :: cmdq
      integer, intent(in) :: dim
      logical, intent(in) :: read
      logical, intent(in) :: write
      type(fclDeviceDouble) :: mem
    end function fclBufferDouble_1

    module function fclBufferDouble_2(dim,read,write) result(mem)
      integer, intent(in) :: dim
      logical, intent(in) :: read
      logical, intent(in) :: write
      type(fclDeviceDouble) :: mem
    end function fclBufferDouble_2

  end interface fclBufferDouble

  interface fclBufferFloat
    !! Generic interface to initialise float array on device

    module function fclBufferFloat_1(cmdq,dim,read,write) result(mem)
      type(fclCommandQ), intent(in) :: cmdq
      integer, intent(in) :: dim
      logical, intent(in) :: read
      logical, intent(in) :: write
      type(fclDeviceFloat) :: mem
    end function fclBufferFloat_1

    module function fclBufferFloat_2(dim,read,write) result(mem)
      integer, intent(in) :: dim
      logical, intent(in) :: read
      logical, intent(in) :: write
      type(fclDeviceFloat) :: mem
    end function fclBufferFloat_2

  end interface fclBufferFloat


  interface fclBufferInt32
    !! Generic interface to initialise int32 array on device

    module function fclBufferInt32_1(cmdq,dim,read,write) result(mem)
      type(fclCommandQ), intent(in) :: cmdq
      integer, intent(in) :: dim
      logical, intent(in) :: read
      logical, intent(in) :: write
      type(fclDeviceInt32) :: mem
    end function fclBufferInt32_1

    module function fclBufferInt32_2(dim,read,write) result(mem)
      integer, intent(in) :: dim
      logical, intent(in) :: read
      logical, intent(in) :: write
      type(fclDeviceInt32) :: mem
    end function fclBufferInt32_2

  end interface fclBufferInt32

  interface
    module function fclBuffer(cmdq,nBytes,read,write) result(cl_mem)
      type(fclCommandQ), intent(in), target :: cmdq
      integer(c_size_t), intent(in) :: nBytes
      logical, intent(in) :: read
      logical, intent(in) :: write
      integer(c_intptr_t) :: cl_mem
    end function fclBuffer

    ! --------- Write scalar to device ---------

    module subroutine fclMemWriteScalar(memObject,hostBufferPtr,nBytesPattern)
      class(fclDeviceBuffer), intent(inout) :: memObject
      type(c_ptr), intent(in) :: hostBufferPtr
      integer(c_size_t), intent(in) :: nBytesPattern
    end subroutine fclMemWriteScalar

    module subroutine fclMemWriteScalarInt32(memObject,hostValue)
      class(fclDeviceInt32), intent(inout) :: memObject
      integer(c_int32_t), intent(in), target :: hostValue
    end subroutine fclMemWriteScalarInt32

    module subroutine fclMemWriteScalarFloat(memObject,hostValue)
      class(fclDeviceFloat), intent(inout) :: memObject
      real(c_float), intent(in), target :: hostValue
    end subroutine fclMemWriteScalarFloat

    module subroutine fclMemWriteScalarDouble(memObject,hostValue)
      class(fclDeviceDouble), intent(inout) :: memObject
      real(c_double), intent(in), target :: hostValue
    end subroutine fclMemWriteScalarDouble

    ! --------- Write host array to device array ---------

    module subroutine fclMemWrite(memObject,hostBufferPtr,nBytes)
      class(fclDeviceBuffer), intent(inout) :: memObject
      type(c_ptr), intent(in) :: hostBufferPtr
      integer(c_size_t), intent(in) :: nBytes
    end subroutine fclMemWrite

    module subroutine fclMemWriteInt32(memObject,hostBuffer)
      class(fclDeviceInt32), intent(inout) :: memObject
      integer(c_int32_t), intent(in), target :: hostBuffer(:)
    end subroutine fclMemWriteInt32

    module subroutine fclMemWriteFloat(memObject,hostBuffer)
      class(fclDeviceFloat), intent(inout) :: memObject
      real(c_float), intent(in), target :: hostBuffer(:)
    end subroutine fclMemWriteFloat

    module subroutine fclMemWriteDouble(memObject,hostBuffer)
      class(fclDeviceDouble), intent(inout) :: memObject
      real(c_double), intent(in), target :: hostBuffer(:)
    end subroutine fclMemWriteDouble

    ! --------- Read device array into host array ---------

    module subroutine fclMemRead(hostBufferPtr,memObject,nBytes)
      type(c_ptr), intent(in) :: hostBufferPtr
      class(fclDeviceBuffer), intent(in) :: memObject
      integer(c_size_t), intent(in) :: nBytes
    end subroutine fclMemRead

    module subroutine fclMemReadInt32(hostBuffer,memObject)
      integer(c_int32_t), intent(inout), target :: hostBuffer(:)
      class(fclDeviceInt32), intent(in) :: memObject
    end subroutine fclMemReadInt32

    module subroutine fclMemReadFloat(hostBuffer,memObject)
      real(c_float), intent(inout), target :: hostBuffer(:)
      class(fclDeviceFloat), intent(in) :: memObject
    end subroutine fclMemReadFloat

    module subroutine fclMemReadDouble(hostBuffer,memObject)
      real(c_double), intent(inout), target :: hostBuffer(:)
      class(fclDeviceDouble), intent(in) :: memObject
    end subroutine fclMemReadDouble

    ! --------- Copy device array to device array ---------

    module subroutine fclMemCopy(memObject1,memObject2)
      class(fclDeviceBuffer), intent(inout) :: memObject1
      class(fclDeviceBuffer), intent(in) :: memObject2
    end subroutine fclMemCopy

    module subroutine fclMemCopyInt32(memObject1,memObject2)
      class(fclDeviceInt32), intent(inout), target :: memObject1
      class(fclDeviceInt32), intent(in) :: memObject2
    end subroutine fclMemCopyInt32

    module subroutine fclMemCopyFloat(memObject1,memObject2)
      class(fclDeviceFloat), intent(inout), target :: memObject1
      class(fclDeviceFloat), intent(in) :: memObject2
    end subroutine fclMemCopyFloat

    module subroutine fclMemCopyDouble(memObject1,memObject2)
      class(fclDeviceDouble), intent(inout), target :: memObject1
      class(fclDeviceDouble), intent(in) :: memObject2
    end subroutine fclMemCopyDouble

  end interface





  ! ---------------------------- QUERY ROUTINES -------------------------------
  interface

    module function fclGetPlatforms() result(platforms)
      type(fclPlatform), pointer :: platforms(:)
    end function fclGetPlatforms

    module function fclGetPlatform(platform_id) result(platform)
      integer(c_intptr_t), intent(in) :: platform_id
      type(fclPlatform), target :: platform
    end function fclGetPlatform

    module function fclGetPlatformDevices(platform_id) result(devices)
      integer(c_intptr_t), intent(in) :: platform_id
      type(fclDevice), pointer :: devices(:)
    end function fclGetPlatformDevices

    module function fclGetDevice(device_id) result(device)
      integer(c_intptr_t), intent(in) :: device_id
      type(fclDevice), target :: device
    end function fclGetDevice

  end interface


  ! ---------------------------- SETUP ROUTINES -------------------------------
  interface

    module function fclCreateContext(platform) result(ctx)
      type(fclPlatform), intent(inout), target :: platform
      type(fclContext), target :: ctx
    end function fclCreateContext

    module function fclCreateDeviceCommandQ(ctx,device,enableProfiling,outOfOrderExec) result(cmdq)
      type(fclContext), intent(in), target :: ctx
      type(fclDevice), intent(inout), target :: device
      logical, intent(in), optional :: enableProfiling
      logical, intent(in), optional :: outOfOrderExec
      type(fclCommandQ) :: cmdq
    end function fclCreateDeviceCommandQ

  end interface

  interface fclCompileProgram
    !! Generic interface to compile an openCL program

    module function fclCompileProgram_1(ctx,source,options) result(prog)
      type(fclContext), intent(in), target :: ctx
      character(*), intent(in) :: source
      character(*), intent(in), optional :: options
      type(fclProgram) :: prog
    end function fclCompileProgram_1

    module function fclCompileProgram_2(source,options) result(prog)
      character(*), intent(in) :: source
      character(*), intent(in), optional :: options
      type(fclProgram) :: prog
    end function fclCompileProgram_2

  end interface fclCompileProgram

  interface

    module function fclGetProgramKernel(prog,kernelName) result(kern)
      type(fclProgram), intent(in) :: prog
      character(*), intent(in) :: kernelName
      type(fclKernel) :: kern
    end function fclGetProgramKernel

    module subroutine fclLaunchKernel(kernel,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
      !! Enqueue a kernel with command arguments
      class(fclKernel), intent(in), target :: kernel   !! Focal kernel object
      class(*), intent(in), optional, target :: a0
        !! Focal command queue or first kernel argument
      class(*), intent(in), optional, target :: a1,a2,a3,a4,a5,a6,a7,a8,a9,a10
        !! Subsequent kernel arguments
    end subroutine fclLaunchKernel

    module subroutine fclSetKernelArg(kernel,argIndex,argValue)
      type(fclKernel), intent(in) :: kernel
      integer(c_int32_t), intent(in) :: argIndex
      class(*), intent(in), target :: argValue
    end subroutine fclSetKernelArg

  end interface

end module Focal
