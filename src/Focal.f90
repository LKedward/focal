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
    !! Max length of OpenCL error code strings


  ! ---------------------------- FOCAL TYPES ----------------------------------
  type :: fclPlatform
    !! Type wrapper for openCL platform objects
    integer(c_intptr_t) :: cl_platform_id            !! OpenCL platform pointer
    character(:), allocatable :: profile             !! OpenCL Profile string
    character(:), allocatable :: version             !! OpenCL Version
    character(:), allocatable :: name                !! Platform name
    character(:), allocatable :: vendor              !! Platform vendor
    character(:), allocatable :: extensions          !! Platform extensions
    integer :: numDevice                             !! No. of devices
    type(fclDevice), pointer :: devices(:)           !! Focal device objects
    integer(c_intptr_t), allocatable :: cl_device_ids(:) !! openCL device pointers
  end type fclPlatform

  type :: fclContext
    !! Type wrapper for openCL context objects
    integer(c_intptr_t) :: cl_context                !! openCL context pointer
    type(fclPlatform) :: platform                    !! Focal platform object
  end type fclContext

  type :: fclDevice
    !! Type wrapper for openCL device objects
    integer(c_intptr_t) :: cl_device_id              !! OpenCL device pointer
    integer(c_int64_t) :: cl_device_type             !! Device type
    character(:), allocatable :: name                !! Device name
    integer(c_int32_t) :: nComputeUnits              !! Number of device compute units
    integer(c_int64_t) :: global_memory              !! Total global memory, bytes
    integer(c_int32_t) :: clock_freq                 !! Max clock frequency, MHz
    character(:), allocatable :: version             !! OpenCL version
  end type fclDevice

  type :: fclEvent
    !! Type wrapper for OpenCL event pointers
    type(c_ptr) :: cl_event                          !! OpenCL event pointer
  end type fclEvent

  type :: fclCommandQ
    !! Type wrapper for openCL command queue objects
    integer(c_intptr_t) :: cl_command_queue          !! openCL command Q pointer
    logical :: blockingWrite = .true.
      !! Enable/disable blocking writes when copying from host to device
    logical :: blockingRead = .true.
      !! Enable/disable block reads when copying from device to host
    type(fclEvent) :: lastWriteEvent
      !! Focal event object for the most recent write event (host-to-device) to be enqueued
    type(fclEvent) :: lastReadEvent
      !! Focal event object for the most recent read event (device-to-host) to be enqueued
    type(fclEvent) :: lastCopyEvent
      !! Focal event object for the most recent copy event (device-to-device) to be enqueued
    type(fclEvent) :: lastKernelEvent
      !! Focal event object for the most recent kernel event to be enqueued
  end type fclCommandQ

  type :: fclProgram
    !! Type wrapper for openCL program objects
    integer(c_intptr_t) :: cl_program                !! openCL program pointer
  end type fclProgram


  type :: fclKernel
    !! Type wrapper for openCL kernel objects
    integer(c_intptr_t) :: cl_kernel                 !! openCL kernel pointer
    character(:), allocatable :: name                !! Kernel name
    integer(c_int32_t) :: work_dim = 1               !! Number of work-range dimensions
    integer(c_size_t) :: global_work_offset(3) = 0   !! Global work dimension offsets
    integer(c_size_t) :: global_work_size(3) = 0     !! Global work-range dimensions
    integer(c_size_t) :: local_work_size(3) = 0      !! Local work-group dimensions
    contains
    procedure, pass :: launch => fclLaunchKernel     !! Launch the kernel
  end type fclKernel

  type :: fclDeviceBuffer
    !! Type wrapper for openCL memory objects
    integer(c_intptr_t) :: cl_mem                    !! openCL memory pointer
    type(fclCommandQ), pointer :: cmdq               !! Focal commandQ object
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

  type :: fclLocalArgument
    !! Type for specifying local kernel arguments.
    !!  Instantiate with on of: fclLocalInt32, fclLocalFloat, fclLocalDouble
    integer(c_size_t) :: nBytes                      !! Size of local argument in bytes
  end type fclLocalArgument

  ! ---------------------------- ABSTRACT INTERFACES --------------------------

  abstract interface
    subroutine fclErrorHandlerInterface(errcode,focalCall,oclCall)
      use iso_c_binding
      integer(c_int32_t), intent(in) :: errcode
      character(*), intent(in) :: focalCall
      character(*), intent(in) :: oclCall
    end subroutine fclErrorHandlerInterface
  end interface

  ! ---------------------------- GLOBAL PARAMETERS ----------------------------

  !! @note Use of global parameters must not restrict ability to use the module
  !!       asynchronously or within parallel/multithread environment @endnote

  type(fclCommandQ), target :: fclDefaultCmdQ
    !! Default command queue: used when command queue is omittetd in focal api calls

  type(fclContext), target :: fclDefaultCtx
    !! Default context: used when context is omittetd in focal api calls

  type(fclEvent), target :: fclLastWriteEvent
    !! Focal event object for the most recent write event (host-to-device) to be enqueued
  type(fclEvent), target :: fclLastReadEvent
    !! Focal event object for the most recent read event (device-to-host) to be enqueued
  type(fclEvent), target :: fclLastCopyEvent
    !! Focal event object for the most recent copy event (device-to-device) to be enqueued
  type(fclEvent), target :: fclLastKernelEvent
    !! Focal event object for the most recent kernel event to be enqueued

  procedure(fclErrorHandlerInterface), pointer :: fclErrorHandler => fclDefaultErrorHandler


  ! ---------------------------- ERROR ROUTINES -------------------------------
  interface

    module subroutine fclHandleBuildError(builderrcode,prog,ctx)
      !! Check an openCL error code and print build log if necessary
      integer, intent(in) :: builderrcode            !! OpenCL API error code
      type(fclProgram), intent(in) :: prog           !! Focal program object
      type(fclContext), intent(in) :: ctx            !! Focal context object
    end subroutine fclHandleBuildError

    module subroutine fclDefaultErrorHandler(errcode,focalCall,oclCall)
      integer(c_int32_t), intent(in) :: errcode
      character(*), intent(in) :: focalCall
      character(*), intent(in) :: oclCall
    end subroutine fclDefaultErrorHandler

    module function fclGetErrorString(errcode) result(errstr)
      !! Return the text representation for an openCL error code
      integer, intent(in) :: errcode                 !! OpenCL API error code
      character(errStringLen) :: errstr              !! Returns OpenCL error string
    end function fclGetErrorString

    module subroutine fclRuntimeError(descrip)
      !! Stop and print message for Focal errors not caused by openCL API call
      character(*), intent(in), optional :: descrip  !! Description of current API call
    end subroutine fclRuntimeError

  end interface


  ! ---------------------------- MEMORY ROUTINES -------------------------------
  interface assignment(=)
    !! Generic interface for assignment of fclBuffer objects by operator-overloading
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
      !! Interface for user-specified command queue
      type(fclCommandQ), intent(in), target :: cmdq  !! Queue with which to associate new buffer
      integer, intent(in) :: dim                     !! Dimension of new buffer
      logical, intent(in) :: read                    !! Read access of device kernels
      logical, intent(in) :: write                   !! Write access of device kernels
      type(fclDeviceDouble) :: mem                   !! Returns focal memory object
    end function fclBufferDouble_1

    module function fclBufferDouble_2(dim,read,write) result(mem)
      !! Interface to use the default command queue
      integer, intent(in) :: dim                     !! Dimension of new buffer
      logical, intent(in) :: read                    !! Read access of device kernels
      logical, intent(in) :: write                   !! Write access of device kernels
      type(fclDeviceDouble) :: mem                   !! Returns focal memory object
    end function fclBufferDouble_2

  end interface fclBufferDouble

  interface fclBufferFloat
    !! Generic interface to initialise float array on device

    module function fclBufferFloat_1(cmdq,dim,read,write) result(mem)
      !! Interface for user-specified command queue
      type(fclCommandQ), intent(in), target :: cmdq  !! Queue with which to associate new buffer
      integer, intent(in) :: dim                     !! Dimension of new buffer
      logical, intent(in) :: read                    !! Read access of device kernels
      logical, intent(in) :: write                   !! Write access of device kernels
      type(fclDeviceFloat) :: mem                    !! Returns focal memory object
    end function fclBufferFloat_1

    module function fclBufferFloat_2(dim,read,write) result(mem)
      !! Interface to use the default command queue
      integer, intent(in) :: dim                     !! Dimension of new buffer
      logical, intent(in) :: read                    !! Read access of device kernels
      logical, intent(in) :: write                   !! Write access of device kernels
      type(fclDeviceFloat) :: mem                    !! Returns focal memory object
    end function fclBufferFloat_2

  end interface fclBufferFloat


  interface fclBufferInt32
    !! Generic interface to initialise int32 array on device

    module function fclBufferInt32_1(cmdq,dim,read,write) result(mem)
      !! Interface for user-specified command queue
      type(fclCommandQ), intent(in), target :: cmdq  !! Queue with which to associate new buffer
      integer, intent(in) :: dim                     !! Dimension of new buffer
      logical, intent(in) :: read                    !! Read access of device kernels
      logical, intent(in) :: write                   !! Write access of device kernels
      type(fclDeviceInt32) :: mem                    !! Returns focal memory object
    end function fclBufferInt32_1

    module function fclBufferInt32_2(dim,read,write) result(mem)
      !! Interface to use the default command queue
      integer, intent(in) :: dim                     !! Dimension of new buffer
      logical, intent(in) :: read                    !! Read access of device kernels
      logical, intent(in) :: write                   !! Write access of device kernels
      type(fclDeviceInt32) :: mem                    !! Returns focal memory object
    end function fclBufferInt32_2

  end interface fclBufferInt32

  interface
    module function fclBuffer(cmdq,nBytes,read,write) result(cl_mem)
      !! Initialise a device memory buffer with nBytes
      type(fclCommandQ), intent(in), target :: cmdq  !! Dimension of new buffer
      integer(c_size_t), intent(in) :: nBytes        !! Size of new buffer in bytes
      logical, intent(in) :: read                    !! Read access of device kernels
      logical, intent(in) :: write                   !! Write access of device kernels
      integer(c_intptr_t) :: cl_mem                  !! Returns OpenCL memory pointer
    end function fclBuffer

    ! --------- Write scalar to device ---------

    module subroutine fclMemWriteScalar(memObject,hostBufferPtr,nBytesPattern)
      !! Fill device buffer with scalar pattern
      class(fclDeviceBuffer), intent(inout), target :: memObject   !! Focal memory object to fill
      type(c_ptr), intent(in) :: hostBufferPtr             !! C Pointer to host scalar patter
      integer(c_size_t), intent(in) :: nBytesPattern       !! Size of scalar pattern in bytes
    end subroutine fclMemWriteScalar

    module subroutine fclMemWriteScalarInt32(memObject,hostValue)
      !! Assign a scalar integer to a device integer memory buffer
      !!  Called by operator-overloading of assignment(=)
      class(fclDeviceInt32), intent(inout) :: memObject    !! Focal memory object to fill
      integer(c_int32_t), intent(in), target :: hostValue  !! Host value with which to fill
    end subroutine fclMemWriteScalarInt32

    module subroutine fclMemWriteScalarFloat(memObject,hostValue)
      !! Assign a scalar float to a device float memory buffer
      !!  Called by operator-overloading of assignment(=)
      class(fclDeviceFloat), intent(inout) :: memObject    !! Focal memory object to fill
      real(c_float), intent(in), target :: hostValue       !! Host value with which to fill
    end subroutine fclMemWriteScalarFloat

    module subroutine fclMemWriteScalarDouble(memObject,hostValue)
      !! Assign a scalar double to a device double memory buffer
      !!  Called by operator-overloading of assignment(=)
      class(fclDeviceDouble), intent(inout) :: memObject   !! Focal memory object to fill
      real(c_double), intent(in), target :: hostValue      !! Host value with which to fill
    end subroutine fclMemWriteScalarDouble

    ! --------- Write host array to device array ---------

    module subroutine fclMemWrite(memObject,hostBufferPtr,nBytes)
      !! Transfer host buffer to device buffer
      class(fclDeviceBuffer), intent(inout), target :: memObject   !! Focal memory object (target)
      type(c_ptr), intent(in) :: hostBufferPtr             !! C Pointer to host array (source)
      integer(c_size_t), intent(in) :: nBytes              !! Size of buffers in bytes
    end subroutine fclMemWrite

    module subroutine fclMemWriteInt32(memObject,hostBuffer)
      !! Transfer host integer array to device integer array
      !!  Called by operator-overloading of assignment(=)
      class(fclDeviceInt32), intent(inout) :: memObject    !! Focal memory object (target)
      integer(c_int32_t), intent(in), target :: hostBuffer(:) !! Host array (source)
    end subroutine fclMemWriteInt32

    module subroutine fclMemWriteFloat(memObject,hostBuffer)
      !! Transfer host float array to device float array
      !!  Called by operator-overloading of assignment(=)
      class(fclDeviceFloat), intent(inout) :: memObject    !! Focal memory object (target)
      real(c_float), intent(in), target :: hostBuffer(:)   !! Host array (source)
    end subroutine fclMemWriteFloat

    module subroutine fclMemWriteDouble(memObject,hostBuffer)
      !! Transfer host double array to device double array
      !!  Called by operator-overloading of assignment(=)
      class(fclDeviceDouble), intent(inout) :: memObject   !! Focal memory object (target)
      real(c_double), intent(in), target :: hostBuffer(:)  !! Host array (source)
    end subroutine fclMemWriteDouble

    ! --------- Read device array into host array ---------

    module subroutine fclMemRead(hostBufferPtr,memObject,nBytes)
      !! Transfer device buffer to host buffer
      type(c_ptr), intent(in) :: hostBufferPtr             !! C pointer to host buffer (target)
      class(fclDeviceBuffer), intent(in), target :: memObject      !! Focal memory object (source)
      integer(c_size_t), intent(in) :: nBytes              !! Size of buffers in bytes
    end subroutine fclMemRead

    module subroutine fclMemReadInt32(hostBuffer,memObject)
      !! Transfer device integer array to host integer array
      !!  Called by operator-overloading of assignment(=)
      integer(c_int32_t), intent(inout), target :: hostBuffer(:) !! Host array (target)
      class(fclDeviceInt32), intent(in) :: memObject       !! Focal memory object (source)
    end subroutine fclMemReadInt32

    module subroutine fclMemReadFloat(hostBuffer,memObject)
      !! Transfer device float array to host float array
      !!  Called by operator-overloading of assignment(=)
      real(c_float), intent(inout), target :: hostBuffer(:) !! Host array (target)
      class(fclDeviceFloat), intent(in) :: memObject       !! Focal memory object (source)
    end subroutine fclMemReadFloat

    module subroutine fclMemReadDouble(hostBuffer,memObject)
      !! Transfer device double array to host double array
      !!  Called by operator-overloading of assignment(=)
      real(c_double), intent(inout), target :: hostBuffer(:) !! Host array (target)
      class(fclDeviceDouble), intent(in) :: memObject      !! Focal memory object (source)
    end subroutine fclMemReadDouble

    ! --------- Copy device array to device array ---------

    module subroutine fclMemCopy(memObject1,memObject2)
      !! Transfer device buffer to device buffer
      class(fclDeviceBuffer), intent(inout), target :: memObject1  !! Focal memory object (target)
      class(fclDeviceBuffer), intent(in) :: memObject2     !! Focal memory object (source)
    end subroutine fclMemCopy

    module subroutine fclMemCopyInt32(memObject1,memObject2)
      !! Transfer device integer array to device integer array
      !!  Called by operator-overloading of assignment(=)
      class(fclDeviceInt32), intent(inout), target :: memObject1 !! Focal memory object (target)
      class(fclDeviceInt32), intent(in) :: memObject2      !! Focal memory object (source)
    end subroutine fclMemCopyInt32

    module subroutine fclMemCopyFloat(memObject1,memObject2)
      !! Transfer device float array to device float array
      !!  Called by operator-overloading of assignment(=)
      class(fclDeviceFloat), intent(inout), target :: memObject1 !! Focal memory object (target)
      class(fclDeviceFloat), intent(in) :: memObject2      !! Focal memory object (source)
    end subroutine fclMemCopyFloat

    module subroutine fclMemCopyDouble(memObject1,memObject2)
      !! Transfer device double array to device double array
      !!  Called by operator-overloading of assignment(=)
      class(fclDeviceDouble), intent(inout), target :: memObject1 !! Focal memory object (target)
      class(fclDeviceDouble), intent(in) :: memObject2     !! Focal memory object (source)
    end subroutine fclMemCopyDouble

    ! --------- Free device memory object ---------
    module subroutine fclFreeBuffer(memObject)
      !! Release device memory associated with memObject
      class(fclDeviceBuffer) :: memObject
    end subroutine fclFreeBuffer

  end interface





  ! ---------------------------- QUERY ROUTINES -------------------------------
  interface

    module subroutine fclGetPlatformInfo(platform,key,value)
      !! Query platform information.
      !! See [clGetPlatformInfo](https://www.khronos.org/registry/OpenCL/sdk/1.2/docs/man/xhtml/clGetPlatformInfo.html)
      !!  for values of 'key' argument containined in clfortran module.
      type(fclplatform), intent(in) :: platform
      integer(c_int32_t), intent(in) :: key
      character(:), allocatable, intent(out), target :: value
    end subroutine fclGetPlatformInfo

  end interface

  interface fclGetDeviceInfo
    !! Generic interface to query device information.
    !! See [clGetDeviceInfo](https://www.khronos.org/registry/OpenCL/sdk/1.2/docs/man/xhtml/clGetDeviceInfo.html)
    !! for values of 'key' argument contained in clfortran module.

    module subroutine fclGetDeviceInfoString(device,key,value)
      type(fclDevice), intent(in) :: device
      integer(c_int32_t), intent(in) :: key
      character(:), allocatable, intent(out), target :: value
    end subroutine fclGetDeviceInfoString

    module subroutine fclGetDeviceInfoInt32(device,key,value)
      type(fclDevice), intent(in) :: device
      integer(c_int32_t), intent(in) :: key
      integer(c_int32_t), intent(out), target :: value
    end subroutine fclGetDeviceInfoInt32

    module subroutine fclGetDeviceInfoInt64(device,key,value)
      type(fclDevice), intent(in) :: device
      integer(c_int32_t), intent(in) :: key
      integer(c_int64_t), intent(out), target :: value
    end subroutine fclGetDeviceInfoInt64

  end interface fclGetDeviceInfo

  interface

    module function fclGetPlatforms() result(platforms)
      !! Return pointer to array of available fclPlatforms
      type(fclPlatform), pointer :: platforms(:)
    end function fclGetPlatforms

    module function fclGetPlatform(platform_id) result(platform)
      !! Return fclPlatform object for OpenCL platform id
      integer(c_intptr_t), intent(in) :: platform_id !! OpenCL platform id
      type(fclPlatform), target :: platform
    end function fclGetPlatform

    module function fclGetPlatformDevices(platform_id) result(devices)
      !! Return pointer to array of fclDevices on platform id
      integer(c_intptr_t), intent(in) :: platform_id !! OpenCL platform id
      type(fclDevice), pointer :: devices(:)
    end function fclGetPlatformDevices

    module function fclGetDevice(device_id) result(device)
      !! Return fclDevice for OpenCL device id
      integer(c_intptr_t), intent(in) :: device_id   !! OpenCL device id
      type(fclDevice), target :: device
    end function fclGetDevice

  end interface


  ! ---------------------------- SETUP ROUTINES -------------------------------
  interface fclCreateContext
    !! Generic interface to create a context

    module function fclCreateContextWithPlatform(platform) result(ctx)
      !! Create a context with fclPlatform object
      type(fclPlatform), intent(inout), target :: platform
      type(fclContext), target :: ctx
    end function fclCreateContextWithPlatform

    module function fclCreateContextWithVendor(vendor) result(ctx)
      !! Create a context with the first platform where the vendor property
      !!  contains a specified string (case-insensitive).
      character(*), intent(in) :: vendor             !! String with which to match platform vendor
      type(fclContext), target :: ctx
    end function fclCreateContextWithVendor

  end interface fclCreateContext

  interface
    module subroutine fclSetDefaultContext(ctx)
      !! Set the global default context
      type(fclContext), intent(in) :: ctx
    end subroutine fclSetDefaultContext
  end interface

  interface fclFindDevices
    !! Generic interface to list devices, sorted and filtered by properties

    module function fclFindDevices_1(ctx,type,nameLike,sortBy) result(deviceList)
      type(fclContext), intent(in), target :: ctx
      character(*), intent(in), optional :: type
      character(*), intent(in), optional :: nameLike
      character(*), intent(in), optional :: sortBy
      type(fclDevice), pointer :: deviceList(:)
    end function fclFindDevices_1

    module function fclFindDevices_2(type,nameLike,sortBy) result(deviceList)
      character(*), intent(in), optional :: type
      character(*), intent(in), optional :: nameLike
      character(*), intent(in), optional :: sortBy
      type(fclDevice), pointer :: deviceList(:)
    end function fclFindDevices_2

  end interface fclFindDevices

  interface fclCreateCommandQ
    !! Generic interface to create a device command queue

    module function fclCreateCommandQ_1(ctx,device,enableProfiling,outOfOrderExec) result(cmdq)
      !! Create a command queue with a Focal device object
      type(fclContext), intent(in), target :: ctx          !! Context containing device for command queue
      type(fclDevice), intent(inout), target :: device     !! Device on which to create command queue
      logical, intent(in), optional :: enableProfiling     !! Enable OpenCL profiling
      logical, intent(in), optional :: outOfOrderExec      !! Enable out of order execution
      type(fclCommandQ) :: cmdq                            !! Returns fclCommandQ object
    end function fclCreateCommandQ_1

    module function fclCreateCommandQ_2(device,enableProfiling,outOfOrderExec) result(cmdq)
      !! Create a command queue with a Focal device object using default context
      type(fclDevice), intent(inout), target :: device     !! Device on which to create command queue
      logical, intent(in), optional :: enableProfiling     !! Enable OpenCL profiling
      logical, intent(in), optional :: outOfOrderExec      !! Enable out of order execution
      type(fclCommandQ) :: cmdq                            !! Returns fclCommandQ object
    end function fclCreateCommandQ_2

  end interface fclCreateCommandQ

  interface

    module subroutine fclSetDefaultCommandQ(cmdq)
      !! Set the global default command queue
      type(fclCommandQ), intent(in) :: cmdq

    end subroutine fclSetDefaultCommandQ

  end interface

  interface fclCompileProgram
    !! Generic interface to compile an openCL program

    module function fclCompileProgram_1(ctx,source,options) result(prog)
      !! Compile program source on context ctx
      type(fclContext), intent(in), target :: ctx
      character(*), intent(in) :: source             !! Program source code
      character(*), intent(in), optional :: options  !! OpenCL compilation options
      type(fclProgram) :: prog                       !! Returns fclProgram object
    end function fclCompileProgram_1

    module function fclCompileProgram_2(source,options) result(prog)
      !! Compile program source on fclDefaultContext
      character(*), intent(in) :: source             !! Program source code
      character(*), intent(in), optional :: options  !! OpenCL compilation options
      type(fclProgram) :: prog                       !! Returns fclProgram object
    end function fclCompileProgram_2

  end interface fclCompileProgram

  interface

    module function fclGetProgramKernel(prog,kernelName) result(kern)
      !! Extract a kernel object for execution from a compiled program object
      type(fclProgram), intent(in) :: prog           !! Compiled program object containing kernel
      character(*), intent(in) :: kernelName         !! Name of kernel to extract for execution
      type(fclKernel) :: kern                        !! Returns fclKernel object for execution
    end function fclGetProgramKernel

    module subroutine fclLaunchKernel(kernel,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
      !! Enqueue a kernel with command arguments
      class(fclKernel), intent(in), target :: kernel   !! Focal kernel object
      class(*), intent(in), optional, target :: a0
        !! Focal command queue or first kernel argument
      class(*), intent(in), optional, target :: a1,a2,a3,a4,a5,a6,a7,a8,a9,a10
        !! Subsequent kernel arguments.
        !! Can be a scalar, an fclDeviceBuffer object, or an fclLocalArgument
    end subroutine fclLaunchKernel

    module subroutine fclSetKernelArg(kernel,argIndex,argValue)
      !! Set a specific kernel argument
      type(fclKernel), intent(in) :: kernel          !! Focal kernel object
      integer(c_int32_t), intent(in) :: argIndex     !! Index of kernel argument to set
      class(*), intent(in), target :: argValue
        !! Value of kernel argument.
        !! Can be a scalar, an fclDeviceBuffer object, or an fclLocalArgument
    end subroutine fclSetKernelArg

    module function fclLocalInt32(nElem) result(localArg)
      !! Create a integer local kernel argument object for launching kernels
      integer, intent(in) :: nElem                   !! No of array elements
      type(fclLocalArgument) :: localArg             !! Returns local argument object
    end function fclLocalInt32

    module function fclLocalFloat(nElem) result(localArg)
      !! Create a float local kernel argument object for launching kernels
      integer, intent(in) :: nElem                   !! No of array elements
      type(fclLocalArgument) :: localArg             !! Returns local argument object
    end function fclLocalFloat

    module function fclLocalDouble(nElem) result(localArg)
      !! Create a double local kernel argument object for launching kernels
      integer, intent(in) :: nElem                   !! No of array elements
      type(fclLocalArgument) :: localArg             !! Returns local argument object
    end function fclLocalDouble

  end interface

  interface fclBarrier
    !! Generic interface to enqueue a command queue barrier
    !!  Wait on device for all preceding queue events to complete before
    !!  subsequent events can proceed.

    module subroutine fclBarrier_1(cmdq)
      !! Enqueue barrier on all events in command queue
      type(fclCommandQ), intent(in) :: cmdq
    end subroutine fclBarrier_1

    module subroutine fclBarrier_2()
      !! Enqueue barrier on all events in default command queue
    end subroutine fclBarrier_2

  end interface fclBarrier

  interface fclWait
    !! Generic interface to wait on host for events

    module subroutine fclFinish_1(cmdq)
      !! Wait on host for all events in user-specified command queue
      type(fclCommandQ), intent(in) :: cmdq
    end subroutine fclFinish_1

    module subroutine fclFinish_2()
      !! Wait on host for all events in focal default command queue
    end subroutine fclFinish_2

    module subroutine fclWaitEvent(event)
      !! Wait on host for a specific event
      type(fclEvent), intent(in), target :: event
    end subroutine fclWaitEvent

    module subroutine fclWaitEventList(eventList)
      !! Wait on host for set of events
      type(fclEvent), intent(in), target :: eventList(:)
    end subroutine fclWaitEventList

  end interface fclWait


  ! ---------------------------- UTILITY ROUTINES -------------------------------

  interface

    module subroutine fclGetKernelResource(kernelString)
      !! Retrieve kernel source linked as a binary resource.
      !!  Use linker ld to include kernel source with:
      !!   ld -r -b binary -o fclKernels.o fclKernels.cl
      !! (Object file MUST be called fclKernels.o, with no path)
      !! Then link resulting object file as normal
      character(:), allocatable, intent(out) :: kernelString
        !! Kernel source as fortran character string
    end subroutine fclGetKernelResource

    module function upperstr(linei)
      !! Return copy of string converted to uppercase
      !! Used for case-insensitive string comparison
      character(len=*),intent(in) :: linei
        !! Input string to convert to uppercase
      character(len=len(linei)) upperstr
        !! Converted string output
    end function upperstr

    module subroutine fclSourceFromFile(filename,sourceString)
      !! Allocate and fill character string from file
      character(*), intent(in) :: filename
      character(:), intent(out), allocatable :: sourceString
    end subroutine fclSourceFromFile

  end interface

end module Focal
