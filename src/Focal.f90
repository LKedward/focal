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

  integer, parameter :: fclAllocationSize = 10
    !! Default allocation increment for dynamically growing lists

  integer, parameter :: CL_PLATFORM_NOT_FOUND_KHR = -1001
    !! Extension error: No valid ICDs found

  integer, parameter :: NV_ILLEGAL_BUFFER_READ_WRITE = -9999
    !! Vendor error: Illegal read or write to a buffer in NDRangeKernel

  ! ---------------------------- FOCAL TYPES ----------------------------------
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

  type :: fclPlatform
    !! Type wrapper for openCL platform objects
    integer(c_intptr_t) :: cl_platform_id            !! OpenCL platform pointer
    character(:), allocatable :: profile             !! OpenCL Profile string
    character(:), allocatable :: version             !! OpenCL Version
    character(:), allocatable :: name                !! Platform name
    character(:), allocatable :: vendor              !! Platform vendor
    character(:), allocatable :: extensions          !! Platform extensions
    integer :: numDevice                             !! No. of devices
    type(fclDevice), allocatable :: devices(:)           !! Focal device objects
    integer(c_intptr_t), allocatable :: cl_device_ids(:) !! openCL device pointers
  end type fclPlatform

  type :: fclContext
    !! Type wrapper for openCL context objects
    integer(c_intptr_t) :: cl_context                !! openCL context pointer
    type(fclPlatform) :: platform                    !! Focal platform object
  end type fclContext

    type :: fclEvent
    !! Type wrapper for OpenCL event pointers
    integer(c_intptr_t) :: cl_event                          !! OpenCL event pointer
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
    type(fclEvent) :: lastBarrierEvent
      !! Focal event object for the most recent barrier event to be enqueued
    integer(c_intptr_t), allocatable :: dependencyList(:)
      !! List of pre-requisite events for next enqueued action.
      !!  All events in this list are used as dependencies for the next enqueued
      !!   operation. At enqueueing, the list is cleared unless holdDependencies is .true.
    type(c_ptr) :: dependencyListPtr = C_NULL_PTR
      !! C pointer to dependency list. C_NULL_PTR when nDependency is zero.
    integer :: nDependency = 0
      !! Number of items in dependency list
    logical :: holdDependencies = .false.
      !! Set to true to not automatically clear dependencies after enqueueing.
      !! Use for applying the same dependencies to multiple commands.
      !! Use fclClearDependencies to clear and reset.
  end type fclCommandQ

  type :: fclCommandQPool
    !! Collection of fclCommandQ objects with round-robin scheduling.
    !!  Allows easy handling of multiple command queues for parallel kernels
    !!  data transfers.
    integer :: length
      !! Number of command queues
    type(fclCommandQ), allocatable :: queues(:)
      !! Array of command queues
    integer :: idx = 1
      !! Index of current command queue
    contains
    procedure, pass :: next => fclCommandQPool_Next
      !! Returns next scheduled queue in queue pool
    procedure, pass :: current => fclCommandQPool_Current
      !! Returns current scheduled queue in queue pool
  end type fclCommandQPool

  type :: fclProgram
    !! Type wrapper for openCL program objects
    integer(c_intptr_t) :: cl_program                !! openCL program pointer
  end type fclProgram

 type :: fclKernelPointer
    !! Wrapper type for implementing an array of pointers to kernel objects
    class(fclKernel), pointer :: target
  end type fclKernelPointer

  type :: fclBufferPointer
    !! Wrapper type for implementing an array of pointers to buffer objects
    class(fclDeviceBuffer), pointer :: target
  end type fclBufferPointer


  type :: fclProfiler
    !! Helper type to collect objects (kernels and buffers) that
    !!  are profiled to simply user code.
    type(fclDevice) :: device
      !! Device for which to dump profile data
    type(fclKernelPointer), allocatable :: kernels(:)
      !! List of pointers to kernels to be profiled
    integer :: nKernels = 0
      !! Number of kernels in kernels array
    type(fclBufferPointer), allocatable :: buffers(:)
      !! List of pointers to buffers to be profiled
    integer :: nBuffers = 0
      !! Number of buffers in buffers array
    contains
      procedure, pass :: add => fclProfilerAdd
  end type fclProfiler

   type :: fclProfileContainer
    !! Base container type for event profiling
    character(:), allocatable :: profileName
      !! Descriptive name for output of profiling information
    logical :: profilingEnabled = .false.
      !! Switch to enable saving of events for profiling
    type(fclEvent), allocatable :: profileEvents(:)
      !! Array of events for profiling
    integer :: profileSize = 0
      !! Allocation size of profileEvents(:) array
    integer :: nProfileEvent = 0
      !! Number of events saved to profileEvents(:) array
    contains
      ! procedure, pass :: enableProfiling => fclEnableProfiling
      procedure, pass :: pushProfileEvent => fclPushProfileEvent
      ! procedure, pass :: dumpProfileData => fclDumpProfileData
  end type fclProfileContainer

  type, extends(fclProfileContainer) :: fclKernel
    !! Type wrapper for openCL kernel objects
    integer(c_intptr_t) :: cl_kernel                 !! openCL kernel pointer
    character(:), allocatable :: name                !! Kernel name
    integer(c_int32_t) :: work_dim = 1               !! Number of work-range dimensions
    integer(c_size_t) :: global_work_offset(3) = 0   !! Global work dimension offsets
    integer(c_size_t) :: global_work_size(3) = 0     !! Global work-range dimensions
    integer(c_size_t) :: local_work_size(3) = 0      !! Local work-group dimensions
    contains
    procedure, pass :: setArgs => fclSetKernelArgs         !! Set kernel arguments without launching
    procedure, pass :: launch => fclLaunchKernel           !! Launch the kernel
    procedure, pass, private :: launchKernelAfterEvent_1 => fclLaunchKernelAfterEvent_1
    procedure, pass, private :: launchKernelAfterEvent_2 => fclLaunchKernelAfterEvent_2
    procedure, pass, private :: launchKernelAfterEventList_1 => fclLaunchKernelAfterEventList_1
    procedure, pass, private :: launchKernelAfterEventList_2 => fclLaunchKernelAfterEventList_2
    generic :: launchAfter => launchKernelAfterEvent_1, launchKernelAfterEvent_2, &
           launchKernelAfterEventList_1, launchKernelAfterEventList_2
     !! Launch a kernel with event dependencies
  end type fclKernel

  type, extends(fclProfileContainer) :: fclDeviceBuffer
    !! Type wrapper for openCL memory objects
    integer(c_intptr_t) :: cl_mem                    !! openCL memory pointer
    type(fclCommandQ), pointer :: cmdq               !! Focal commandQ object
    integer(c_size_t) :: nBytes = -1                 !! Size of buffer in bytes
    integer, allocatable :: profileEventType(:)      !! Integer for indicating type of buffer event
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

  type, extends(fclLocalArgument) :: fclLocalArgInt32
    !! Type wrapper for local kernel arguments representing 32 bit integers
  end type fclLocalArgInt32

  type, extends(fclLocalArgument) :: fclLocalArgFloat
    !! Type wrapper for local kernel arguments representing floats
  end type fclLocalArgFloat

  type, extends(fclLocalArgument) :: fclLocalArgDouble
    !! Type wrapper for local kernel arguments representing doubles
  end type fclLocalArgDouble

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
  type(fclEvent), target :: fclLastBarrierEvent
    !! Focal event object for the most recent barrier event to be enqueued

  character(len=1,kind=c_char), target, bind(C,name="_binary_fclKernels_cl_start") :: fclKernelStart
    !! c interoperable character for start of fclKernels binary resource
  character(len=1,kind=c_char), target, bind(C,name="_binary_fclKernels_cl_end") :: fclKernelEnd
    !! c interoperable character for sendtart of fclKernels binary resource

  procedure(fclErrorHandlerInterface), pointer :: fclErrorHandler => fclDefaultErrorHandler
    !! Procedure pointer for custom OpenCL runtime error handler

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

  ! -------------------------- HOST MEMORY ROUTINES -----------------------------

  ! --------- Pinned memory allocation ---------

  interface
    module function fclAllocHostPtr(cmdq,nBytes) result(ptr)
      !! Allocate a 'pinned' (non-paged) host array
      type(fclCommandQ), intent(in) :: cmdq
        !! Command Q with which to associate the allocated device memory
      integer(c_int64_t), intent(in) :: nBytes
        !! Desired array size in bytes
      type(c_ptr) :: ptr
        !! c pointer to allocated host memory
    end function fclAllocHostPtr
  end interface

  interface fclAllocHost
    !! Generic interface for allocating host arrays using 
    !!  'pinned' (non-paged) memory. This is required for asynchronous transfers.
    !!
    !! Currently implements interfaces for 1D and 2D int32, float and double arrays.
    !!
    !!
    !! __Example:__
    !!  Allocate a 1D integer array with 100 elements
    !!
    !! `integer, pointer :: hostArray(:)`
    !!
    !! `call fclAllocHost(cmdq,hostArray,100)`
    !!
    !! __NB:__ `cmdq` is optional, if omitted then the default command queue is used

    module subroutine fclAllocHostInt32D1_1(cmdq,hostPtr,dim)
      !! Allocate a 1D 'pinned' host array for 32bit integers
      type(fclCommandQ), intent(in) :: cmdq
        !! Command Q with which to associate the allocated device memory
      integer(c_int32_t), intent(inout), pointer :: hostPtr(:)
        !! Host array pointer to allocate
      integer, intent(in) :: dim
        !! Size of array to allocate
    end subroutine fclAllocHostInt32D1_1

    module subroutine fclAllocHostInt32D1_2(hostPtr,dim)
      !! Allocate a 1D 'pinned' host array for 32bit integers on default cmdq
      integer(c_int32_t), intent(inout), pointer :: hostPtr(:)
        !! Host array pointer to allocate
      integer, intent(in) :: dim
        !! Size of array to allocate
    end subroutine fclAllocHostInt32D1_2

    module subroutine fclAllocHostInt32D2_1(cmdq,hostPtr,dim)
      !! Allocate a 2D 'pinned' host array for 32bit integers
      type(fclCommandQ), intent(in) :: cmdq
        !! Command Q with which to associate the allocated device memory
      integer(c_int32_t), intent(inout), pointer :: hostPtr(:,:)
        !! Host array pointer to allocate
      integer, intent(in) :: dim(2)
        !! Size of array to allocate
    end subroutine fclAllocHostInt32D2_1

    module subroutine fclAllocHostInt32D2_2(hostPtr,dim)
      !! Allocate a 2D 'pinned' host array for 32bit integers on default cmdq
      integer(c_int32_t), intent(inout), pointer :: hostPtr(:,:)
        !! Host array pointer to allocate
      integer, intent(in) :: dim(2)
        !! Size of array to allocate
    end subroutine fclAllocHostInt32D2_2
    
    module subroutine fclAllocHostFloatD1_1(cmdq,hostPtr,dim)
      !! Allocate a 1D 'pinned' host array for 32bit reals
      type(fclCommandQ), intent(in) :: cmdq
        !! Command Q with which to associate the allocated device memory
      real(c_Float), intent(inout), pointer :: hostPtr(:)
        !! Host array pointer to allocate
      integer, intent(in) :: dim
        !! Size of array to allocate
    end subroutine fclAllocHostFloatD1_1

    module subroutine fclAllocHostFloatD1_2(hostPtr,dim)
      !! Allocate a 1D 'pinned' host array for 32bit reals on default cmdq
      real(c_Float), intent(inout), pointer :: hostPtr(:)
        !! Host array pointer to allocate
      integer, intent(in) :: dim
        !! Size of array to allocate
    end subroutine fclAllocHostFloatD1_2

    module subroutine fclAllocHostFloatD2_1(cmdq,hostPtr,dim)
      !! Allocate a 2D 'pinned' host array for 32bit reals
      type(fclCommandQ), intent(in) :: cmdq
        !! Command Q with which to associate the allocated device memory
      real(c_Float), intent(inout), pointer :: hostPtr(:,:)
        !! Host array pointer to allocate
      integer, intent(in) :: dim(2)
        !! Size of array to allocate
    end subroutine fclAllocHostFloatD2_1

    module subroutine fclAllocHostFloatD2_2(hostPtr,dim)
      !! Allocate a 2D 'pinned' host array for 32bit reals on default cmdq
      real(c_Float), intent(inout), pointer :: hostPtr(:,:)
        !! Host array pointer to allocate
      integer, intent(in) :: dim(2)
        !! Size of array to allocate
    end subroutine fclAllocHostFloatD2_2

    module subroutine fclAllocHostDoubleD1_1(cmdq,hostPtr,dim)
      !! Allocate a 1D 'pinned' host array for 64bit reals
      type(fclCommandQ), intent(in) :: cmdq
        !! Command Q with which to associate the allocated device memory
      real(c_Double), intent(inout), pointer :: hostPtr(:)
        !! Host array pointer to allocate
      integer, intent(in) :: dim
        !! Size of array to allocate
    end subroutine fclAllocHostDoubleD1_1

    module subroutine fclAllocHostDoubleD1_2(hostPtr,dim)
      !! Allocate a 1D 'pinned' host array for 64bit reals on default cmdq
      real(c_Double), intent(inout), pointer :: hostPtr(:)
        !! Host array pointer to allocate
      integer, intent(in) :: dim
        !! Size of array to allocate
    end subroutine fclAllocHostDoubleD1_2

    module subroutine fclAllocHostDoubleD2_1(cmdq,hostPtr,dim)
      !! Allocate a 2D 'pinned' host array for 64bit reals
      type(fclCommandQ), intent(in) :: cmdq
        !! Command Q with which to associate the allocated device memory
      real(c_Double), intent(inout), pointer :: hostPtr(:,:)
        !! Host array pointer to allocate
      integer, intent(in) :: dim(2)
        !! Size of array to allocate
    end subroutine fclAllocHostDoubleD2_1

    module subroutine fclAllocHostDoubleD2_2(hostPtr,dim)
      !! Allocate a 2D 'pinned' host array for 64bit reals on default cmdq
      real(c_Double), intent(inout), pointer :: hostPtr(:,:)
        !! Host array pointer to allocate
      integer, intent(in) :: dim(2)
        !! Size of array to allocate
    end subroutine fclAllocHostDoubleD2_2

  end interface fclAllocHost
  

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

  ! --------- Pointer swap ---------
  interface
    module subroutine fclBufferSwap(memObject1, memObject2)
      !! Helper routine for swapping device buffer pointers.
      !! Also swaps the command queue pointers associated with each buffer if different.
      !! @note The debug build will throw an error if either buffer is uninitialised
      !!        or if the buffers do not match in size. @endnote
      class(fclDeviceBuffer), intent(inout) :: memObject1, memObject2
        !! Buffer objects with which to swap pointers
    end subroutine fclBufferSwap
  end interface

  ! --------- Buffer Creation ---------

  interface fclBufferDouble
    !! Generic interface to initialise double array on device

    module function fclBufferDouble_1(cmdq,dim,read,write,profileName) result(mem)
      !! Interface for user-specified command queue
      type(fclCommandQ), intent(in), target :: cmdq  !! Queue with which to associate new buffer
      integer, intent(in) :: dim                     !! Dimension of new buffer
      logical, intent(in) :: read                    !! Read access of device kernels
      logical, intent(in) :: write                   !! Write access of device kernels
      character(*), intent(in), optional :: profileName !! Descriptive name for profiling output
      type(fclDeviceDouble) :: mem                   !! Returns focal memory object
    end function fclBufferDouble_1

    module function fclBufferDouble_2(dim,read,write,profileName) result(mem)
      !! Interface to use the default command queue
      integer, intent(in) :: dim                     !! Dimension of new buffer
      logical, intent(in) :: read                    !! Read access of device kernels
      logical, intent(in) :: write                   !! Write access of device kernels
      character(*), intent(in), optional :: profileName !! Descriptive name for profiling output
      type(fclDeviceDouble) :: mem                   !! Returns focal memory object
    end function fclBufferDouble_2

  end interface fclBufferDouble

  interface fclBufferFloat
    !! Generic interface to initialise float array on device

    module function fclBufferFloat_1(cmdq,dim,read,write,profileName) result(mem)
      !! Interface for user-specified command queue
      type(fclCommandQ), intent(in), target :: cmdq  !! Queue with which to associate new buffer
      integer, intent(in) :: dim                     !! Dimension of new buffer
      logical, intent(in) :: read                    !! Read access of device kernels
      logical, intent(in) :: write                   !! Write access of device kernels
      character(*), intent(in), optional :: profileName !! Descriptive name for profiling output
      type(fclDeviceFloat) :: mem                    !! Returns focal memory object
    end function fclBufferFloat_1

    module function fclBufferFloat_2(dim,read,write,profileName) result(mem)
      !! Interface to use the default command queue
      integer, intent(in) :: dim                     !! Dimension of new buffer
      logical, intent(in) :: read                    !! Read access of device kernels
      logical, intent(in) :: write                   !! Write access of device kernels
      character(*), intent(in), optional :: profileName !! Descriptive name for profiling output
      type(fclDeviceFloat) :: mem                    !! Returns focal memory object
    end function fclBufferFloat_2

  end interface fclBufferFloat


  interface fclBufferInt32
    !! Generic interface to initialise int32 array on device

    module function fclBufferInt32_1(cmdq,dim,read,write,profileName) result(mem)
      !! Interface for user-specified command queue
      type(fclCommandQ), intent(in), target :: cmdq  !! Queue with which to associate new buffer
      integer, intent(in) :: dim                     !! Dimension of new buffer
      logical, intent(in) :: read                    !! Read access of device kernels
      logical, intent(in) :: write                   !! Write access of device kernels
      character(*), intent(in), optional :: profileName !! Descriptive name for profiling output
      type(fclDeviceInt32) :: mem                    !! Returns focal memory object
    end function fclBufferInt32_1

    module function fclBufferInt32_2(dim,read,write,profileName) result(mem)
      !! Interface to use the default command queue
      integer, intent(in) :: dim                     !! Dimension of new buffer
      logical, intent(in) :: read                    !! Read access of device kernels
      logical, intent(in) :: write                   !! Write access of device kernels
      character(*), intent(in), optional :: profileName !! Descriptive name for profiling output
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
      class(fclDeviceBuffer), target :: memObject      !! Focal memory object (source)
      integer(c_size_t), intent(in) :: nBytes              !! Size of buffers in bytes
    end subroutine fclMemRead

    module subroutine fclMemReadInt32(hostBuffer,memObject)
      !! Transfer device integer array to host integer array
      !!  Called by operator-overloading of assignment(=)
      integer(c_int32_t), intent(inout), target :: hostBuffer(:) !! Host array (target)
      class(fclDeviceInt32) :: memObject       !! Focal memory object (source)
    end subroutine fclMemReadInt32

    module subroutine fclMemReadFloat(hostBuffer,memObject)
      !! Transfer device float array to host float array
      !!  Called by operator-overloading of assignment(=)
      real(c_float), intent(inout), target :: hostBuffer(:) !! Host array (target)
      class(fclDeviceFloat) :: memObject       !! Focal memory object (source)
    end subroutine fclMemReadFloat

    module subroutine fclMemReadDouble(hostBuffer,memObject)
      !! Transfer device double array to host double array
      !!  Called by operator-overloading of assignment(=)
      real(c_double), intent(inout), target :: hostBuffer(:) !! Host array (target)
      class(fclDeviceDouble) :: memObject      !! Focal memory object (source)
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

  interface fclGetKernelInfo
    !! Generic interface to query kernel information.
    !! See [clGetDeviceInfo](https://www.khronos.org/registry/OpenCL/sdk/1.2/docs/man/xhtml/clGetKernelInfo.html)
    !! for values of 'key' argument contained in clfortran module.

    module subroutine fclGetKernelInfoString(kernel,key,value)
      !! Query kernel information for string info.
      !! See [clGetPlatformInfo](https://www.khronos.org/registry/OpenCL/sdk/1.2/docs/man/xhtml/clGetKernelInfo.html)
      !!  for values of 'key' argument containined in clfortran module.
      type(fclKernel), intent(in) :: kernel
      integer(c_int32_t), intent(in) :: key
      character(:), allocatable, intent(out), target :: value
    end subroutine fclGetKernelInfoString

    module subroutine fclGetKernelInfoInt32(kernel,key,value)
      !! Query kernel information for 32bit integer.
      !! See [clGetPlatformInfo](https://www.khronos.org/registry/OpenCL/sdk/1.2/docs/man/xhtml/clGetKernelInfo.html)
      !!  for values of 'key' argument containined in clfortran module.
      type(fclKernel), intent(in) :: kernel
      integer(c_int32_t), intent(in) :: key
      integer(c_int32_t), intent(out), target :: value
    end subroutine fclGetKernelInfoInt32

  end interface fclGetKernelInfo

  interface fclGetKernelWorkGroupInfo

    module subroutine fclGetKernelWorkGroupInfoInt64(kernel,device,key,value)
      !! Query kernel work group information for 64bit integer.
      !! See [clGetPlatformInfo](https://www.khronos.org/registry/OpenCL/sdk/1.2/docs/man/xhtml/clGetKernelInfo.html)
      !!  for values of 'key' argument containined in clfortran module.
      type(fclKernel), intent(in) :: kernel
      type(fclDevice), intent(in) :: device
      integer(c_int32_t), intent(in) :: key
      integer(c_int64_t), intent(out), target :: value
    end subroutine fclGetKernelWorkGroupInfoInt64

  end interface fclGetKernelWorkGroupInfo

  interface fclGetKernelArgInfo
    !! Generic interface to query kernel argument information.
    !! See [clGetDeviceInfo](https://www.khronos.org/registry/OpenCL/sdk/1.2/docs/man/xhtml/clGetKernelArgInfo.html)
    !! for values of 'key' argument contained in clfortran module.

    module subroutine fclGetKernelArgInfoString(kernel,argNo,key,value)
      !! Query kernel information for string info.
      !! See [clGetPlatformInfo](https://www.khronos.org/registry/OpenCL/sdk/1.2/docs/man/xhtml/clGetKernelArgInfo.html)
      !!  for values of 'key' argument containined in clfortran module.
      type(fclKernel), intent(in) :: kernel
      integer, intent(in) :: argNo
      integer(c_int32_t), intent(in) :: key
      character(:), allocatable, intent(out), target :: value
    end subroutine fclGetKernelArgInfoString

    module subroutine fclGetKernelArgInfoInt32(kernel,argNo,key,value)
      !! Query kernel information for 32bit integer.
      !! See [clGetPlatformInfo](https://www.khronos.org/registry/OpenCL/sdk/1.2/docs/man/xhtml/clGetKernelArgInfo.html)
      !!  for values of 'key' argument containined in clfortran module.
      type(fclKernel), intent(in) :: kernel
      integer, intent(in) :: argNo
      integer(c_int32_t), intent(in) :: key
      integer(c_int32_t), intent(out), target :: value
    end subroutine fclGetKernelArgInfoInt32

  end interface fclGetKernelArgInfo

  interface

    module subroutine fclGetEventInfo(event,key,value)
      !! Query kernel information for 32bit integer.
      !! See [clGetPlatformInfo](https://www.khronos.org/registry/OpenCL/sdk/1.2/docs/man/xhtml/clGetKernelArgInfo.html)
      !!  for values of 'key' argument containined in clfortran module.
      type(fclEvent), intent(in) :: event
      integer(c_int32_t), intent(in) :: key
      integer(c_int32_t), intent(out), target :: value
    end subroutine fclGetEventInfo

  end interface

  interface

    module function fclGetPlatforms() result(platforms)
      !! Return pointer to array of available fclPlatforms
      type(fclPlatform), allocatable :: platforms(:)
    end function fclGetPlatforms

    module function fclGetPlatform(platform_id) result(platform)
      !! Return fclPlatform object for OpenCL platform id
      integer(c_intptr_t), intent(in) :: platform_id !! OpenCL platform id
      type(fclPlatform), target :: platform
    end function fclGetPlatform

    module function fclGetPlatformDevices(platform_id) result(devices)
      !! Return pointer to array of fclDevices on platform id
      integer(c_intptr_t), intent(in) :: platform_id !! OpenCL platform id
      type(fclDevice), allocatable :: devices(:)
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
      character(*), intent(in) :: vendor
        !! String with which to match platform vendor. Separate multiple vendors
        !!  with commas. First matching vendor in list is used.
        !!  Matching is case-insensitive substring.
        !!
        !!  *e.g.* `vendor='i'` matches 'nvidia' and 'intel' platforms
        !!
        !!  *e.g.* `vendor='nvidia,intel'` matches nvidia platform if available,
        !!  then intel platform if available, then fails fatally if neither
        !!  are available.
        !!
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
      type(fclDevice), allocatable :: deviceList(:)
    end function fclFindDevices_1

    module function fclFindDevices_2(type,nameLike,sortBy) result(deviceList)
      character(*), intent(in), optional :: type
      character(*), intent(in), optional :: nameLike
      character(*), intent(in), optional :: sortBy
      type(fclDevice), allocatable :: deviceList(:)
    end function fclFindDevices_2

  end interface fclFindDevices

  interface fclCreateCommandQ
    !! Generic interface to create a device command queue

    module function fclCreateCommandQ_1(ctx,device,enableProfiling,outOfOrderExec,&
                                          blockingWrite,blockingRead) result(cmdq)
      !! Create a command queue with a Focal device object
      type(fclContext), intent(in), target :: ctx          !! Context containing device for command queue
      type(fclDevice), intent(inout), target :: device     !! Device on which to create command queue
      logical, intent(in), optional :: enableProfiling     !! Enable OpenCL profiling
      logical, intent(in), optional :: outOfOrderExec      !! Enable out of order execution
      logical, intent(in), optional :: blockingWrite       !! Enable/disable host-blocking write to device
      logical, intent(in), optional :: blockingRead        !! Enable/disable host-blocking read from device
      type(fclCommandQ) :: cmdq                            !! Returns fclCommandQ object
    end function fclCreateCommandQ_1

    module function fclCreateCommandQ_2(device,enableProfiling,outOfOrderExec,&
                                          blockingWrite,blockingRead) result(cmdq)
      !! Create a command queue with a Focal device object using default context
      type(fclDevice), intent(inout), target :: device     !! Device on which to create command queue
      logical, intent(in), optional :: enableProfiling     !! Enable OpenCL profiling
      logical, intent(in), optional :: outOfOrderExec      !! Enable out of order execution
      logical, intent(in), optional :: blockingWrite       !! Enable/disable host-blocking write to device
      logical, intent(in), optional :: blockingRead        !! Enable/disable host-blocking read from device
      type(fclCommandQ) :: cmdq                            !! Returns fclCommandQ object
    end function fclCreateCommandQ_2

  end interface fclCreateCommandQ

  interface fclCreateCommandQPool
    !! Generic interface to create a pool of command queues

    module function fclCreateCommandQPool_1(ctx,N,device,enableProfiling,outOfOrderExec,&
                                          blockingWrite,blockingRead) result(qPool)
      !! Create a command queue pool with a Focal device object
      type(fclContext), intent(in), target :: ctx          !! Context containing device for command queue
      integer, intent(in) :: N                             !! Number of command queues to create in pool
      type(fclDevice), intent(inout), target :: device     !! Device on which to create command queue
      logical, intent(in), optional :: enableProfiling     !! Enable OpenCL profiling
      logical, intent(in), optional :: outOfOrderExec      !! Enable out of order execution
      logical, intent(in), optional :: blockingWrite       !! Enable/disable host-blocking write to device
      logical, intent(in), optional :: blockingRead        !! Enable/disable host-blocking read from device
      type(fclCommandQPool) :: qPool                       !! Returns fclCommandQPool object
    end function fclCreateCommandQPool_1

    module function fclCreateCommandQPool_2(N,device,enableProfiling,outOfOrderExec,&
                                          blockingWrite,blockingRead) result(qPool)
      !! Create a command queue pool with a Focal device object using default context
      integer, intent(in) :: N                             !! Number of command queues to create in pool
      type(fclDevice), intent(inout), target :: device     !! Device on which to create command queue
      logical, intent(in), optional :: enableProfiling     !! Enable OpenCL profiling
      logical, intent(in), optional :: outOfOrderExec      !! Enable out of order execution
      logical, intent(in), optional :: blockingWrite       !! Enable/disable host-blocking write to device
      logical, intent(in), optional :: blockingRead        !! Enable/disable host-blocking read from device
      type(fclCommandQPool) :: qPool                       !! Returns fclCommandQPool object
    end function fclCreateCommandQPool_2

  end interface fclCreateCommandQPool


  interface

    module function fclCommandQPool_Next(qPool) result(cmdQ)
      !! Returns next scheduled queue in queue pool
      class(fclCommandQPool), intent(inout) :: qPool
      type(fclCommandQ) :: cmdQ
    end function fclCommandQPool_Next

    module function fclCommandQPool_Current(qPool) result(cmdQ)
      !! Returns current scheduled queue in queue pool
      class(fclCommandQPool), intent(in) :: qPool
      type(fclCommandQ) :: cmdQ
    end function fclCommandQPool_Current

  end interface


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

  interface fclDumpBuildLog

    module subroutine fclDumpBuildLog_1(ctx,prog,device,outputUnit)
      type(fclContext), intent(in) :: ctx
      type(fclProgram), intent(in) :: prog
      type(fclDevice), intent(in) :: device
      integer, intent(in), optional :: outputUnit
    end subroutine fclDumpBuildLog_1

    module subroutine fclDumpBuildLog_2(prog,device,outputUnit)
      type(fclProgram), intent(in) :: prog
      type(fclDevice), intent(in) :: device
      integer, intent(in), optional :: outputUnit
    end subroutine fclDumpBuildLog_2

  end interface fclDumpBuildLog

  interface

    module function fclGetProgramKernel(prog,kernelName,global_work_size,local_work_size, &
                                             work_dim,global_work_offset) result(kern)
      !! Extract a kernel object for execution from a compiled program object
      type(fclProgram), intent(in) :: prog                   !! Compiled program object containing kernel
      character(*), intent(in) :: kernelName                 !! Name of kernel to extract for execution
      integer, intent(in), optional :: global_work_size(:)
        !! Global work group dimensions, default unset (must set prior to launching)
      integer, intent(in), optional :: local_work_size(:)
        !! Local work group dimensions, default zeros (decided by OpenCL runtime)
      integer, intent(in), optional :: work_dim              !! Number of dimensions for kernel work group, default 1
      integer, intent(in), optional :: global_work_offset(:) !! Global work group offsets, default zeros
      type(fclKernel) :: kern                                !! Returns fclKernel object for execution
    end function fclGetProgramKernel

  end interface

  interface fclLaunchKernelAfter
    !! Generic interface to launch a kernel with event dependencies

    module subroutine fclLaunchKernelAfterEvent_1(kernel,cmdQ,event)
      !! Specific interface for a single event dependency on a specific command queue
      class(fclKernel), intent(inout) :: kernel                !! Focal kernel object to launch
      type(fclCommandQ), intent(inout) :: cmdQ             !! CmdQ on which to launch kernel
      type(fclEvent), intent(in) :: event                  !! Event dependency for kernel
    end subroutine fclLaunchKernelAfterEvent_1

    module subroutine fclLaunchKernelAfterEvent_2(kernel,event)
      !! Specific interface a single event dependency on the __default command queue__
      class(fclKernel), intent(inout) :: kernel                !! Focal kernel object to launch
      type(fclEvent), intent(in) :: event                  !! Event dependency for kernel
    end subroutine fclLaunchKernelAfterEvent_2

    module subroutine fclLaunchKernelAfterEventList_1(kernel,cmdQ,eventList)
      !! Specific interface for a multiple event dependencies on a specific command queue
      class(fclKernel), intent(inout) :: kernel                !! Focal kernel object to launch
      type(fclCommandQ), intent(inout) :: cmdQ             !! CmdQ on which to launch kernel
      type(fclEvent), intent(in) :: eventList(:)           !! Event dependency list for kernel
    end subroutine fclLaunchKernelAfterEventList_1

    module subroutine fclLaunchKernelAfterEventList_2(kernel,eventList)
      !! Specific interface for a multiple event dependencies on the __default command queue__
      class(fclKernel), intent(inout) :: kernel                !! Focal kernel object to launch
      type(fclEvent), intent(in) :: eventList(:)           !! Event dependency list for kernel
    end subroutine fclLaunchKernelAfterEventList_2

  end interface fclLaunchKernelAfter

  interface
    module subroutine fclLaunchKernel(kernel,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9, &
                                        a10,a11,a12,a13,a14,a15,a16,a17,a18,a19)
      !! Enqueue a kernel with command arguments
      class(fclKernel), intent(inout), target :: kernel   !! Focal kernel object
      class(*), intent(in), optional, target :: a0
        !! Focal command queue or first kernel argument
      class(*), intent(in), optional, target :: a1,a2,a3,a4,a5,a6,a7,a8,a9, &
                                               a10,a11,a12,a13,a14,a15,a16,a17,a18,a19
        !! Subsequent kernel arguments.
        !! Can be a scalar, an fclDeviceBuffer object, or an fclLocalArgument
    end subroutine fclLaunchKernel

    module subroutine fclProcessKernelArgs(kernel,cmdq,narg,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9, &
                                              a10,a11,a12,a13,a14,a15,a16,a17,a18,a19)
      !! Sets kernel arguments and parses argument list for optional cmdq and actual number of arguments.
      !! @note This is helper routine used internally by focal.  If you just want set kernel arguments
      !!  without launching a kernel, use `fclSetKernelArgs`. @endnote
      class(fclKernel), intent(in), target :: kernel   !! Focal kernel object
      type(fclCommandQ), intent(out), pointer :: cmdq
        !! Returns a0 if it is cmdq, otherwise returns fclDefaultCommandQ
      integer, intent(out) :: narg
        !! Returns the actual number of arguments passed
      class(*), intent(in), optional, target :: a0
        !! Focal command queue or first kernel argument
      class(*), intent(in), optional, target :: a1,a2,a3,a4,a5,a6,a7,a8,a9, &
                                               a10,a11,a12,a13,a14,a15,a16,a17,a18,a19
        !! Subsequent kernel arguments.
        !! Can be a scalar, an fclDeviceBuffer object, or an fclLocalArgument
    end subroutine fclProcessKernelArgs

    module subroutine fclSetKernelArgs(kernel,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9, &
                                         a10,a11,a12,a13,a14,a15,a16,a17,a18,a19)
      !! Set all kernel arguments at once without launching kernel.
      class(fclKernel), intent(in), target :: kernel    !! Focal kernel object
      class(*), intent(in), optional, target :: a0,a1,a2,a3,a4,a5,a6,a7,a8,a9, &
                                               a10,a11,a12,a13,a14,a15,a16,a17,a18,a19
        !! Kernel arguments.
        !! Can be a scalar, an fclDeviceBuffer object, or an fclLocalArgument
    end subroutine fclSetKernelArgs

    module subroutine fclSetKernelArg(kernel,argIndex,argValue)
      !! Set or change a single kernel argument
      type(fclKernel), intent(in) :: kernel          !! Focal kernel object
      integer(c_int32_t), intent(in) :: argIndex     !! Index of kernel argument to set
      class(*), intent(in), target :: argValue
        !! Value of kernel argument.
        !! Can be a scalar, an fclDeviceBuffer object, or an fclLocalArgument
    end subroutine fclSetKernelArg

    module function fclLocalInt32(nElem) result(localArg)
      !! Create a integer local kernel argument object for launching kernels
      integer, intent(in) :: nElem                   !! No of array elements
      type(fclLocalArgInt32) :: localArg             !! Returns local argument object
    end function fclLocalInt32

    module function fclLocalFloat(nElem) result(localArg)
      !! Create a float local kernel argument object for launching kernels
      integer, intent(in) :: nElem                   !! No of array elements
      type(fclLocalArgFloat) :: localArg             !! Returns local argument object
    end function fclLocalFloat

    module function fclLocalDouble(nElem) result(localArg)
      !! Create a double local kernel argument object for launching kernels
      integer, intent(in) :: nElem                   !! No of array elements
      type(fclLocalArgDouble) :: localArg            !! Returns local argument object
    end function fclLocalDouble

  end interface

  interface fclBarrier
    !! Generic interface to enqueue a command queue barrier
    !!  Wait on device for all preceding queue events to complete before
    !!  subsequent events can proceed.

    module subroutine fclBarrier_1(cmdq)
      !! Enqueue barrier on all events in command queue
      type(fclCommandQ), intent(inout), target :: cmdq
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

    module subroutine fclFinish_3(qPool)
      !! Wait on host for all events in all queues in a queue pool
      type(fclCommandQPool), intent(in) :: qPool
    end subroutine fclFinish_3

    module subroutine fclWaitEvent(event)
      !! Wait on host for a specific event
      type(fclEvent), intent(in), target :: event
    end subroutine fclWaitEvent

    module subroutine fclWaitEventList(eventList)
      !! Wait on host for set of events
      type(fclEvent), intent(in), target :: eventList(:)
    end subroutine fclWaitEventList

  end interface fclWait

  interface fclSetDependency
    !! Generic interface to set pre-requisite events for the next enqueued action.
    !!  This does not append to any existing dependencies - it overwrites the dependency list.

    module subroutine fclSetDependencyEvent_1(cmdQ,event,hold)
      !! Interface for specifying a single event dependency on specific cmdq
      type(fclCommandQ), intent(inout), target :: cmdQ     !! Command queue
      type(fclEvent), intent(in) :: event                  !! Event dependency
      logical, intent(in), optional :: hold
        !! Hold dependency list: set to true to not automatically clear dependencies after enqueueing.
        !!  Use for applying the same dependency to multiple commands. Default false.
    end subroutine fclSetDependencyEvent_1

    module subroutine fclSetDependencyEvent_2(event,hold)
      !! Interface for specifying a single event dependency on __default cmdq__
      type(fclEvent), intent(in) :: event                  !! Event dependency
      logical, intent(in), optional :: hold
        !! Hold dependency list: set to true to not automatically clear dependencies after enqueueing.
        !!  Use for applying the same dependency to multiple commands. Default false.
    end subroutine fclSetDependencyEvent_2

    module subroutine fclSetDependencyEventList_1(cmdq,eventList,hold)
      !! Interface for specifying a list of dependent events on specific cmdq
      type(fclCommandQ), intent(inout), target :: cmdQ     !! Command queue
      type(fclEvent), intent(in) :: eventList(:)           !! List of event dependencies
      logical, intent(in), optional :: hold
        !! Hold dependency list: set to true to not automatically clear dependencies after enqueueing.
        !!  Use for applying the same dependency to multiple commands. Default false.
    end subroutine fclSetDependencyEventList_1

    module subroutine fclSetDependencyEventList_2(eventList,hold)
      !! Interface for specifying a list of dependent events on __default cmdq__
      type(fclEvent), intent(in) :: eventList(:)           !! List of event dependencies
      logical, intent(in), optional :: hold                !! Event dependency
        !! Hold dependency list: set to true to not automatically clear dependencies after enqueueing.
        !!  Use for applying the same dependency to multiple commands. Default false.
    end subroutine fclSetDependencyEventList_2

  end interface fclSetDependency

  interface
    module subroutine fclPopDependencies(cmdq)
      !! Called after every enqueue operation:
      !! Clear dependencies unless dependency hold is .true.
      type(fclCommandQ), intent(inout) :: cmdq
    end subroutine fclPopDependencies
  end interface

  interface fclClearDependencies
    !! Generic interface to clear dependency list and reset dependency hold to .false.

    module subroutine fclClearDependencies_1(cmdq)
      !! Interface for specific command queue
      type(fclCommandQ), intent(inout) :: cmdq
    end subroutine fclClearDependencies_1

    module subroutine fclClearDependencies_2()
      !! Interface for default command queueu
    end subroutine fclClearDependencies_2

  end interface fclClearDependencies

  ! ------------------------- PROFILING  ROUTINES -----------------------------

  interface

    module subroutine fclProfilerAdd(profiler,profileSize,c0,c1,c2,c3,c4,c5,c6,c7,c8,c9)
      !! Enable profiling for multiple container (kernel/buffer) and add to profiler collection
      class(fclProfiler), intent(inout) :: profiler
        !! Profiler - collection of objects to profile
      integer, intent(in) :: profileSize
        !! Number of events to save for profiling (allocation size)
      class(fclProfileContainer), intent(inout), target :: c0
        !! Object (kernel/buffer) for which to enable profiling
      class(fclProfileContainer), intent(inout), target, optional :: c1, c2, c3,c4,c5,c6,c7,c8,c9
        !! Subsequent objects (kernel/buffer) for which to enable profiling
    end subroutine fclProfilerAdd

    module subroutine fclEnableProfiling(container,profileSize,profiler)
      !! Enable profiling on a specific container by allocating space to save events
      class(fclProfileContainer), intent(inout), target :: container
        !! Container on which to enable profiling. This can be one of:
        !! `fclKernel`,`fclDeviceBuffer`,`fclProfileContainer`.
      integer, intent(in) :: profileSize
        !! Number of events to allocate space for
      type(fclProfiler), intent(inout), optional :: profiler
        !! Profiler collection object to which to add the kernel/buffer.
    end subroutine fclEnableProfiling

    module subroutine fclPushProfileEvent(container,event,type)
      !! If profiling is enabled for the container, save an event to it
      class(fclProfileContainer), intent(inout) :: container
        !! Profiling container (`fclKernel`,`fclDeviceBuffer`,`fclProfileContainer`)
      type(fclEvent), intent(in) :: event
        !! Event to push to container
      integer, intent(in), optional :: type
        !! Type info for buffer objects
    end subroutine fclPushProfileEvent

    module function fclGetEventDurations(eventList) result(durations)
      type(fclEvent), intent(in) :: eventList(:)
      integer(c_int64_t) :: durations(size(eventList,1))
    end function fclGetEventDurations
    
    module subroutine fclDumpProfileData(profiler,outputUnit)
      !! Dump summary of profiler data for list of kernels to specific output unit
      class(fclProfiler), intent(in) :: profiler
        !! Profiler object containing collection of kernels & buffers to profile
      integer, intent(in), optional :: outputUnit
        !! Output unit to write summary data
    end subroutine fclDumpProfileData

    module subroutine fclDumpKernelProfileData(outputUnit,kernelList,device)
      !! Dump summary of profile data for list of kernels to specific output unit
      integer, intent(in) :: outputUnit
        !! Output unit to write summary data
      class(fclKernel), intent(in) :: kernelList(:)
        !! List of kernels for which to dump profile data
      type(fclDevice), intent(in) :: device
        !! Device on which the kernels were executed
        !! Needed for kernel work group info.
    end subroutine fclDumpKernelProfileData

    module subroutine fclDumpBufferProfileData(outputUnit,bufferList1,bufferList2,bufferList3)
      !! Dump summary of profile data for list of buffers to specific output unit.
      !!
      !! Three buffer list inputs are provided for different buffer types
      integer, intent(in) :: outputUnit
        !! Output unit to write summary data.
      class(fclDeviceBuffer), intent(in), target :: bufferList1(:)
        !! List of buffers for which to dump profile data
      class(fclDeviceBuffer), intent(in), optional, target :: bufferList2(:)
        !! List of buffers for which to dump profile data
      class(fclDeviceBuffer), intent(in), optional, target:: bufferList3(:)
        !! List of buffers for which to dump profile data
    end subroutine fclDumpBufferProfileData

    module subroutine fclDumpTracingData(profiler, filename)
      !! Writes a chrome://tracing data format for profiled events
      class(fclProfiler), intent(in) :: profiler
        !! Profiler collection object containing kernels/buffers that have been profiled
      character(*), intent(in) :: filename
        !! Filename to which to write chrome://tracing format
    end subroutine fclDumpTracingData

  end interface


  ! ---------------------------- DEBUG ROUTINES -------------------------------
  interface

    module subroutine fclDbgCheckBufferInit(memObject,descrip)
      !! Check that a device buffer object has been initialised.
      !! @note Debug routine: only executed for debug build. @endnote
      class(fclDeviceBuffer), intent(in) :: memObject
      character(*), intent(in) :: descrip
    end subroutine fclDbgCheckBufferInit

    module subroutine fclDbgCheckBufferSize(memObject,hostBytes,descrip)
      !! Check that a host buffer matches the size in bytes of a device buffer.
      !! @note Debug routine: only executed for debug build. @endnote
      class(fclDeviceBuffer), intent(in) :: memObject
      integer(c_size_t), intent(in) :: hostBytes
      character(*), intent(in) :: descrip
    end subroutine fclDbgCheckBufferSize

    module subroutine fclDbgCheckCopyBufferSize(memObject1,memObject2)
      !! Check that a host buffer matches the size in bytes of a device buffer.
      !! @note Debug routine: only executed for debug build. @endnote
      class(fclDeviceBuffer), intent(in) :: memObject1 ! Destination buffer
      class(fclDeviceBuffer), intent(in) :: memObject2 ! Source buffer
    end subroutine fclDbgCheckCopyBufferSize

    module subroutine fclDbgCheckKernelNArg(kernel,nArg)
      !! Check that number of actual args matches number of kernel args.
      !! @note Debug routine: only executed for debug build. @endnote
      type(fclKernel), intent(in) :: kernel
      integer, intent(in) :: nArg
    end subroutine fclDbgCheckKernelNArg

    module subroutine fclDbgCheckKernelArgType(kernel,argNo,type)
      !! Checks the types of arguments passed to kernels
      !! @note Debug routine: only executed for debug build. @endnote
      type(fclKernel), intent(in) :: kernel
      integer, intent(in) :: argNo
      character(*), intent(in) :: type
    end subroutine fclDbgCheckKernelArgType

    module subroutine fclDbgCheckKernelArgQualifier(kernel,argNo,qualifier)
      !! Checks the address qualifier of arguments passed to kernels.
      !! @note Debug routine: only executed for debug build. @endnote
      type(fclKernel), intent(in) :: kernel
      integer, intent(in) :: argNo
      character(*), intent(in) :: qualifier
    end subroutine fclDbgCheckKernelArgQualifier

    module function fclDbgOptions() result(options) !(userOptions,options)
      !! Returns OpenCL compile options as interoperable string for debug mode
      !! @note Debug routine: only executed for debug build. @endnote
      ! character(*), intent(in) :: userOptions
      character(:), allocatable :: options
    end function fclDbgOptions

    module subroutine fclDbgWait(event,descrip)
      !! Wait for an event to complete and check for successful completion.
      !! Throw runtime error if status is not CL_COMPLETE.
      !! @note Debug routine: only executed for debug build. @endnote
      type(fclEvent), intent(in), target :: event              !! Event object to check
      character(*), intent(in), optional :: descrip    !! Description for debugging
    end subroutine fclDbgWait

  end interface



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

    module subroutine fclSourceFromFile(filename,sourceString)
      !! Allocate and fill character string from file
      character(*), intent(in) :: filename
      character(:), intent(out), allocatable :: sourceString
    end subroutine fclSourceFromFile

  end interface

  interface

    module function strStripNum(linei)
      !! Return copy of string with numerical characters removed
      character(len=*),intent(in) :: linei
        !! Input string
      character(len=len(linei)) strStripNum
        !! Converted string output
    end function strStripNum

  end interface

end module Focal
