submodule (Focal) Focal_Memory
  !! FOCAL: openCL abstraction layer for fortran
  !!  Implementation module

  !! @note This is an implementation submodule: it contains the code implementing the subroutines defined in the
  !!  corresponding header module file. See header module file (Focal.f90) for interface definitions. @endnote

  use clfortran
  implicit none

  contains


  module procedure fclBufferSwap !(memObject1, memObject2)
    !! Helper routine for swapping device buffer pointers

    integer(c_intptr_t) :: tempPtr
    type(fclCommandQ), pointer :: tempCmdQ

    call fclDbgCheckBufferInit(memObject1,'fclBufferSwap:memObject1')
    call fclDbgCheckBufferInit(memObject2,'fclBufferSwap:memObject2')
    call fclDbgCheckCopyBufferSize(memObject1,memObject2)

    ! Swap OpenCL pointers
    tempPtr = memObject2%cl_mem
    memObject2%cl_mem = memObject1%cl_mem
    memObject1%cl_mem = tempPtr

    ! Swap command queue pointers
    tempCmdQ => memObject2%cmdq
    memObject2%cmdQ => memObject1%cmdQ
    memObject1%cmdQ => tempCmdQ

  end procedure fclBufferSwap
  ! ---------------------------------------------------------------------------


  module procedure fclInitBufferUntyped_1 !(cmdq,buffer,nBytes,profileName,access)
    !! Initialise untyped buffer object on specified command queue
    use M_strings, only: upperStr=>upper
    integer(c_int32_t) :: errcode
    integer(c_int64_t) :: MEM_FLAGS

    integer(c_intptr_t), target :: cl_context
    integer(c_size_t) :: size_ret
    logical :: read, write

    ! Get command queue context
    errcode = clGetCommandQueueInfo(cmdq%cl_command_queue, &
                  CL_QUEUE_CONTEXT,c_sizeof(cl_context), &
                  c_loc(cl_context), size_ret)

    call fclErrorHandler(errcode,'fclBuffer','clGetCommandQueueInfo')

    ! Check kernel access flags
    if (present(access)) then
      read = index(upperstr(access),'R') > 0
      write = index(upperstr(access),'W') > 0
    else
      read = .true.
      write = .true.
    end if

    MEM_FLAGS = CL_MEM_READ_WRITE
    if (.not.write.and..not.read) then
      call fclRuntimeError('fclBuffer: memory must be at least read or write.')

    elseif (.not.write) then
      MEM_FLAGS = CL_MEM_READ_ONLY

    elseif (.not.read) then
      MEM_FLAGS = CL_MEM_WRITE_ONLY

    end if

    buffer%cl_mem = clCreateBuffer(cl_context,MEM_FLAGS, &
                      nBytes,C_NULL_PTR,errcode)

    call fclErrorHandler(errcode,'fclBuffer','clCreateBuffer')

    buffer%nBytes = nBytes
    buffer%cmdq => cmdq

    if (present(profileName)) then
      if (allocated(buffer%profileName)) then
        deallocate(buffer%profileName)
      end if
      buffer%profileName = profileName
    end if

  end procedure fclInitBufferUntyped_1
  ! ---------------------------------------------------------------------------


  module procedure fclInitBufferUntyped_2 !(buffer,nBytes,profileName,access)
    !! Initialise untyped buffer object on the default command queue

    call fclInitBufferUntyped_1(fclDefaultCmdQ,buffer,nBytes,profileName,access)

  end procedure fclInitBufferUntyped_2
  ! ---------------------------------------------------------------------------


  module procedure fclInitBufferFloat_1 !(cmdq,buffer,dim,profileName,access)
    !! Initialise float buffer object on specific command queue

    integer(c_size_t) :: nBytes
    nBytes = c_sizeof(real(1.0d0,c_float))*dim

    call fclInitBufferUntyped_1(cmdq,buffer%fclDeviceBuffer,nBytes,profileName,access)

  end procedure fclInitBufferFloat_1
  ! ---------------------------------------------------------------------------


  module procedure fclInitBufferFloat_2 !(buffer,dim,profileName,access)
    !! Initialise float buffer object on the default command queue

    call fclInitBufferFloat_1(fclDefaultCmdQ,buffer,dim,profileName,access)
    
  end procedure fclInitBufferFloat_2
  ! ---------------------------------------------------------------------------


  module procedure fclInitBufferDouble_1 !(cmdq,buffer,dim,profileName,access)
    !! Initialise double buffer object on specific command queue

    integer(c_size_t) :: nBytes
    nBytes = c_sizeof(real(1.0d0,c_double))*dim

    call fclInitBufferUntyped_1(cmdq,buffer%fclDeviceBuffer,nBytes,profileName,access)

  end procedure fclInitBufferDouble_1
  ! ---------------------------------------------------------------------------


  module procedure fclInitBufferDouble_2 !(buffer,dim,profileName,access)
    !! Initialise double buffer object on the default command queue

    call fclInitBufferDouble_1(fclDefaultCmdQ,buffer,dim,profileName,access)
    
  end procedure fclInitBufferDouble_2
  ! ---------------------------------------------------------------------------


  module procedure fclInitBufferInt32_1 !(cmdq,buffer,dim,profileName,access)
    !! Initialise 32bit integer buffer object on specific command queue

    integer(c_size_t) :: nBytes
    nBytes = c_sizeof(int(1,c_int32_t))*dim

    call fclInitBufferUntyped_1(cmdq,buffer%fclDeviceBuffer,nBytes,profileName,access)

  end procedure fclInitBufferInt32_1
  ! ---------------------------------------------------------------------------


  module procedure fclInitBufferInt32_2 !(buffer,dim,profileName,access)
    !! Initialise 32bit integer buffer object on the default command queue

    call fclInitBufferInt32_1(fclDefaultCmdQ,buffer,dim,profileName,access)
    
  end procedure fclInitBufferInt32_2
  ! ---------------------------------------------------------------------------


  module procedure fclMemWriteScalar !(memObject,hostBufferPtr,nBytesPattern)

    integer(c_int32_t) :: errcode

    call fclDbgCheckBufferInit(memObject,'fclMemWriteScalar')

    errcode = clEnqueueFillBuffer(memObject%cmdq%cl_command_queue, &
                memObject%cl_mem, hostBufferPtr, nBytesPattern, &
                int(0,c_size_t), memObject%nBytes, &
                memObject%cmdq%nDependency, memObject%cmdq%dependencyListPtr, &
                c_loc(memObject%cmdq%lastWriteEvent%cl_event))

    call fclPopDependencies(memObject%cmdq)
    fclLastWriteEvent = memObject%cmdq%lastWriteEvent

    call memObject%pushProfileEvent(memObject%cmdq%lastWriteEvent,1)

    call fclErrorHandler(errcode,'fclMemWriteScalar','clEnqueueFillBuffer')

  end procedure fclMemWriteScalar
  ! ---------------------------------------------------------------------------


  module procedure fclMemWriteScalarInt32 !(memObject,hostValue)

    integer(c_size_t) :: nBytesPattern
    nBytesPattern = c_sizeof(int(1,c_int32_t))
    call fclMemWriteScalar(memObject,c_loc(hostValue),nBytesPattern)

  end procedure fclMemWriteScalarInt32
  ! ---------------------------------------------------------------------------


  module procedure fclMemWriteScalarFloat !(memObject,hostValue)

    integer(c_size_t) :: nBytesPattern
    nBytesPattern = c_sizeof(real(1.0,c_float))
    call fclMemWriteScalar(memObject,c_loc(hostValue),nBytesPattern)

  end procedure fclMemWriteScalarFloat
  ! ---------------------------------------------------------------------------


  module procedure fclMemWriteScalarDouble !(memObject,hostValue)

    integer(c_size_t) :: nBytesPattern
    nBytesPattern = c_sizeof(real(1.0d0,c_double))
    call fclMemWriteScalar(memObject,c_loc(hostValue),nBytesPattern)

  end procedure fclMemWriteScalarDouble
  ! ---------------------------------------------------------------------------


  module procedure fclMemWrite !(memObject,hostBufferPtr,nBytes)

    integer(c_int32_t) :: errcode
    integer(c_int32_t) :: blocking_write

    call fclDbgCheckBufferInit(memObject,'fclMemWrite')
    call fclDbgCheckBufferSize(memObject,nBytes,'fclMemWrite')

    if (memObject%cmdq%blockingWrite) then
      blocking_write = CL_TRUE
    else
      blocking_write = CL_FALSE
    end if

    errcode = clEnqueueWriteBuffer(memObject%cmdq%cl_command_queue,memObject%cl_mem, &
          blocking_write,int(0,c_size_t),nBytes,hostBufferPtr, &
          memObject%cmdq%nDependency, memObject%cmdq%dependencyListPtr, &
          c_loc(memObject%cmdq%lastWriteEvent%cl_event))

    call fclPopDependencies(memObject%cmdq)
    fclLastWriteEvent = memObject%cmdq%lastWriteEvent

    call memObject%pushProfileEvent(memObject%cmdq%lastWriteEvent,1)

    call fclErrorHandler(errcode,'fclMemWrite','clEnqueueWriteBuffer')

  end procedure fclMemWrite
  ! ---------------------------------------------------------------------------


  module procedure fclMemWriteInt32 !(memObject,hostBuffer)

    integer(c_size_t) :: nBytes

    nBytes = c_sizeof(int(1,c_int32_t))*size(hostBuffer,1)
    call fclMemWrite(memObject,c_loc(hostBuffer),nBytes)

  end procedure fclMemWriteInt32
  ! ---------------------------------------------------------------------------


  module procedure fclMemWriteFloat !(memObject,hostBuffer)

    integer(c_size_t) :: nBytes

    nBytes = c_sizeof(real(1.0,c_float))*size(hostBuffer,1)
    call fclMemWrite(memObject,c_loc(hostBuffer),nBytes)

  end procedure fclMemWriteFloat
  ! ---------------------------------------------------------------------------


  module procedure fclMemWriteDouble !(memObject,hostBuffer)

    integer(c_size_t) :: nBytes

    nBytes = c_sizeof(real(1.0d0,c_double))*size(hostBuffer,1)
    call fclMemWrite(memObject,c_loc(hostBuffer),nBytes)

  end procedure fclMemWriteDouble
  ! ---------------------------------------------------------------------------


  module procedure fclMemRead !(hostBufferPtr,memObject,nBytes)

    integer(c_int32_t) :: errcode
    integer(c_int32_t) :: blocking_read

    call fclDbgCheckBufferInit(memObject,'fclMemRead')
    call fclDbgCheckBufferSize(memObject,nBytes,'fclMemRead')

    if (memObject%cmdq%blockingRead) then
      blocking_read = CL_TRUE
    else
      blocking_read = CL_FALSE
    end if

    errcode = clEnqueueReadBuffer(memObject%cmdq%cl_command_queue,memObject%cl_mem, &
          blocking_read,int(0,c_size_t),nBytes,hostBufferPtr, &
          memObject%cmdq%nDependency, memObject%cmdq%dependencyListPtr, &
          c_loc(memObject%cmdq%lastReadEvent%cl_event))

    call fclPopDependencies(memObject%cmdq)
    fclLastReadEvent = memObject%cmdq%lastReadEvent

    call memObject%pushProfileEvent(memObject%cmdq%lastReadEvent,2)

    call fclErrorHandler(errcode,'fclMemRead','clEnqueueReadBuffer')

  end procedure fclMemRead
  ! ---------------------------------------------------------------------------


  module procedure fclMemReadInt32 !(hostBuffer,memObject)

    integer(c_size_t) :: nBytes

    nBytes = c_sizeof(int(1,c_int32_t))*size(hostBuffer,1)
    call fclMemRead(c_loc(hostBuffer),memObject,nBytes)

  end procedure fclMemReadInt32
  ! ---------------------------------------------------------------------------


  module procedure fclMemReadFloat !(hostBuffer,memObject)

    integer(c_size_t) :: nBytes

    nBytes = c_sizeof(real(1,c_float))*size(hostBuffer,1)
    call fclMemRead(c_loc(hostBuffer),memObject,nBytes)

  end procedure fclMemReadFloat
  ! ---------------------------------------------------------------------------


  module procedure fclMemReadDouble !(hostBuffer,memObject)

    integer(c_size_t) :: nBytes

    nBytes = c_sizeof(real(1,c_double))*size(hostBuffer,1)
    call fclMemRead(c_loc(hostBuffer),memObject,nBytes)

  end procedure fclMemReadDouble
  ! ---------------------------------------------------------------------------


  module procedure fclMemCopy !(memObject1,memObject2)

    integer(c_int32_t) :: errcode

    if (memObject2%nBytes < 0) then
      ! Source object is uninitialised: nothing to copy

      call fclRuntimeError('fclMemCopy: source memory object is uninitialised.')

    else if (memObject1%nBytes < 0) then
      ! Receiving memory object is uninitialised
      !  therefore copy host pointer from source object

      memObject1%cl_mem = memObject2%cl_mem
      memObject1%cmdQ => memObject2%cmdQ
      memObject1%nBytes = memObject2%nBytes

      if (allocated(memObject2%profileName)) then
        memObject1%profileName = memObject2%profileName
      end if

      if (memObject2%profilingEnabled) then  
        memObject1%profilingEnabled = memObject2%profilingEnabled
        memObject1%profileEvents = memObject2%profileEvents
        memObject1%profileSize = memObject2%profileSize
        memObject1%nProfileEvent = memObject2%nProfileEvent
        memObject1%profileEventType = memObject2%profileEventType
      end if
    else
      ! Receiving memory object is initialised
      !  therefore perform a device-to-device copy

      call fclDbgCheckCopyBufferSize(memObject1,memObject2)

      errcode = clEnqueueCopyBuffer(memObject1%cmdq%cl_command_queue, &
                memObject2%cl_mem, memObject1%cl_mem, &
                int(0,c_size_t), int(0,c_size_t), &
                memObject2%nBytes, &
                memObject1%cmdq%nDependency, memObject1%cmdq%dependencyListPtr, &
                c_loc(memObject1%cmdq%lastCopyEvent%cl_event))

      call fclPopDependencies(memObject1%cmdq)
      fclLastCopyEvent = memObject1%cmdq%lastCopyEvent

      call memObject1%pushProfileEvent(memObject1%cmdq%lastCopyEvent,3)

      call fclErrorHandler(errcode,'fclMemCopy','clEnqueueCopyBuffer')

    end if

  end procedure fclMemCopy
  ! ---------------------------------------------------------------------------


  module procedure fclMemCopyInt32 !(memObject1,memObject2)
    call fclMemCopy(memObject1,memObject2)
  end procedure fclMemCopyInt32
  ! ---------------------------------------------------------------------------


  module procedure fclMemCopyFloat !(memObject1,memObject2)
    call fclMemCopy(memObject1,memObject2)
  end procedure fclMemCopyFloat
  ! ---------------------------------------------------------------------------


  module procedure fclMemCopyDouble !(memObject1,memObject2)
    call fclMemCopy(memObject1,memObject2)
  end procedure fclMemCopyDouble
  ! ---------------------------------------------------------------------------


  module procedure fclFreeBuffer !(memObject)
    !! Release device memory associated with memObject

    integer(c_int32_t) :: errcode

    call fclDbgCheckBufferInit(memObject,'fclFreeBuffer')

    errcode = clReleaseMemObject(memObject%cl_mem)

    call fclErrorHandler(errcode,'fclFreeBuffer','clReleaseMemObject')

    memObject%nBytes = -1

  end procedure fclFreeBuffer
  ! ---------------------------------------------------------------------------

end submodule Focal_Memory
