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


  module procedure fclBufferDouble_1 !(cmdq,dim,read,write,profileName) result(mem)

    integer(c_size_t) :: nBytes
    nBytes = c_sizeof(real(1.0d0,c_double))*dim
    mem%cl_mem = fclBuffer(cmdq,nBytes,read,write)
    mem%cmdq => cmdq
    mem%nBytes = nBytes

    if (present(profileName)) then
      if (allocated(mem%profileName)) then
        deallocate(mem%profileName)
      end if
      mem%profileName = profileName
    end if

  end procedure fclBufferDouble_1
  ! ---------------------------------------------------------------------------


  module procedure fclBufferDouble_2 !(dim,read,write,profileName) result(mem)

    mem = fclBufferDouble_1(fclDefaultCmdQ,dim,read,write,profileName)

  end procedure fclBufferDouble_2
  ! ---------------------------------------------------------------------------


  module procedure fclBufferFloat_1 !(cmdq,dim,read,write,profileName) result(mem)

    integer(c_size_t) :: nBytes
    nBytes = c_sizeof(real(1.0,c_float))*dim
    mem%cl_mem = fclBuffer(cmdq,nBytes,read,write)
    mem%cmdq => cmdq
    mem%nBytes = nBytes

    if (present(profileName)) then
      if (allocated(mem%profileName)) then
        deallocate(mem%profileName)
      end if
      mem%profileName = profileName
    end if

  end procedure fclBufferFloat_1
  ! ---------------------------------------------------------------------------


  module procedure fclBufferFloat_2 !(dim,read,write,profileName) result(mem)

    mem = fclBufferFloat_1(fclDefaultCmdQ,dim,read,write,profileName)

  end procedure fclBufferFloat_2
  ! ---------------------------------------------------------------------------


  module procedure fclBufferInt32_1 !(cmdq,dim,read,write,profileName) result(mem)

    integer(c_size_t) :: nBytes
    nBytes = c_sizeof(int(1,c_int32_t))*dim
    mem%cl_mem = fclBuffer(cmdq,nBytes,read,write)
    mem%cmdq => cmdq
    mem%nBytes = nBytes

    if (present(profileName)) then
      if (allocated(mem%profileName)) then
        deallocate(mem%profileName)
      end if
      mem%profileName = profileName
    end if

  end procedure fclBufferInt32_1
  ! ---------------------------------------------------------------------------


  module procedure fclBufferInt32_2 !(dim,read,write,profileName) result(mem)

    mem = fclBufferInt32_1(fclDefaultCmdQ,dim,read,write,profileName)

  end procedure fclBufferInt32_2
  ! ---------------------------------------------------------------------------


  module procedure fclBuffer !(cmdq,nBytes,read,write) result(mem)

    !! @note
    !! "The memory associated with pattern can be reused or freed after the function returns."
    !! https://www.khronos.org/registry/OpenCL/sdk/1.2/docs/man/xhtml/clEnqueueFillBuffer.html
    !! @endnote

    integer(c_int32_t) :: errcode
    integer(c_int64_t) :: MEM_FLAGS

    integer(c_intptr_t), target :: cl_context
    integer(c_size_t) :: size_ret

    ! Get command queue context
    errcode = clGetCommandQueueInfo(cmdq%cl_command_queue, &
                  CL_QUEUE_CONTEXT,c_sizeof(cl_context), &
                  c_loc(cl_context), size_ret)

    call fclErrorHandler(errcode,'fclBuffer','clGetCommandQueueInfo')

    MEM_FLAGS = CL_MEM_READ_WRITE
    if (.not.write.and..not.read) then
      call fclRuntimeError('fclBuffer: memory must be at least read or write.')

    elseif (.not.write) then
      MEM_FLAGS = CL_MEM_READ_ONLY

    elseif (.not.read) then
      MEM_FLAGS = CL_MEM_WRITE_ONLY

    end if

    cl_mem = clCreateBuffer(cl_context,MEM_FLAGS, &
                      nBytes,C_NULL_PTR,errcode)

    call fclErrorHandler(errcode,'fclBuffer','clCreateBuffer')

  end procedure fclBuffer
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
      memObject1%profileName = memObject2%profileName

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
