submodule (Focal) Focal_Memory
  !! FOCAL: openCL abstraction layer for fortran
  !!  Implementation module

  !! @note This is an implementation submodule: it contains the code implementing the subroutines defined in the
  !!  corresponding header module file. See header module file (Focal.f90) for interface definitions. @endnote

  use clfortran
  implicit none

  contains

  module procedure fclBufferDouble_1 !(cmdq,dim,read,write) result(mem)

    integer(c_size_t) :: nBytes
    nBytes = c_sizeof(real(1.0d0,c_double))*dim
    mem%cl_mem = fclBuffer(cmdq,nBytes,read,write)
    mem%cmdq = cmdq
    mem%nBytes = nBytes

  end procedure fclBufferDouble_1
  ! ---------------------------------------------------------------------------


  module procedure fclBufferDouble_2 !(dim,read,write) result(mem)

    mem = fclBufferDouble_1(fclDefaultCmdQ,dim,read,write)

  end procedure fclBufferDouble_2
  ! ---------------------------------------------------------------------------


  module procedure fclBufferFloat_1 !(cmdq,dim,read,write) result(mem)

    integer(c_size_t) :: nBytes
    nBytes = c_sizeof(real(1.0,c_float))*dim
    mem%cl_mem = fclBuffer(cmdq,nBytes,read,write)
    mem%cmdq = cmdq
    mem%nBytes = nBytes

  end procedure fclBufferFloat_1
  ! ---------------------------------------------------------------------------


  module procedure fclBufferFloat_2 !(dim,read,write) result(mem)

    mem = fclBufferFloat_1(fclDefaultCmdQ,dim,read,write)

  end procedure fclBufferFloat_2
  ! ---------------------------------------------------------------------------


  module procedure fclBufferInt32_1 !(cmdq,dim,read,write) result(mem)

    integer(c_size_t) :: nBytes
    nBytes = c_sizeof(int(1,c_int32_t))*dim
    mem%cl_mem = fclBuffer(cmdq,nBytes,read,write)
    mem%cmdq = cmdq
    mem%nBytes = nBytes

  end procedure fclBufferInt32_1
  ! ---------------------------------------------------------------------------


  module procedure fclBufferInt32_2 !(dim,read,write) result(mem)

    mem = fclBufferInt32_1(fclDefaultCmdQ,dim,read,write)

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

    errcode = clEnqueueFillBuffer(memObject%cmdq%cl_command_queue, &
                memObject%cl_mem, hostBufferPtr, nBytesPattern, &
                int(0,c_size_t), memObject%nBytes, 0, C_NULL_PTR, &
                c_loc(memObject%cmdq%lastWriteEvent%cl_event))

    fclLastWriteEvent = memObject%cmdq%lastWriteEvent

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

    if (memObject%cmdq%fclBlockingWrite) then
      blocking_write = CL_TRUE
    else
      blocking_write = CL_FALSE
    end if

    errcode = clEnqueueWriteBuffer(memObject%cmdq%cl_command_queue,memObject%cl_mem, &
          blocking_write,int(0,c_size_t),nBytes,hostBufferPtr, &
          0,C_NULL_PTR,c_loc(memObject%cmdq%lastWriteEvent%cl_event))

    fclLastWriteEvent = memObject%cmdq%lastWriteEvent

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

    if (memObject%cmdq%fclBlockingRead) then
      blocking_read = CL_TRUE
    else
      blocking_read = CL_FALSE
    end if

    errcode = clEnqueueReadBuffer(memObject%cmdq%cl_command_queue,memObject%cl_mem, &
          blocking_read,int(0,c_size_t),nBytes,hostBufferPtr, &
          0,C_NULL_PTR,c_loc(memObject%cmdq%lastReadEvent%cl_event))

    fclLastReadEvent = memObject%cmdq%lastReadEvent

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

    !! @todo Implement debug check for matching dimensions @endtodo

    if (memObject2%nBytes < 0) then
      ! Source object is uninitialised: nothing to copy

      call fclRuntimeError('fclMemCopy: source memory object is uninitialised.')

    else if (memObject1%nBytes < 0) then
      ! Receiving memory object is uninitialised
      !  therefore copy host pointer from source object

      memObject1%cl_mem = memObject2%cl_mem
      memObject1%cmdQ = memObject2%cmdQ
      memObject1%nBytes = memObject2%nBytes

    else
      ! Receiving memory object is initialised
      !  therefore perform a device-to-device copy

      errcode = clEnqueueCopyBuffer(memObject1%cmdq%cl_command_queue, &
                memObject2%cl_mem, memObject1%cl_mem, &
                int(0,c_size_t), int(0,c_size_t), &
                memObject2%nBytes, &
                0,C_NULL_PTR,c_loc(memObject1%cmdq%lastCopyEvent%cl_event))

      fclLastCopyEvent = memObject1%cmdq%lastCopyEvent

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


end submodule Focal_Memory
