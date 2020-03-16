! -----------------------------------------------------------------------------
!  FOCAL
!
!   A modern Fortran abstraction layer for OpenCL
!   https://lkedward.github.io/focal-docs
!
! -----------------------------------------------------------------------------
!
! Copyright (c) 2020 Laurence Kedward
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
! -----------------------------------------------------------------------------

submodule (Focal) Focal_Debug
  !!  Implementation module for focal debug routines.
  !!  This submodule is linked in the debug version of Focal build.

  !! @note This is an implementation submodule: it contains the code implementing the subroutines defined in the
  !!  corresponding header module file. See header module file (Focal.f90) for interface definitions. @endnote
  use clfortran
  implicit none

  contains

  module procedure fclDbgCheckContext !(descrip,ctx)
    !! Check the (default) context is initialised.
    !! Assumes uninitialised contexts have cl_context = -1.

    if (present(ctx)) then
      if (ctx%cl_context == -1) then

        write(*,*) '(!) Focal (debug build) runtime assertion failed.'
        write(*,*) ' Attempt to use uninitialised context at: ',descrip
        write(*,*)

        call fclRuntimeError('fclDbgCheckContext')

      end if
    else

      if (fclDefaultCtx%cl_context == -1) then

        write(*,*) '(!) Focal (debug build) runtime assertion failed.'
        write(*,*) ' The default context is uninitialised.'
        write(*,*) '  but referenced at: ',descrip
        write(*,*)

        call fclRuntimeError('fclDbgCheckContext')

      end if

    end if

  end procedure fclDbgCheckContext
  ! ---------------------------------------------------------------------------


  module procedure fclDbgCheckDevice !(device,descrip)
    !! Check a device object is valid

    if (device%cl_device_id < 0) then

      write(*,*) '(!) Focal (debug build) runtime assertion failed.'
      write(*,*) ' Attempt to use uninitialised device at: ',descrip
      write(*,*)

      call fclRuntimeError('fclDbgCheckDevice')

    end if

  end procedure fclDbgCheckDevice
  ! ---------------------------------------------------------------------------


  module procedure fclDbgCheckBufferInit !(memObject,descrip)
    !! Check that a device buffer object has been initialised.

    if (memObject%nBytes <= 0) then

      write(*,*) '(!) Focal (debug build) runtime assertion failed.'
      write(*,*) ' Attempt to use uninitialised device buffer at: ',descrip
      write(*,*)

      call fclRuntimeError('fclDbgCheckBufferInit')

    end if

  end procedure fclDbgCheckBufferInit
  ! ---------------------------------------------------------------------------


  module procedure fclDbgCheckBufferSize !(memObject,hostBytes,descrip)
    !! Check that a host buffer matches the size in bytes of a device buffer

    if (hostBytes /= memObject%nBytes) then

      write(*,*) '(!) Focal (debug build) runtime assertion failed.'
      write(*,*) ' Mismatch in size between host buffer and device buffer at: ',descrip
      write(*,*) ' Host buffer size: ',hostBytes
      write(*,*) ' Device buffer size: ',memObject%nBytes
      write(*,*)

      call fclRuntimeError('fclDbgCheckBufferSize')

    end if

  end procedure fclDbgCheckBufferSize
  ! ---------------------------------------------------------------------------


  module procedure fclDbgCheckCopyBufferSize !(memObject1,memObject2)
    !! Check that device buffers match in size in bytes for copying

    if (memObject1%nBytes /= memObject2%nBytes) then

      write(*,*) '(!) Focal (debug build) runtime assertion failed.'
      write(*,*) ' Mismatch in size between source buffer and destination buffer'//&
                  '  while attempting to copy or swap device buffers. (fclMemCopy)'
      write(*,*) ' Source buffer size: ',memObject2%nBytes
      write(*,*) ' Destination buffer size: ',memObject1%nBytes
      write(*,*)

      call fclRuntimeError('fclDbgCheckCopyBufferSize')

    end if

  end procedure fclDbgCheckCopyBufferSize
  ! ---------------------------------------------------------------------------


  module procedure fclDbgCheckKernelNArg !(kernel,nArg)
    !! Check that number of actual args matches number of kernel args

    integer :: nKernelArg

    call fclGetKernelInfo(kernel,CL_KERNEL_NUM_ARGS,nKernelArg)

    if (nKernelArg /= nArg) then

      write(*,*) '(!) Focal (debug build) runtime assertion failed.'
      write(*,*) ' Mismatch in number of kernel arguments.'
      write(*,*) ' Kernel name: ',kernel%name
      write(*,*) ' Number of kernel arguments: ',nKernelArg
      write(*,*) ' Number of arguments passed: ',nArg
      write(*,*)

      call fclRuntimeError('fclDbgCheckKernelNArg')

    end if

  end procedure fclDbgCheckKernelNArg
  ! ---------------------------------------------------------------------------


  module procedure fclDbgCheckKernelArgType !(kernel,argNo,type)

    character(:), allocatable :: argType
    call fclGetKernelArgInfo(kernel,argNo,CL_KERNEL_ARG_TYPE_NAME,argType)

    ! Use strStripNum to remove vector type information for comparison
    argType = trim(strStripNum(argType))

    if (index(argType,type) == 0 .or. &
          index(argType,'*') /= index(type,'*')) then

      write(*,*) '(!) Focal (debug build) runtime assertion failed.'
      write(*,*) ' Mismatch in type of kernel argument.'
      write(*,*) ' Kernel name: ',kernel%name
      write(*,*) ' Argument index: ',argNo
      write(*,*) ' Expecting ',argType,' but got ',type
      write(*,*)

      call fclRuntimeError('fclDbgCheckKernelArgType')

    end if

  end procedure fclDbgCheckKernelArgType
  ! ---------------------------------------------------------------------------


  module procedure fclDbgCheckKernelArgQualifier !(kernel,argNo,qualifier)

    integer :: argQual
    character(10) :: qualStr
    logical :: matched

    call fclGetKernelArgInfo(kernel,argNo,CL_KERNEL_ARG_ADDRESS_QUALIFIER,argQual)
    matched = .false.

    select case(argQual)

      case(CL_KERNEL_ARG_ADDRESS_LOCAL)
        qualStr = 'local'
        if (index(qualifier,'local') > 0) then
          matched =.true.
        end if

      case(CL_KERNEL_ARG_ADDRESS_GLOBAL)
        qualStr = 'global'
        if (index(qualifier,'global') > 0) then
          matched =.true.
        end if

      case(CL_KERNEL_ARG_ADDRESS_PRIVATE)
        qualStr = 'private'
        if (index(qualifier,'private') > 0) then
          matched =.true.
        end if

      case(CL_KERNEL_ARG_ADDRESS_CONSTANT)
        qualStr = 'constant'
        if (index(qualifier,'constant') > 0) then
          matched =.true.
        end if

      case default
        call fclRuntimeError('fclDbgCheckKernelArgQualifier: unknown qualifier returned by opencl api.')

    end select

    if(.not.matched) then

      write(*,*) '(!) Focal (debug build) runtime assertion failed.'
      write(*,*) ' Mismatch in address space qualifier of kernel argument.'
      write(*,*) ' Kernel name: ',kernel%name
      write(*,*) ' Argument index: ',argNo
      write(*,*) ' Expecting qualifer "',trim(qualStr),'" but given argument was one of "',qualifier,'".'
      write(*,*)

      call fclRuntimeError('fclDbgCheckKernelArgType')

    end if

  end procedure fclDbgCheckKernelArgQualifier
  ! ---------------------------------------------------------------------------


  module procedure fclDbgOptions !(options)
    !! Returns OpenCL compile options as interoperable string for debug mode

    options = '-cl-kernel-arg-info'   ! Required for focaldbg kernel argument checks

  end procedure fclDbgOptions
  ! ---------------------------------------------------------------------------


  module procedure fclDbgWait !(event,descrip)
    !! Wait for an event to complete and check for successful completion.
    !! Throw runtime error if status is not CL_COMPLETE.

    integer(c_int32_t) :: errcode
    integer(c_int32_t), target :: eStatus
    integer(c_size_t) :: temp_size, size_ret

    ! Call clWaitForEvents & clGetEventInfo directly to avoid built-in error handling here
    errcode = clWaitForEvents ( 1, c_loc(event%cl_event) )

    temp_size = c_sizeof(int(1,c_int32_t))
    errcode = ior(errcode , clGetEventInfo(event%cl_event, CL_EVENT_COMMAND_EXECUTION_STATUS, &
                temp_size, C_LOC(eStatus), size_ret))

    ! Catch errors
    if (errcode /= CL_SUCCESS .or. eStatus /= CL_SUCCESS) then

        write(*,*) '(!) Focal (debug build) runtime assertion failed.'
        if (present(descrip)) then
          write(*,*) ' An event ('//descrip//') has terminated abnormally.'
        else
          write(*,*) ' An event has terminated abnormally.'
        end if

        if (errcode == CL_SUCCESS) then

          ! Error is only in event status
          eStatus = -1 * eStatus
          write(*,*) ' Event execution error code: ',eStatus,' : ',trim(fclGetErrorString(eStatus))
          call fclRuntimeError('fclDbgWait')

        else

          ! Error is in api calls
          call fclErrorHandler(errcode,'fclDbgWait','clWaitForEvents|clGetEventInfo')

        end if

    end if

  end procedure fclDbgWait
  ! ---------------------------------------------------------------------------


end submodule Focal_Debug
