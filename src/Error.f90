submodule (Focal) Focal_Error
  !! FOCAL: openCL abstraction layer for fortran
  !!  Implementation module for error handling

  !! @note This is an implementation submodule: it contains the code implementing the subroutines defined in the
  !!  corresponding header module file. See header module file (Focal.f90) for interface definitions. @endnote

  use clfortran
  implicit none
  
  integer, parameter :: CL_PLATFORM_NOT_FOUND_KHR = -1001
    !! Extension error: No valid ICDs found
  
  contains

  module procedure fclDefaultErrorHandler
    
    if (errcode /= CL_SUCCESS) then

      write(*,*) '(!) Fatal openCl error ',errcode,' : ',trim(fclGetErrorString(errcode))
      write(*,*) '      at ',focalCall,':',oclCall
      
      stop 1
    end if

  end procedure fclDefaultErrorHandler
  ! ---------------------------------------------------------------------------


  module procedure fclHandleBuildError !(builderrcode,prog,ctx)

    integer :: i
    integer(c_int32_t) :: errcode
    integer(c_size_t), target :: buffLen, int32_ret
    character(len=1,kind=c_char), allocatable, target :: buildLogBuffer(:)

    ! Handle compilation error
    if (builderrcode /= CL_SUCCESS) then

      write(*,*) '(!) Fatal openCl error while building kernel: ',errcode,' : ',trim(fclGetErrorString(errcode))

      ! Iterate over context devices 
      do i=1,ctx%platform%numDevice

        write(*,'(A,I3)') ' Build log for context device ',i
        write(*,*) ' (',ctx%platform%devices(i)%name,'):'

        errcode = clGetProgramBuildInfo(prog%cl_program, ctx%platform%cl_device_ids(1), &
          CL_PROGRAM_BUILD_LOG, int(0,c_size_t), C_NULL_PTR, buffLen);

        call fclErrorHandler(errcode,'fclCompileProgram','clGetProgramBuildInfo')

        allocate(buildLogBuffer(buffLen))
        buffLen = size(buildLogBuffer,1)

        errcode = clGetProgramBuildInfo(prog%cl_program, ctx%platform%cl_device_ids(1), & 
          CL_PROGRAM_BUILD_LOG, buffLen, c_loc(buildLogBuffer), int32_ret);

        call fclErrorHandler(errcode,'fclCompileProgram','clGetProgramBuildInfo')

        write(*,*) buildLogBuffer
        write(*,*)

        deallocate(buildLogBuffer)

      end do

      stop
    end if

  end procedure fclHandleBuildError
  ! ---------------------------------------------------------------------------


  module procedure fclGetErrorString !(errcode)

    select case(errcode)
      case (CL_DEVICE_NOT_FOUND)
        errstr = 'CL_DEVICE_NOT_FOUND'

      case (CL_DEVICE_NOT_AVAILABLE)
        errstr = 'CL_DEVICE_NOT_AVAILABLE'

      case (CL_COMPILER_NOT_AVAILABLE)
        errstr = 'CL_COMPILER_NOT_AVAILABLE'

      case (CL_MEM_OBJECT_ALLOCATION_FAILURE)
        errstr = 'CL_MEM_OBJECT_ALLOCATION_FAILURE'

      case (CL_OUT_OF_HOST_MEMORY)
        errstr = 'CL_OUT_OF_HOST_MEMORY'

      case (CL_BUILD_PROGRAM_FAILURE)
        errstr = 'CL_BUILD_PROGRAM_FAILURE'

      case (CL_INVALID_VALUE)
        errstr = 'CL_INVALID_VALUE'

      case (CL_INVALID_PLATFORM)
        errstr = 'CL_INVALID_PLATFORM'

      case (CL_INVALID_DEVICE)
        errstr = 'CL_INVALID_DEVICE'

      case (CL_INVALID_CONTEXT)
        errstr = 'CL_INVALID_CONTEXT'

      case (CL_INVALID_COMMAND_QUEUE)
        errstr = 'CL_INVALID_COMMAND_QUEUE'

      case (CL_INVALID_MEM_OBJECT)
        errstr = 'CL_INVALID_MEM_OBJECT'

      case (CL_INVALID_BINARY)
        errstr = 'CL_INVALID_BINARY'

      case (CL_INVALID_BUILD_OPTIONS)
        errstr = 'CL_INVALID_BUILD_OPTIONS'

      case (CL_INVALID_PROGRAM)
        errstr = 'CL_INVALID_PROGRAM'
        
      case (CL_INVALID_ARG_INDEX)
        errstr = 'CL_INVALID_ARG_INDEX'
        
      case (CL_INVALID_ARG_VALUE)
        errstr = 'CL_INVALID_ARG_VALUE'

      case (CL_INVALID_ARG_SIZE)
        errstr = 'CL_INVALID_ARG_SIZE'

      case (CL_INVALID_KERNEL_ARGS)
        errstr = 'CL_INVALID_KERNEL_ARGS'

      case (CL_INVALID_EVENT_WAIT_LIST)
        errstr = 'CL_INVALID_EVENT_WAIT_LIST'

      case (CL_INVALID_EVENT)
        errstr = 'CL_INVALID_EVENT'

      case (CL_INVALID_OPERATION)
        errstr = 'CL_INVALID_OPERATION'

      case (CL_PLATFORM_NOT_FOUND_KHR)
        errstr = 'CL_PLATFORM_NOT_FOUND_KHR'

      case default
        errstr = 'UNKNOWN'

    end select

  end procedure fclGetErrorString
  ! ---------------------------------------------------------------------------


  module procedure fclRuntimeError !(descrip)

    write(*,*) '(!) Fatal runtime error: an incorrect Focal program has been written.'
    if (present(descrip)) then
        write(*,*) '      at ',descrip
      end if
    stop

  end procedure fclRuntimeError
  ! ---------------------------------------------------------------------------


end submodule Focal_Error