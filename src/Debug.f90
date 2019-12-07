submodule (Focal) Focal_Debug
  !! FOCAL: openCL abstraction layer for fortran
  !!  Implementation module for focal debug routines.
  !!  This submodule is linked in the debug version of Focal build.

  !! @note This is an implementation submodule: it contains the code implementing the subroutines defined in the
  !!  corresponding header module file. See header module file (Focal.f90) for interface definitions. @endnote
  use clfortran
  implicit none

  contains

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


end submodule Focal_Debug
