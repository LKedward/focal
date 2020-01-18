submodule (Focal) Focal_HostMemory
  !! FOCAL: openCL abstraction layer for fortran
  !!  Implementation module

  !! @note This is an implementation submodule: it contains the code implementing the subroutines defined in the
  !!  corresponding header module file. See header module file (Focal.f90) for interface definitions. @endnote

  use clfortran
  implicit none

  contains


  module procedure fclAllocHostPtr !(cmdq,nBytes) result(ptr)
    !! Allocate a 'pinned' host array

    integer(c_int32_t) :: errcode
    integer(c_intptr_t), target :: cl_context
    integer(c_size_t) :: size_ret

    integer(c_intptr_t) :: devicePtr

    ! Get command queue context
    errcode = clGetCommandQueueInfo(cmdq%cl_command_queue, &
                  CL_QUEUE_CONTEXT,c_sizeof(cl_context), &
                  c_loc(cl_context), size_ret)

    call fclErrorHandler(errcode,'fclAllocHostPtr','clGetCommandQueueInfo')

    devicePtr = clCreateBuffer(cl_context, CL_MEM_ALLOC_HOST_PTR, nBytes,&
                                  C_NULL_PTR, errcode)

    call fclErrorhandler(errcode,'fclAllocHostPtr','clCreateBuffer')


    ptr = clEnqueueMapBuffer(cmdq%cl_command_queue,&
            devicePtr, CL_TRUE,&
            CL_MAP_WRITE, int(0,c_int64_t), nBytes, 0,&
            C_NULL_PTR, C_NULL_PTR, errcode)

    call fclErrorhandler(errcode,'fclAllocHostPtr','clEnqueueMapBuffer')

  end procedure fclAllocHostPtr
  ! ---------------------------------------------------------------------------
  

  module procedure fclAllocHostInt32D1_1 !(cmdq,hostPtr,dim)
    !! Allocate a 1D 'pinned' host array for 32bit integers

    type(c_ptr) :: ptr

    ptr = fclAllocHostPtr(cmdq,dim*c_sizeof(int(1,c_int32_t)))

    call c_f_pointer(ptr,hostPtr,[dim])

  end procedure fclAllocHostInt32D1_1
  ! ---------------------------------------------------------------------------
  

  module procedure fclAllocHostInt32D1_2 !(hostPtr,dim)
    !! Allocate a 1D 'pinned' host array for 32bit integers on default cmdq

    call fclAllocHostInt32D1_1 (fclDefaultCmdQ,hostPtr,dim)

  end procedure fclAllocHostInt32D1_2
  ! ---------------------------------------------------------------------------
  

  module procedure fclAllocHostInt32D2_1 !(cmdq,hostPtr,dim)
    !! Allocate a 2D 'pinned' host array for 32bit integers

    type(c_ptr) :: ptr

    ptr = fclAllocHostPtr(cmdq,product(dim)*c_sizeof(int(1,c_int32_t)))

    call c_f_pointer(ptr,hostPtr,dim)

  end procedure fclAllocHostInt32D2_1
  ! ---------------------------------------------------------------------------
  

  module procedure fclAllocHostInt32D2_2 !(hostPtr,dim)
    !! Allocate a 2D 'pinned' host array for 32bit integers on default cmdq

    call fclAllocHostInt32D2_1 (fclDefaultCmdQ,hostPtr,dim)

  end procedure fclAllocHostInt32D2_2
  ! ---------------------------------------------------------------------------


  module procedure fclAllocHostFloatD1_1 !(cmdq,hostPtr,dim)
    !! Allocate a 1D 'pinned' host array for 32bit reals

    type(c_ptr) :: ptr

    ptr = fclAllocHostPtr(cmdq,dim*c_sizeof(real(1.0,c_float)))

    call c_f_pointer(ptr,hostPtr,[dim])

  end procedure fclAllocHostFloatD1_1
  ! ---------------------------------------------------------------------------
  

  module procedure fclAllocHostFloatD1_2 !(hostPtr,dim)
    !! Allocate a 1D 'pinned' host array for 32bit reals on default cmdq

    call fclAllocHostFloatD1_1 (fclDefaultCmdQ,hostPtr,dim)

  end procedure fclAllocHostFloatD1_2
  ! ---------------------------------------------------------------------------
  

  module procedure fclAllocHostFloatD2_1 !(cmdq,hostPtr,dim)
    !! Allocate a 2D 'pinned' host array for 32bit reals

    type(c_ptr) :: ptr

    ptr = fclAllocHostPtr(cmdq,product(dim)*c_sizeof(real(1,c_Float)))

    call c_f_pointer(ptr,hostPtr,dim)

  end procedure fclAllocHostFloatD2_1
  ! ---------------------------------------------------------------------------
  

  module procedure fclAllocHostFloatD2_2 !(hostPtr,dim)
    !! Allocate a 2D 'pinned' host array for 32bit reals on default cmdq

    call fclAllocHostFloatD2_1 (fclDefaultCmdQ,hostPtr,dim)

  end procedure fclAllocHostFloatD2_2
  ! ---------------------------------------------------------------------------


  module procedure fclAllocHostDoubleD1_1 !(cmdq,hostPtr,dim)
    !! Allocate a 1D 'pinned' host array for 64bit reals

    type(c_ptr) :: ptr

    ptr = fclAllocHostPtr(cmdq,dim*c_sizeof(real(1.0,c_Double)))

    call c_f_pointer(ptr,hostPtr,[dim])

  end procedure fclAllocHostDoubleD1_1
  ! ---------------------------------------------------------------------------
  

  module procedure fclAllocHostDoubleD1_2 !(hostPtr,dim)
    !! Allocate a 1D 'pinned' host array for 64bit reals on default cmdq

    call fclAllocHostDoubleD1_1 (fclDefaultCmdQ,hostPtr,dim)

  end procedure fclAllocHostDoubleD1_2
  ! ---------------------------------------------------------------------------
  

  module procedure fclAllocHostDoubleD2_1 !(cmdq,hostPtr,dim)
    !! Allocate a 2D 'pinned' host array for 64bit reals

    type(c_ptr) :: ptr

    ptr = fclAllocHostPtr(cmdq,product(dim)*c_sizeof(real(1,c_Double)))

    call c_f_pointer(ptr,hostPtr,dim)

  end procedure fclAllocHostDoubleD2_1
  ! ---------------------------------------------------------------------------
  

  module procedure fclAllocHostDoubleD2_2 !(hostPtr,dim)
    !! Allocate a 2D 'pinned' host array for 64bit reals on default cmdq

    call fclAllocHostDoubleD2_1 (fclDefaultCmdQ,hostPtr,dim)

  end procedure fclAllocHostDoubleD2_2
  ! ---------------------------------------------------------------------------


end submodule Focal_HostMemory
