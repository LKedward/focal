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

submodule (Focal) Focal_HostMemory
  !!  Implementation module

  !! @note This is an implementation submodule: it contains the code implementing the subroutines defined in the
  !!  corresponding header module file. See header module file (Focal.f90) for interface definitions. @endnote

  use clfortran
  implicit none

  contains


  module procedure fclAllocHostPtr_1 !(cmdq,hostPtr,nBytes)
    !! Allocate a 'pinned' (non-paged) host array

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


    hostPtr = clEnqueueMapBuffer(cmdq%cl_command_queue,&
            devicePtr, CL_TRUE,&
            ior(CL_MAP_WRITE,CL_MAP_READ), int(0,c_int64_t), nBytes, 0,&
            C_NULL_PTR, C_NULL_PTR, errcode)

    call fclErrorhandler(errcode,'fclAllocHostPtr','clEnqueueMapBuffer')

  end procedure fclAllocHostPtr_1
  ! ---------------------------------------------------------------------------
  
  module procedure fclAllocHostPtr_2 !(hostPtr,nBytes)
    !! Allocate a 'pinned' (non-paged) host array on default command queue

    call fclAllocHostPtr_1(fclDefaultCmdQ,hostPtr,nBytes)

  end procedure fclAllocHostPtr_2
  ! ---------------------------------------------------------------------------

  module procedure fclAllocHostInt32D1_1 !(cmdq,hostPtr,dim)
    !! Allocate a 1D 'pinned' host array for 32bit integers

    type(c_ptr) :: ptr

    call fclAllocHostPtr_1(cmdq,ptr,dim*c_sizeof(int(1,c_int32_t)))

    call c_f_pointer(ptr,hostPtr,[dim])

  end procedure fclAllocHostInt32D1_1
  ! ---------------------------------------------------------------------------
  

  module procedure fclAllocHostInt32D1_2 !(hostPtr,dim)
    !! Allocate a 1D 'pinned' host array for 32bit integers on default cmdq

    call fclAllocHostInt32D1_1(fclDefaultCmdQ,hostPtr,dim)

  end procedure fclAllocHostInt32D1_2
  ! ---------------------------------------------------------------------------
  

  module procedure fclAllocHostInt32D2_1 !(cmdq,hostPtr,dim)
    !! Allocate a 2D 'pinned' host array for 32bit integers

    type(c_ptr) :: ptr

    call fclAllocHostPtr_1(cmdq,ptr,product(dim)*c_sizeof(int(1,c_int32_t)))

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

    call fclAllocHostPtr_1(cmdq,ptr,dim*c_sizeof(real(1.0,c_float)))

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

    call fclAllocHostPtr_1(cmdq,ptr,product(dim)*c_sizeof(real(1,c_Float)))

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

    call fclAllocHostPtr_1(cmdq,ptr,dim*c_sizeof(real(1.0,c_Double)))

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

    call fclAllocHostPtr_1(cmdq,ptr,product(dim)*c_sizeof(real(1,c_Double)))

    call c_f_pointer(ptr,hostPtr,dim)

  end procedure fclAllocHostDoubleD2_1
  ! ---------------------------------------------------------------------------
  

  module procedure fclAllocHostDoubleD2_2 !(hostPtr,dim)
    !! Allocate a 2D 'pinned' host array for 64bit reals on default cmdq

    call fclAllocHostDoubleD2_1 (fclDefaultCmdQ,hostPtr,dim)

  end procedure fclAllocHostDoubleD2_2
  ! ---------------------------------------------------------------------------


end submodule Focal_HostMemory
