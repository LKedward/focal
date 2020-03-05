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

submodule (Focal) Focal_NoDebug
  !!  Implementation module for focal debug routines.
  !!  This submodule is linked in the release version of Focal build.

  !! @note This is an implementation submodule: it contains the code implementing the subroutines defined in the
  !!  corresponding header module file. See header module file (Focal.f90) for interface definitions. @endnote

  implicit none

  contains

  module procedure fclDbgCheckContext !(ctx)
    !! Check the (default) context is initialised.

  end procedure fclDbgCheckContext
  ! ---------------------------------------------------------------------------


  module procedure fclDbgCheckBufferInit !(memObject)
    !! Check that a device buffer object has been initialised.

  end procedure fclDbgCheckBufferInit
  ! ---------------------------------------------------------------------------

  module procedure fclDbgCheckBufferSize !(memObject,hostBytes)
    !! Check that a host buffer matches the size in bytes of a device buffer

  end procedure fclDbgCheckBufferSize
  ! ---------------------------------------------------------------------------

  module procedure fclDbgCheckCopyBufferSize !(memObject1,memObject2)
    !! Check that device buffers match in size in bytes for copying

  end procedure fclDbgCheckCopyBufferSize
  ! ---------------------------------------------------------------------------

  module procedure fclDbgCheckKernelNArg !(kernel,nArg)
    !! Check that number of actual args matches number of kernel args

  end procedure fclDbgCheckKernelNArg
  ! ---------------------------------------------------------------------------

  module procedure fclDbgCheckKernelArgType !(kernel,argNo,type)

  end procedure fclDbgCheckKernelArgType
  ! ---------------------------------------------------------------------------

  module procedure fclDbgCheckKernelArgQualifier !(kernel,argNo,qualifier)

  end procedure fclDbgCheckKernelArgQualifier
  ! ---------------------------------------------------------------------------

  module procedure fclDbgOptions !(options)
    options = ''
  end procedure fclDbgOptions
  ! ---------------------------------------------------------------------------

  module procedure fclDbgWait !(event,descrip)

  end procedure fclDbgWait
  ! ---------------------------------------------------------------------------

end submodule Focal_NoDebug
