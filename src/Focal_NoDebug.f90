submodule (Focal) Focal_NoDebug
  !! FOCAL: openCL abstraction layer for fortran
  !!  Implementation module for focal debug routines.
  !!  This submodule is linked in the release version of Focal build.

  !! @note This is an implementation submodule: it contains the code implementing the subroutines defined in the
  !!  corresponding header module file. See header module file (Focal.f90) for interface definitions. @endnote

  implicit none

  contains

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
