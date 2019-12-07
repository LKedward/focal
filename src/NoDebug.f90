submodule (Focal) Focal_NoDebug
  !! FOCAL: openCL abstraction layer for fortran
  !!  Implementation module for focal debug routines.
  !!  This submodule is linked in the release version of Focal build.

  !! @note This is an implementation submodule: it contains the code implementing the subroutines defined in the
  !!  corresponding header module file. See header module file (Focal.f90) for interface definitions. @endnote

  implicit none

  contains

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

end submodule Focal_NoDebug
