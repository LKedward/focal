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

module Focal_Test_Utils
!! Utility module for running tests
  
use iso_fortran_env, only: stderr => error_unit, sp=>real32, dp=>real64
use iso_c_binding, only: int32=>c_int32_t
use clfortran, only: CL_COMPILER_NOT_AVAILABLE
use Focal

implicit none

! --------- Global constants ---------
integer, parameter :: FCL_TEST_SUCCESS = 0
integer, parameter :: FCL_TEST_NOT_RUN = 101
integer, parameter :: FCL_TEST_FAILED = 102

integer, parameter :: FCL_TEST_SIZE = 100    !! Size of test arrays

real(sp), parameter :: SP_TOL = epsilon(real(1.0,sp))
real(dp), parameter :: DP_TOL = epsilon(real(1.0d0,dp))

! --------- Global variables ---------
integer :: fclTestResult
type(fclDevice) :: ocl_device

! --------- Interfaces ---------
interface fclTestAssertEqual
  module procedure fclTestAssertEqualReal32
  module procedure fclTestAssertEqualReal64
  module procedure fclTestAssertEqualInt32
  module procedure fclTestAssertEqualCChar
end interface fclTestAssertEqual

contains

  subroutine fclTestInit()
    !! Perform initialisation for test framework

    type(fclDevice), allocatable :: devices(:)

    ! Initialise test flag
    fclTestResult = FCL_TEST_SUCCESS

    ! Override error handler with custom one
    fclErrorHandler => fclTestErrorHandler

    ! Create context with first platform
    call fclSetDefaultContext(fclCreateContext(vendor='nvidia,amd,intel'))

    ! Select device with most cores and create command queue
    devices = fclFindDevices(sortBy='cores')
    ocl_device = devices(1)
    call fclSetDefaultCommandQ(fclCreateCommandQ(ocl_device, &
                enableProfiling=.true.,outOfOrderExec=.false.))

  end subroutine fclTestInit
  ! ---------------------------------------------------------------------------


  subroutine fclTestErrorHandler(errcode, focalCall, oclCall)
    !! Error handler to catch certain errors
    integer, intent(in) :: errcode
    character(*), intent(in) :: focalCall
    character(*), intent(in) :: oclCall


    if (errcode == CL_PLATFORM_NOT_FOUND_KHR) then

      write(stderr,*) 'CL_PLATFORM_NOT_FOUND_KHR'
      write(stderr,*) 'No OpenCL platform was found: not running tests.'

      write(stderr,*) 'TEST_NOT_RUN'
      stop FCL_TEST_NOT_RUN

    elseif (errcode == CL_COMPILER_NOT_AVAILABLE) then

      write(stderr,*) 'CL_COMPILER_NOT_AVAILABLE'
      write(stderr,*) 'No compiler was available on the OpenCL platform: not running tests.'

      write(stderr,*) 'TEST_NOT_RUN'
      stop FCL_TEST_NOT_RUN

    else
      ! Rethrow all other OpenCL errors
      call fclDefaultErrorHandler(errcode, focalCall, oclCall)

    end if

  end subroutine fclTestErrorHandler
  ! ---------------------------------------------------------------------------


  subroutine fclTestAssert(a,descrip)
    !! Check if a logical condition holds
    logical, intent(in) :: a
    character(*), intent(in) :: descrip

    if (.not.a) then
      call fclTestAssertFailed(descrip)
    end if

  end subroutine fclTestAssert
  ! ---------------------------------------------------------------------------


  subroutine fclTestAssertEqualReal32(a1,a2,descrip,tol)
    !! Check if two real32 arrays are equal (to tolerance)
    real(sp), intent(in) :: a1(:)
    real(sp), intent(in) :: a2(size(a1,1))
    character(*), intent(in) :: descrip
    real(sp), intent(in), optional :: tol

    integer :: i
    real(sp) :: tolerance
    logical :: failed

    if (present(tol)) then
      tolerance = tol
    else
      tolerance = SP_TOL
    end if

    failed = .false.
    do i=1,size(a1,1)
      if (abs(a1(i)-a2(i)) > tolerance) then
        failed = .true.
        exit
      end if
    end do

    if (failed) then
      call fclTestAssertFailed(descrip)
    end if

  end subroutine fclTestAssertEqualReal32
  ! ---------------------------------------------------------------------------

  subroutine fclTestAssertEqualReal64(a1,a2,descrip,tol)
    !! Check if two real64 arrays are equal (to tolerance)
    real(dp), intent(in) :: a1(:)
    real(dp), intent(in) :: a2(size(a1,1))
    character(*), intent(in) :: descrip
    real(dp), intent(in), optional :: tol

    integer :: i
    real(dp) :: tolerance
    logical :: failed

    if (present(tol)) then
      tolerance = tol
    else
      tolerance = DP_TOL
    end if

    failed = .false.
    do i=1,size(a1,1)
      if (abs(a1(i)-a2(i)) > tolerance) then
        failed = .true.
        exit
      end if
    end do

    if (failed) then
      call fclTestAssertFailed(descrip)
    end if

  end subroutine fclTestAssertEqualReal64
  ! ---------------------------------------------------------------------------


  subroutine fclTestAssertEqualInt32(a1,a2,descrip)
    !! Check if two int32 arrays are equal
    integer(int32), intent(in) :: a1(:)
    integer(int32), intent(in) :: a2(size(a1,1))
    character(*), intent(in) :: descrip

    integer :: i
    logical :: failed

    failed = .false.
    do i=1,size(a1,1)
      if (a1(i) /= a2(i)) then
        failed = .true.
        exit
      end if
    end do

    if (failed) then
      call fclTestAssertFailed(descrip)
    end if

  end subroutine fclTestAssertEqualInt32
  ! ---------------------------------------------------------------------------


  subroutine fclTestAssertEqualCChar(a1,a2,descrip)
    !! Check if two c character arrays are equal
    character(len=1), intent(in) :: a1(:)
    character(len=1), intent(in) :: a2(size(a1,1))
    character(*), intent(in) :: descrip

    integer :: i
    logical :: failed

    failed = .false.
    do i=1,size(a1,1)
      if (a1(i) /= a2(i)) then
        failed = .true.
        exit
      end if
    end do

    if (failed) then
      call fclTestAssertFailed(descrip)
    end if

  end subroutine fclTestAssertEqualCChar
  ! ---------------------------------------------------------------------------


  subroutine fclTestAssertFailed(descrip)
    character(*), intent(in) :: descrip

    fclTestResult = FCL_TEST_FAILED
    write(stderr,*) ' (!) Assertion failed: ', descrip

  end subroutine fclTestAssertFailed
  ! ---------------------------------------------------------------------------

  subroutine fclTestFinish

    if (fclTestResult == FCL_TEST_SUCCESS) then

      write(stderr,*) 'TEST_SUCCESS'
      stop FCL_TEST_SUCCESS

    elseif (fclTestResult == FCL_TEST_NOT_RUN) then

      write(stderr,*) 'TEST_NOT_RUN'
      stop FCL_TEST_NOT_RUN

    else

      write(stderr,*) 'TEST_FAILED'
      stop FCL_TEST_FAILED

    end if

  end subroutine fclTestFinish

end module Focal_Test_Utils
