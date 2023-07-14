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

submodule (Focal) Focal_Query
  !!  Implementation module for query routines

  !! @note This is an implementation submodule: it contains the code implementing the subroutines defined in the
  !!  corresponding header module file. See header module file (Focal.f90) for interface definitions. @endnote

  use clfortran
  implicit none

  contains

  module procedure fclGetPlatformInfo !(platform,key,value)
    ! https://www.khronos.org/registry/OpenCL/sdk/1.0/docs/man/xhtml/clGetDeviceInfo.html

    integer(c_size_t) :: zero_size = 0
    integer(c_int32_t) :: errcode
    integer(c_size_t) :: temp_size, size_ret

    errcode = clGetPlatformInfo(platform%cl_platform_id, key, zero_size, C_NULL_PTR, temp_size)
    call fclHandleError(errcode,'fclGetPlatformInfo','clGetPlatformInfo')

    allocate(character(len=temp_size) :: value)

    errcode = clGetPlatformInfo(platform%cl_platform_id, key, temp_size, C_LOC(value), size_ret)
    call fclHandleError(errcode,'fclGetPlatformInfo','clGetPlatformInfo')

    ! Remove non-printable characters (terminating null char)
    value = str_noesc(value)

  end procedure fclGetPlatformInfo
  ! ---------------------------------------------------------------------------


  module procedure fclGetDeviceInfoString !(device,key,value)
    ! https://www.khronos.org/registry/OpenCL/sdk/1.0/docs/man/xhtml/clGetDeviceInfo.html

    integer(c_size_t) :: zero_size = 0
    integer(c_int32_t) :: errcode
    integer(c_size_t) :: temp_size, size_ret

    errcode = clGetDeviceInfo(device%cl_device_id, key, zero_size, C_NULL_PTR, temp_size)
    call fclHandleError(errcode,'fclGetDeviceInfoString','clGetDeviceInfo')

    allocate( character(len=temp_size) :: value)
    errcode = clGetDeviceInfo(device%cl_device_id, key, temp_size, C_LOC(value), size_ret)
    call fclHandleError(errcode,'fclGetDeviceInfoString','clGetDeviceInfo')

    ! Remove non-printable characters (terminating null char)
    value = str_noesc(value)

  end procedure fclGetDeviceInfoString
  ! ---------------------------------------------------------------------------


  module procedure fclGetDeviceInfoInt32 !(device,key,value)
    ! https://www.khronos.org/registry/OpenCL/sdk/1.0/docs/man/xhtml/clGetDeviceInfo.html

    integer(c_int32_t) :: errcode
    integer(c_size_t) :: temp_size, size_ret

    temp_size = c_sizeof(int(1,c_int32_t))
    errcode = clGetDeviceInfo(device%cl_device_id, key, temp_size, C_LOC(value), size_ret)
    call fclHandleError(errcode,'fclGetDeviceInfoInt32','clGetDeviceInfo')

  end procedure fclGetDeviceInfoInt32
  ! ---------------------------------------------------------------------------


  module procedure fclGetDeviceInfoInt64 !(device,key,value)
    ! https://www.khronos.org/registry/OpenCL/sdk/1.0/docs/man/xhtml/clGetDeviceInfo.html

    integer(c_int32_t) :: errcode
    integer(c_size_t) :: temp_size, size_ret

    temp_size = c_sizeof(int(1,c_int64_t))
    errcode = clGetDeviceInfo(device%cl_device_id, key, temp_size, C_LOC(value), size_ret)
    call fclHandleError(errcode,'fclGetDeviceInfoInt64','clGetDeviceInfo')

  end procedure fclGetDeviceInfoInt64
  ! ---------------------------------------------------------------------------


  module procedure fclGetKernelInfoString !(kernel,key,value)
    !! Query kernel information for string info.
    ! https://www.khronos.org/registry/OpenCL/sdk/1.0/docs/man/xhtml/clGetKernelInfo.html

    integer(c_size_t) :: zero_size = 0
    integer(c_int32_t) :: errcode
    integer(c_size_t) :: temp_size, size_ret

    errcode = clGetKernelInfo(kernel%cl_kernel, key, zero_size, C_NULL_PTR, temp_size)
    call fclHandleError(errcode,'fclGetKernelInfoString','clGetKernelInfo')

    allocate( character(len=temp_size) :: value)
    errcode = clGetKernelInfo(kernel%cl_kernel, key, temp_size, C_LOC(value), size_ret)
    call fclHandleError(errcode,'fclGetKernelInfoString','clGetKernelInfo')

    ! Remove non-printable characters (terminating null char)
    value = str_noesc(value)

  end procedure fclGetKernelInfoString
  ! ---------------------------------------------------------------------------


  module procedure fclGetKernelInfoInt32 !(kernel,key,value)
    !! Query kernel information for 32bit integer.
    ! https://www.khronos.org/registry/OpenCL/sdk/1.0/docs/man/xhtml/clGetKernelInfo.html

    integer(c_int32_t) :: errcode
    integer(c_size_t) :: temp_size, size_ret

    temp_size = c_sizeof(int(1,c_int32_t))
    errcode = clGetKernelInfo(kernel%cl_kernel, key, temp_size, C_LOC(value), size_ret)
    call fclHandleError(errcode,'fclGetKernelInfoInt32','clGetKernelInfo')

  end procedure fclGetKernelInfoInt32
  ! ---------------------------------------------------------------------------


  module procedure fclGetKernelWorkGroupInfoInt64 !(kernel,device,key,value)
    !! Query kernel work group information for 64bit integer.
    ! https://www.khronos.org/registry/OpenCL/sdk/1.0/docs/man/xhtml/clGetKernelWorkGroupInfo.html

    integer(c_int32_t) :: errcode
    integer(c_size_t) :: temp_size, size_ret

    temp_size = c_sizeof(int(1,c_int64_t))
    errcode = clGetKernelWorkGroupInfo(kernel%cl_kernel, device%cl_device_id, key, temp_size, C_LOC(value), size_ret)
    call fclHandleError(errcode,'fclGetKernelWorkGroupInfoInt64','clGetKernelWorkGroupInfo')

  end procedure fclGetKernelWorkGroupInfoInt64
  ! ---------------------------------------------------------------------------


  module procedure fclGetKernelArgInfoString !(kernel,key,value)
    !! Query kernel argument information for string info.
    ! https://www.khronos.org/registry/OpenCL/sdk/1.0/docs/man/xhtml/clGetKernelArgInfo.html

    integer(c_size_t) :: zero_size = 0
    integer(c_int32_t) :: errcode
    integer(c_size_t) :: temp_size, size_ret

    errcode = clGetKernelArgInfo(kernel%cl_kernel, argNo, key, zero_size, C_NULL_PTR, temp_size)
    call fclHandleError(errcode,'fclGetKernelArgInfoString','clGetKernelArgInfo')

    allocate( character(len=temp_size) :: value)
    errcode = clGetKernelArgInfo(kernel%cl_kernel, argNo, key, temp_size, C_LOC(value), size_ret)
    call fclHandleError(errcode,'fclGetKernelArgInfoString','clGetKernelArgInfo')

    ! Remove non-printable characters (terminating null char)
    value = str_noesc(value)
    
  end procedure fclGetKernelArgInfoString
  ! ---------------------------------------------------------------------------


  module procedure fclGetKernelArgInfoInt32 !(kernel,argNo,key,value)
    !! Query kernel argument information for 32bit integer.
    ! https://www.khronos.org/registry/OpenCL/sdk/1.0/docs/man/xhtml/clGetKernelArgInfo.html

    integer(c_int32_t) :: errcode
    integer(c_size_t) :: temp_size, size_ret

    temp_size = c_sizeof(int(1,c_int32_t))
    errcode = clGetKernelArgInfo(kernel%cl_kernel, argNo, key, temp_size, C_LOC(value), size_ret)
    call fclHandleError(errcode,'fclGetKernelInfoInt32','clGetKernelInfo')

  end procedure fclGetKernelArgInfoInt32
  ! ---------------------------------------------------------------------------


  module procedure fclGetEventInfo !(event,key,value)
    !! Query event information for 32bit integer.
    ! https://www.khronos.org/registry/OpenCL/sdk/1.0/docs/man/xhtml/clGetEventInfo.html

    integer(c_int32_t) :: errcode
    integer(c_size_t) :: temp_size, size_ret

    temp_size = c_sizeof(int(1,c_int32_t))
    errcode = clGetEventInfo(event%cl_event, key, temp_size, C_LOC(value), size_ret)
    call fclHandleError(errcode,'fclGetEventInfo','clGetEventInfo')

  end procedure fclGetEventInfo
  ! ---------------------------------------------------------------------------


  module procedure fclGetPlatforms !result(platforms)

    integer :: i
    integer(c_int32_t) :: num_platforms, int32_ret
    integer(c_int32_t) :: errcode

    integer(c_intptr_t), allocatable, target :: platform_ids(:)

    ! Get number of platforms
    errcode = clGetPlatformIDs(0,C_NULL_PTR,num_platforms)
    call fclHandleError(errcode,'fclGetPlatforms','clGetPlatformIDs')

    ! Allocate platform_ids array
    allocate(platform_ids(num_platforms))

    ! Populate platform_ids array
    errcode = clGetPlatformIDs(num_platforms,c_loc(platform_ids),int32_ret)
    call fclHandleError(errcode,'fclGetPlatforms','clGetPlatformIDs')

    ! Populate output fclPlatform structure array
    allocate(platforms(num_platforms))

    do i=1,num_platforms

      platforms(i) = fclGetPlatform(platform_ids(i))

    end do

    ! Cleanup
    deallocate(platform_ids)



  end procedure fclGetPlatforms
  ! ---------------------------------------------------------------------------


  module procedure fclGetPlatform !(platform_id) result(platform)

    integer :: i

    integer(c_int32_t) :: errcode
    integer(c_int32_t) :: int32_ret


    platform%cl_platform_id = platform_id

    ! --- Get number of devices ---
    errcode = clGetDeviceIDs(platform_id, CL_DEVICE_TYPE_ALL, 0, C_NULL_PTR, platform%numDevice)
    call fclHandleError(errcode,'fclGetPlatform','clGetDeviceIDs')

    allocate(platform%devices(platform%numDevice))
    allocate(platform%cl_device_ids(platform%numDevice))

    ! --- Get device ids ---
    errcode = clGetDeviceIDs(platform_id, CL_DEVICE_TYPE_ALL, platform%numDevice, &
                                  C_LOC(platform%cl_device_ids), int32_ret)
    call fclHandleError(errcode,'fclGetPlatform','clGetDeviceIDs')

    ! --- Populate fclPlatform info strings ---
    call fclGetPlatformInfo(platform,CL_PLATFORM_PROFILE,platform%profile)
    call fclGetPlatformInfo(platform,CL_PLATFORM_VERSION,platform%version)
    call fclGetPlatformInfo(platform,CL_PLATFORM_NAME,platform%name)
    call fclGetPlatformInfo(platform,CL_PLATFORM_VENDOR,platform%vendor)
    call fclGetPlatformInfo(platform,CL_PLATFORM_EXTENSIONS,platform%extensions)

    ! --- Populate fclDevice structure array ---
    do i=1,platform%numDevice

      platform%devices(i) = fclGetDevice(platform%cl_device_ids(i))
      platform%devices(i)%platformName = platform%name
      platform%devices(i)%platformVendor = platform%vendor
      platform%devices(i)%cl_platform_id = platform_id

    end do


  end procedure fclGetPlatform
  ! ---------------------------------------------------------------------------


  module procedure fclGetDevice !(device_id) result(device)
    ! https://www.khronos.org/registry/OpenCL/sdk/1.0/docs/man/xhtml/clGetDeviceInfo.html

    device%cl_device_id = device_id

    call fclGetDeviceInfo(device,CL_DEVICE_NAME,device%name)
    call fclGetDeviceInfo(device,CL_DEVICE_TYPE,device%cl_device_type)
    call fclGetDeviceInfo(device,CL_DEVICE_MAX_COMPUTE_UNITS,device%nComputeUnits)
    call fclGetDeviceInfo(device,CL_DEVICE_GLOBAL_MEM_SIZE,device%global_memory)
    call fclGetDeviceInfo(device,CL_DEVICE_MAX_CLOCK_FREQUENCY,device%clock_freq)
    call fclGetDeviceInfo(device,CL_DEVICE_VERSION,device%version)
    call fclGetDeviceInfo(device,CL_DEVICE_EXTENSIONS,device%extensions)

  end procedure fclGetDevice
  ! ---------------------------------------------------------------------------



end submodule Focal_Query
