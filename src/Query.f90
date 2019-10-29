submodule (Focal) Focal_Query
  !! FOCAL: openCL abstraction layer for fortran
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
    call fclHandleErrorCode(errcode,'fclGetPlatformInfo::clGetPlatformInfo')

    allocate(character(len=temp_size) :: value)

    errcode = clGetPlatformInfo(platform%cl_platform_id, key, temp_size, C_LOC(value), size_ret)
    call fclHandleErrorCode(errcode,'fclGetPlatformInfo::clGetPlatformInfo')

  end procedure fclGetPlatformInfo
  ! ---------------------------------------------------------------------------


  module procedure fclGetDeviceInfoString !(device,key,value)
    ! https://www.khronos.org/registry/OpenCL/sdk/1.0/docs/man/xhtml/clGetDeviceInfo.html

    integer(c_size_t) :: zero_size = 0
    integer(c_int32_t) :: errcode
    integer(c_size_t) :: temp_size, size_ret

    errcode = clGetDeviceInfo(device%cl_device_id, key, zero_size, C_NULL_PTR, temp_size)
    call fclHandleErrorCode(errcode,'fclGetDeviceInfoString::clGetDeviceInfo')

    allocate( character(len=temp_size) :: value)
    errcode = clGetDeviceInfo(device%cl_device_id, key, temp_size, C_LOC(value), size_ret)
    call fclHandleErrorCode(errcode,'fclGetDeviceInfoString::clGetDeviceInfo')

  end procedure fclGetDeviceInfoString
  ! ---------------------------------------------------------------------------

  
  module procedure fclGetDeviceInfoInt32 !(device,key,value)
    ! https://www.khronos.org/registry/OpenCL/sdk/1.0/docs/man/xhtml/clGetDeviceInfo.html
    
    integer(c_int32_t) :: errcode
    integer(c_size_t) :: temp_size, size_ret

    temp_size = c_sizeof(int(1,c_int32_t))
    errcode = clGetDeviceInfo(device%cl_device_id, key, temp_size, C_LOC(value), size_ret)
    call fclHandleErrorCode(errcode,'fclGetDeviceInfoInt32::clGetDeviceInfo')

  end procedure fclGetDeviceInfoInt32
  ! ---------------------------------------------------------------------------


  module procedure fclGetDeviceInfoInt64 !(device,key,value)
    ! https://www.khronos.org/registry/OpenCL/sdk/1.0/docs/man/xhtml/clGetDeviceInfo.html
    
    integer(c_int32_t) :: errcode
    integer(c_size_t) :: temp_size, size_ret

    temp_size = c_sizeof(int(1,c_int64_t))
    errcode = clGetDeviceInfo(device%cl_device_id, key, temp_size, C_LOC(value), size_ret)
    call fclHandleErrorCode(errcode,'fclGetDeviceInfoInt64::clGetDeviceInfo')

  end procedure fclGetDeviceInfoInt64
  ! ---------------------------------------------------------------------------


  module procedure fclGetPlatforms !result(platforms)

    integer :: i
    integer(c_int32_t) :: num_platforms, int32_ret
    integer(c_int32_t) :: errcode

    integer(c_intptr_t), allocatable, target :: platform_ids(:)

    ! Get number of platforms
    errcode = clGetPlatformIDs(0,C_NULL_PTR,num_platforms)
    call fclHandleErrorCode(errcode)

    ! Allocate platform_ids array
    allocate(platform_ids(num_platforms))

    ! Populate platform_ids array
    errcode = clGetPlatformIDs(num_platforms,c_loc(platform_ids),int32_ret)
    call fclHandleErrorCode(errcode)
    
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
    call fclHandleErrorCode(errcode,'fclGetPlatform::clGetDeviceIDs')

    allocate(platform%devices(platform%numDevice))
    allocate(platform%cl_device_ids(platform%numDevice))

    ! --- Get device ids ---
    errcode = clGetDeviceIDs(platform_id, CL_DEVICE_TYPE_ALL, platform%numDevice, &
                                  C_LOC(platform%cl_device_ids), int32_ret)
    call fclHandleErrorCode(errcode)

    ! --- Populate fclDevice structure array ---
    do i=1,platform%numDevice

      platform%devices(i) = fclGetDevice(platform%cl_device_ids(i))

    end do

    ! --- Populate fclPlatform info strings ---
    call fclGetPlatformInfo(platform,CL_PLATFORM_PROFILE,platform%profile)
    call fclGetPlatformInfo(platform,CL_PLATFORM_VERSION,platform%version)
    call fclGetPlatformInfo(platform,CL_PLATFORM_NAME,platform%name)
    call fclGetPlatformInfo(platform,CL_PLATFORM_VENDOR,platform%vendor)
    call fclGetPlatformInfo(platform,CL_PLATFORM_EXTENSIONS,platform%extensions)

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

  end procedure fclGetDevice
  ! ---------------------------------------------------------------------------
  
  

end submodule Focal_Query
