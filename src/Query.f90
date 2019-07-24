submodule (Focal) Focal_Query
  !! FOCAL: openCL abstraction layer for fortran
  !!  Implementation module for query routines

  !! @note This is an implementation submodule: it contains the code implementing the subroutines defined in the
  !!  corresponding header module file. See header module file (Focal.f90) for interface definitions. @endnote

  use clfortran
  implicit none

  contains

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
    integer(c_size_t) :: zero_size = 0
    integer(c_size_t) :: temp_size
    integer(c_size_t) :: size_ret

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

    ! --- Platform profile ---
    errcode = clGetPlatformInfo(platform_id, CL_PLATFORM_PROFILE, zero_size, C_NULL_PTR, temp_size)
    call fclHandleErrorCode(errcode)

    allocate(platform%profile(temp_size))

    errcode = clGetPlatformInfo(platform_id, CL_PLATFORM_PROFILE, temp_size, C_LOC(platform%profile), size_ret)
    call fclHandleErrorCode(errcode)

    ! --- Platform version ---
    errcode = clGetPlatformInfo(platform_id, CL_PLATFORM_VERSION, zero_size, C_NULL_PTR, temp_size)
    call fclHandleErrorCode(errcode)

    allocate(platform%version(temp_size))

    errcode = clGetPlatformInfo(platform_id, CL_PLATFORM_VERSION, temp_size, C_LOC(platform%version), size_ret)
    call fclHandleErrorCode(errcode)

    ! --- Platform name ---
    errcode = clGetPlatformInfo(platform_id, CL_PLATFORM_NAME, zero_size, C_NULL_PTR, temp_size)
    call fclHandleErrorCode(errcode)

    allocate(platform%name(temp_size))

    errcode = clGetPlatformInfo(platform_id, CL_PLATFORM_NAME, temp_size, C_LOC(platform%name), size_ret)
    call fclHandleErrorCode(errcode)

    ! --- Platform vendor ---
    errcode = clGetPlatformInfo(platform_id, CL_PLATFORM_VENDOR, zero_size, C_NULL_PTR, temp_size)
    call fclHandleErrorCode(errcode)

    allocate(platform%vendor(temp_size))

    errcode = clGetPlatformInfo(platform_id, CL_PLATFORM_VENDOR, temp_size, C_LOC(platform%vendor), size_ret)
    call fclHandleErrorCode(errcode)

    ! --- Platform extensions ---
    errcode = clGetPlatformInfo(platform_id, CL_PLATFORM_EXTENSIONS, zero_size, C_NULL_PTR, temp_size)
    call fclHandleErrorCode(errcode)

    allocate(platform%extensions(temp_size))

    errcode = clGetPlatformInfo(platform_id, CL_PLATFORM_EXTENSIONS, temp_size, C_LOC(platform%extensions), size_ret)
    call fclHandleErrorCode(errcode)

  end procedure fclGetPlatform
  ! ---------------------------------------------------------------------------


  module procedure fclGetDevice !(device_id) result(device)
    ! https://www.khronos.org/registry/OpenCL/sdk/1.0/docs/man/xhtml/clGetDeviceInfo.html

    integer(c_size_t) :: zero_size = 0
    integer(c_int32_t) :: errcode
    integer(c_size_t) :: temp_size, size_ret

    device%cl_device_id = device_id

    ! Name
    errcode = clGetDeviceInfo(device_id, CL_DEVICE_NAME, zero_size, C_NULL_PTR, temp_size)
    call fclHandleErrorCode(errcode)

    allocate(device%name(temp_size))
    errcode = clGetDeviceInfo(device_id, CL_DEVICE_NAME, temp_size, C_LOC(device%name), size_ret)
    call fclHandleErrorCode(errcode)

    ! Device type
    temp_size = c_sizeof(device%cl_device_type)
    errcode = clGetDeviceInfo(device_id, CL_DEVICE_TYPE, temp_size, C_LOC(device%cl_device_type), size_ret)
    call fclHandleErrorCode(errcode)

    ! Maximum compute units.
    temp_size = c_sizeof(device%nComputeUnits)
    errcode = clGetDeviceInfo(device_id, CL_DEVICE_MAX_COMPUTE_UNITS, temp_size, C_LOC(device%nComputeUnits), size_ret)
    call fclHandleErrorCode(errcode)

    !! @todo CL_DEVICE_GLOBAL_MEM_SIZE CL_DEVICE_MAX_CLOCK_FREQUENCY
    !!  CL_DEVICE_VERSION CL_DRIVER_VERSION @endtodo

  end procedure fclGetDevice
  ! ---------------------------------------------------------------------------

end submodule Focal_Query
