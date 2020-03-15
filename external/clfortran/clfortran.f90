! -----------------------------------------------------------------------------
! CLFORTRAN - OpenCL bindings module for Fortran.
!
! This is the main module file and contains all OpenCL API definitions to be
! invoked from Fortran programs.
!
! -----------------------------------------------------------------------------
!
! Copyright (C) 2013 Company for Advanced Supercomputing Solutions LTD
! Bosmat 2a St.
! Shoham
! Israel 60850
! http://www.cass-hpc.com
!
! Author: Mordechai Butrashvily <support@cass-hpc.com>
!
! -----------------------------------------------------------------------------
!
! Modified by: LKedward 2019
!  Add explicit definition of 'BOZ' constants as integers.
!  Fix interface definitions to be in interface block.
!  Add definitions of cl_event_command_execution_status constants
!  Add definitions of cl_map_flags bitfields
!  Fix type of context in clCreateUserEvent interface
!  Add definitions of cl_command_type
!
!
! -----------------------------------------------------------------------------
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU Lesser General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
! -----------------------------------------------------------------------------

module clfortran
    USE ISO_C_BINDING
    implicit none

    ! Error Codes
    integer(c_int32_t), parameter :: CL_SUCCESS                                   =  0
    integer(c_int32_t), parameter :: CL_DEVICE_NOT_FOUND                          = -1
    integer(c_int32_t), parameter :: CL_DEVICE_NOT_AVAILABLE                      = -2
    integer(c_int32_t), parameter :: CL_COMPILER_NOT_AVAILABLE                    = -3
    integer(c_int32_t), parameter :: CL_MEM_OBJECT_ALLOCATION_FAILURE             = -4
    integer(c_int32_t), parameter :: CL_OUT_OF_RESOURCES                          = -5
    integer(c_int32_t), parameter :: CL_OUT_OF_HOST_MEMORY                        = -6
    integer(c_int32_t), parameter :: CL_PROFILING_INFO_NOT_AVAILABLE              = -7
    integer(c_int32_t), parameter :: CL_MEM_COPY_OVERLAP                          = -8
    integer(c_int32_t), parameter :: CL_IMAGE_FORMAT_MISMATCH                     = -9
    integer(c_int32_t), parameter :: CL_IMAGE_FORMAT_NOT_SUPPORTED                = -10
    integer(c_int32_t), parameter :: CL_BUILD_PROGRAM_FAILURE                     = -11
    integer(c_int32_t), parameter :: CL_MAP_FAILURE                               = -12
    integer(c_int32_t), parameter :: CL_MISALIGNED_SUB_BUFFER_OFFSET              = -13
    integer(c_int32_t), parameter :: CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST = -14
    integer(c_int32_t), parameter :: CL_COMPILE_PROGRAM_FAILURE                   = -15
    integer(c_int32_t), parameter :: CL_LINKER_NOT_AVAILABLE                      = -16
    integer(c_int32_t), parameter :: CL_LINK_PROGRAM_FAILURE                      = -17
    integer(c_int32_t), parameter :: CL_DEVICE_PARTITION_FAILED                   = -18
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_INFO_NOT_AVAILABLE             = -19

    integer(c_int32_t), parameter :: CL_INVALID_VALUE                             = -30
    integer(c_int32_t), parameter :: CL_INVALID_DEVICE_TYPE                       = -31
    integer(c_int32_t), parameter :: CL_INVALID_PLATFORM                          = -32
    integer(c_int32_t), parameter :: CL_INVALID_DEVICE                            = -33
    integer(c_int32_t), parameter :: CL_INVALID_CONTEXT                           = -34
    integer(c_int32_t), parameter :: CL_INVALID_QUEUE_PROPERTIES                  = -35
    integer(c_int32_t), parameter :: CL_INVALID_COMMAND_QUEUE                     = -36
    integer(c_int32_t), parameter :: CL_INVALID_HOST_PTR                          = -37
    integer(c_int32_t), parameter :: CL_INVALID_MEM_OBJECT                        = -38
    integer(c_int32_t), parameter :: CL_INVALID_IMAGE_FORMAT_DESCRIPTOR           = -39
    integer(c_int32_t), parameter :: CL_INVALID_IMAGE_SIZE                        = -40
    integer(c_int32_t), parameter :: CL_INVALID_SAMPLER                           = -41
    integer(c_int32_t), parameter :: CL_INVALID_BINARY                            = -42
    integer(c_int32_t), parameter :: CL_INVALID_BUILD_OPTIONS                     = -43
    integer(c_int32_t), parameter :: CL_INVALID_PROGRAM                           = -44
    integer(c_int32_t), parameter :: CL_INVALID_PROGRAM_EXECUTABLE                = -45
    integer(c_int32_t), parameter :: CL_INVALID_KERNEL_NAME                       = -46
    integer(c_int32_t), parameter :: CL_INVALID_KERNEL_DEFINITION                 = -47
    integer(c_int32_t), parameter :: CL_INVALID_KERNEL                            = -48
    integer(c_int32_t), parameter :: CL_INVALID_ARG_INDEX                         = -49
    integer(c_int32_t), parameter :: CL_INVALID_ARG_VALUE                         = -50
    integer(c_int32_t), parameter :: CL_INVALID_ARG_SIZE                          = -51
    integer(c_int32_t), parameter :: CL_INVALID_KERNEL_ARGS                       = -52
    integer(c_int32_t), parameter :: CL_INVALID_WORK_DIMENSION                    = -53
    integer(c_int32_t), parameter :: CL_INVALID_WORK_GROUP_SIZE                   = -54
    integer(c_int32_t), parameter :: CL_INVALID_WORK_ITEM_SIZE                    = -55
    integer(c_int32_t), parameter :: CL_INVALID_GLOBAL_OFFSET                     = -56
    integer(c_int32_t), parameter :: CL_INVALID_EVENT_WAIT_LIST                   = -57
    integer(c_int32_t), parameter :: CL_INVALID_EVENT                             = -58
    integer(c_int32_t), parameter :: CL_INVALID_OPERATION                         = -59
    integer(c_int32_t), parameter :: CL_INVALID_GL_OBJECT                         = -60
    integer(c_int32_t), parameter :: CL_INVALID_BUFFER_SIZE                       = -61
    integer(c_int32_t), parameter :: CL_INVALID_MIP_LEVEL                         = -62
    integer(c_int32_t), parameter :: CL_INVALID_GLOBAL_WORK_SIZE                  = -63
    integer(c_int32_t), parameter :: CL_INVALID_PROPERTY                          = -64
    integer(c_int32_t), parameter :: CL_INVALID_IMAGE_DESCRIPTOR                  = -65
    integer(c_int32_t), parameter :: CL_INVALID_COMPILER_OPTIONS                  = -66
    integer(c_int32_t), parameter :: CL_INVALID_LINKER_OPTIONS                    = -67
    integer(c_int32_t), parameter :: CL_INVALID_DEVICE_PARTITION_COUNT            = -68

    ! OpenCL Version
    integer(c_int32_t), parameter :: CL_VERSION_1_0                               = 1
    integer(c_int32_t), parameter :: CL_VERSION_1_1                               = 1
    integer(c_int32_t), parameter :: CL_VERSION_1_2                               = 1

    ! cl_bool
    integer(c_int32_t), parameter :: CL_FALSE                                     = 0
    integer(c_int32_t), parameter :: CL_TRUE                                      = 1
    integer(c_int32_t), parameter :: CL_BLOCKING                                  = CL_TRUE
    integer(c_int32_t), parameter :: CL_NON_BLOCKING                              = CL_FALSE

    ! cl_platform_info
    integer(c_int32_t), parameter :: CL_PLATFORM_PROFILE                        = int(Z'0900',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_PLATFORM_VERSION                        = int(Z'0901',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_PLATFORM_NAME                           = int(Z'0902',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_PLATFORM_VENDOR                         = int(Z'0903',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_PLATFORM_EXTENSIONS                     = int(Z'0904',kind=c_int32_t)

    ! cl_device_type - bitfield
    integer(c_int64_t), parameter :: CL_DEVICE_TYPE_DEFAULT                     = int(b'00001'    ,kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_DEVICE_TYPE_CPU                         = int(b'00010'    ,kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_DEVICE_TYPE_GPU                         = int(b'00100'    ,kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_DEVICE_TYPE_ACCELERATOR                 = int(b'01000'    ,kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_DEVICE_TYPE_CUSTOM                      = int(b'10000'    ,kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_DEVICE_TYPE_ALL                         = int(Z'FFFFFFFF' ,kind=c_int64_t)

    ! cl_device_info
    integer(c_int32_t), parameter :: CL_DEVICE_TYPE                             = int(Z'1000',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_VENDOR_ID                        = int(Z'1001',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_MAX_COMPUTE_UNITS                = int(Z'1002',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS         = int(Z'1003',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_MAX_WORK_GROUP_SIZE              = int(Z'1004',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_MAX_WORK_ITEM_SIZES              = int(Z'1005',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR      = int(Z'1006',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT     = int(Z'1007',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT       = int(Z'1008',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG      = int(Z'1009',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT     = int(Z'100A',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE    = int(Z'100B',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_MAX_CLOCK_FREQUENCY              = int(Z'100C',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_ADDRESS_BITS                     = int(Z'100D',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_MAX_READ_IMAGE_ARGS              = int(Z'100E',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_MAX_WRITE_IMAGE_ARGS             = int(Z'100F',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_MAX_MEM_ALLOC_SIZE               = int(Z'1010',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_IMAGE2D_MAX_WIDTH                = int(Z'1011',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_IMAGE2D_MAX_HEIGHT               = int(Z'1012',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_IMAGE3D_MAX_WIDTH                = int(Z'1013',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_IMAGE3D_MAX_HEIGHT               = int(Z'1014',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_IMAGE3D_MAX_DEPTH                = int(Z'1015',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_IMAGE_SUPPORT                    = int(Z'1016',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_MAX_PARAMETER_SIZE               = int(Z'1017',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_MAX_SAMPLERS                     = int(Z'1018',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_MEM_BASE_ADDR_ALIGN              = int(Z'1019',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE         = int(Z'101A',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_SINGLE_FP_CONFIG                 = int(Z'101B',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_GLOBAL_MEM_CACHE_TYPE            = int(Z'101C',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE        = int(Z'101D',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_GLOBAL_MEM_CACHE_SIZE            = int(Z'101E',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_GLOBAL_MEM_SIZE                  = int(Z'101F',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE         = int(Z'1020',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_MAX_CONSTANT_ARGS                = int(Z'1021',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_LOCAL_MEM_TYPE                   = int(Z'1022',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_LOCAL_MEM_SIZE                   = int(Z'1023',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_ERROR_CORRECTION_SUPPORT         = int(Z'1024',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_PROFILING_TIMER_RESOLUTION       = int(Z'1025',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_ENDIAN_LITTLE                    = int(Z'1026',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_AVAILABLE                        = int(Z'1027',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_COMPILER_AVAILABLE               = int(Z'1028',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_EXECUTION_CAPABILITIES           = int(Z'1029',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_QUEUE_PROPERTIES                 = int(Z'102A',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_NAME                             = int(Z'102B',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_VENDOR                           = int(Z'102C',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DRIVER_VERSION                          = int(Z'102D',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_PROFILE                          = int(Z'102E',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_VERSION                          = int(Z'102F',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_EXTENSIONS                       = int(Z'1030',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_PLATFORM                         = int(Z'1031',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_DOUBLE_FP_CONFIG                 = int(Z'1032',kind=c_int32_t)
    ! 0x1033 reserved for CL_DEVICE_HALF_FP_CONFIG
    integer(c_int32_t), parameter :: CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF      = int(Z'1034',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_HOST_UNIFIED_MEMORY              = int(Z'1035',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR         = int(Z'1036',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT        = int(Z'1037',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_NATIVE_VECTOR_WIDTH_INT          = int(Z'1038',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG         = int(Z'1039',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT        = int(Z'103A',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE       = int(Z'103B',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF         = int(Z'103C',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_OPENCL_C_VERSION                 = int(Z'103D',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_LINKER_AVAILABLE                 = int(Z'103E',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_BUILT_IN_KERNELS                 = int(Z'103F',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_IMAGE_MAX_BUFFER_SIZE            = int(Z'1040',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_IMAGE_MAX_ARRAY_SIZE             = int(Z'1041',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_PARENT_DEVICE                    = int(Z'1042',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_PARTITION_MAX_SUB_DEVICES        = int(Z'1043',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_PARTITION_PROPERTIES             = int(Z'1044',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_PARTITION_AFFINITY_DOMAIN        = int(Z'1045',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_PARTITION_TYPE                   = int(Z'1046',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_REFERENCE_COUNT                  = int(Z'1047',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_PREFERRED_INTEROP_USER_SYNC      = int(Z'1048',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEVICE_PRINTF_BUFFER_SIZE               = int(Z'1049',kind=c_int32_t)

    ! cl_device_fp_config - bitfield
    integer(c_int64_t), parameter :: CL_FP_DENORM                               = int(b'00000001',kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_FP_INF_NAN                              = int(b'00000010',kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_FP_ROUND_TO_NEAREST                     = int(b'00000100',kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_FP_ROUND_TO_ZERO                        = int(b'00001000',kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_FP_ROUND_TO_INF                         = int(b'00010000',kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_FP_FMA                                  = int(b'00100000',kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_FP_SOFT_FLOAT                           = int(b'01000000',kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_FP_CORRECTLY_ROUNDED_DIVIDE_SQRT        = int(b'10000000',kind=c_int64_t)

    ! cl_device_mem_cache_type
    integer(c_int32_t), parameter :: CL_NONE                                    = int(Z'0',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_READ_ONLY_CACHE                         = int(Z'1',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_READ_WRITE_CACHE                        = int(Z'2',kind=c_int32_t)

    ! cl_device_local_mem_type
    integer(c_int32_t), parameter :: CL_LOCAL                                   = int(Z'1',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_GLOBAL                                  = int(Z'2',kind=c_int32_t)

    ! cl_device_exec_capabilities - bitfield
    integer(c_int64_t), parameter :: CL_EXEC_KERNEL                             = int(b'01' ,kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_EXEC_NATIVE_KERNEL                      = int(b'10' ,kind=c_int64_t)

    ! cl_command_queue_properties - bitfield
    integer(c_int64_t), parameter :: CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE     = int(b'01',kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_QUEUE_PROFILING_ENABLE                  = int(b'10',kind=c_int64_t)

    ! cl_context_info
    integer(c_int32_t), parameter :: CL_CONTEXT_REFERENCE_COUNT                 = int(Z'1080',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_CONTEXT_DEVICES                         = int(Z'1081',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_CONTEXT_PROPERTIES                      = int(Z'1082',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_CONTEXT_NUM_DEVICES                     = int(Z'1083',kind=c_int32_t)

    ! cl_context_properties type(c_ptr)
    integer(c_intptr_t), parameter :: CL_CONTEXT_PLATFORM                         = int(Z'1084' ,kind=c_intptr_t)
    integer(c_intptr_t), parameter :: CL_CONTEXT_INTEROP_USER_SYNC                = int(Z'1085' ,kind=c_intptr_t)

    ! cl_command_queue_info
    integer(c_int32_t), parameter :: CL_QUEUE_CONTEXT                           = int(Z'1090',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_QUEUE_DEVICE                            = int(Z'1091',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_QUEUE_REFERENCE_COUNT                   = int(Z'1092',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_QUEUE_PROPERTIES                        = int(Z'1093',kind=c_int32_t)

    ! cl_mem_flags - bitfield (int64)
    integer(c_int64_t), parameter :: CL_MEM_READ_WRITE                          = int(b'0000000001' ,kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_MEM_WRITE_ONLY                          = int(b'0000000010' ,kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_MEM_READ_ONLY                           = int(b'0000000100' ,kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_MEM_USE_HOST_PTR                        = int(b'0000001000' ,kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_MEM_ALLOC_HOST_PTR                      = int(b'0000010000' ,kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_MEM_COPY_HOST_PTR                       = int(b'0000100000' ,kind=c_int64_t)
    !integer(c_int64_t), parameter :: reserved                                  = int(b'0001000000' ,kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_MEM_HOST_WRITE_ONLY                     = int(b'0010000000' ,kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_MEM_HOST_READ_ONLY                      = int(b'0100000000' ,kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_MEM_HOST_NO_ACCESS                      = int(b'1000000000' ,kind=c_int64_t)

    ! cl_buffer_create_type
    integer(c_int32_t), parameter :: CL_BUFFER_CREATE_TYPE_REGION               = int(Z'1220',kind=c_int32_t)

    ! cl_channel_order
    integer(c_int32_t), parameter :: CL_R                                       = int(Z'10B0',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_A                                       = int(Z'10B1',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_RG                                      = int(Z'10B2',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_RA                                      = int(Z'10B3',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_RGB                                     = int(Z'10B4',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_RGBA                                    = int(Z'10B5',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_BGRA                                    = int(Z'10B6',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_ARGB                                    = int(Z'10B7',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_INTENSITY                               = int(Z'10B8',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_LUMINANCE                               = int(Z'10B9',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_Rx                                      = int(Z'10BA',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_RGx                                     = int(Z'10BB',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_RGBx                                    = int(Z'10BC',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEPTH                                   = int(Z'10BD',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_DEPTH_STENCIL                           = int(Z'10BE',kind=c_int32_t)

    ! cl_channel_type
    integer(c_int32_t), parameter :: CL_SNORM_INT8                              = int(Z'10D0',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_SNORM_INT16                             = int(Z'10D1',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_UNORM_INT8                              = int(Z'10D2',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_UNORM_INT16                             = int(Z'10D3',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_UNORM_SHORT_565                         = int(Z'10D4',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_UNORM_SHORT_555                         = int(Z'10D5',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_UNORM_INT_101010                        = int(Z'10D6',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_SIGNED_INT8                             = int(Z'10D7',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_SIGNED_INT16                            = int(Z'10D8',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_SIGNED_INT32                            = int(Z'10D9',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_UNSIGNED_INT8                           = int(Z'10DA',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_UNSIGNED_INT16                          = int(Z'10DB',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_UNSIGNED_INT32                          = int(Z'10DC',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_HALF_FLOAT                              = int(Z'10DD',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_FLOAT                                   = int(Z'10DE',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_UNORM_INT24                             = int(Z'10DF',kind=c_int32_t)

    ! cl_mem_object_type
    integer(c_int32_t), parameter :: CL_MEM_OBJECT_BUFFER                       = int(Z'10F0',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_MEM_OBJECT_IMAGE2D                      = int(Z'10F1',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_MEM_OBJECT_IMAGE3D                      = int(Z'10F2',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_MEM_OBJECT_IMAGE2D_ARRAY                = int(Z'10F3',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_MEM_OBJECT_IMAGE1D                      = int(Z'10F4',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_MEM_OBJECT_IMAGE1D_ARRAY                = int(Z'10F5',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_MEM_OBJECT_IMAGE1D_BUFFER               = int(Z'10F6',kind=c_int32_t)

    ! cl_mem_info
    integer(c_int32_t), parameter :: CL_MEM_TYPE                                = int(Z'1100',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_MEM_FLAGS                               = int(Z'1101',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_MEM_SIZE                                = int(Z'1102',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_MEM_HOST_PTR                            = int(Z'1103',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_MEM_MAP_COUNT                           = int(Z'1104',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_MEM_REFERENCE_COUNT                     = int(Z'1105',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_MEM_CONTEXT                             = int(Z'1106',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_MEM_ASSOCIATED_MEMOBJECT                = int(Z'1107',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_MEM_OFFSET                              = int(Z'1108',kind=c_int32_t)

    ! cl_image_info - Note that INFO was added to resolve naming conflicts.
    integer(c_int32_t), parameter :: CL_IMAGE_INFO_FORMAT                       = int(Z'1110',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_IMAGE_INFO_ELEMENT_SIZE                 = int(Z'1111',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_IMAGE_INFO_ROW_PITCH                    = int(Z'1112',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_IMAGE_INFO_SLICE_PITCH                  = int(Z'1113',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_IMAGE_INFO_WIDTH                        = int(Z'1114',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_IMAGE_INFO_HEIGHT                       = int(Z'1115',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_IMAGE_INFO_DEPTH                        = int(Z'1116',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_IMAGE_INFO_ARRAY_SIZE                   = int(Z'1117',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_IMAGE_INFO_BUFFER                       = int(Z'1118',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_IMAGE_INFO_NUM_MIP_LEVELS               = int(Z'1119',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_IMAGE_INFO_NUM_SAMPLES                  = int(Z'111A',kind=c_int32_t)

    ! cl_addressing_mode
    integer(c_int32_t), parameter :: CL_ADDRESS_NONE                            = int(Z'1130',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_ADDRESS_CLAMP_TO_EDGE                   = int(Z'1131',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_ADDRESS_CLAMP                           = int(Z'1132',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_ADDRESS_REPEAT                          = int(Z'1133',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_ADDRESS_MIRRORED_REPEAT                 = int(Z'1134',kind=c_int32_t)

    ! cl_filter_mode
    integer(c_int32_t), parameter :: CL_FILTER_NEAREST                          = int(Z'1140',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_FILTER_LINEAR                           = int(Z'1141',kind=c_int32_t)

    ! cl_sampler_info
    integer(c_int32_t), parameter :: CL_SAMPLER_REFERENCE_COUNT                 = int(Z'1150',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_SAMPLER_CONTEXT                         = int(Z'1151',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_SAMPLER_NORMALIZED_COORDS               = int(Z'1152',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_SAMPLER_ADDRESSING_MODE                 = int(Z'1153',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_SAMPLER_FILTER_MODE                     = int(Z'1154',kind=c_int32_t)

    ! cl_program_info
    integer(c_int32_t), parameter :: CL_PROGRAM_REFERENCE_COUNT                 = int(Z'1160',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_PROGRAM_CONTEXT                         = int(Z'1161',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_PROGRAM_NUM_DEVICES                     = int(Z'1162',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_PROGRAM_DEVICES                         = int(Z'1163',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_PROGRAM_SOURCE                          = int(Z'1164',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_PROGRAM_BINARY_SIZES                    = int(Z'1165',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_PROGRAM_BINARIES                        = int(Z'1166',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_PROGRAM_NUM_KERNELS                     = int(Z'1167',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_PROGRAM_KERNEL_NAMES                    = int(Z'1168',kind=c_int32_t)

    ! cl_map_flags - bitfield
    integer(c_int64_t), parameter :: CL_MAP_READ                                =int(b'1'  ,kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_MAP_WRITE                               =int(b'10' ,kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_MAP_WRITE_INVALIDATE_REGION             =int(b'100',kind=c_int64_t)

    ! cl_program_build_info
    integer(c_int32_t), parameter :: CL_PROGRAM_BUILD_STATUS                    = int(Z'1181',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_PROGRAM_BUILD_OPTIONS                   = int(Z'1182',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_PROGRAM_BUILD_LOG                       = int(Z'1183',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_PROGRAM_BINARY_TYPE                     = int(Z'1184',kind=c_int32_t)

    ! cl_program_binary_type
    integer(c_int32_t), parameter :: CL_PROGRAM_BINARY_TYPE_NONE                = int(Z'0',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_PROGRAM_BINARY_TYPE_COMPILED_OBJECT     = int(Z'1',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_PROGRAM_BINARY_TYPE_LIBRARY             = int(Z'2',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_PROGRAM_BINARY_TYPE_EXECUTABLE          = int(Z'4',kind=c_int32_t)

    ! cl_build_status
    integer(c_int32_t), parameter :: CL_BUILD_SUCCESS                           = 0
    integer(c_int32_t), parameter :: CL_BUILD_NONE                              = -1
    integer(c_int32_t), parameter :: CL_BUILD_ERROR                             = -2
    integer(c_int32_t), parameter :: CL_BUILD_IN_PROGRESS                       = -3

    ! cl_kernel_info
    integer(c_int32_t), parameter :: CL_KERNEL_FUNCTION_NAME                    = int(Z'1190',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_KERNEL_NUM_ARGS                         = int(Z'1191',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_KERNEL_REFERENCE_COUNT                  = int(Z'1192',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_KERNEL_CONTEXT                          = int(Z'1193',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_KERNEL_PROGRAM                          = int(Z'1194',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_KERNEL_ATTRIBUTES                       = int(Z'1195',kind=c_int32_t)

    ! cl_kernel_arg_info
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_ADDRESS_QUALIFIER            = int(Z'1196',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_ACCESS_QUALIFIER             = int(Z'1197',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_TYPE_NAME                    = int(Z'1198',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_TYPE_QUALIFIER               = int(Z'1199',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_NAME                         = int(Z'119A',kind=c_int32_t)

    ! cl_kernel_arg_address_qualifier
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_ADDRESS_GLOBAL               = int(Z'119B',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_ADDRESS_LOCAL                = int(Z'119C',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_ADDRESS_CONSTANT             = int(Z'119D',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_ADDRESS_PRIVATE              = int(Z'119E',kind=c_int32_t)

    ! cl_kernel_arg_access_qualifier
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_ACCESS_READ_ONLY             = int(Z'11A0',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_ACCESS_WRITE_ONLY            = int(Z'11A1',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_ACCESS_READ_WRITE            = int(Z'11A2',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_ACCESS_NONE                  = int(Z'11A3',kind=c_int32_t)

    ! cl_kernel_arg_type_qualifer - bitfield (int64)
    integer(c_int64_t), parameter :: CL_KERNEL_ARG_TYPE_NONE                    = int(b'000',kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_KERNEL_ARG_TYPE_CONST                   = int(b'001',kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_KERNEL_ARG_TYPE_RESTRICT                = int(b'010',kind=c_int64_t)
    integer(c_int64_t), parameter :: CL_KERNEL_ARG_TYPE_VOLATILE                = int(b'100',kind=c_int64_t)

    ! cl_kernel_work_group_info
    integer(c_int32_t), parameter :: CL_KERNEL_WORK_GROUP_SIZE                  = int(Z'11B0',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_KERNEL_COMPILE_WORK_GROUP_SIZE          = int(Z'11B1',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_KERNEL_LOCAL_MEM_SIZE                   = int(Z'11B2',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE=int(Z'11B3',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_KERNEL_PRIVATE_MEM_SIZE                 = int(Z'11B4',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_KERNEL_GLOBAL_WORK_SIZE                 = int(Z'11B5',kind=c_int32_t)

    ! cl_event_info
    integer(c_int32_t), parameter :: CL_EVENT_COMMAND_QUEUE                     = int(Z'11D0',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_EVENT_COMMAND_TYPE                      = int(Z'11D1',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_EVENT_REFERENCE_COUNT                   = int(Z'11D2',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_EVENT_COMMAND_EXECUTION_STATUS          = int(Z'11D3',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_EVENT_CONTEXT                           = int(Z'11D4',kind=c_int32_t)

    ! cl_command_type
    integer(c_int32_t), parameter :: CL_COMMAND_NDRANGE_KERNEL                  = int(Z'11F0',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_COMMAND_TASK                            = int(Z'11F1',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_COMMAND_NATIVE_KERNEL                   = int(Z'11F2',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_COMMAND_READ_BUFFER                     = int(Z'11F3',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_COMMAND_WRITE_BUFFER                    = int(Z'11F4',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_COMMAND_COPY_BUFFER                     = int(Z'11F5',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_COMMAND_READ_IMAGE                      = int(Z'11F6',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_COMMAND_WRITE_IMAGE                     = int(Z'11F7',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_COMMAND_COPY_IMAGE                      = int(Z'11F8',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_COMMAND_COPY_IMAGE_TO_BUFFER            = int(Z'11F9',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_COMMAND_COPY_BUFFER_TO_IMAGE            = int(Z'11FA',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_COMMAND_COPY_MAP_BUFFER                 = int(Z'11FB',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_COMMAND_COPY_MAP_IMAGE                  = int(Z'11FC',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_COMMAND_COPY_UNMAP_MEM_OBJECT           = int(Z'11FD',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_COMMAND_MARKER                          = int(Z'11FE',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_COMMAND_ACQUIRE_GL_OBJECTS              = int(Z'11FF',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_COMMAND_RELEASE_GL_OBJECTS              = int(Z'1200',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_COMMAND_READ_BUFFER_RECT                = int(Z'1201',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_COMMAND_WRITE_BUFFER_RECT               = int(Z'1202',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_COMMAND_COPY_BUFFER_RECT                = int(Z'1203',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_COMMAND_USER                            = int(Z'1204',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_COMMAND_BARRIER                         = int(Z'1205',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_COMMAND_MIGRATE_MEM_OBJECTS             = int(Z'1206',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_COMMAND_FILL_BUFFER                     = int(Z'1207',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_COMMAND_FILL_IMAGE                      = int(Z'1208',kind=c_int32_t)

    ! cl_profiling_info
    integer(c_int32_t), parameter :: CL_PROFILING_COMMAND_QUEUED                = int(Z'1280',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_PROFILING_COMMAND_SUBMIT                = int(Z'1281',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_PROFILING_COMMAND_START                 = int(Z'1282',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_PROFILING_COMMAND_END                   = int(Z'1283',kind=c_int32_t)

    ! cl_event_command_execution_status
    integer(c_int32_t), parameter :: CL_COMPLETE                                = int(Z'0',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_RUNNING                                 = int(Z'1',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_SUBMITTED                               = int(Z'2',kind=c_int32_t)
    integer(c_int32_t), parameter :: CL_QUEUED                                  = int(Z'3',kind=c_int32_t)

    type, BIND(C) :: cl_image_format
        integer(c_int32_t) :: image_channel_order
        integer(c_int32_t) :: image_channel_data_type
    end type cl_image_format

    type, BIND(C) :: cl_image_desc
        integer(c_int32_t)  :: image_type
        integer(c_size_t)   :: image_width
        integer(c_size_t)   :: image_height
        integer(c_size_t)   :: image_depth
        integer(c_size_t)   :: image_array_size
        integer(c_size_t)   :: image_row_pitch
        integer(c_size_t)   :: image_slice_pitch
        integer(c_int32_t)  :: num_mip_levels
        integer(c_int32_t)  :: num_samples
        integer(c_intptr_t) :: buffer
    end type cl_image_desc

    !
    ! Start interfaces.
    !
!    contains

    interface
!    ! ------------
!    ! Platform API
!    ! ------------
!
!    ! clGetPlatformIDs
        integer(c_int32_t) function clGetPlatformIDs(num_entries, &
                platforms, num_platforms) &
            BIND(C, NAME='clGetPlatformIDs')
            USE ISO_C_BINDING

            integer(c_int32_t), value, intent(in)   :: num_entries
            type(c_ptr), value, intent(in)          :: platforms
            integer(c_int32_t), intent(out)         :: num_platforms
        end function

!    ! clGetPlatformInfo
        integer(c_int32_t) function clGetPlatformInfo(platform, param_name, &
                param_value_size, param_value, param_value_size_ret) &
            BIND(C, NAME='clGetPlatformInfo')
            USE ISO_C_BINDING

            integer(c_intptr_t), value, intent(in)      :: platform
            integer(c_int32_t), value, intent(in)       :: param_name
            integer(c_size_t), value, intent(in)        :: param_value_size
            type(c_ptr), value, intent(in)              :: param_value
            integer(c_size_t), intent(out)              :: param_value_size_ret
        end function
!
!    ! ----------
!    ! Device API
!    ! ----------
!
!    ! clGetDeviceIDs
        integer(c_int32_t) function clGetDeviceIDs(platform, &
                device_type, &
                num_entries, &
                devices, &
                num_devices) &
            BIND(C, NAME='clGetDeviceIDs')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: platform
            integer(c_int64_t), value  :: device_type
            integer(c_int32_t), value  :: num_entries
            type(c_ptr), value         :: devices
            integer(c_int32_t), intent(out) :: num_devices

        end function
!
!    ! clGetDeviceInfo
        integer(c_int) function clGetDeviceInfo(device, &
                param_name, &
                param_value_size, &
                param_value, &
                param_value_size_ret) &
            BIND(C, NAME='clGetDeviceInfo')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: device
            integer(c_int32_t), value  :: param_name
            integer(c_size_t), value   :: param_value_size
            type(c_ptr), value         :: param_value
            integer(c_size_t), intent(out) :: param_value_size_ret

        end function
!
!    ! clCreateSubDevices
        integer(c_int32_t) function clCreateSubDevices(in_device, &
                properties, &
                num_devices, &
                out_devices, &
                num_devices_ret) &
            BIND(C, NAME='clCreateSubDevices')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: in_device
            type(c_ptr), value         :: properties
            integer(c_int32_t), value  :: num_devices
            type(c_ptr), value         :: out_devices
            integer(c_int32_t), intent(out) :: num_devices_ret

        end function

    ! clRetainDevice
        integer(c_int32_t) function clRetainDevice(device) &
            BIND(C, NAME='clRetainDevice')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: device
        end function

    ! clReleaseDevice
        integer(c_int32_t) function clReleaseDevice(device) &
            BIND(C, NAME='clReleaseDevice')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: device
        end function

    ! ------------
    ! Context APIs
    ! ------------

    ! clCreateContext
        integer(c_intptr_t) function clCreateContext(properties, &
                num_devices, &
                devices, &
                pfn_notify, &
                user_data, &
                errcode_ret) &
            BIND(C, NAME='clCreateContext')
            USE ISO_C_BINDING

            ! Define parameters.
            type(c_ptr), value        :: properties
            integer(c_int32_t), value :: num_devices
            type(c_ptr), value        :: devices
            type(c_funptr), value     :: pfn_notify
            type(c_ptr), value        :: user_data
            integer(c_int32_t), intent(out) :: errcode_ret

        end function

    ! clCreateContextFromType
        integer(c_intptr_t) function clCreateContextFromType(properties, &
                device_type, &
                pfn_notify, &
                user_data, &
                errcode_ret) &
            BIND(C, NAME='clCreateContextFromType')
            USE ISO_C_BINDING

            ! Define parameters.
            type(c_ptr), value        :: properties
            integer(c_int64_t), value :: device_type
            type(c_funptr), value     :: pfn_notify
            type(c_ptr), value        :: user_data
            integer(c_int32_t), intent(out) :: errcode_ret

        end function

    ! clRetainContext
        integer(c_int32_t) function clRetainContext(context) &
            BIND(C, NAME='clRetainContext')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: context

        end function

    ! clReleaseContext
        integer(c_int32_t) function clReleaseContext(context) &
            BIND(C, NAME='clReleaseContext')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: context

        end function
!
!    ! clGetContextInfo
        integer(c_int32_t) function clGetContextInfo(context, &
                 param_name, &
                 param_value_size, &
                 param_value, &
                 param_value_size_ret) &
            BIND(C, NAME='clGetContextInfo')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: context
            integer(c_int32_t), value :: param_name
            integer(c_size_t), value :: param_value_size
            type(c_ptr), value :: param_value
            integer(c_size_t), intent(out) :: param_value_size_ret

        end function

    ! ------------------
    ! Command Queue APIs
    ! ------------------

    ! clCreateCommandQueue
        integer(c_intptr_t) function clCreateCommandQueue(context, &
                device, &
                properties, &
                errcode_ret) &
            BIND(C, NAME='clCreateCommandQueue')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: context
            integer(c_intptr_t), value :: device
            integer(c_int64_t), value  :: properties
            integer(c_int32_t), intent(out) :: errcode_ret

        end function

    ! clRetainCommandQueue
        integer(c_int32_t) function clRetainCommandQueue(command_queue) &
            BIND(C, NAME='clRetainCommandQueue')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue

        end function

    ! clReleaseCommandQueue
        integer(c_int32_t) function clReleaseCommandQueue(command_queue) &
            BIND(C, NAME='clReleaseCommandQueue')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue

        end function

    ! clGetCommandQueueInfo
        integer(c_int32_t) function clGetCommandQueueInfo(command_queue, &
                 param_name, &
                 param_value_size, &
                 param_value, &
                 param_value_size_ret) &
            BIND(C, NAME='clGetCommandQueueInfo')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_int32_t), value  :: param_name
            integer(c_size_t), value   :: param_value_size
            type(c_ptr), value         :: param_value
            integer(c_size_t), intent(out) :: param_value_size_ret

        end function
!
!    ! ------------------
!    ! Memory Object APIs
!    ! ------------------
!
    ! clCreateBuffer
        integer(c_intptr_t) function clCreateBuffer(context, &
                flags, &
                sizeb, &
                host_ptr, &
                errcode_ret) &
            BIND(C, NAME='clCreateBuffer')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value  :: context
            integer(c_int64_t), value   :: flags
            integer(c_size_t), value    :: sizeb
            type(c_ptr), value          :: host_ptr
            integer(c_int32_t), intent(out) :: errcode_ret

        end function
!
!    ! clCreateSubBuffer
        integer(c_intptr_t) function clCreateSubBuffer(buffer, &
                flags, &
                buffer_create_type, &
                buffer_create_info, &
                errcode_ret) &
            BIND(C, NAME='clCreateSubBuffer')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value  :: buffer
            integer(c_int64_t), value   :: flags
            integer(c_int32_t), value   :: buffer_create_type
            type(c_ptr), value          :: buffer_create_info
            integer(c_int32_t), intent(out) :: errcode_ret

        end function
!
!    ! clCreateImage
        integer(c_intptr_t) function clCreateImage(context, &
                flags, &
                image_format, &
                image_desc, &
                host_ptr, &
                errcode_ret) &
            BIND(C, NAME='clCreateImage')
            USE ISO_C_BINDING
            import

            ! Define parameters.
            integer(c_intptr_t), value  :: context
            integer(c_int64_t), value   :: flags
            type(cl_image_format)       :: image_format
            type(cl_image_desc)         :: image_desc
            type(c_ptr), value          :: host_ptr
            integer(c_int32_t), intent(out) :: errcode_ret

        end function

    ! clRetainMemObject
        integer(c_int32_t) function clRetainMemObject(mem_obj) &
            BIND(C, NAME='clRetainMemObject')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value  :: mem_obj

        end function

    ! clReleaseMemObject
        integer(c_int32_t) function clReleaseMemObject(mem_obj) &
            BIND(C, NAME='clReleaseMemObject')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value  :: mem_obj

        end function

    ! clGetSupportedImageFormats
        integer(c_int32_t) function clGetSupportedImageFormats(context, &
                flags, &
                image_type, &
                num_entries, &
                image_formats, &
                num_image_formats) &
            BIND(C, NAME='clGetSupportedImageFormats')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value  :: context
            integer(c_int64_t), value   :: flags
            integer(c_int32_t), value   :: image_type
            integer(c_int32_t), value   :: num_entries
            type(c_ptr), value          :: image_formats
            integer(c_int32_t), intent(out) :: num_image_formats

        end function

    ! clGetMemObjectInfo
        integer(c_int32_t) function clGetMemObjectInfo(memobj, &
                 param_name, &
                 param_value_size, &
                 param_value, &
                 param_value_size_ret) &
            BIND(C, NAME='clGetMemObjectInfo')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: memobj
            integer(c_int32_t), value  :: param_name
            integer(c_size_t), value   :: param_value_size
            type(c_ptr), value         :: param_value
            integer(c_size_t), intent(out) :: param_value_size_ret

        end function

    ! clGetImageInfo
        integer(c_int32_t) function clGetImageInfo(image, &
                 param_name, &
                 param_value_size, &
                 param_value, &
                 param_value_size_ret) &
            BIND(C, NAME='clGetImageInfo')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: image
            integer(c_int32_t), value  :: param_name
            integer(c_size_t), value   :: param_value_size
            type(c_ptr), value         :: param_value
            integer(c_size_t), intent(out) :: param_value_size_ret

        end function

    ! clSetMemObjectDestructorCallback
        integer(c_int32_t) function clSetMemObjectDestructorCallback(memobj, &
                 pfn_notify, &
                 user_data) &
            BIND(C, NAME='clSetMemObjectDestructorCallback')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value  :: memobj
            type(c_funptr), value       :: pfn_notify
            type(c_ptr), value          :: user_data

        end function

    ! ------------
    ! Sampler APIs
    ! ------------

    ! clCreateSampler
        integer(c_intptr_t) function clCreateSampler(context, &
                normalized_coords, &
                addressing_mode, &
                filter_mode, &
                errcode_ret) &
            BIND(C, NAME='clCreateSampler')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: context
            integer(c_int32_t), value :: normalized_coords
            integer(c_int32_t), value :: addressing_mode
            integer(c_int32_t), value :: filter_mode
            integer(c_int32_t), intent(out) :: errcode_ret

        end function

    ! clRetainSampler
        integer(c_int32_t) function clRetainSampler(sampler) &
            BIND(C, NAME='clRetainSampler')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: sampler

        end function

    ! clReleaseSampler
        integer(c_int32_t) function clReleaseSampler(sampler) &
            BIND(C, NAME='clReleaseSampler')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: sampler

        end function

    ! clGetSamplerInfo
        integer(c_int32_t) function clGetSamplerInfo(sampler, &
                 param_name, &
                 param_value_size, &
                 param_value, &
                 param_value_size_ret) &
            BIND(C, NAME='clGetSamplerInfo')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: sampler
            integer(c_int32_t), value  :: param_name
            integer(c_size_t), value   :: param_value_size
            type(c_ptr), value         :: param_value
            integer(c_size_t), intent(out) :: param_value_size_ret

        end function

    ! -------------------
    ! Program Object APIs
    ! -------------------

    ! clCreateProgramWithSource
        integer(c_intptr_t) function clCreateProgramWithSource(context, &
                count, &
                strings, &
                lengths, &
                errcode_ret) &
            BIND(C, NAME='clCreateProgramWithSource')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: context
            integer(c_int32_t), value :: count
            type(c_ptr), value :: strings
            type(c_ptr), value :: lengths
            integer(c_int32_t), intent(out) :: errcode_ret

        end function

    ! clCreateProgramWithBinary
        integer(c_intptr_t) function clCreateProgramWithBinary(context, &
                num_devices, &
                device_list, &
                lengths, &
                binaries, &
                binary_status, &
                errcode_ret) &
            BIND(C, NAME='clCreateProgramWithBinary')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: context
            integer(c_int32_t), value :: num_devices
            type(c_ptr), value :: device_list
            type(c_ptr), value :: lengths
            type(c_ptr), value :: binaries
            type(c_ptr), value :: binary_status
            integer(c_int32_t), intent(out) :: errcode_ret

        end function

    ! clCreateProgramWithBuiltInKernels
        integer(c_intptr_t) function clCreateProgramWithBuiltInKernels(context, &
                num_devices, &
                device_list, &
                kernel_names, &
                errcode_ret) &
            BIND(C, NAME='clCreateProgramWithBuiltInKernels')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: context
            integer(c_int32_t), value :: num_devices
            type(c_ptr), value :: device_list
            type(c_ptr), value :: kernel_names
            integer(c_int32_t), intent(out) :: errcode_ret

        end function

    ! clRetainProgram
        integer(c_int32_t) function clRetainProgram(program) &
            BIND(C, NAME='clRetainProgram')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: program

        end function

    ! clReleaseProgram
        integer(c_int32_t) function clReleaseProgram(program) &
            BIND(C, NAME='clReleaseProgram')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: program

        end function

    ! clBuildProgram
        integer(c_int32_t) function clBuildProgram(program, &
                num_devices, &
                device_list, &
                options, &
                pfn_notify, &
                user_data) &
            BIND(C, NAME='clBuildProgram')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: program
            integer(c_int32_t), value :: num_devices
            type(c_ptr), value :: device_list
            type(c_ptr), value :: options
            type(c_funptr), value :: pfn_notify
            type(c_ptr), value :: user_data

        end function

    ! clCompileProgram
        integer(c_int32_t) function clCompileProgram(program, &
                num_devices, &
                device_list, &
                options, &
                num_input_headers, &
                input_headers, &
                header_include_names, &
                pfn_notify, &
                user_data) &
            BIND(C, NAME='clCompileProgram')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: program
            integer(c_int32_t), value :: num_devices
            type(c_ptr), value :: device_list
            type(c_ptr), value :: options
            integer(c_int32_t), value :: num_input_headers
            type(c_ptr), value :: input_headers
            type(c_ptr), value :: header_include_names
            type(c_funptr), value :: pfn_notify
            type(c_ptr), value :: user_data

        end function

    ! clLinkProgram
        integer(c_intptr_t) function clLinkProgram(context, &
                num_devices, &
                device_list, &
                options, &
                num_input_programs, &
                input_programs, &
                pfn_notify, &
                user_data, &
                errcode_ret) &
            BIND(C, NAME='clLinkProgram')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: context
            integer(c_int32_t), value :: num_devices
            type(c_ptr), value :: device_list
            type(c_ptr), value :: options
            integer(c_int32_t), value :: num_input_programs
            type(c_ptr), value :: input_programs
            type(c_funptr), value :: pfn_notify
            type(c_ptr), value :: user_data
            integer(c_int32_t), intent(out) :: errcode_ret

        end function

    ! clUnloadPlatformCompiler
        integer(c_int32_t) function clUnloadPlatformCompiler(platform) &
            BIND(C, NAME='clUnloadPlatformCompiler')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: platform

        end function

    ! clGetProgramInfo
        integer(c_int32_t) function clGetProgramInfo(program, &
                 param_name, &
                 param_value_size, &
                 param_value, &
                 param_value_size_ret) &
            BIND(C, NAME='clGetProgramInfo')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: program
            integer(c_int32_t), value  :: param_name
            integer(c_size_t), value   :: param_value_size
            type(c_ptr), value         :: param_value
            integer(c_size_t), intent(out) :: param_value_size_ret

        end function

    ! clGetProgramBuildInfo
        integer(c_int32_t) function clGetProgramBuildInfo(program, &
                 device, &
                 param_name, &
                 param_value_size, &
                 param_value, &
                 param_value_size_ret) &
            BIND(C, NAME='clGetProgramBuildInfo')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: program
            integer(c_intptr_t), value :: device
            integer(c_int32_t), value  :: param_name
            integer(c_size_t), value   :: param_value_size
            type(c_ptr), value         :: param_value
            integer(c_size_t), intent(out) :: param_value_size_ret

        end function

    ! ------------------
    ! Kernel Object APIs
    ! ------------------

    ! clCreateKernel
        integer(c_intptr_t) function clCreateKernel(program, &
                kernel_name, &
                errcode_ret) &
            BIND(C, NAME='clCreateKernel')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: program
            type(c_ptr), value :: kernel_name
            integer(c_int32_t), intent(out) :: errcode_ret

        end function

    ! clCreateKernelsInProgram
        integer(c_int32_t) function clCreateKernelsInProgram(program, &
                num_kernels, &
                kernels, &
                num_kernels_ret) &
            BIND(C, NAME='clCreateKernelsInProgram')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: program
            integer(c_int32_t), value :: num_kernels
            type(c_ptr), value :: kernels
            integer(c_int32_t), intent(out) :: num_kernels_ret

        end function

    ! clRetainKernel
        integer(c_int32_t) function clRetainKernel(kernel) &
            BIND(C, NAME='clRetainKernel')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: kernel

        end function

    ! clReleaseKernel
        integer(c_int32_t) function clReleaseKernel(kernel) &
            BIND(C, NAME='clReleaseKernel')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: kernel

        end function

    ! clSetKernelArg
        integer(c_int32_t) function clSetKernelArg(kernel, &
                arg_index, &
                arg_size, &
                arg_value) &
            BIND(C, NAME='clSetKernelArg')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: kernel
            integer(c_int32_t), value :: arg_index
            integer(c_size_t), value :: arg_size
            type(c_ptr), value :: arg_value

        end function

    ! clGetKernelInfo
        integer(c_int32_t) function clGetKernelInfo(kernel, &
                 param_name, &
                 param_value_size, &
                 param_value, &
                 param_value_size_ret) &
            BIND(C, NAME='clGetKernelInfo')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: kernel
            integer(c_int32_t), value  :: param_name
            integer(c_size_t), value   :: param_value_size
            type(c_ptr), value         :: param_value
            integer(c_size_t), intent(out) :: param_value_size_ret

        end function

    ! clGetKernelArgInfo
        integer(c_int32_t) function clGetKernelArgInfo(kernel, &
                arg_index, &
                param_name, &
                param_value_size, &
                param_value, &
                param_value_size_ret) &
            BIND(C, NAME='clGetKernelArgInfo')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: kernel
            integer(c_int32_t), value  :: arg_index
            integer(c_int32_t), value  :: param_name
            integer(c_size_t), value   :: param_value_size
            type(c_ptr), value         :: param_value
            integer(c_size_t), intent(out) :: param_value_size_ret

        end function

    ! clGetKernelWorkGroupInfo
        integer(c_int32_t) function clGetKernelWorkGroupInfo(kernel, &
                device, &
                param_name, &
                param_value_size, &
                param_value, &
                param_value_size_ret) &
            BIND(C, NAME='clGetKernelWorkGroupInfo')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: kernel
            integer(c_intptr_t), value  :: device
            integer(c_int32_t), value  :: param_name
            integer(c_size_t), value   :: param_value_size
            type(c_ptr), value         :: param_value
            integer(c_size_t), intent(out) :: param_value_size_ret

        end function

    ! -----------------
    ! Event Object APIs
    ! -----------------

    ! clWaitForEvents
        integer(c_int32_t) function clWaitForEvents(num_events, &
                event_list) &
            BIND(C, NAME='clWaitForEvents')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_int32_t), value :: num_events
            type(c_ptr), value :: event_list

        end function

    ! clGetEventInfo
        integer(c_int32_t) function clGetEventInfo(event, &
                 param_name, &
                 param_value_size, &
                 param_value, &
                 param_value_size_ret) &
            BIND(C, NAME='clGetEventInfo')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: event
            integer(c_int32_t), value  :: param_name
            integer(c_size_t), value   :: param_value_size
            type(c_ptr), value         :: param_value
            integer(c_size_t), intent(out) :: param_value_size_ret

        end function

    ! clCreateUserEvent
        integer(c_intptr_t) function clCreateUserEvent(context, &
                errcode_ret) &
            BIND(C, NAME='clCreateUserEvent')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: context
            integer(c_int32_t), intent(out) :: errcode_ret

        end function

    ! clRetainEvent
        integer(c_int32_t) function clRetainEvent(event) &
            BIND(C, NAME='clRetainEvent')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: event

        end function

    ! clReleaseEvent
        integer(c_int32_t) function clReleaseEvent(event) &
            BIND(C, NAME='clReleaseEvent')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: event

        end function

    ! clSetUserEventStatus
        integer(c_int32_t) function clSetUserEventStatus(event, &
                execution_status) &
            BIND(C, NAME='clSetUserEventStatus')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: event
            integer(c_int32_t), value :: execution_status

        end function

    ! clSetEventCallback
        integer(c_int32_t) function clSetEventCallback(event, &
                command_exec_callback_type, &
                pfn_notify, &
                user_data) &
            BIND(C, NAME='clSetEventCallback')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: event
            integer(c_int32_t), value :: command_exec_callback_type
            type(c_funptr), value :: pfn_notify
            type(c_ptr), value :: user_data

        end function

    ! --------------
    ! Profiling APIs
    ! --------------

    ! clGetEventProfilingInfo
        integer(c_int32_t) function clGetEventProfilingInfo(event, &
                 param_name, &
                 param_value_size, &
                 param_value, &
                 param_value_size_ret) &
            BIND(C, NAME='clGetEventProfilingInfo')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: event
            integer(c_int32_t), value  :: param_name
            integer(c_size_t), value   :: param_value_size
            type(c_ptr), value         :: param_value
            integer(c_size_t), intent(out) :: param_value_size_ret

        end function

    ! ---------------------
    ! Flush and Finish APIs
    ! ---------------------

    ! clFlush
        integer(c_int32_t) function clFlush(command_queue) &
            BIND(C, NAME='clFlush')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue

        end function

    ! clFinish
        integer(c_int32_t) function clFinish(command_queue) &
            BIND(C, NAME='clFinish')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue

        end function

    ! ----------------------
    ! Enqueued Commands APIs
    ! ----------------------

    ! clEnqueueReadBuffer
        integer(c_int32_t) function clEnqueueReadBuffer(command_queue, &
                buffer, &
                blocking_read, &
                offset, &
                size, &
                ptr, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueReadBuffer')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: buffer
            integer(c_int32_t), value :: blocking_read
            integer(c_size_t), value :: offset
            integer(c_size_t), value :: size
            type(c_ptr), value :: ptr
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event

        end function

    ! clEnqueueReadBufferRect
        integer(c_int32_t) function clEnqueueReadBufferRect(command_queue, &
                buffer, &
                blocking_read, &
                buffer_offset, &
                host_offset, &
                region, &
                buffer_row_pitch, &
                buffer_slice_pitch, &
                host_row_pitch, &
                host_slice_pitch, &
                ptr, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueReadBufferRect')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue

            integer(c_intptr_t), value :: buffer
            integer(c_int32_t), value :: blocking_read
            type(c_ptr), value :: buffer_offset
            type(c_ptr), value :: host_offset
            type(c_ptr), value :: region
            integer(c_size_t), value :: buffer_row_pitch
            integer(c_size_t), value :: buffer_slice_pitch
            integer(c_size_t), value :: host_row_pitch
            integer(c_size_t), value :: host_slice_pitch
            type(c_ptr), value :: ptr
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event

        end function

    ! clEnqueueWriteBuffer
        integer(c_int32_t) function clEnqueueWriteBuffer(command_queue, &
                buffer, &
                blocking_write, &
                offset, &
                size, &
                ptr, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueWriteBuffer')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: buffer
            integer(c_int32_t), value :: blocking_write
            integer(c_size_t), value :: offset
            integer(c_size_t), value :: size
            type(c_ptr), value :: ptr
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event

        end function

    ! clEnqueueWriteBufferRect
        integer(c_int32_t) function clEnqueueWriteBufferRect(command_queue, &
                buffer, &
                blocking_write, &
                buffer_offset, &
                host_offset, &
                region, &
                buffer_row_pitch, &
                buffer_slice_pitch, &
                host_row_pitch, &
                host_slice_pitch, &
                ptr, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueWriteBufferRect')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: buffer
            integer(c_int32_t), value :: blocking_write
            type(c_ptr), value :: buffer_offset
            type(c_ptr), value :: host_offset
            type(c_ptr), value :: region
            integer(c_size_t), value :: buffer_row_pitch
            integer(c_size_t), value :: buffer_slice_pitch
            integer(c_size_t), value :: host_row_pitch
            integer(c_size_t), value :: host_slice_pitch
            type(c_ptr), value :: ptr
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event

        end function

    ! clEnqueueFillBuffer
        integer(c_int32_t) function clEnqueueFillBuffer(command_queue, &
                buffer, &
                pattern, &
                pattern_size, &
                offset, &
                size, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueFillBuffer')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: buffer
            type(c_ptr), value :: pattern
            integer(c_size_t), value :: pattern_size
            integer(c_size_t), value :: offset
            integer(c_size_t), value :: size
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event

        end function

    ! clEnqueueCopyBuffer
        integer(c_int32_t) function clEnqueueCopyBuffer(command_queue, &
                src_buffer, &
                dst_buffer, &
                src_offset, &
                dst_offset, &
                size, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueCopyBuffer')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: src_buffer
            integer(c_intptr_t), value :: dst_buffer
            integer(c_size_t), value :: src_offset
            integer(c_size_t), value :: dst_offset
            integer(c_size_t), value :: size
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event

        end function

    ! clEnqueueCopyBufferRect
        integer(c_int32_t) function clEnqueueCopyBufferRect(command_queue, &
                src_buffer, &
                dst_buffer, &
                src_origin, &
                dst_origin, &
                region, &
                src_row_pitch, &
                src_slice_pitch, &
                dst_row_pitch, &
                dst_slice_pitch, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueCopyBufferRect')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: src_buffer
            integer(c_intptr_t), value :: dst_buffer
            type(c_ptr), value :: src_origin
            type(c_ptr), value :: dst_origin
            type(c_ptr), value :: region
            integer(c_size_t), value :: src_row_pitch
            integer(c_size_t), value :: src_slice_pitch
            integer(c_size_t), value :: dst_row_pitch
            integer(c_size_t), value :: dst_slice_pitch
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event

        end function

    ! clEnqueueReadImage
        integer(c_int32_t) function clEnqueueReadImage(command_queue, &
                image, &
                blocking_read, &
                origin, &
                region, &
                row_pitch, &
                slice_pitch, &
                ptr, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueReadImage')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: image
            integer(c_int32_t), value :: blocking_read
            type(c_ptr), value :: origin
            type(c_ptr), value :: region
            integer(c_size_t), value :: row_pitch
            integer(c_size_t), value :: slice_pitch
            type(c_ptr), value :: ptr
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event

        end function

    ! clEnqueueWriteImage
        integer(c_int32_t) function clEnqueueWriteImage(command_queue, &
                image, &
                blocking_write, &
                origin, &
                region, &
                input_row_pitch, &
                input_slice_pitch, &
                ptr, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueWriteImage')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: image
            integer(c_int32_t), value :: blocking_write
            type(c_ptr), value :: origin
            type(c_ptr), value :: region
            integer(c_size_t), value :: input_row_pitch
            integer(c_size_t), value :: input_slice_pitch
            type(c_ptr), value :: ptr
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event

        end function

    ! clEnqueueFillImage
        integer(c_int32_t) function clEnqueueFillImage(command_queue, &
                image, &
                fill_color, &
                origin, &
                region, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueFillImage')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: image
            type(c_ptr), value :: fill_color
            type(c_ptr), value :: origin
            type(c_ptr), value :: region
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event

        end function

    ! clEnqueueCopyImage
        integer(c_int32_t) function clEnqueueCopyImage(command_queue, &
                src_image, &
                dst_image, &
                src_origin, &
                dst_origin, &
                region, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueCopyImage')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: src_image
            integer(c_intptr_t), value :: dst_image
            type(c_ptr), value :: src_origin
            type(c_ptr), value :: dst_origin
            type(c_ptr), value :: region
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event

        end function

    ! clEnqueueCopyImageToBuffer
        integer(c_int32_t) function clEnqueueCopyImageToBuffer(command_queue, &
                src_image, &
                dst_buffer, &
                src_origin, &
                region, &
                dst_offset, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueCopyImageToBuffer')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: src_image
            integer(c_intptr_t), value :: dst_buffer
            type(c_ptr), value :: src_origin
            type(c_ptr), value :: region
            integer(c_size_t), value :: dst_offset
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event

        end function

    ! clEnqueueCopyBufferToImage
        integer(c_int32_t) function clEnqueueCopyBufferToImage(command_queue, &
                src_buffer, &
                dst_image, &
                src_offset, &
                dst_origin, &
                region, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueCopyBufferToImage')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: src_buffer
            integer(c_intptr_t), value :: dst_image
            integer(c_size_t), value :: src_offset
            type(c_ptr), value :: dst_origin
            type(c_ptr), value :: region
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event

        end function

    ! clEnqueueMapBuffer
        type(c_ptr) function clEnqueueMapBuffer(command_queue, &
                buffer, &
                blocking_map, &
                map_flags, &
                offset, &
                size, &
                num_events_in_wait_list, &
                event_wait_list, &
                event, &
                errcode_ret) &
            BIND(C, NAME='clEnqueueMapBuffer')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: buffer
            integer(c_int32_t), value :: blocking_map
            integer(c_int64_t), value :: map_flags
            integer(c_size_t), value :: offset
            integer(c_size_t), value :: size
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event
            integer(c_int32_t), intent(out) :: errcode_ret

        end function

    ! clEnqueueMapImage
        type(c_ptr) function clEnqueueMapImage(command_queue, &
                image, &
                blocking_map, &
                map_flags, &
                origin, &
                region, &
                image_row_pitch, &
                image_slice_pitch, &
                num_events_in_wait_list, &
                event_wait_list, &
                event, &
                errcode_ret) &
            BIND(C, NAME='clEnqueueMapImage')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: image
            integer(c_int32_t), value :: blocking_map
            integer(c_int64_t), value :: map_flags
            type(c_ptr), value :: origin
            type(c_ptr), value :: region
            integer(c_size_t), intent(out) :: image_row_pitch
            integer(c_size_t), intent(out) :: image_slice_pitch
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event
            integer(c_int32_t), intent(out) :: errcode_ret

        end function

    ! clEnqueueUnmapMemObject
        integer(c_int32_t) function clEnqueueUnmapMemObject(command_queue, &
                memobj, &
                mapped_ptr, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueUnmapMemObject')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: memobj
            type(c_ptr), value :: mapped_ptr
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event

        end function

    ! clEnqueueMigrateMemObjects
        integer(c_int32_t) function clEnqueueMigrateMemObjects(command_queue, &
                num_mem_objects, &
                mem_objects, &
                flags, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueMigrateMemObjects')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_int32_t), value :: num_mem_objects
            type(c_ptr), value :: mem_objects
            integer(c_int64_t), value :: flags
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event

        end function

    ! clEnqueueNDRangeKernel.
        integer(c_int32_t) function clEnqueueNDRangeKernel(command_queue, &
                kernel, &
                work_dim, &
                global_work_offset, &
                global_work_size, &
                local_work_size, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueNDRangeKernel')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: kernel
            integer(c_int32_t), value :: work_dim
            type(c_ptr), value :: global_work_offset
            type(c_ptr), value :: global_work_size
            type(c_ptr), value :: local_work_size
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event

        end function

    ! clEnqueueTask
        integer(c_int32_t) function clEnqueueTask(command_queue, &
                kernel, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueTask')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: kernel
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event

        end function

    ! clEnqueueNativeKernel
        integer(c_int32_t) function clEnqueueNativeKernel(command_queue, &
                user_func, &
                args, &
                cb_args, &
                num_mem_objects, &
                mem_list, &
                args_mem_loc, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueNativeKernel')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            type(c_funptr), value :: user_func
            type(c_ptr), value :: args
            integer(c_size_t), value :: cb_args
            integer(c_int32_t), value :: num_mem_objects
            type(c_ptr), value :: mem_list
            type(c_ptr), value :: args_mem_loc
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event

        end function

    ! clEnqueueMarkerWithWaitList
        integer(c_int32_t) function clEnqueueMarkerWithWaitList(command_queue, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueMarkerWithWaitList')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event

        end function

    ! clEnqueueMarkerWithWaitList
        integer(c_int32_t) function clEnqueueBarrierWithWaitList(command_queue, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueBarrierWithWaitList')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event

        end function

    ! clSetPrintfCallback
        integer(c_int32_t) function clSetPrintfCallback(context, &
                pfn_notify, &
                user_data) &
            BIND(C, NAME='clSetPrintfCallback')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: context
            type(c_funptr), value :: pfn_notify
            type(c_ptr), value :: user_data

        end function

    ! -------------------------
    ! Extension function access
    ! -------------------------
        type(c_funptr) function clGetExtensionFunctionAddressForPlatform(platform, &
                func_name) &
            BIND(C, NAME='clGetExtensionFunctionAddressForPlatform')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: platform
            type(c_ptr), value :: func_name

        end function
!
!    !
!    !end interface
!    !
    end interface
!
end module clfortran
