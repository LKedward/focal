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

submodule (Focal) Focal_Setup
  !!  Implementation module for openCL setup routines: context, command queues and programs.

  !! @note This is an implementation submodule: it contains the code implementing the subroutines defined in the
  !!  corresponding header module file. See header module file (Focal.f90) for interface definitions. @endnote

  use clfortran
  use M_strings, only: upperStr=>upper, splitStr=>split
  implicit none

  contains



  module procedure fclCreateContextWithPlatform !(platform) result(ctx)

    integer(c_intptr_t), target :: properties(3)
    integer(c_int32_t) :: errcode

    properties(1) = CL_CONTEXT_PLATFORM
    properties(2) = platform%cl_platform_id
    properties(3) = 0

    ctx%cl_context = clCreateContext(c_loc(properties), &
                platform%numDevice, c_loc(platform%cl_device_ids), &
                C_NULL_FUNPTR, C_NULL_PTR, errcode)

    call fclErrorHandler(errcode,'fclCreateContextWithPlatform','clCreateContext')

    ! platform%ctx = ctx
    ctx%platform = platform

    return

  end procedure fclCreateContextWithPlatform
  ! ---------------------------------------------------------------------------


  module procedure fclCreateContextWithVendor !(vendor) result(ctx)

    integer :: vi, i
    logical :: vendorFound

    type(fclPlatform), allocatable :: platforms(:)
    type(fclPlatform) :: chosenPlatform
    character(:), allocatable :: vendors(:)

    ! Get platforms
    platforms = fclGetPlatforms();

    ! Check for multiple vendors
    call splitStr(vendor,vendors,delimiters=',')

    vendorFound = .FALSE.
    vendorLoop: do vi=1,size(vendors,1)

      do i=1,size(platforms,1)

        if (index( upperstr(platforms(i)%vendor) , upperstr(trim(vendors(vi))) ) > 0 .or. &
            index( upperstr(platforms(i)%name) , upperstr(trim(vendors(vi))) ) > 0) then
          chosenPlatform = platforms(i)
          vendorFound = .TRUE.
          exit vendorLoop
        end if

      end do

    end do vendorLoop

    if (vendorFound) then
      ctx = fclCreateContextWithPlatform(chosenPlatform)
    else
      call fclRuntimeError('fclCreateContextWithVendor: vendor(s) "'//trim(vendor)//'" was not found.')
    end if

  end procedure fclCreateContextWithVendor
  ! ---------------------------------------------------------------------------


  module procedure fclSetDefaultContext !(ctx)
    ! Set the global default context

    call fclDbgCheckContext('fclSetDefaultContext',ctx)
    fclDefaultCtx = ctx

  end procedure fclSetDefaultContext
  ! ---------------------------------------------------------------------------


  module procedure fclFilterDevices !(devices,vendor,type,nameLike,extensions,sortBy) result(deviceList)
    !! Filter and sort list of devices based on criteria
    use futils_sorting, only: argsort
    integer :: i,j

    integer :: sortMetric(size(devices,1))
    integer :: sortList(size(devices,1))
    logical :: filter(size(devices,1)), platformMatch

    integer(c_int64_t) :: typeFilter
    integer(c_int64_t) :: deviceType
    integer :: nFiltered, nFill

    integer(c_int64_t) :: int64Metric

    character(3) :: CPU_TYPE
    character(:), allocatable :: extensionList(:)
    character(:), allocatable :: vendorList(:)
    
    CPU_TYPE = 'CPU'

    ! --- Parse any request to filter by device type ---
    typeFilter = 0
    if (present(type)) then
      if (index(upperstr(type),'CPU') > 0 .and. index(upperstr(type),'GPU') > 0) then
        typeFilter = 0
      else if (index(upperstr(type),'CPU') > 0) then
        typeFilter = CL_DEVICE_TYPE_CPU
      elseif (index( upperstr(type) , 'GPU' ) > 0) then
        typeFilter = CL_DEVICE_TYPE_GPU
      else
        call fclRuntimeError("fclFindDevices: "// &
        "Unknown type specified for type argument. Expecting 'cpu' or 'gpu'.'")
      end if
    end if

    if (present(extensions)) then
      call splitStr(extensions,extensionList,delimiters=',')
    end if

    if (present(vendor)) then
      call splitStr(vendor,vendorList,delimiters=',')
    end if

    ! --- Process the devices ---
    filter = .true.

    do i=1,size(devices,1)

      ! --- Filter by device type ---
      if (typeFilter > 0) then

        call fclGetDeviceInfo(devices(i),CL_DEVICE_TYPE,deviceType)

        if (deviceType /= typeFilter) then
          filter(i) = .false.         ! Filtered out by device type
        end if

      end if

      ! --- Filter by device extensions ---
      if (allocated(extensionList)) then
        do j=1,size(extensionList,1)
          if (index(upperstr(devices(i)%extensions), &
                         upperstr(trim(extensionList(j)))) == 0) then
            filter(i) = .false.      ! Filtered out by device extensions
            exit
          end if
        end do
      end if

      ! --- Filter by device platform vendor ---
      if (allocated(vendorList)) then
        platformMatch = .false.
        do j=1,size(vendorList,1)
          if ( index(upperstr(devices(i)%platformName),upperstr(trim(vendorList(j))))>0 .or. & 
               index(upperstr(devices(i)%platformVendor),upperstr(trim(vendorList(j))))>0 ) then
             platformMatch = .true.      
            exit
          end if
        end do
        filter(i) = filter(i).and.platformMatch ! Filtered out by device platform vendor
      end if

      ! --- Extract sorting metric ---
      if (present(sortBy)) then

        select case (upperstr(sortBy))
        case ('MEMORY')
          call fclGetDeviceInfo(devices(i),CL_DEVICE_GLOBAL_MEM_SIZE,int64Metric)
          sortMetric(i) = int(int64Metric/1000000,c_int32_t) ! Convert to megabytes to avoid overflow in int32

        case ('CORES')
          call fclGetDeviceInfo(devices(i),CL_DEVICE_MAX_COMPUTE_UNITS,sortMetric(i))

        case ('CLOCK')
          call fclGetDeviceInfo(devices(i),CL_DEVICE_MAX_CLOCK_FREQUENCY,sortMetric(i))

        end select

      else
        sortMetric(i) = 0
      end if

      ! --- Filter by device name ---
      if (present(nameLike)) then
        if (index(upperstr(devices(i)%name),upperstr(nameLike)) == 0) then
          filter(i) = .false.         ! Filtered out by device name
        end if
      end if

    end do

    ! --- Sort by sorting metric ---
    sortMetric = -sortMetric          ! Sort descending
    sortList = argsort(sortMetric)
    
    nFiltered = count(filter)
    allocate(deviceList(nFiltered))
    if (nFiltered < 1) then
      return
    end if    

    ! --- Output filtered sorted list of devices ---
    nFill = 1
    do i=1,size(devices,1)

      j = sortList(i)
      if (filter(j)) then
        deviceList(nFill) = devices(j)
        nFill = nFill + 1
      end if

      if (nFill > nFiltered) then
        exit
      end if

    end do

  end procedure fclFilterDevices
  ! ---------------------------------------------------------------------------


  module procedure fclInit !(vendor,type,nameLike,extensions,sortBy) result(device)
    !! Quick setup helper function: find a single device based on criteria
    !!  and set the default context accordingly.
    !!  Raises runtime error if no matching device is found.

    integer :: i

    type(fclPlatform) :: chosenPlatform
    type(fclPlatform), allocatable :: platforms(:)
    type(fclDevice), allocatable :: devices(:), deviceList(:)
    integer :: nDevice
    logical :: found

    ! Get platforms
    platforms = fclGetPlatforms();

    ! Count total number of system devices
    nDevice = 0
    do i=1,size(platforms,1)
      nDevice = nDevice + platforms(i)%numDevice
    end do

    ! Concatenate device lists across platforms
    allocate(devices(nDevice))
    nDevice = 0
    do i=1,size(platforms,1)
      devices(nDevice+1:nDevice+platforms(i)%numDevice) = platforms(i)%devices(:)
      nDevice = nDevice + platforms(i)%numDevice
    end do

    ! Find devices based on criteria
    deviceList = fclFilterDevices(devices,vendor,type,nameLike,extensions,sortBy)

    if (size(deviceList,1) < 1) then
      call fclRuntimeError('fclInit: no devices matching the specified criteria were found.')
    end if

    ! Choose first device in filtered, sorted list
    device = deviceList(1)

    ! Find corresponding platform for creating context
    found = .false.
    do i=1,size(platforms,1)
      
      if (platforms(i)%cl_platform_id == device%cl_platform_id) then
        chosenPlatform = platforms(i)
        found = .true.
        exit
      end if

    end do

    ! Create context and set as default
    call fclSetDefaultContext(fclCreateContext(chosenPlatform))

  end procedure fclInit
  ! ---------------------------------------------------------------------------


  module procedure fclFindDevices_1 !(ctx,vendor,type,nameLike,extensions,sortBy) result(deviceList)
    !! Create command queue by finding a device
    use futils_sorting, only: argsort
    
    call fclDbgCheckContext('fclFindDevices',ctx)

    deviceList = fclFilterDevices(ctx%platform%devices,vendor,type,nameLike,extensions,sortBy)

    if (.not.allocated(deviceList)) then
      call fclRuntimeError('fclFindDevices: no devices matching the specified criteria were found.')
    end if

  end procedure fclFindDevices_1
  ! ---------------------------------------------------------------------------


  module procedure fclFindDevices_2 !(type,vendor,nameLike,extensions,sortBy) result(deviceList)

    call fclDbgCheckContext('fclFindDevices')

    deviceList = fclFindDevices_1(fclDefaultCtx,vendor,type,nameLike,extensions,sortBy)

  end procedure fclFindDevices_2
  ! ---------------------------------------------------------------------------


  module procedure fclCreateCommandQ_1 !(ctx,device,enableProfiling,outOfOrderExec,&
                                         !blockingWrite,blockingRead) result(cmdq)
    !! Create a command queue with a Focal device object

    integer(c_int32_t) :: errcode
    integer(c_int64_t) :: properties

    properties = 0

    call fclDbgCheckContext('fclCreateCommandQ',ctx)

    if (present(enableProfiling)) then
      if (enableProfiling) then
        properties = ior(properties,CL_QUEUE_PROFILING_ENABLE)
      end if
    end if

    if (present(outOfOrderExec)) then
      if (outOfOrderExec) then
        properties = ior(properties,CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE)
      end if
    end if

    if (present(blockingWrite)) then
      cmdq%blockingWrite = blockingWrite
    end if

    if (present(blockingRead)) then
      cmdq%blockingRead = blockingRead
    end if

    cmdq%cl_command_queue = clCreateCommandQueue(ctx%cl_context, device%cl_device_id, &
                                  properties ,errcode)

    call fclErrorHandler(errcode,'fclCreateDeviceCommandQWithDevice','clCreateCommandQueue')

  end procedure fclCreateCommandQ_1
  ! ---------------------------------------------------------------------------


  module procedure fclCreateCommandQ_2 !(device,enableProfiling,outOfOrderExec,&
                                         !blockingWrite,blockingRead) result(cmdq)
    !! Create a command queue with a Focal device object using default context

    call fclDbgCheckContext('fclCreateCommandQ')

    cmdq = fclCreateCommandQ_1(fclDefaultCtx,device,enableProfiling,outOfOrderExec, &
                                           blockingWrite,blockingRead)

  end procedure fclCreateCommandQ_2
  ! ---------------------------------------------------------------------------


  module procedure fclCreateCommandQPool_1 !(ctx,N,device,enableProfiling,outOfOrderExec,&
      ! blockingWrite,blockingRead) result(qPool)
    !! Create a command queue pool with a Focal device object

    integer :: i

    call fclDbgCheckContext('fclCreateCommandQPool',ctx)

    qPool%length = N

    allocate(qPool%queues(N))

    do i=1,N
      qPool%queues(i) = fclCreateCommandQ_1(ctx,device,enableProfiling,outOfOrderExec, &
                                                blockingWrite, blockingRead)
    end do

  end procedure fclCreateCommandQPool_1
  ! ---------------------------------------------------------------------------


  module procedure fclCreateCommandQPool_2 !(N,device,enableProfiling,outOfOrderExec,&
    ! blockingWrite,blockingRead) result(qPool)
    !! Create a command queue pool with a Focal device object using the default context

    call fclDbgCheckContext('fclCreateCommandQPool')

    qPool = fclCreateCommandQPool_1(fclDefaultCtx,N,device,enableProfiling,outOfOrderExec,&
                                      blockingWrite,blockingRead)

  end procedure fclCreateCommandQPool_2
  ! ---------------------------------------------------------------------------


  module procedure fclCommandQPool_Next !(qPool) result(cmdQ)
    !! Returns next scheduled queue in queue pool

    ! Increment queue index (round-robin scheduling)
    qPool%idx = qPool%idx + 1
    qPool%idx = mod(qPool%idx-1,qPool%length) + 1

    ! Return next queue
    cmdQ => qPool%queues(qPool%idx)

  end procedure fclCommandQPool_Next
  ! ---------------------------------------------------------------------------

  
  module procedure fclCommandQPool_Current !(qPool) result(cmdQ)
    !! Returns current scheduled queue in queue pool

    cmdQ => qPool%queues(qPool%idx)

  end procedure fclCommandQPool_Current
  ! ---------------------------------------------------------------------------


  module procedure fclSetDefaultCommandQ !(cmdq)
    !! Set the global default command queue
    fclDefaultCmdQ = cmdq

  end procedure fclSetDefaultCommandQ
  ! ---------------------------------------------------------------------------

  
  module procedure fclCompileProgram_1 !(ctx,source,options) result(prog)

    integer :: i
    integer(c_int32_t) :: errcode
    character(len=1,kind=c_char), target :: c_source(len(source)+1)
    type(c_ptr), target :: c_source_p
    character(:), allocatable :: options_temp
    character(len=1,kind=c_char), allocatable, target :: c_options(:)

    call fclDbgCheckContext('fclCompileProgram',ctx)

    ! Convert to c character array
    do i=1,len(source)
      c_source(i) = source(i:i)
    end do
    c_source(len(source)+1) = C_NULL_CHAR

    c_source_p = c_loc(c_source)
    prog%cl_program = clCreateProgramWithSource(ctx%cl_context,1, &
                          C_LOC(c_source_p),C_NULL_PTR,errcode)

    call fclErrorHandler(errcode,'fclCompileProgram','clCreateProgramWithSource')

    if (present(options)) then
      options_temp = options//' '//fclDbgOptions()
    else
      options_temp = fclDbgOptions()
    end if

    allocate(c_options(len(options_temp)+1))
    do i=1,len(options_temp)
      c_options(i) = options_temp(i:i)
    end do
    c_options(len(options_temp)+1) = C_NULL_CHAR

    errcode = clBuildProgram(prog%cl_program,0, &
          C_NULL_PTR,C_LOC(c_options),C_NULL_FUNPTR,C_NULL_PTR)

    call fclHandleBuildError(errcode,prog,ctx)

    deallocate(c_options)

  end procedure fclCompileProgram_1
  ! ---------------------------------------------------------------------------


  module procedure fclCompileProgram_2 !(source,options) result(prog)

    call fclDbgCheckContext('fclCompileProgram')

    prog = fclCompileProgram_1(fclDefaultCtx,source,options)

  end procedure fclCompileProgram_2
  ! ---------------------------------------------------------------------------


  module procedure  fclDumpBuildLog_1 !(ctx,prog,device,outputUnit)
    use iso_fortran_env, only: stdout => output_unit

    integer(c_int32_t) :: errcode
    integer :: out
    integer(c_size_t) :: buffLen, int32_ret
    character(len=1), allocatable, target :: buildLogBuffer(:)

    call fclDbgCheckContext('fclDumpBuildLog',ctx)

    if (present(outputUnit)) then
      out = outputUnit
    else
      out = stdout
    end if

    errcode = clGetProgramBuildInfo(prog%cl_program, device%cl_device_id, &
          CL_PROGRAM_BUILD_LOG, int(0,c_size_t), C_NULL_PTR, buffLen)

    call fclErrorHandler(errcode,'fclCompileProgram','clGetProgramBuildInfo')

    allocate(buildLogBuffer(buffLen))
    buffLen = size(buildLogBuffer,1)

    errcode = clGetProgramBuildInfo(prog%cl_program, device%cl_device_id, &
      CL_PROGRAM_BUILD_LOG, buffLen, c_loc(buildLogBuffer), int32_ret)

    call fclErrorHandler(errcode,'fclCompileProgram','clGetProgramBuildInfo')

    write(*,*) ' fclDumpBuildLog: Build log for context device: ',device%name
    write(out,*) buildLogBuffer
    write(out,*)

    deallocate(buildLogBuffer)

  end procedure fclDumpBuildLog_1
  ! ---------------------------------------------------------------------------


  module procedure fclDumpBuildLog_2 !(prog,device,outputUnit)

    call fclDbgCheckContext('fclDumpBuildLog')

    call fclDumpBuildLog_1(fclDefaultCtx,prog,device,outputUnit)

  end procedure fclDumpBuildLog_2
  ! ---------------------------------------------------------------------------


  module procedure fclGetProgramKernel !(prog,kernelName,global_work_size,local_work_size, &
                                           ! work_dim,global_work_offset) result(kern)

    integer :: i
    integer(c_int32_t) :: errcode
    character(len=1,kind=c_char), target :: c_name(len(kernelName)+1)

    do i=1,len(kernelName)
      c_name(i) = kernelName(i:i)
    end do
    c_name(len(kernelName)+1) = C_NULL_CHAR

    kern%cl_kernel = clCreateKernel(prog%cl_program,C_LOC(c_name),errcode)

    call fclErrorHandler(errcode,'fclGetProgramKernel','clCreateKernel')

    allocate(character(len=len(kernelName)) :: kern%name)
    kern%name = kernelName

    if (present(global_work_size)) then
      if (size(global_work_size,1) > 3) then
        call fclRuntimeError('fclGetProgramKernel: global work size must have dimension less than or equal to three.')
      else
        kern%work_dim = size(global_work_size,1)
        kern%global_work_size(1:size(global_work_size,1)) = global_work_size
      end if
    end if

    if (present(local_work_size)) then
      if (size(local_work_size,1) > 3) then
        call fclRuntimeError('fclGetProgramKernel: local work size must have dimension less than or equal to three.')
      else
        kern%local_work_size(1:size(local_work_size,1)) = local_work_size
      end if
    end if

    if (present(work_dim)) then
      if (work_dim > 3) then
        call fclRuntimeError('fclGetProgramKernel: kernel work dimensionmust be less than or equal to three.')
      else
        kern%work_dim = work_dim
      end if
    end if

    if (present(global_work_offset)) then
      if (size(global_work_offset,1) > 3) then
        call fclRuntimeError('fclGetProgramKernel: global work offset must have dimension less than or equal to three.')
      else
        kern%global_work_offset(1:size(global_work_offset,1)) = global_work_offset
      end if
    end if

  end procedure fclGetProgramKernel
  ! ---------------------------------------------------------------------------


  module procedure fclLaunchKernelAfterEvent_1 !(kernel,cmdQ,event)
    !! Specific interface for a single event dependency on a specific command queue

    call fclSetDependency(cmdQ,event)
    call fclLaunchKernel(kernel,cmdQ)

  end procedure fclLaunchKernelAfterEvent_1
  ! ---------------------------------------------------------------------------


  module procedure fclLaunchKernelAfterEvent_2 !(kernel,event)
    !! Specific interface a single event dependency on the __default command queue__

    call fclLaunchKernelAfterEvent_1(kernel,fclDefaultCmdQ,event)

  end procedure fclLaunchKernelAfterEvent_2
  ! ---------------------------------------------------------------------------


  module procedure fclLaunchKernelAfterEventList_1 !(kernel,cmdQ,eventList)
    !! Specific interface for a multiple event dependencies on a specific command queue

    call fclSetDependency(cmdQ,eventList)
    call fclLaunchKernel(kernel,cmdQ)

  end procedure fclLaunchKernelAfterEventList_1
  ! ---------------------------------------------------------------------------


  module procedure fclLaunchKernelAfterEventList_2 !(kernel,eventList)
    !! Specific interface for a multiple event dependencies on the __default command queue__

    call fclLaunchKernelAfterEventList_1(kernel,fclDefaultCmdQ,eventList)

  end procedure fclLaunchKernelAfterEventList_2
  ! ---------------------------------------------------------------------------


  module procedure fclLaunchKernel !(kernel,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,&
                                      ! a10,a11,a12,a13,a14,a15,a16,a17,a18,a19, &
                                      ! a20,a21,a22,a23,a24,a25,a26,a27,a28,a29)

    integer(c_size_t) :: i, nBlocki
    integer(c_int32_t) :: errcode
    type(fclCommandQ), pointer :: cmdQ
    type(c_ptr) :: localSizePtr
    integer :: nArg
    type(fclEvent), target :: kernelEvent

    ! Check global size has been set
    if (sum(abs(kernel%global_work_size)) == 0) then
      write(*,*) 'Kernel name: ',trim(kernel%name)
      call fclRuntimeError('fclLaunchKernel: kernel global_work_size is unset.')
    end if

    ! Check if local size has been set
    if (sum(abs(kernel%local_work_size)) == 0) then
      localSizePtr = C_NULL_PTR
    else
      localSizePtr = c_loc(kernel%local_work_size)

      ! Check global dims are multiples of user-specified 
      !  local dims and update if necessary
      do i=1,kernel%work_dim
        if (mod(kernel%global_work_size(i),kernel%local_work_size(i)) > 0) then
          nBlocki = (kernel%global_work_size(i) + kernel%local_work_size(i) - 1)/kernel%local_work_size(i)
          kernel%global_work_size(i) = nBlocki*kernel%local_work_size(i)
        end if
      end do

    end if

    ! Set arguments and parse (get number of args and cmdq if specified)
    call fclProcessKernelArgs(kernel,cmdq,narg,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,&
                                a10,a11,a12,a13,a14,a15,a16,a17,a18,a19, &
                                a20,a21,a22,a23,a24,a25,a26,a27,a28,a29)

    errcode = clEnqueueNDRangeKernel(cmdq%cl_command_queue, &
                kernel%cl_kernel, kernel%work_dim, &
                c_loc(kernel%global_work_offset), &
                c_loc(kernel%global_work_size), localSizePtr, &
                cmdq%nDependency, cmdq%dependencyListPtr, &
                c_loc(kernelEvent%cl_event))

    call fclDbgWait(kernelEvent)
    call fclPopDependencies(cmdq)
    call fclErrorHandler(errcode,'fclLaunchKernel','clEnqueueNDRangeKernel')

    fclLastKernelEvent = kernelEvent
    cmdQ%lastKernelEvent = kernelEvent

    call kernel%pushProfileEvent(kernelEvent)

  end procedure fclLaunchKernel
  ! ---------------------------------------------------------------------------


  module procedure fclProcessKernelArgs !(kernel,cmdq,narg,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9, &
                                            ! a10,a11,a12,a13,a14,a15,a16,a17,a18,a19, &
                                            !  a20,a21,a22,a23,a24,a25,a26,a27,a28,a29)
    !! Sets kernel arguments and parses argument list for optional cmdq and actual number of arguments

    integer :: i0

    ! --- Check if command queue was specified ---
    nArg = 0
    i0 = 0
    cmdQ => fclDefaultCmdQ
    if (present(a0)) then
      select type(arg => a0)

      class is (fclCommandQ)
        !! cmdQ is specified in first arg
        cmdQ => arg
        i0 = 0

      class default
        !! First arg is not cmdQ: then it is a kernel arg
        call fclSetKernelArg(kernel,0,arg)
        i0 = 1
        nArg = nArg + 1

      end select
    end if

    ! --- Set arguments ---
    if (present(a1)) then
      call fclSetKernelArg(kernel,i0+0,a1)
      nArg = nArg + 1
    end if
    if (present(a2)) then
      call fclSetKernelArg(kernel,i0+1,a2)
      nArg = nArg + 1
    end if
    if (present(a3)) then
      call fclSetKernelArg(kernel,i0+2,a3)
      nArg = nArg + 1
    end if
    if (present(a4)) then
      call fclSetKernelArg(kernel,i0+3,a4)
      nArg = nArg + 1
    end if
    if (present(a5)) then
      call fclSetKernelArg(kernel,i0+4,a5)
      nArg = nArg + 1
    end if
    if (present(a6)) then
      call fclSetKernelArg(kernel,i0+5,a6)
      nArg = nArg + 1
    end if
    if (present(a7)) then
      call fclSetKernelArg(kernel,i0+6,a7)
      nArg = nArg + 1
    end if
    if (present(a8)) then
      call fclSetKernelArg(kernel,i0+7,a8)
      nArg = nArg + 1
    end if
    if (present(a9)) then
      call fclSetKernelArg(kernel,i0+8,a9)
      nArg = nArg + 1
    end if
    if (present(a10)) then
      call fclSetKernelArg(kernel,i0+9,a10)
      nArg = nArg + 1
    end if
    if (present(a11)) then
      call fclSetKernelArg(kernel,i0+10,a11)
      nArg = nArg + 1
    end if
    if (present(a12)) then
      call fclSetKernelArg(kernel,i0+11,a12)
      nArg = nArg + 1
    end if
    if (present(a13)) then
      call fclSetKernelArg(kernel,i0+12,a13)
      nArg = nArg + 1
    end if
    if (present(a14)) then
      call fclSetKernelArg(kernel,i0+13,a14)
      nArg = nArg + 1
    end if
    if (present(a15)) then
      call fclSetKernelArg(kernel,i0+14,a15)
      nArg = nArg + 1
    end if
    if (present(a16)) then
      call fclSetKernelArg(kernel,i0+15,a16)
      nArg = nArg + 1
    end if
    if (present(a17)) then
      call fclSetKernelArg(kernel,i0+16,a17)
      nArg = nArg + 1
    end if
    if (present(a18)) then
      call fclSetKernelArg(kernel,i0+17,a18)
      nArg = nArg + 1
    end if
    if (present(a19)) then
      call fclSetKernelArg(kernel,i0+18,a19)
      nArg = nArg + 1
    end if
    if (present(a20)) then
      call fclSetKernelArg(kernel,i0+19,a20)
      nArg = nArg + 1
    end if
    if (present(a21)) then
      call fclSetKernelArg(kernel,i0+20,a21)
      nArg = nArg + 1
    end if
    if (present(a22)) then
      call fclSetKernelArg(kernel,i0+21,a22)
      nArg = nArg + 1
    end if
    if (present(a23)) then
      call fclSetKernelArg(kernel,i0+22,a23)
      nArg = nArg + 1
    end if
    if (present(a24)) then
      call fclSetKernelArg(kernel,i0+23,a24)
      nArg = nArg + 1
    end if
    if (present(a25)) then
      call fclSetKernelArg(kernel,i0+24,a25)
      nArg = nArg + 1
    end if
    if (present(a26)) then
      call fclSetKernelArg(kernel,i0+25,a26)
      nArg = nArg + 1
    end if
    if (present(a27)) then
      call fclSetKernelArg(kernel,i0+26,a27)
      nArg = nArg + 1
    end if
    if (present(a28)) then
      call fclSetKernelArg(kernel,i0+27,a28)
      nArg = nArg + 1
    end if
    if (present(a29)) then
      call fclSetKernelArg(kernel,i0+28,a29)
      nArg = nArg + 1
    end if

    if (nArg > 0) then
      ! If any kernel arguments are specified, check that they are all present
      call fclDbgCheckKernelNArg(kernel,nArg)
    end if

  end procedure fclProcessKernelArgs
  ! ---------------------------------------------------------------------------


  module procedure fclSetKernelArgs !(kernel,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9, &
                                    ! a10,a11,a12,a13,a14,a15,a16,a17,a18,a19, &
                                    ! a20,a21,a22,a23,a24,a25,a26,a27,a28,a29)
    !! Set all kernel arguments at once without launching kernel.

    type(fclCommandQ), pointer :: cmdq
    integer :: nArg

    call fclProcessKernelArgs(kernel,cmdq,narg,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9, &
                               a10,a11,a12,a13,a14,a15,a16,a17,a18,a19, &
                               a20,a21,a22,a23,a24,a25,a26,a27,a28,a29)

  end procedure fclSetKernelArgs
  ! ---------------------------------------------------------------------------


  module procedure fclSetKernelArg !(kernel,argIndex,argValue)

    integer(c_int32_t) :: errcode
    type(c_ptr) :: argPtr
    integer(c_size_t) :: argSize

    !! @note
    !! " The argument data pointed to by arg_value is copied and the arg_value pointer
    !!   can therefore be reused by the application after clSetKernelArg returns."
    !! https://www.khronos.org/registry/OpenCL/sdk/1.2/docs/man/xhtml/clSetKernelArg.html
    !! @endnote

    !! @todo Debug check argument types against kernel arg types (clGetKernelArgInfo) @endtodo

    argPtr = C_NULL_PTR
    argSize = -1

    select type(arg => argValue)

    class is (fclDeviceBuffer)
        argPtr = c_loc(arg%cl_mem)
        argSize = c_sizeof(arg%cl_mem)
        call fclDbgCheckKernelArgQualifier(kernel,argIndex,'global,constant')
        call fclDbgCheckBufferInit(arg,'fclSetKernelArg')

      class is (fclDeviceInt32)
        argPtr = c_loc(arg%cl_mem)
        argSize = c_sizeof(arg%cl_mem)
        call fclDbgCheckKernelArgType(kernel,argIndex,'int*')
        call fclDbgCheckKernelArgQualifier(kernel,argIndex,'global,constant')
        call fclDbgCheckBufferInit(arg,'fclSetKernelArg')

      class is (fclDeviceFloat)
        argPtr = c_loc(arg%cl_mem)
        argSize = c_sizeof(arg%cl_mem)
        call fclDbgCheckKernelArgType(kernel,argIndex,'float*')
        call fclDbgCheckKernelArgQualifier(kernel,argIndex,'global,constant')
        call fclDbgCheckBufferInit(arg,'fclSetKernelArg')

      class is (fclDeviceDouble)
        argPtr = c_loc(arg%cl_mem)
        argSize = c_sizeof(arg%cl_mem)
        call fclDbgCheckKernelArgType(kernel,argIndex,'double*')
        call fclDbgCheckKernelArgQualifier(kernel,argIndex,'global,constant')
        call fclDbgCheckBufferInit(arg,'fclSetKernelArg')

      class is (fclLocalArgument)
        argPtr = C_NULL_PTR
        argSize = arg%nBytes
        call fclDbgCheckKernelArgQualifier(kernel,argIndex,'local')

      class is (fclLocalArgInt32)
        argPtr = C_NULL_PTR
        argSize = arg%nBytes
        call fclDbgCheckKernelArgType(kernel,argIndex,'int*')
        call fclDbgCheckKernelArgQualifier(kernel,argIndex,'local')

      class is (fclLocalArgFloat)
        argPtr = C_NULL_PTR
        argSize = arg%nBytes
        call fclDbgCheckKernelArgType(kernel,argIndex,'float*')
        call fclDbgCheckKernelArgQualifier(kernel,argIndex,'local')

      class is (fclLocalArgDouble)
        argPtr = C_NULL_PTR
        argSize = arg%nBytes
        call fclDbgCheckKernelArgType(kernel,argIndex,'double*')
        call fclDbgCheckKernelArgQualifier(kernel,argIndex,'local')

      type is (integer(c_int32_t))
        argPtr = c_loc(arg)
        argSize = c_sizeof(int(1,c_int32_t))
        call fclDbgCheckKernelArgType(kernel,argIndex,'int')
        call fclDbgCheckKernelArgQualifier(kernel,argIndex,'private')

      type is (real(c_float))
        argPtr = c_loc(arg)
        argSize = c_sizeof(real(1.0,c_float))
        call fclDbgCheckKernelArgType(kernel,argIndex,'float')
        call fclDbgCheckKernelArgQualifier(kernel,argIndex,'private')

      type is (real(c_double))
        argPtr = c_loc(arg)
        argSize = c_sizeof(real(1.0d0,c_double))
        call fclDbgCheckKernelArgType(kernel,argIndex,'double')
        call fclDbgCheckKernelArgQualifier(kernel,argIndex,'private')

      class default
        write(*,*) 'Kernel name: ',trim(kernel%name)
        write(*,'(A,I4)') 'Argument index: ',argIndex
        call fclRuntimeError('fclSetKernelArg: unsupported argument type passed to kernel.')

    end select

    errcode = clSetKernelArg(kernel%cl_kernel,argIndex,argSize,argPtr)

    call fclErrorHandler(errcode,'fclSetKernelArg','clSetKernelArg')

  end procedure fclSetKernelArg
  ! ---------------------------------------------------------------------------


  module procedure fclLocalInt32 !(nElem) result(localArg)
    !! Create a integer local kernel argument object for launching kernels
    localArg%nBytes = c_sizeof(int(1,c_int32_t))*nElem

  end procedure fclLocalInt32
  ! ---------------------------------------------------------------------------


  module procedure fclLocalFloat !(nElem) result(localArg)
    !! Create a integer local kernel argument object for launching kernels
    localArg%nBytes = c_sizeof(real(1.0,c_float))*nElem

  end procedure fclLocalFloat
  ! ---------------------------------------------------------------------------


  module procedure fclLocalDouble !(nElem) result(localArg)
    !! Create a integer local kernel argument object for launching kernels
    localArg%nBytes = c_sizeof(real(1.0d0,c_double))*nElem

  end procedure fclLocalDouble
  ! ---------------------------------------------------------------------------


  module procedure fclBarrier_1 !(cmdq)
    !! Enqueue barrier on all events in command queue
    integer(c_int32_t) :: errcode
    type(fclEvent), target :: barrierEvent

    errcode = clEnqueueBarrierWithWaitList( cmdq%cl_command_queue, &
                  cmdq%nDependency, cmdq%dependencyListPtr , &
                  c_loc(barrierEvent%cl_event))

    call fclPopDependencies(cmdq)
    call fclErrorHandler(errcode,'fclBarrierAll','clEnqueueBarrierWithWaitList')

    fclLastBarrierEvent = barrierEvent
    cmdq%lastBarrierEvent = barrierEvent

  end procedure fclBarrier_1
  ! ---------------------------------------------------------------------------


  module procedure fclBarrier_2 !(cmdq)
    !! Enqueue barrier on all events in default command queue
    call fclBarrier_1(fclDefaultCmdQ)

  end procedure fclBarrier_2
  ! ---------------------------------------------------------------------------


  module procedure fclFinish_1 !(cmdq)
    !! Wait on host for all events in user-specified command queue
    integer(c_int32_t) :: errcode

    errcode = clFinish(cmdq%cl_command_queue)

    call fclErrorHandler(errcode,'fclFinish','clFinish')

  end procedure fclFinish_1
  ! ---------------------------------------------------------------------------


  module procedure fclFinish_2
    !! Wait on host for all events in focal default command queue
    call fclFinish_1(fclDefaultCmdQ)

  end procedure fclFinish_2
  ! ---------------------------------------------------------------------------


  module procedure fclFinish_3 !(qPool)
    !! Wait on host for all events in all queues in a queue pool

    integer :: i

    do i=1,qPool%length
      call fclFinish_1(qPool%queues(i))
    end do

  end procedure fclFinish_3
  ! ---------------------------------------------------------------------------


  module procedure fclWaitEvent !(event)
    !! Wait on host for a specific event
    integer(c_int32_t) :: errcode

    errcode = clWaitForEvents ( 1, c_loc(event%cl_event) )

    call fclErrorHandler(errcode,'fclWaitEvent','clWaitForEvents')

  end procedure fclWaitEvent
  ! ---------------------------------------------------------------------------


  module procedure fclWaitEventList !(eventList)
    !! Wait on host for set of events
    integer :: i
    integer(c_int32_t) :: errcode
    integer(c_intptr_t), target :: cl_eventList(size(eventList,1))

    ! Populate array of c_ptr
    cl_eventList = [(eventList(i)%cl_event,i=1,size(eventList,1))]

    errcode = clWaitForEvents ( size(eventList,1), c_loc(cl_eventList) )

    call fclErrorHandler(errcode,'fclWaitEventList','clWaitForEvents')

  end procedure fclWaitEventList
  ! ---------------------------------------------------------------------------


  module procedure fclEventCopy !(target, source)
    !! Overloaded assignment for event assignment.
    !!  Handles opencl reference counting for the underlying event object

    if (target%cl_event > 0) then

      call fclReleaseEvent(target)

    end if

    call fclRetainEvent(source)

    target%cl_event = source%cl_event

  end procedure fclEventCopy
  ! ---------------------------------------------------------------------------


  module procedure fclReleaseEvent !(event)
    !! Light weight wrapper for clReleaseEvent (decrement reference count)
    integer(c_int32_t) :: errcode

    if (event%cl_event > 0) then

      errcode = clReleaseEvent(event%cl_event)
      call fclErrorHandler(errcode,'fclReleaseEvent','clReleaseEvent')

    end if

  end procedure fclReleaseEvent
  ! ---------------------------------------------------------------------------


  module procedure fclRetainEvent !(event)
    !! Light weight wrapper for clRetainEvent (increment reference count)
    integer(c_int32_t) :: errcode


    if (event%cl_event > 0) then

      errcode = clRetainEvent(event%cl_event)
      call fclErrorHandler(errcode,'fclRetainEvent','clRetainEvent')

    end if

  end procedure fclRetainEvent
  ! ---------------------------------------------------------------------------


  module procedure fclSetDependencyEvent_1 !(cmdq,event,hold)
    !! Specify a single event dependency on specific cmdq

    integer(c_int32_t) :: errcode

    if (.not.allocated(cmdq%dependencyList)) then

     allocate(cmdq%dependencyList(fclAllocationSize))

    end if

    cmdq%dependencyList(1) = event%cl_event
    cmdq%nDependency = 1
    cmdq%dependencyListPtr = c_loc(cmdq%dependencyList)

    ! Explicitly increment event reference counter
    errcode = clRetainEvent(event%cl_event)
    call fclErrorHandler(errcode,'fclSetDependencyEvent','clRetainEvent')

    if (present(hold)) then
      cmdq%holdDependencies = hold
    end if

  end procedure fclSetDependencyEvent_1
   ! ---------------------------------------------------------------------------


  module procedure fclSetDependencyEvent_2 !(event,hold)
    !! Specify a single event dependency on default cmdq
    call fclSetDependencyEvent_1(fclDefaultCmdQ,event,hold)

  end procedure fclSetDependencyEvent_2
  ! ---------------------------------------------------------------------------


  module procedure fclSetDependencyEventList_1 !(cmdq,eventList,hold)
    !! Specify a list of dependent events on specific cmdq
    
    integer :: i, nEvent, nAlloc
    integer(c_int32_t) :: errcode

    nEvent = size(eventList,1)
    nAlloc = max(fclAllocationSize,nEvent)

    if (.not.allocated(cmdq%dependencyList)) then
     !! Allocate for first time
     allocate(cmdq%dependencyList(nAlloc))

    elseif (size(cmdq%dependencyList,1) < nEvent) then
     !! Re-allocate bigger
     deallocate(cmdq%dependencyList)
     allocate(cmdq%dependencyList(nAlloc))

    end if

    cmdq%dependencyList(1:nEvent) = [(eventList(i)%cl_event,i=1,nEvent)]
    cmdq%nDependency = nEvent
    cmdq%dependencyListPtr = c_loc(cmdq%dependencyList)

    ! Explicitly increment event reference counters
    do i=1,nEvent
      errcode = clRetainEvent(eventList(i)%cl_event)
      call fclErrorHandler(errcode,'fclSetDependencyEvent','clRetainEvent') 
    end do

    if (present(hold)) then
      cmdq%holdDependencies = hold
    end if

  end procedure fclSetDependencyEventList_1
  ! ---------------------------------------------------------------------------


  module procedure fclSetDependencyEventList_2 !(eventList,hold)
    !! Specify a list of dependent events on the default cmdq

    call fclSetDependencyEventList_1(fclDefaultCmdQ,eventList,hold)

  end procedure fclSetDependencyEventList_2
  ! ---------------------------------------------------------------------------


  module procedure fclPopDependencies !(cmdq)
    !! Called after every enqueue operation:
    !! Clear dependencies unless dependency hold is .true.

    if (.not.cmdq%holdDependencies) then
      call fclClearDependencies(cmdq)
    end if

  end procedure fclPopDependencies
  ! ---------------------------------------------------------------------------


  module procedure fclClearDependencies_1 !(cmdq)
    !! Reset dependency list

    integer :: i
    integer(c_int32_t) :: errcode

    ! Explicitly decrement event reference counters
    do i=1,cmdq%nDependency
      errcode = clReleaseEvent(cmdq%dependencyList(i))
      call fclErrorHandler(errcode,'fclClearDependencies','clReleaseEvent') 
    end do

    cmdq%nDependency = 0
    cmdq%dependencyListPtr = C_NULL_PTR
    cmdq%holdDependencies = .false.

  end procedure fclClearDependencies_1
  ! ---------------------------------------------------------------------------


  module procedure fclClearDependencies_2
    !! Reset dependency list on default command queue
    call fclClearDependencies_1(fclDefaultCmdQ)

  end procedure fclClearDependencies_2
  ! ---------------------------------------------------------------------------


  module procedure fclCreateUserEvent_1 !(ctx) result(userEvent)
    !! Create user event in a specific context
    
    integer(c_int32_t) :: errcode

    userEvent%cl_event = clCreateUserEvent(ctx%cl_context,errcode)

    call fclErrorHandler(errcode,'fclCreateUserEvent','clCreateUserEvent') 

  end procedure fclCreateUserEvent_1
  ! ---------------------------------------------------------------------------


  module procedure fclCreateUserEvent_2 !() result(userEvent)
    !! Create user event in in the default context
    
    userEvent = fclCreateUserEvent_1(fclDefaultCtx)

  end procedure fclCreateUserEvent_2
  ! ---------------------------------------------------------------------------
  

  module procedure fclSetUserEvent !(event,stat)
     !! Set status of a user event

    integer(c_int32_t) :: errcode, eStatus

    if (present(stat)) then
      eStatus = stat
    else
      eStatus = 0
    end if

    errcode = clSetUserEventStatus(event%cl_event, eStatus)
    
    call fclErrorHandler(errcode,'fclSetUserEvent','clSetUserEventStatus') 

  end procedure fclSetUserEvent
  ! ---------------------------------------------------------------------------

end submodule Focal_Setup
