submodule (Focal) Focal_Setup
  !! FOCAL: openCL abstraction layer for fortran
  !!  Implementation module for openCL setup routines: context, command queues and programs.

  !! @note This is an implementation submodule: it contains the code implementing the subroutines defined in the
  !!  corresponding header module file. See header module file (Focal.f90) for interface definitions. @endnote

  use clfortran
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

    integer :: i
    logical :: vendorFound

    type(fclPlatform), pointer :: platforms(:)
    type(fclPlatform) :: chosenPlatform

    ! Get platforms
    platforms => fclGetPlatforms();

    vendorFound = .FALSE.
    do i=1,size(platforms,1)

      if (index( upperstr(platforms(i)%vendor) , upperstr(vendor) ) > 0) then
        chosenPlatform = platforms(i)
        vendorFound = .TRUE.
        exit
      end if

    end do

    if (vendorFound) then
      ctx = fclCreateContextWithPlatform(chosenPlatform)
    else
      call fclRuntimeError('fclCreateContextWithVendor: vendor "'//trim(vendor)//'" was not found.')
    end if

  end procedure fclCreateContextWithVendor
  ! ---------------------------------------------------------------------------


  module procedure fclSetDefaultContext !(ctx)
    ! Set the global default context
    fclDefaultCtx = ctx

  end procedure fclSetDefaultContext
  ! ---------------------------------------------------------------------------


  module procedure fclFindDevices_1 !(ctx,type,nameLike,sortBy) result(deviceList)
    !! Create command queue by finding a device
    use quicksort
    integer :: i,j

    integer :: sortMetric(ctx%platform%numDevice)
    integer :: sortList(ctx%platform%numDevice)
    logical :: filter(ctx%platform%numDevice)

    integer(c_int64_t) :: typeFilter
    integer(c_int64_t) :: deviceType
    integer :: nFiltered, nFill

    integer(c_int64_t) :: int64Metric

    character(3) :: CPU_TYPE
    CPU_TYPE = 'CPU'

    ! --- Parse any request to filter by device type ---
    typeFilter = 0
    if (present(type)) then
      if (index(upperstr(type),'CPU') > 0) then
        typeFilter = CL_DEVICE_TYPE_CPU
      elseif (index( upperstr(type) , 'GPU' ) > 0) then
        typeFilter = CL_DEVICE_TYPE_GPU
      else
        call fclRuntimeError("fclFindDevices: "// &
        "Unknown type specified for type argument. Expecting 'cpu' or 'gpu'.'")
      end if
    end if

    ! --- Process the devices ---
    filter = .true.

    do i=1,ctx%platform%numDevice

      ! --- Filter by device type ---
      if (typeFilter > 0) then

        call fclGetDeviceInfo(ctx%platform%devices(i),CL_DEVICE_TYPE,deviceType)

        if (deviceType /= typeFilter) then
          filter(i) = .false.         ! Filtered out by device type
        end if

      end if

      ! --- Extract sorting metric ---
      if (present(sortBy)) then

        select case (upperstr(sortBy))
        case ('MEMORY')
          call fclGetDeviceInfo(ctx%platform%devices(i),CL_DEVICE_GLOBAL_MEM_SIZE,int64Metric)
          sortMetric(i) = int(int64Metric/1000000,c_int32_t) ! Convert to megabytes to avoid overflow in int32

        case ('CORES')
          call fclGetDeviceInfo(ctx%platform%devices(i),CL_DEVICE_MAX_COMPUTE_UNITS,sortMetric(i))

        case ('CLOCK')
          call fclGetDeviceInfo(ctx%platform%devices(i),CL_DEVICE_MAX_CLOCK_FREQUENCY,sortMetric(i))

        end select

      else
        sortMetric(i) = 0
      end if

      ! --- Filter by device name ---
      if (present(nameLike)) then
        if (index(upperstr(ctx%platform%devices(i)%name),upperstr(nameLike)) == 0) then
          filter(i) = .false.         ! Filtered out by device name
        end if
      end if

    end do

    ! --- Sort by sorting metric ---
    sortMetric = -sortMetric          ! Sort descending
    sortList = [(i,i=1,ctx%platform%numDevice)]
    call quick_sort(sortMetric,sortList)

    nFiltered = count(filter)
    if (nFiltered < 1) then
      call fclRuntimeError("fclFindDevices: no devices found matching criteria")
    end if

    ! --- Output filtered sorted list of devices ---
    allocate(deviceList(nFiltered))
    nFill = 1
    do i=1,ctx%platform%numDevice

      if (filter(i)) then
        j = sortList(i)
        deviceList(nFill) = ctx%platform%devices(j)
        nFill = nFill + 1
      end if

      if (nFill > nFiltered) then
        exit
      end if

    end do

  end procedure fclFindDevices_1
  ! ---------------------------------------------------------------------------


  module procedure fclFindDevices_2 !(type,nameLike,sortBy) result(deviceList)

    deviceList => fclFindDevices_1(fclDefaultCtx,type,nameLike,sortBy)

  end procedure fclFindDevices_2
  ! ---------------------------------------------------------------------------


  module procedure fclCreateCommandQ_1 !(ctx,device,enableProfiling,outOfOrderExec,&
                                         !blockingWrite,blockingRead) result(cmdq)
    !! Create a command queue with a Focal device object

    integer(c_int32_t) :: errcode
    integer(c_int64_t) :: properties

    properties = 0

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
    cmdq = fclCreateCommandQ_1(fclDefaultCtx,device,enableProfiling,outOfOrderExec, &
                                           blockingWrite,blockingRead)

  end procedure fclCreateCommandQ_2
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

    prog = fclCompileProgram_1(fclDefaultCtx,source,options)

  end procedure fclCompileProgram_2
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


  module procedure fclLaunchKernel !(kernel,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)

    !! @warning Updates to this implementation need to be mirrored
    !!  in the corresponding fclLaunchKernel_2 procedure @endwarning

    integer(c_int32_t) :: errcode
    type(fclCommandQ), pointer :: cmdQ
    type(c_ptr) :: localSizePtr
    integer :: nArg

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
    end if

    ! Set arguments and parse (get number of args and cmdq if specified) 
    call fclProcessKernelArgs(kernel,cmdq,narg,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)

    errcode = clEnqueueNDRangeKernel(cmdq%cl_command_queue, &
                kernel%cl_kernel, kernel%work_dim, &
                c_loc(kernel%global_work_offset), &
                c_loc(kernel%global_work_size), localSizePtr, &
                cmdq%nDependency, cmdq%dependencyListPtr, &
                c_loc(cmdQ%lastKernelEvent%cl_event))

    call fclPopDependencies(cmdq)
    call fclErrorHandler(errcode,'fclLaunchKernel','clEnqueueNDRangeKernel')

    fclLastKernelEvent = cmdQ%lastKernelEvent

  end procedure fclLaunchKernel
  ! ---------------------------------------------------------------------------


  module procedure fclProcessKernelArgs !(kernel,cmdq,narg,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
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

    if (nArg > 0) then
      ! If any kernel arguments are specified, check that they are all present
      call fclDbgCheckKernelNArg(kernel,nArg)
    end if

  end procedure fclProcessKernelArgs
  ! ---------------------------------------------------------------------------


  module procedure fclSetKernelArgs !(kernel,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
    !! Set all kernel arguments at once without launching kernel.

    type(fclCommandQ), pointer :: cmdq
    integer :: nArg

    call fclProcessKernelArgs(kernel,cmdq,narg,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)

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

    errcode = clEnqueueBarrierWithWaitList( cmdq%cl_command_queue, &
                  cmdq%nDependency, cmdq%dependencyListPtr , &
                  c_loc(cmdq%lastBarrierEvent%cl_event))

    call fclPopDependencies(cmdq)
    call fclErrorHandler(errcode,'fclBarrierAll','clEnqueueBarrierWithWaitList')

    fclLastBarrierEvent = cmdq%lastBarrierEvent

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


  module procedure fclSetDependencyEvent_1 !(cmdq,event,hold)
    !! Specify a single event dependency on specific cmdq
    if (.not.allocated(cmdq%dependencyList)) then

     allocate(cmdq%dependencyList(dependencyListAllocation))

    end if

    cmdq%dependencyList(1) = event%cl_event
    cmdq%nDependency = 1
    cmdq%dependencyListPtr = c_loc(cmdq%dependencyList)

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

    nEvent = size(eventList,1)
    nAlloc = max(dependencyListAllocation,nEvent)

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


end submodule Focal_Setup
