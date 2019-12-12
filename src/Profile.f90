submodule (Focal) Focal_Profile
  !! FOCAL: openCL abstraction layer for fortran
  !!  Implementation module for openCL profiling routines

  !! @note This is an implementation submodule: it contains the code implementing the subroutines defined in the
  !!  corresponding header module file. See header module file (Focal.f90) for interface definitions. @endnote

  use clfortran
  implicit none

  contains

  module procedure fclEnableProfiling !(container,profileSize,profileName)
    !! Enable profiling on a specific container by allocating space to save events

    ! character(10) :: tempStr

    container%profilingEnabled = .true.
    
    if (allocated(container%profileName)) then
      deallocate(container%profileName)
    end if

    if (present(profileName)) then
      ! allocate(character(len=len(profileName)) :: container%profileName)
      container%profileName = profileName
    else
      select type(c => container)

      class is (fclKernel)
        ! allocate(character(len=len(c%name)) :: container%profileName)
        container%profileName = c%name

      class is (fclDeviceBuffer)
        ! write(tempStr,'(I10)') c%nBytes
        container%profileName = 'Unnamed' ! ('//trim(tempStr)//'B)'

      end select
    end if

    if (allocated(container%profileEvents)) then
      deallocate(container%profileEvents)
    end if

    allocate(container%profileEvents(profileSize))
    container%nProfileEvent = 0
    container%profileSize = profileSize

    select type(b=>container)
    class is (fclDeviceBuffer)
      if (allocated(b%profileEventType)) then
        deallocate(b%profileEventType)
      end if
      allocate(b%profileEventType(profileSize))
    end select

  end procedure fclEnableProfiling
  ! ---------------------------------------------------------------------------
  

  module procedure fclPushProfileEvent !(container,event,type)
    !! If profiling is enabled for the container, save an event to it

    integer :: i

    if (.not.container%profilingEnabled) then
      return
    end if

    ! Increment number of events
    container%nProfileEvent = container%nProfileEvent + 1

    ! Wrap index around
    ! (Overwrite previous events if we exceed allocation size)
    i = mod(container%nProfileEvent-1,container%profileSize) + 1

    ! Save event
    container%profileEvents(i) = event

    select type(c=>container)
    class is (fclDeviceBuffer)
      if (present(type)) then
        c%profileEventType(i) = type
      end if
    end select

  end procedure fclPushProfileEvent
  ! ---------------------------------------------------------------------------
  

  module procedure fclDumpKernelProfileData !(kernelList,device,outputUnit)
    !! Dump summary of profile data for list of kernels
    use iso_fortran_env, only: stdout=>output_unit, sp=>real32

    integer :: k, i, N, outUnit
    integer(c_int32_t) :: errcode
    integer(c_int64_t), target :: startTime, endTime
    integer(c_size_t) :: size_ret
    integer(c_int64_t), allocatable :: durations(:)

    integer(c_int64_t) :: localMem, privateMem, preferredWorkGroup

    logical :: profileSizeWarning
    profileSizeWarning = .false.

    if (present(outputUnit)) then
      outUnit = outputUnit
    else
      outUnit = stdout
    end if

    ! Get allocation size for durations array
    N = 0
    do k=1,size(kernelList,1)
      N = max(N,kernelList(k)%profileSize)
    end do

    ! Allocate durations array
    allocate(durations(N))

    ! Write table header
    write(outUnit,*) ''
    write(outUnit,*) ('-',i=1,77)
    write(outUnit,'(A20,A1,A8,A1,A8,A1,A8,A1,A8,A1,A6,A1,A6,A1,A4)') &
           'Profile name','|','No. of','|','T_avg','|','T_max','|','T_min','|',&
           'Local','|','Private','|','PWGS'
    write(outUnit,'(A20,A1,A8,A1,A8,A1,A8,A1,A8,A1,A6,A1,A6,A1,A4)') &
            '(Kernel)','|','events','|', '(ns)','|','(ns)','|','(ns)','|',&
            'Mem.','|','Mem.','|',''
    write(outUnit,*) ('-',i=1,77)

    durations = 0
    localMem = 0
    privateMem = 0
    preferredWorkGroup = 0


    ! Iterate over list of kernels
    do k=1,size(kernelList,1)
      associate(kern=>kernelList(k))

        if (.not.kern%profilingEnabled) then
          cycle
        end if

        ! Get kernel info
        call fclGetKernelWorkGroupInfo(kern,device,CL_KERNEL_LOCAL_MEM_SIZE,localMem)
        call fclGetKernelWorkGroupInfo(kern,device,CL_KERNEL_PRIVATE_MEM_SIZE,privateMem)
        call fclGetKernelWorkGroupInfo(kern,device,CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE, &
                                    preferredWorkGroup)

        N = min(kern%profileSize,kern%nProfileEvent)

        ! Iterate over kernel profile events
        do i=1,N

          ! Get event start time
          errcode = clGetEventProfilingInfo(kern%profileEvents(i)%cl_event, &
            CL_PROFILING_COMMAND_START, c_sizeof(startTime), c_loc(startTime), size_ret)
          call fclErrorHandler(errcode,'fclDumpProfileData','clGetEventProfilingInfo')
          
          ! Get event end time
          errcode = clGetEventProfilingInfo(kern%profileEvents(i)%cl_event, &
            CL_PROFILING_COMMAND_END, c_sizeof(endTime), c_loc(endTime), size_ret)
          call fclErrorHandler(errcode,'fclDumpProfileData','clGetEventProfilingInfo')
          
          ! Save duration (nanoseconds)
          durations(i) = endTime - startTime
    
        end do

        ! Write to table
        if (kern%nProfileEvent > kern%profileSize) then
          write(outUnit,'(A20,A1,I8,A1,I8,A1,I8,A1,I8,A1,I6,A1,I6,A1,I4,A)') &
          kern%profileName,'|', kern%nProfileEvent,'|', sum(durations(1:N))/N, '|',&
          maxval(durations(1:N)),'|', minval(durations(1:N)),'|',&
          localMem,'|',privateMem,'|',preferredWorkGroup,' *'
          profileSizeWarning = .true.
        else
          write(outUnit,'(A20,A1,I8,A1,I8,A1,I8,A1,I8,A1,I6,A1,I6,A1,I4)') &
          kern%profileName,'|', kern%nProfileEvent,'|', sum(durations(1:N))/N, '|', &
          maxval(durations(1:N)),'|', minval(durations(1:N)),'|',&
          localMem,'|',privateMem,'|',preferredWorkGroup
        end if

      end associate
    end do

    ! Table footer
    write(outUnit,*) ('-',i=1,77)
    if (profileSizeWarning) then
      write(outUnit,*) ' * - Not all events profiled, increase profileSize'
    end if
    write(outUnit,*) ' ns: nanoseconds,  PWGS: Preferred work group size,  Mem: Memory in bytes.'
    write(outUnit,*) ('-',i=1,77)
    write(outUnit,*) ''

    ! Deallocate durations array
    deallocate(durations)

  end procedure fclDumpKernelProfileData
  ! ---------------------------------------------------------------------------

  
  module procedure fclDumpBufferProfileData !(bufferList,outputUnit)
    !! Dump summary of profile data for list of buffers
    use iso_fortran_env, only: stdout=>output_unit, sp=>real32

    integer :: k, i, N, m, outUnit
    integer(c_int32_t) :: errcode
    integer(c_int64_t), target :: startTime, endTime
    integer(c_size_t) :: size_ret
    integer(c_int64_t), allocatable :: durations(:)
    real(sp) :: S_avg,S_min, S_max

    logical :: profileSizeWarning
    profileSizeWarning = .false.

    if (present(outputUnit)) then
      outUnit = outputUnit
    else
      outUnit = stdout
    end if

    ! Get allocation size for durations array
    N = 0
    do k=1,size(bufferList,1)
      N = max(N,bufferList(k)%profileSize)
    end do

    ! Allocate durations array
    allocate(durations(N))

    ! Write table header
    write(outUnit,*) ''
    write(outUnit,*) ('-',i=1,77)
    write(outUnit,'(A20,A1,A8,A1,A8,A1,A8,A1,A8,A1,A8,A1,A8)') &
           'Profile name','|','Size','|','Mode','|','No. of','|','S_avg','|','S_max','|','S_min'
    write(outUnit,'(A20,A1,A8,A1,A8,A1,A8,A1,A8,A1,A8,A1,A8)') &
            '(Buffer)','|','(KBytes)','|', '','|','events','|','(GB/S)','|','(GB/S)','|','(GB/S)'
    write(outUnit,*) ('-',i=1,77)

    durations = 0

    ! Iterate over list of buffers
    do k=1,size(bufferList,1)
      associate(buff=>bufferList(k))

        if (.not.buff%profilingEnabled) then
          cycle
        end if

        N = min(buff%profileSize,buff%nProfileEvent)

        ! Iterate over buffer profile events
        do i=1,N

          ! Get event start time
          errcode = clGetEventProfilingInfo(buff%profileEvents(i)%cl_event, &
            CL_PROFILING_COMMAND_START, c_sizeof(startTime), c_loc(startTime), size_ret)
          call fclErrorHandler(errcode,'fclDumpProfileData','clGetEventProfilingInfo')
          
          ! Get event end time
          errcode = clGetEventProfilingInfo(buff%profileEvents(i)%cl_event, &
            CL_PROFILING_COMMAND_END, c_sizeof(endTime), c_loc(endTime), size_ret)
          call fclErrorHandler(errcode,'fclDumpProfileData','clGetEventProfilingInfo')
          
          ! Save duration (nanoseconds)
          durations(i) = endTime - startTime
    
        end do

        ! Write to table, iterate over write,read,copy
        do i=1,3

          m = count(buff%profileEventType(1:N)==i)
          if (m > 0) then

            S_avg = real(buff%nBytes,sp)/(real(sum(durations(1:N),mask=buff%profileEventType(1:N)==i),sp)/m)
            S_max = real(buff%nBytes,sp)/real(minval(durations(1:N),mask=buff%profileEventType(1:N)==i),sp)
            S_min = real(buff%nBytes,sp)/real(maxval(durations(1:N),mask=buff%profileEventType(1:N)==i),sp)

            if (buff%nProfileEvent > buff%profileSize) then
              write(outUnit,'(A20,A1,I8,A1,I8,A1,I8,A1,F8.4,A1,F8.4,A1,F8.4,A)') &
              buff%profileName,'|', buff%nBytes/1000,'|', i, '|',buff%nProfileEvent,'|',&
              S_avg,'|', S_max,'|',S_min,' *'
              profileSizeWarning = .true.
            else
              write(outUnit,'(A20,A1,I8,A1,I8,A1,I8,A1,F8.4,A1,F8.4,A1,F8.4)') &
              buff%profileName,'|', buff%nBytes/1000,'|', i, '|',buff%nProfileEvent,'|',&
              S_avg,'|', S_max,'|',S_min
            end if

          end if

        end do

      end associate
    end do

    ! Table footer
    write(outUnit,*) ('-',i=1,77)
    if (profileSizeWarning) then
      write(outUnit,*) ' * - Not all events profiled, increase profileSize'
    end if
    write(outUnit,*) ' 1: Write to device,  2: Read from device,  3: Copy on device.'
    write(outUnit,*) ('-',i=1,77)
    write(outUnit,*) ''

    ! Deallocate durations array
    deallocate(durations)

  end procedure fclDumpBufferProfileData
  ! ---------------------------------------------------------------------------


  ! module procedure fclDumpProfileData !(container,outputUnit)
  !   use iso_fortran_env, only: stdout=>output_unit, sp=>real32

  !   integer :: i, N, outUnit
  !   integer(c_int32_t) :: errcode
  !   integer(c_int64_t), target :: startTime, endTime
  !   integer(c_size_t) :: size_ret
  !   integer(c_int64_t) :: durations(container%profileSize)

  !   if (.not.container%profilingEnabled) then
  !     return
  !   end if

  !   if (present(outputUnit)) then
  !     outUnit = outputUnit
  !   else
  !     outUnit = stdout
  !   end if

  !   N = min(container%profileSize,container%nProfileEvent)

  !   do i=1,N

  !     errcode = clGetEventProfilingInfo(container%profileEvents(i)%cl_event, &
  !       CL_PROFILING_COMMAND_START, c_sizeof(startTime), c_loc(startTime), size_ret)

  !     call fclErrorHandler(errcode,'fclDumpProfileData','clGetEventProfilingInfo')

  !     errcode = clGetEventProfilingInfo(container%profileEvents(i)%cl_event, &
  !       CL_PROFILING_COMMAND_END, c_sizeof(endTime), c_loc(endTime), size_ret)

  !     call fclErrorHandler(errcode,'fclDumpProfileData','clGetEventProfilingInfo')

  !     durations(i) = endTime - startTime

  !   end do

   
  !   if (N > 0) then

  !     select type(c=>container)

  !     class is(fclKernel)

  !       write(outUnit,*) ' Focal Profiling results for kernel: ',c%profileName
  !       write(*,'(A,I8,A,I8,A,I8,A,I8,A)') '    ',c%nProfileEvent,' events: Duration (avg/min/max)', &
  !                   sum(durations(1:N))/N,'ns',minval(durations(1:N)),'ns',maxval(durations(1:N)),'ns'

  !     class is(fclDeviceBuffer)

  !       write(outUnit,'(A,A,A,I8,A)') ' Focal Profiling results for buffer: ',c%profileName,' ',c%nBytes/1000,' KBytes'
  !       write(*,'(A,I8,A,F8.4,A,F8.4,A,F8.4,A)') '    ',c%nProfileEvent,' events: Duration (avg/min/max)', &
  !                                 sum(real(c%nBytes,sp)/real(durations(1:N),sp))/N,'GB/S', &
  !                                 minval(real(c%nBytes,sp)/real(durations(1:N),sp)),'GB/S', &
  !                                 maxval(real(c%nBytes,sp)/real(durations(1:N),sp)),'GB/S'

  !     class default

  !       write(outUnit,*) ' Focal Profiling results for container: ',c%profileName
  !       write(*,'(A,I8,A,I8,A,I8,A,I8,A)') '    ',c%nProfileEvent,' events: Speed (avg/min/max)', &
  !                   sum(durations(1:N))/N,'ns',minval(durations(1:N)),'ns',maxval(durations(1:N)),'ns'
  !     end select

      
      
  !   end if

  !   if (container%nProfileEvent > container%profileSize) then
  !     write(*,'(A,I8,A)') '    (!) Only the last ',container%profileSize,' events were profiled. (Increase profileSize)'
  !   end if

  !   write(outUnit,*) ''

  ! end procedure fclDumpProfileData
  ! ! ---------------------------------------------------------------------------


end submodule Focal_Profile