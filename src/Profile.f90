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
  

  module procedure fclDumpKernelProfileData_1 !(outputUnit,kernelList,device)
    !! Dump summary of profile data for list of kernels to specific output unit
    use iso_fortran_env, only: sp=>real32

    integer :: k, i, N
    integer(c_int32_t) :: errcode
    integer(c_int64_t), target :: startTime, endTime
    integer(c_size_t) :: size_ret
    integer(c_int64_t), allocatable :: durations(:)

    integer(c_int64_t) :: localMem, privateMem, preferredWorkGroup

    logical :: profileSizeWarning
    profileSizeWarning = .false.

    ! Get allocation size for durations array
    N = 0
    do k=1,size(kernelList,1)
      N = max(N,kernelList(k)%profileSize)
    end do

    ! Allocate durations array
    allocate(durations(N))

    ! Write table header
    write(outputUnit,*) ''
    write(outputUnit,*) ('-',i=1,77)
    write(outputUnit,'(A20,A1,A8,A1,A8,A1,A8,A1,A8,A1,A6,A1,A6,A1,A4)') &
           'Profile name','|','No. of','|','T_avg','|','T_max','|','T_min','|',&
           'Local','|','Private','|','PWGS'
    write(outputUnit,'(A20,A1,A8,A1,A8,A1,A8,A1,A8,A1,A6,A1,A6,A1,A4)') &
            '(Kernel)','|','events','|', '(ns)','|','(ns)','|','(ns)','|',&
            'Mem.','|','Mem.','|',''
    write(outputUnit,*) ('-',i=1,77)

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
        if (N>0) then
          if (kern%nProfileEvent > kern%profileSize) then
            write(outputUnit,'(A20,A1,I8,A1,I8,A1,I8,A1,I8,A1,I6,A1,I6,A1,I4,A)') &
            kern%profileName,'|', kern%nProfileEvent,'|', sum(durations(1:N))/N, '|',&
            maxval(durations(1:N)),'|', minval(durations(1:N)),'|',&
            localMem,'|',privateMem,'|',preferredWorkGroup,' *'
            profileSizeWarning = .true.
          else
            write(outputUnit,'(A20,A1,I8,A1,I8,A1,I8,A1,I8,A1,I6,A1,I6,A1,I4)') &
            kern%profileName,'|', kern%nProfileEvent,'|', sum(durations(1:N))/N, '|', &
            maxval(durations(1:N)),'|', minval(durations(1:N)),'|',&
            localMem,'|',privateMem,'|',preferredWorkGroup
          end if
        end if

      end associate
    end do

    ! Table footer
    write(outputUnit,*) ('-',i=1,77)
    if (profileSizeWarning) then
      write(outputUnit,*) ' * - Not all events profiled, increase profileSize'
    end if
    write(outputUnit,*) ' ns: nanoseconds,  PWGS: Preferred work group size,  Mem: Memory in bytes.'
    write(outputUnit,*) ('-',i=1,77)
    write(outputUnit,*) ''

    ! Deallocate durations array
    deallocate(durations)

  end procedure fclDumpKernelProfileData_1
  ! ---------------------------------------------------------------------------


  module procedure fclDumpKernelProfileData_2 !(kernelList,device)
    !! Dump summary of profile data for list of kernels to standard output
    use iso_fortran_env, only: stdout=>output_unit

    call fclDumpKernelProfileData_1(stdout,kernelList,device)

  end procedure fclDumpKernelProfileData_2
  ! ---------------------------------------------------------------------------


  module procedure fclDumpBufferProfileData_1 !(outputUnit,bufferList1,bufferList2,bufferList3)
    !! Dump summary of profile data for list of buffers to specific output unit
    use iso_fortran_env, only: sp=>real32

    integer :: k, i, N, m, bl
    integer(c_int32_t) :: errcode
    integer(c_int64_t), target :: startTime, endTime
    integer(c_size_t) :: size_ret
    integer(c_int64_t), allocatable :: durations(:)
    real(sp) :: S_avg,S_min, S_max

    class(fclDeviceBuffer), pointer :: bList(:)

    character(5), parameter :: modes(3) = ['WRITE','READ ','COPY ']

    logical :: profileSizeWarning
    profileSizeWarning = .false.

    ! Get allocation size for durations array
    N = 0
    do k=1,size(bufferList1,1)
      N = max(N,bufferList1(k)%profileSize)
    end do
    if (present(bufferList2)) then
      do k=1,size(bufferList2,1)
        N = max(N,bufferList2(k)%profileSize)
      end do
    end if
    if (present(bufferList3)) then
      do k=1,size(bufferList3,1)
        N = max(N,bufferList3(k)%profileSize)
      end do
    end if

    ! Allocate durations array
    allocate(durations(N))

    ! Write table header
    write(outputUnit,*) ''
    write(outputUnit,*) ('-',i=1,77)
    write(outputUnit,'(A20,A1,A8,A1,A8,A1,A8,A1,A8,A1,A8,A1,A8)') &
           'Profile name','|','No. of','|','Size','|','Transfer','|','S_avg','|','S_max','|','S_min'
    write(outputUnit,'(A20,A1,A8,A1,A8,A1,A8,A1,A8,A1,A8,A1,A8)') &
            '(Buffer)','|','events','|','(KBytes)','|','mode','|','(GB/S)','|','(GB/S)','|','(GB/S)'
    write(outputUnit,*) ('-',i=1,77)

    durations = 0

    ! Iterate over list of buffers
    do bl=1,3

      if (bl == 1) then
        bList(1:size(bufferList1,1)) => bufferList1(1:size(bufferList1,1))
      else if (bl == 2) then
        if (.not.present(bufferList2)) then
          exit
        else
          bList(1:size(bufferList2,1)) => bufferList2(1:size(bufferList2,1))
        end if
      elseif (bl == 3) then
        if (.not.present(bufferList3)) then
          exit
        else
          bList(1:size(bufferList3,1)) => bufferList3(1:size(bufferList3,1))
        end if
      end if

      do k=1,size(bList,1)
        associate(buff=>bList(k))

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
                write(outputUnit,'(A20,A1,I8,A1,I8,A1,A8,A1,F8.4,A1,F8.4,A1,F8.4,A)') &
                buff%profileName,'|',buff%nProfileEvent,'|', buff%nBytes/1000,'|', modes(i), '|',&
                S_avg,'|', S_max,'|',S_min,' *'
                profileSizeWarning = .true.
              else
                write(outputUnit,'(A20,A1,I8,A1,I8,A1,A8,A1,F8.4,A1,F8.4,A1,F8.4)') &
                buff%profileName,'|',buff%nProfileEvent,'|', buff%nBytes/1000,'|', modes(i), '|',&
                S_avg,'|', S_max,'|',S_min
              end if

            end if

          end do

        end associate
      end do
    end do

    ! Table footer
    write(outputUnit,*) ('-',i=1,77)
    if (profileSizeWarning) then
      write(outputUnit,*) ' * - Not all events profiled, increase profileSize'
      write(outputUnit,*) ('-',i=1,77)
    end if
    
    write(outputUnit,*) ''

    ! Deallocate durations array
    deallocate(durations)

  end procedure fclDumpBufferProfileData_1
  ! ---------------------------------------------------------------------------


  module procedure fclDumpBufferProfileData_2 !(bufferList1,bufferList2,bufferList3)
    !! Dump summary of profile data for list of buffers to standard output
    use iso_fortran_env, only: stdout=>output_unit

    call fclDumpBufferProfileData_1(stdout,bufferList1,bufferList2,bufferList3)

  end procedure fclDumpBufferProfileData_2
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