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

submodule (Focal) Focal_Profile
  !!  Implementation module for openCL profiling routines

  !! @note This is an implementation submodule: it contains the code implementing the subroutines defined in the
  !!  corresponding header module file. See header module file (Focal.f90) for interface definitions. @endnote

  use clfortran
  implicit none

  contains

  module procedure fclProfilerAdd_1 !(profiler,profileSize,c0,c1,c2,c3,c4,c5,c6,c7,c8,c9)
    !! Enable profiling for multiple containers (kernel/buffer) and add to profiler collection

    call fclEnableProfiling(c0,profileSize,profiler)

    if (present(c1)) then
      call fclEnableProfiling(c1,profileSize,profiler)
    end if
    if (present(c2)) then
      call fclEnableProfiling(c2,profileSize,profiler)
    end if
    if (present(c3)) then
      call fclEnableProfiling(c3,profileSize,profiler)
    end if
    if (present(c4)) then
      call fclEnableProfiling(c4,profileSize,profiler)
    end if
    if (present(c5)) then
      call fclEnableProfiling(c5,profileSize,profiler)
    end if
    if (present(c6)) then
      call fclEnableProfiling(c6,profileSize,profiler)
    end if
    if (present(c7)) then
      call fclEnableProfiling(c7,profileSize,profiler)
    end if
    if (present(c8)) then
      call fclEnableProfiling(c8,profileSize,profiler)
    end if
    if (present(c9)) then
      call fclEnableProfiling(c9,profileSize,profiler)
    end if

  end procedure fclProfilerAdd_1
  ! ---------------------------------------------------------------------------


  module procedure fclProfilerAdd_2 !(profileSize,c0,c1,c2,c3,c4,c5,c6,c7,c8,c9)
    !! Enable profiling for multiple containers (kernel/buffer) and add to the default profiler

    call fclProfilerAdd_1(fclDefaultProfiler,profileSize,c0,c1,c2,c3,c4,c5,c6,c7,c8,c9)

  end procedure fclProfilerAdd_2
  ! ---------------------------------------------------------------------------


  module procedure fclEnableProfiling !(container,profileSize,profiler)
    !! Enable profiling on a specific container by allocating space to save events

    type(fclKernelPointer), allocatable :: kernelsTemp(:)
    type(fclBufferPointer), allocatable :: buffersTemp(:)

    container%profilingEnabled = .true.

    ! ------ Allocate space for event objects ------
    if (associated(container%profileEvents)) then
      deallocate(container%profileEvents)
    end if
    allocate(container%profileEvents(profileSize))

    if (associated(container%nProfileEvent)) then
      deallocate(container%nProfileEvent)
    end if
    allocate(container%nProfileEvent)

    if (associated(container%profileEventType)) then
      deallocate(container%profileEventType)
    end if
    allocate(container%profileEventType(profileSize))

    container%nProfileEvent = 0
    container%profileSize = profileSize

    ! ------ Set profile name, if not specified ------
    if(.not.allocated(container%profileName)) then

      select type(c => container)

      class is (fclKernel)
        ! allocate(character(len=len(c%name)) :: container%profileName)
        container%profileName = c%name

      class is (fclDeviceBuffer)
        ! write(tempStr,'(I10)') c%nBytes
        container%profileName = 'Unnamed' ! ('//trim(tempStr)//'B)'

      end select

    end if

    ! ------ Add to profiler collection object, if specified ------
    if (present(profiler)) then
      select type(c=>container)

        type is(fclKernel)
          ! ------ Kernels ------
          profiler%nKernels = profiler%nKernels + 1

          if (.not.allocated(profiler%kernels)) then
            ! --- Allocate for first time ---
            allocate(profiler%kernels(fclAllocationSize))
          else
            if (profiler%nKernels > size(profiler%kernels,1)) then
              ! --- Need to reallocate ---
              kernelsTemp = profiler%kernels
              deallocate(profiler%kernels)
              allocate(profiler%kernels(profiler%nKernels + fclAllocationSize))
              profiler%kernels(1:size(kernelsTemp,1)) = kernelsTemp
              deallocate(kernelsTemp)
            end if
          end if

          profiler%kernels(profiler%nKernels)%target => c

        class is(fclDeviceBuffer)
          ! ------ Buffers ------
          profiler%nBuffers = profiler%nBuffers + 1

          if (.not.allocated(profiler%buffers)) then
            ! --- Allocate for first time ---
            allocate(profiler%buffers(fclAllocationSize))
          else
            if (profiler%nBuffers > size(profiler%buffers,1)) then
              ! --- Need to reallocate ---
              buffersTemp = profiler%buffers
              deallocate(profiler%buffers)
              allocate(profiler%buffers(profiler%nBuffers + fclAllocationSize))
              profiler%buffers(1:size(buffersTemp,1)) = buffersTemp
              deallocate(buffersTemp)
            end if
          end if

          profiler%buffers(profiler%nBuffers)%target => c

        class default

      end select
    end if

  end procedure fclEnableProfiling
  ! ---------------------------------------------------------------------------


  module procedure fclPushProfileEvent !(container,event,type)
    !! If profiling is enabled for the container, save an event to it

    integer(c_int32_t) :: errcode

    if (.not.container%profilingEnabled) then
      return
    end if

    ! Increment number of events
    container%nProfileEvent = container%nProfileEvent + 1

    ! Don't save if exceeded profile size
    !  (Only first profileSize events are saved)
    if (container%nProfileEvent > container%profileSize) then
      return
    end if

    ! Save event
    container%profileEvents(container%nProfileEvent) = event

    ! Save event type if specified
    if (present(type)) then
      container%profileEventType(container%nProfileEvent) = type
    end if

  end procedure fclPushProfileEvent
  ! ---------------------------------------------------------------------------


  module procedure fclGetEventDurations !(eventList) result(durations)

    integer :: i, N
    integer(c_int32_t) :: errcode
    integer(c_int64_t), target :: startTime, endTime
    integer(c_size_t) :: size_ret

    N = size(eventList,1)

    ! Iterate over kernel profile events
    do i=1,N

      ! Get event start time
      errcode = clGetEventProfilingInfo(eventList(i)%cl_event, &
        CL_PROFILING_COMMAND_START, c_sizeof(startTime), c_loc(startTime), size_ret)
      call fclHandleError(errcode,'fclGetProfileEventDurations','clGetEventProfilingInfo')

      ! Get event end time
      errcode = clGetEventProfilingInfo(eventList(i)%cl_event, &
        CL_PROFILING_COMMAND_END, c_sizeof(endTime), c_loc(endTime), size_ret)
      call fclHandleError(errcode,'fclGetProfileEventDurations','clGetEventProfilingInfo')

      ! Save duration (nanoseconds)
      durations(i) = endTime - startTime

    end do

  end procedure fclGetEventDurations
  ! ---------------------------------------------------------------------------


  module procedure fclDumpProfileData_1 !(profiler,outputUnit)
    !! Dump summary of profiler data for list of kernels to specific output unit
    use iso_fortran_env, only: stdout=>output_unit

    integer :: i, unit

    type(fclKernel), allocatable :: kernels(:)
    type(fclDeviceBuffer), allocatable :: buffers(:)

    call fclDbgCheckDevice(profiler%device,'fclDumpProfileData. '// &
        '(Has the profiler%device been set?)')

    if (.not.present(outputUnit)) then
      unit = stdout
    else
      unit = outputUnit
    end if

    if (profiler%nKernels > 0) then

      allocate(kernels(profiler%nKernels))
      do i=1,profiler%nKernels
        kernels(i) = profiler%kernels(i)%target
      end do

      call fclDumpKernelProfileData(unit,kernels,profiler%device)

      deallocate(kernels)

    end if

    if (profiler%nBuffers > 0) then

      allocate(buffers(profiler%nBuffers))
      do i=1,profiler%nBuffers
        buffers(i) = profiler%buffers(i)%target
      end do

      call fclDumpBufferProfileData(unit,buffers)

      deallocate(buffers)

    end if

  end procedure fclDumpProfileData_1
  ! ---------------------------------------------------------------------------


  module procedure fclDumpProfileData_2 !(outputUnit)
    !! Dump summary of default profiler data for list of kernels to specific output unit

    call fclDumpProfileData_1(fclDefaultProfiler, outputUnit)

  end procedure fclDumpProfileData_2
  ! ---------------------------------------------------------------------------


  module procedure fclDumpKernelProfileData !(outputUnit,kernelList,device)
    !! Dump summary of profile data for list of kernels to specific output unit
    use iso_fortran_env, only: sp=>real32

    integer :: k, i, N
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
        durations(1:N) = fclGetEventDurations(kern%profileEvents(1:N))

        ! Minimum of 100 nanosecond resolution
        durations = max(durations,int(100,c_int64_t))

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

  end procedure fclDumpKernelProfileData
  ! ---------------------------------------------------------------------------


  module procedure fclDumpBufferProfileData !(outputUnit,bufferList1,bufferList2,bufferList3)
    !! Dump summary of profile data for list of buffers to specific output unit
    use iso_fortran_env, only: sp=>real32

    integer :: k, i, N, m, bl
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
          durations(1:N) = fclGetEventDurations(buff%profileEvents(1:N))

          ! Minimum of 100 nanosecond resolution
          durations = max(durations,int(100,c_int64_t))

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

  end procedure fclDumpBufferProfileData
  ! ---------------------------------------------------------------------------


  module procedure fclDumpTracingData_1 !(profiler,filename)

    integer :: fh, kb, j, i, N, tid
    integer(c_intptr_t), target :: qid
    integer(c_int32_t) :: errcode
    integer(c_int64_t), target :: startTime, endTime
    integer(c_size_t) :: size_ret
    logical :: isFirstEvent

    type(fclKernel), allocatable, target :: kernels(:)
    type(fclDeviceBuffer), allocatable, target :: buffers(:)
    class(fclProfileContainer), pointer :: containers(:)

    isFirstEvent = .true.

    open(newunit=fh,file=filename,status='unknown')

    write(fh,*) '['

    do kb=1,2

      if (kb == 1) then

        if (profiler%nKernels > 0) then

          allocate(kernels(profiler%nKernels))
          do i=1,profiler%nKernels
            kernels(i) = profiler%kernels(i)%target
          end do

          ! deallocate(kernels)
          containers => kernels
          tid = 1

        else
          cycle
        end if

      else if(kb == 2) then

        if (profiler%nBuffers > 0) then

          allocate(buffers(profiler%nBuffers))
          do i=1,profiler%nBuffers
            buffers(i) = profiler%buffers(i)%target
          end do

          ! deallocate(buffers)
          containers => buffers
          tid = 2

        else
          cycle
        end if

      end if

      do j=1,size(containers,1)

        associate(profileContainer => containers(j))

          N = min(profileContainer%profileSize,profileContainer%nProfileEvent)

          ! Iterate over kernel profile events
          do i=1,N

            ! Get event start time
            errcode = clGetEventProfilingInfo(profileContainer%profileEvents(i)%cl_event, &
              CL_PROFILING_COMMAND_START, c_sizeof(startTime), c_loc(startTime), size_ret)
            call fclHandleError(errcode,'fclGetProfileEventDurations','clGetEventProfilingInfo')

            ! Get event end time
            errcode = clGetEventProfilingInfo(profileContainer%profileEvents(i)%cl_event, &
              CL_PROFILING_COMMAND_END, c_sizeof(endTime), c_loc(endTime), size_ret)
            call fclHandleError(errcode,'fclGetProfileEventDurations','clGetEventProfilingInfo')

            ! Get event command queue
            errcode = clGetEventInfo(profileContainer%profileEvents(i)%cl_event, &
              CL_EVENT_COMMAND_QUEUE, c_sizeof(qid), c_loc(qid), size_ret)
            call fclHandleError(errcode,'fclGetProfileEventDurations','clGetEventInfo')

            if (.not.isFirstEvent) then
              write(fh,*) ','
            else
              isFirstEvent = .false.
            end if

            write(fh,*) '{'
            write(fh,*) '"cat": "Focal",'
            write(fh,*) '"pid": 1, "tid": ',qid,','
            write(fh,*) '"ts": ',startTime/1000,','
            write(fh,*) '"ph": "B",'
            write(fh,*) '"name": "',profileContainer%profileName,'"'
            write(fh,*) '},'

            write(fh,*) '{'
            write(fh,*) '"cat": "Focal",'
            write(fh,*) '"pid": 1, "tid": ',qid,','
            write(fh,*) '"ts": ',endTime/1000,','
            write(fh,*) '"ph": "E",'
            write(fh,*) '"name": "',profileContainer%profileName,'"'
            write(fh,*) '}'


          end do ! loop over container events

        end associate

      end do ! loop over containers

      if (allocated(kernels)) then
        deallocate(kernels)
      end if

      if (allocated(buffers)) then
        deallocate(buffers)
      end if

    end do ! loop between kernels and buffers

    write(fh,*) ',{"name": "process_name", "ph": "M", "pid": 1, "tid": 1,'
    write(fh,*) '"args": {"name" : "OpenCL device: ',profiler%device%name,'"}'
    write(fh,*)  '}'
    write(fh,*) ',{"name": "thread_name", "ph": "M", "pid": 1, "tid": 1,'
    write(fh,*) '"args": {"name" : "Kernels"}'
    write(fh,*)  '}'
    write(fh,*) ',{"name": "thread_name", "ph": "M", "pid": 1, "tid": 2,'
    write(fh,*) '"args": {"name" : "Buffers"}'
    write(fh,*)  '}'


    write(fh,*) ']'
    close(fh)

  end procedure fclDumpTracingData_1
  ! ---------------------------------------------------------------------------

  module procedure fclDumpTracingData_2 !(filename)
    !! Writes a chrome://tracing data format for the default profiler

    call fclDumpTracingData_1(fclDefaultProfiler,filename)

  end procedure fclDumpTracingData_2
  ! ---------------------------------------------------------------------------


end submodule Focal_Profile
