program platform_query
!! Focal example program: list available openCL platforms and devices
!

use Focal
use clfortran, only: CL_DEVICE_TYPE_CPU, CL_DEVICE_TYPE_GPU
implicit none

integer :: i, j
type(fclPlatform), pointer :: platforms(:)

platforms => fclGetPlatforms();

do i=1,size(platforms,1)

  write(*,*) ' --------- Platform ',i,'---------'
  write(*,*)
  write(*,*) '  Profile: ',platforms(i)%profile
  write(*,*) '  Version: ',platforms(i)%version
  write(*,*) '  Name: ',platforms(i)%name
  write(*,*) '  Vendor: ',platforms(i)%vendor
  write(*,*) '  No. of devices: ', platforms(i)%numDevice
  write(*,*)

  do j=1,platforms(i)%numDevice

    select case (platforms(i)%devices(j)%cl_device_type)
      case (CL_DEVICE_TYPE_CPU)
        write(*,'(I4,A)',advance='no') j,' : CPU : '
      case (CL_DEVICE_TYPE_GPU)
        write(*,'(I4,A)',advance='no') j,' : GPU : '
    end select

    write(*,*) platforms(i)%devices(j)%name
    write(*,'(A,I6,A,I6,A,I4,A,A)') '          ', &
               platforms(i)%devices(j)%nComputeUnits,' cores, ', &
               platforms(i)%devices(j)%global_memory/1024/1024,'MB, ', &
               platforms(i)%devices(j)%clock_freq, 'MHz, ',&
               platforms(i)%devices(j)%version

  end do

  write(*,*)

end do

end program platform_query
