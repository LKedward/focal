# Changes

Most recent release shown first.

## Release v1.0.1
*Backwards-compatible update.*
__Date:__ 29/05/2020

__Add: reference counting for fclEvent objects__
Uses overloaded assignment and finalisation routines
to call OpenCL reference counting routines for the
underlying event objects.
This is needed for when library users store their
own copies of event objects for dependency management
and fixes a bug where event objects were invalid.

__Add: deallocation routines for pinned host memory__
A global variable is used to keep track of device pointers
that correspond to host mapped memory and which are needed
for the unmap command during deallocation.
Also add clWaitForEvents to fclAllocHost and fclFreeHost
to make these commands host blocking.
Also removed old 2D array interfaces from header module.

__Update: tests__


## Release v1.0.0
*First stable release.*
__Date:__ 16/03/20
__Commit:__ a108d3de720a5745817cc93760cbe6617edc4232

### Incompatible changes
(Incompatible with previous unversioned beta code)

- Update buffer initialisation interface to be generic subroutine

### New features and enhancements
These changes do not affect existing interfaces from the pre-release version.

- Add _fclCommandQPool_ object for handling multiple command queues
- Automatically make sure global dims are multiples of local dims at kernel launch
- Get ifort build working 
- Add interfaces for creating sub-buffers
- Add `fclInit()` quick context setup function
- Allow device filtering based on supported features
- Incorporate reference counting of OpenCL events to avoid excessive memory growth
- Add support for OpenCL user events
- Populate all OpenCL 1.2 error codes
- Add copyright and license headers to library source files 
- Add continuous integration to run tests
- Add test code coverage check to CI


