progs = [
  #['testExample',['testExample.cl', 'testExample.f90']],
  ['testEvents',['testEvents.cl', 'testEvents.f90']],
  ['testFileSource',['testFileSource.cl', 'testFileSource.f90']],
  ['testKernelSetup',['testKernelSetup.cl', 'testKernelSetup.f90']],
  ['testLocalMem',['testLocalMem.cl', 'testLocalMem.f90']],
  ['testMemoryTransfer',['testMemoryTransfer.cl', 'testMemoryTransfer.f90']],
  ['testPinnedMemory',['testPinnedMemory.cl', 'testPinnedMemory.f90']],
  ['testProfiling',['testProfiling.cl', 'testProfiling.f90']],
  ['testQueuePool',['testQueuePool.cl', 'testQueuePool.f90']],
  ['testSubBuffers',['testSubBuffers.cl', 'testSubBuffers.f90']]
  ]

moduleFocal_Test_Utils = files('Focal_Test_Utils.f90')

foreach p : progs
  openCL_obj = custom_target(p[0] + '_kernel',
    input: p[1][0],
    output: p[1][0] + '.o',
    command: [python_exe, script, '@INPUT@', '@OUTPUT@'])

  exe = executable(p[0],[openCL_obj, p[1][1], moduleFocal_Test_Utils],
    dependencies: libFocal_dep)

  test(p[0], exe)
endforeach
