project(
  'Focal',
  'fortran',
  version: '1.0.1',
  license: 'MIT License',
  )

subdir('external')
subdir('src')

opencl_dep = dependency('OpenCL', required: false)
pkg = import('pkgconfig')

# ---NonDebug Library--- #
focalNonDebugFileList = sources + nonDebugFile + externalSources
libFocal = static_library('Focal', focalNonDebugFileList,
  fortran_args: ['-std=f2008', '-fimplicit-none'],
  install: true
)

pkg.generate(libFocal)

libFocal_dep = declare_dependency(link_with: libFocal,
  dependencies: opencl_dep,)

# ----Debug Library---- #
focalDebugFileList = sources + debugFile + externalSources
libFocalDbg = static_library('FocalDbg', focalDebugFileList,
  fortran_args: ['-std=f2008', '-fimplicit-none']
)

libFocalDbg_dep = declare_dependency(link_with: libFocalDbg,
  dependencies: opencl_dep)

# ------Examples------ #
python_exe = find_program('python')
script = join_paths(meson.source_root(),'getCLBinary.py')
subdir('examples')

executable('nbody',[nbody_files,nbody_obj],
  dependencies: libFocal_dep)
executable('sum',[sum_files,sum_obj],
  dependencies: libFocal_dep)
executable('platform_query',[platform_query_files,platform_query_obj],
  dependencies: libFocal_dep)

# -------Tests-------#
subdir('test')

