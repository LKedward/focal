# -----nbody------ #

nbody_files = files('nbody.f90')
nbody_kernel_files = files('nbody.cl')

nbody_obj = custom_target('nbody_kernel',
  input: nbody_kernel_files,
  output: 'nbody.cl.o',
  command: [python_exe, script, '@INPUT@', '@OUTPUT@'])

# -platform query- #

platform_query_files = files('platform_query.f90')
platform_query_kernel_files = files('platform_query.cl')

platform_query_obj = custom_target('platform_query_kernel',
  input: platform_query_kernel_files,
  output: 'platform_query.cl.o',
  command: [python_exe, script, '@INPUT@', '@OUTPUT@'])

# ------sum------- #

sum_files = files('sum.f90')
sum_kernel_files = files('sum.cl')

sum_obj = custom_target('sum_kernel',
  input: sum_kernel_files,
  output: 'sum.cl.o',
  command: [python_exe, script, '@INPUT@', '@OUTPUT@'])
