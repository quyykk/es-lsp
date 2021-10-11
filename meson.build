project('es-lsp', 'cpp',
  version : '0.1',
  default_options : ['cpp_rtti=false', 'cpp_eh=none'])

compiler = meson.get_compiler('cpp')
if build_machine.system() == 'windows'
  add_project_arguments('/std:c++latest', language : 'cpp')
else
  add_project_arguments('-std=c++20', language : 'cpp')
endif

if compiler.get_id() == 'clang'
  add_project_arguments('-stdlib=libc++', language : 'cpp')
  add_project_link_arguments('-stdlib=libc++', language : 'cpp')
endif

executable('es-lsp', 'src/main.cpp', 'src/DataNode.cpp', 'src/fmt/format.cc', 'src/lsp.cpp', 'src/log.cpp', 'src/definitions.cpp', 'src/utils.cpp',
  include_directories : include_directories('include'))
