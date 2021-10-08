project('es-lsp', 'cpp',
  default_options : ['cpp_std=c++20'])

compiler = meson.get_compiler('cpp')
if compiler.get_id() == 'clang'
  add_project_arguments('-stdlib=libc++', language : 'cpp')
  add_project_link_arguments('-stdlib=libc++', language : 'cpp')
endif

executable('es-lsp', 'src/main.cpp', 'src/DataNode.cpp', 'src/fmt/format.cc', 'src/lsp.cpp', 'src/log.cpp', 'src/definitions.cpp',
  include_directories : include_directories('include'),)
