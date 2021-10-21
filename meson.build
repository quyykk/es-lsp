project('es-lsp', 'cpp',
  version : '0.1',
  default_options : ['cpp_rtti=false'])

compiler = meson.get_compiler('cpp')
libstdflag = []
if build_machine.system() == 'windows'
  add_project_arguments('/std:c++latest', language : 'cpp')
else
  add_project_arguments('-std=c++20', language : 'cpp')
  libstdflag = ['-D_GLIBCXX_DEBUG']
endif

if compiler.get_id() == 'clang'
  add_project_arguments('-stdlib=libc++', language : 'cpp')
  add_project_arguments('-Wno-ambiguous-reversed-operator', language : 'cpp')
  add_project_link_arguments('-stdlib=libc++', language : 'cpp')
endif

s = files('src/DataNode.cpp', 'src/fmt/format.cc', 'src/lsp.cpp', 'src/log.cpp', 'src/definitions.cpp', 'src/utils.cpp', 'src/simdjson/simdjson.cpp')
includes = include_directories('include')

executable('es-lsp', sources : s + files('src/main.cpp'),
  include_directories : includes)

t = executable('es-lsp-test', sources : s + files('tests/main.cpp', 'tests/tests.cpp'),
  cpp_args : libstdflag,
  include_directories : includes)
test('tests', t, args : ['--use-colour', 'yes'])
