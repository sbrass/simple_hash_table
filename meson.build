project('hash_table', ['c', 'fortran'],
  version : '0.1',
  default_options : ['warning_level=3'])

hash_table = library('hash_table', [
                      'hash.c',
                      'hash.f90',
                      ],
                     install : true)

hash_table_ut = executable('hash_ut', ['hash_ut.f90'], link_with: hash_table)
test('Hash Table Example', hash_table_ut)
