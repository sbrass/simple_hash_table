program main
  use hash_table
  implicit none

  type(hash_table_t) :: table
  type(hash_table_iterator_t) :: iterator
  character(len=:), allocatable :: key
  class(*), pointer :: data

  integer, target :: test = 12345

  key = "ABC"

  table = hash_table_t(n_entries = 10)

  data => test
  call table%insert (key, data)
  key = "ABCD"
  call table%insert (key, data)
  key = "BCD"
  call table%insert (key, data)
  key = "12"
  call table%insert (key, data)
  key = "1"
  call table%insert (key, data)
  key = "1"
  call table%insert (key, data)

  call table%write ()

  data => null ()
  call table%search ("12", data)
  if (associated (data)) then
     select type (data)
     type is (integer)
        print *, data
     end select
  end if

  data => null ()
  call table%remove ("1", data)
  print *, associated (data)
  call table%remove ("2", data)
  print *, associated (data)


  call table%write ()
  iterator = table%get_iterator()
  do while (iterator%has_entry())
     call iterator%get_entry (key, data)
     print *, key
  end do
end program main
