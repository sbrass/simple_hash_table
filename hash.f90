module hash_table
  !! author: Simon Braß
  !! date: 2021
  !! version: 0.1
  !! licence: [MIT License](COPYRIGHT.org)
  !! summary: A simple hashtable with string keys and arbitrary values using unlimited polymorphism.
  use iso_fortran_env, only: i32 => INT32, OUTPUT_UNIT

  implicit none

  private

  integer, parameter :: MIN_SIZE = 8, &
       MAX_SIZE = huge(1_i32)

  character(len=1), parameter :: DEFAULT_KEY = ""

  interface
     function c_hash_key(k) result(h) bind(C, name='keyhash')
       use iso_c_binding, only: c_char, c_size_t
       character(kind=c_char), dimension(*), intent(in) :: k
       integer(c_size_t) :: h
     end function c_hash_key
  end interface

  type :: hash_table_entry_t
     !! Hash table entry.
     private
     character(:), allocatable :: key
     class(*), pointer :: data
   contains
     procedure :: match => hash_table_entry_match
  end type hash_table_entry_t

  type :: hash_table_t
     !! Hash table.
     type(hash_table_entry_t), dimension(:), allocatable :: entries
     integer :: n_used_entries = 0, n_entries = 0
     integer :: mask = 0
   contains
     procedure, private :: lookup => hash_table_lookup
     procedure, private :: resize => hash_table_resize
     procedure :: write => hash_table_write
     procedure :: get_iterator => hash_table_get_iterator
     procedure :: has_key => hash_table_has_key
     procedure :: insert => hash_table_insert
     procedure :: search => hash_table_search
     procedure :: remove => hash_table_remove
  end type hash_table_t

  type :: hash_table_iterator_t
     type(hash_table_t), pointer :: table => null ()
     integer :: i_current = 0
   contains
     procedure :: has_entry => hash_table_iterator_has_entry
     procedure :: get_entry => hash_table_iterator_get_entry
  end type hash_table_iterator_t

  interface hash_table_t
     module procedure hash_table_init
  end interface hash_table_t

  public :: hash_table_t, hash_table_iterator_t

contains
  integer function hash_key(key) result (hash)
    use iso_c_binding, only: c_char, c_null_char
    character(len=*), intent(in) :: key
    character(len(key) + 1, c_char) :: c_key
    c_key = key // c_null_char
    hash = int(c_hash_key (c_key))
  end function hash_key

  logical function hash_table_entry_match (entry, key) result (flag)
    class(hash_table_entry_t), intent(in) :: entry
    character(len=*), intent(in) :: key
    flag = .false.
    if (allocated(entry%key)) then
       flag = entry%key == key
    end if
  end function hash_table_entry_match

  type(hash_table_t) function hash_table_init (n_entries) result (table)
    integer, intent(in) :: n_entries
    table%n_used_entries = 0
    call table%resize (n_entries)
  end function hash_table_init

  subroutine hash_table_resize (table, n_entries)
    class(hash_table_t), intent(inout) :: table
    integer, intent(in) :: n_entries
    type(hash_table_entry_t), dimension(:), allocatable :: entries
    integer :: i, new_size
    if (allocated (table%entries)) call move_alloc(table%entries, entries)
    table%n_entries = min (n_entries, MAX_SIZE)
    new_size = MIN_SIZE
    do while (new_size < n_entries)
       new_size = 2 * new_size
    end do
    table%mask = table%n_entries - 1
    allocate (table%entries(new_size))
    if (allocated (entries)) then
       do i = 1, size(entries)
          if (allocated (entries(i)%key)) then
             call table%insert(entries(i)%key, entries(i)%data)
          end if
       end do
    end if
  end subroutine hash_table_resize

  integer function hash_table_lookup (table, key) result (i_entry)
    class(hash_table_t), intent(in) :: table
    character(len=*), intent(in) :: key
    integer :: i, j
    i = hash_key(key)
    j = 1
    do
       i_entry = mod (i, table%mask) + 1
       if (.not. allocated (table%entries(i_entry)%key)) exit
       if (table%entries(i_entry)%match (key)) exit
       i = i + j
       j = j + 1
    end do
  end function hash_table_lookup

  subroutine hash_table_write (table, unit)
    class(hash_table_t), intent(in) :: table
    integer, intent(in), optional :: unit
    integer :: u, i
    u = OUTPUT_UNIT; if (present (unit)) u = unit
    write (u, "(A,1X,I0,1X,'/',1X,I0,1X,'(',I0,')')") &
         "N_ENTRIES", table%n_used_entries, table%n_entries, table%mask
    if (table%n_used_entries < 1) then
       write (u, "(A)") "[EMPTY]"
    else
       do i = 1, table%n_entries
          associate (table_entry => table%entries(i))
            if (allocated (table_entry%key)) then
               write (u, "(I0,1X,A)") i, table_entry%key
            end if
          end associate
       end do
    end if
  end subroutine hash_table_write

  logical function hash_table_has_key (table, key) result (flag)
    class(hash_table_t), intent(in) :: table
    character(len=*), intent(in) :: key
    integer :: i_entry
    i_entry = table%lookup (key)
    associate (table_entry => table%entries(i_entry))
      flag = table_entry%match (key)
    end associate
  end function hash_table_has_key

  subroutine hash_table_insert (table, key, data)
    class(hash_table_t), target, intent(inout) :: table
    character(len=*), intent(in) :: key
    class(*), pointer, intent(in) :: data
    integer :: i_entry
    i_entry = table%lookup (key)
    associate (table_entry => table%entries(i_entry))
      if (.not. table_entry%match (key)) table%n_used_entries = table%n_used_entries + 1
      table_entry%key = key
      table_entry%data => data
    end associate
    if (table%n_used_entries > 0.75 * table%n_entries) then
       call table%resize (2 * table%n_used_entries)
    end if
  end subroutine hash_table_insert

  subroutine hash_table_search (table, key, data)
    class(hash_table_t), intent(in) :: table
    character(len=*), intent(in) :: key
    class(*), pointer, intent(out) :: data
    integer :: i_entry
    i_entry = table%lookup (key)
    associate (table_entry => table%entries(i_entry))
      if (table_entry%match (key)) then
         data => table_entry%data
      else
         print *, "KEY NOT FOUND"
         data => null ()
      end if
    end associate
  end subroutine hash_table_search

  subroutine hash_table_remove (table, key, data)
    class(hash_table_t), intent(inout) :: table
    character(len=*), intent(in) :: key
    class(*), pointer, intent(out) :: data
    integer :: i_entry
    i_entry = table%lookup (key)
    associate (table_entry => table%entries(i_entry))
      if (table_entry%match (key)) then
         deallocate (table_entry%key)
         data => table_entry%data
         table_entry%data => null ()
         table%n_used_entries = table%n_used_entries - 1
      else
         data => null ()
      end if
    end associate
  end subroutine hash_table_remove

  type(hash_table_iterator_t) function hash_table_get_iterator (table) result (iterator)
    class(hash_table_t), target, intent(in) :: table
    iterator%table => table
    iterator%i_current = 1
  end function hash_table_get_iterator

  logical function hash_table_iterator_has_entry (iterator) result (flag)
    class(hash_table_iterator_t), intent(inout) :: iterator
    flag = iterator%i_current <= iterator%table%n_entries
  end function hash_table_iterator_has_entry

  subroutine hash_table_iterator_get_entry (iterator, key, data)
    class(hash_table_iterator_t), intent(inout) :: iterator
    character(:), allocatable, intent(out) :: key
    class(*), pointer :: data
    integer :: i_current
    do i_current = iterator%i_current + 1, iterator%table%n_entries
       if (allocated (iterator%table%entries(i_current)%key)) exit
    end do
    iterator%i_current = i_current
    if (iterator%has_entry()) then
       associate (table_entry => iterator%table%entries(iterator%i_current))
         key = table_entry%key(:)
         data => table_entry%data
       end associate
    else
       key = ""
       data => null ()
    end if
  end subroutine hash_table_iterator_get_entry
end module hash_table
