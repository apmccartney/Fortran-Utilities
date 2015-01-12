! Copyright 2013 Austin McCartney

! A superficial implementation of a dictionary

! This is quicky implementation of a python style dictionary, complete with the python
!    collision resolution strategy. The essential methods are provided but the sugar methods
!    have yet to be implemented. As a result of time constraints, the size of the list
!    is fixed in this implementation. In future implementations, this limitation will be 
!    removed and dynamic resizing will be implemented.

! For information on python dictionaries, see:
! http://blip.tv/pycon-us-videos-2009-2010-2011/pycon-2010-the-mighty-dictionary-55-3352147

module dictionaries_char256_int
  use hash_singleton
  implicit none
  private

  type, public :: dictionary_char256_int
     private
     ! fields shared by array implementations of dictionary
     integer, dimension(:), pointer :: hash_values => null()
     integer :: no_entries = 0
     integer :: default_val
     integer :: order
     logical :: base_2
     ! fields specific to key and value
     character(len = 256), dimension(:), pointer :: key_var => null()
     integer, dimension(:), pointer :: val_var => null()
   contains
     ! mutation methods
     procedure :: init ! Private because I don't trust other people
     procedure, public :: associate
     procedure, public :: delete
     procedure, public :: set_default
     ! inquiry methods
     procedure, public :: has_key
     procedure, public :: value
     procedure, public :: keys
     procedure, public :: values
     procedure, public :: length
     procedure :: index
  end type dictionary_char256_int

contains

  subroutine init(self, size)
    implicit none
    class(dictionary_char256_int), intent(inout) :: self
    integer, intent(in), optional :: size

    integer :: size_def

    if (present(size)) then
       size_def = size
       if ((iand(size_def, size_def - 1) == 0) .and. (size_def .ne. 1)) then
          self%base_2 = .true.
          self%order = nint(log(real(size_def))/log(2.0))
       else
          self%base_2 = .false.
       endif
    else
       size_def = 128
       self%order = nint(log(real(size_def))/log(2.0))
       self%base_2 = .true.
    endif
    if (associated(self%hash_values)) then
       deallocate(self%hash_values); self%hash_values => null()
       deallocate(self%key_var); self%key_var => null()
       deallocate(self%val_var); self%val_var => null()
    endif
    allocate(self%hash_values(size_def))
    allocate(character(len = 256) :: self%key_var(size_def))
    allocate(self%val_var(size_def))

    self%hash_values = -1
    self%default_val = huge(size) - 1
    self%default_val = self%default_val * (-1) 
  end subroutine init

  subroutine associate(self, key, val)
    implicit none
    class(dictionary_char256_int), intent(inout) :: self
    character(len = *), intent(in) :: key
    integer, intent(in) :: val

    integer :: index

    if (.not. associated(self%hash_values)) call self%init()

    index = self%index(key, report_missing = .false.)
    self%hash_values(index) = hash%eval(key)
    self%no_entries         = self%no_entries + 1
    self%key_var(index)        = key
    self%val_var(index)      = val
  end subroutine associate

  subroutine delete(self, key)
    implicit none
    class(dictionary_char256_int), intent(inout) :: self
    character(len = *), intent(in) :: key
    integer :: index_val

    index_val = self%index(key, report_missing = .true.)
    if (index_val > 0) then
       self%hash_values(index_val) = -2
       self%no_entries = self%no_entries - 1
    endif
  end subroutine delete

  subroutine set_default(self, default_value)
    implicit none
    class(dictionary_char256_int), intent(inout) :: self
    integer, intent(in) :: default_value

    self%default_val = default_value
  end subroutine set_default

  pure elemental function has_key(self, key)
    implicit none
    logical :: has_key
    class(dictionary_char256_int), intent(in) :: self
    character(len = *), intent(in) :: key


    if (self%index(key, report_missing = .true.) > 0) then 
       has_key = .true.
    else
       has_key = .false.
    endif
  end function has_key

  pure elemental function value(self, key) result(val)
    implicit none
    integer :: val
    class(dictionary_char256_int), intent(in) :: self
    character(len = *), intent(in) :: key

    integer :: index

    index = self%index(key, report_missing = .true.)
    if (index > 0) then
       val = self%val_var(index)
    else
       val = self%default_val
    endif
  end function value
  
  pure function keys(self) result(key_array)
    implicit none
    class(dictionary_char256_int), intent(in) :: self
    character(256), dimension(self%no_entries) :: key_array

    key_array = pack(self%key_var, self%hash_values >= 0)
  end function keys

  pure function values(self) result(value_array)
    implicit none
    class(dictionary_char256_int), intent(in) :: self
    integer, dimension(self%no_entries) :: value_array

    value_array = pack(self%val_var, self%hash_values >= 0)
  end function values

  pure elemental function length(self) result(len_val)
    implicit none
    class(dictionary_char256_int), intent(in) :: self
    integer :: len_val

!    if (.not. associated(self%hash_values)) then 
!       len_val = 0
!    else
       len_val = self%no_entries
!    endif
  end function length


  pure elemental function index(self, key, report_missing) result(index_val)
!  function index(self, key, report_missing) result(index_val)
    implicit none
    class(dictionary_char256_int), intent(in) :: self
    character(len = *), intent(in) :: key
    logical, intent(in) :: report_missing

    integer :: original_hash_val, hash_val, index_val, search_count

    search_count = 0
    original_hash_val = hash%eval(key)
    hash_val = original_hash_val

    if (report_missing) then
       if (self%base_2) then
          do
             index_val = iand(hash_val, size(self%hash_values, 1) - 1) + 1
             ! if hash values match
             if (self%hash_values(index_val) == original_hash_val) then
                ! if key_var match
                if (self%key_var(index_val) == key) then
                   !position is found
                   exit
                ! if true hash collision
                else
                   search_count = search_count + 1
                   ! if we've searched either the total number of entries in the dictionary
                   if (search_count == self%no_entries) then
                      index_val = -1
                      exit
                   endif
                   hash_val = ishft(hash_val, -1*self%order) 
                endif
          ! if unoccupied and not previously occupied
             elseif (self%hash_values(index_val) == -1) then
                index_val = -1
                exit
                ! if hash values do not match
             else
                search_count = search_count + 1
                ! if we've searched either the total number of entries in the dictionary or run out of higher bits
                if ((search_count == self%no_entries) .or. (search_count >= ((hash%order() + self%order - 1) / self%order))) then
                   index_val = -1
                   exit
                endif
                hash_val = ishft(hash_val, -1*self%order)
             endif
          enddo
       else
          do
             index_val = mod(hash_val, size(self%hash_values, 1)) + 1
             ! if hash values match
             if (self%hash_values(index_val) == original_hash_val) then
                ! if key_var match
                if (self%key_var(index_val) == key) then
                   !position is found
                   exit
                ! if true hash collision
                else
                   search_count = search_count + 1
                   ! if we've searched either the total number of entries in the dictionary
                   if (search_count == self%no_entries) then
                      index_val = -1
                      exit
                   endif
                   hash_val = hash_val / size(self%hash_values, 1)
                endif
             ! if unoccupied and not previously occupied
             elseif (self%hash_values(index_val) == -1) then
                index_val = -1
                exit
             ! if hash values do not match
             else
                search_count = search_count + 1
                ! if we've searched either the total number of entries in the dictionary or run out of higher bits
                if ((search_count == self%no_entries)) then
                   index_val = -1
                   exit
                endif
                hash_val = hash_val / size(self%hash_values, 1)
             endif
          enddo
          index_val = abs(index_val)
       endif
    else
       if (self%base_2) then
          do
             index_val = iand(hash_val, size(self%hash_values, 1) - 1) + 1
             ! if hash values match
             if (self%hash_values(index_val) == original_hash_val) then
                ! if key_var match
                if (self%key_var(index_val) == key) then
                   !position is found
                   exit
                   ! if true hash collision
                else
                   hash_val = ishft(hash_val, -1*self%order)
                endif
             ! if unoccupied
             elseif (self%hash_values(index_val) < 0) then
                exit
             ! if hash values do not match and occupied
             else
                hash_val = ishft(hash_val, -1*self%order)
             endif
          enddo
       else
          do
             index_val = mod(hash_val, size(self%hash_values, 1)) + 1
             ! if hash values match
             if (self%hash_values(index_val) == original_hash_val) then
                ! if key_var match
                if (self%key_var(index_val) == key) then
                   !position is found
                   exit
                   ! if true hash collision
                else
                   hash_val = hash_val / size(self%hash_values, 1)
                endif
                ! if unoccupied
             elseif (self%hash_values(index_val) < 0) then
                exit
                ! if hash values do not match
             else
                hash_val = hash_val / size(self%hash_values, 1) + 1
             endif
          enddo
           index_val = abs(index_val)
       endif
    endif       
  end function index
  
end module dictionaries_char256_int

!!$program debug
!!$  use dictionaries_char256_int
!!$  implicit none
!!$
!!$  type(dictionary_char256_int) :: test
!!$  character(len = 256), dimension(:), allocatable :: test_keys
!!$  integer, dimension(:), allocatable :: test_vals
!!$  integer :: i
!!$
!!$  write(*,*)
!!$  write(*,*) " Number of entries: ", test%length()
!!$
!!$  call test%associate('Alysia', 1)
!!$  call test%associate('Austin', 1)
!!$  call test%associate('Jeff', 2)
!!$  call test%associate('Bianca', -1)
!!$  call test%associate('Ron', -1)
!!$
!!$  write(*,*)
!!$  write(*,*) " Alysia : ", test%value('Alysia')
!!$  write(*,*) " Austin : ", test%value('Austin')
!!$  write(*,*) " Jeff : ", test%value('Jeff')
!!$  write(*,*) " Bianca : ", test%value('Bianca')
!!$  write(*,*) " Ron : ", test%value('Ron')
!!$  write(*,*) " Yojimbo : ", test%value('Yojimbo')
!!$  write(*,*)
!!$  write(*,*) " Number of entries: ", test%length()
!!$  write(*,*)
!!$  allocate(character(len = 256) :: test_keys(test%length()))
!!$  allocate(test_vals(test%length()))
!!$  test_keys = test%keys()
!!$  test_vals = test%values()
!!$  do i = 1, test%length()
!!$     write(*,*) trim(test_keys(i)), test_vals(i)
!!$  enddo
!!$end program debug
