! Copyright 2014 Austin McCartney

! A singleton which provides an interface to a hash function for each of the intrinsic datatypes.
! Currently, only the string hash is implemented. As necessity arises, other hash functions will
! be implemented.

module hash_singleton
  implicit none
  private

  type :: definition
     private
   contains
     generic, public :: eval => char_hash
     procedure, nopass :: char_hash
     procedure, nopass, public :: order
  end type definition

  integer, parameter :: order_val = 31
  type(definition), public :: hash
contains

  ! Modified Jenkins one-at-a-time hash
  ! Fortran doesn't have an unsigned integer type and it is desirable to have 
  ! positive hash values.

  pure function char_hash(string) result(hash_value)
    implicit none
    character(len = *), intent(in) :: string

    integer :: hash_value , i

    hash_value = 0
    do i = 1, len(string)
       hash_value = hash_value + ichar(string(i:i))
       hash_value = hash_value + ishft(hash_value, 10)
       hash_value = hash_value .xor. ishft(hash_value, -6)
       hash_value = iand(hash_value, 1073741823) ! equivalent to modulo 2^30
                                                 ! prevents overflow
    enddo
    hash_value = hash_value + ishft(hash_value, 3)
    hash_value = hash_value .xor. ishft(hash_value, -11)
    hash_value = hash_value + ishft(hash_value, 15)
    hash_value = abs(hash_value)
  end function char_hash

  pure function order()
    implicit none
    integer :: order

    order = order_val
  end function order
end module hash_singleton

!!$program debug
!!$  use hash_singleton
!!$  implicit none
!!$
!!$  character(len = 1) :: a = 'a', b = 'b'
!!$  character(len = 2) :: ab = 'ab'
!!$ 
!!$  write(*,*) a, mod(hash%eval(a), 32) + 1, iand(hash%eval(a), 31) + 1 
!!$  write(*,*) b, mod(hash%eval(b), 32) + 1,  iand(hash%eval(b), 31) + 1 
!!$  write(*,*) ab, mod(hash%eval(ab), 32) + 1,  iand(hash%eval(ab), 31) + 1
!!$  write(*,*) 'namea', hash%eval('namea') + 1,  iand(hash%eval('namea'), 31) + 1 
!!$  write(*,*) 'nameb', hash%eval('nameb') + 1,  iand(hash%eval('nameb'), 31) + 1
!!$  write(*,*) 'namec', hash%eval('namec') + 1,  iand(hash%eval('namec'), 31) + 1
!!$  write(*,*) 'alysia', hash%eval('alysia') + 1,  iand(hash%eval('alysia'), 31) + 1 
!!$  write(*,*) 'Alysia', hash%eval('Alysia') + 1,  iand(hash%eval('Alysia'), 31) + 1
!!$  write(*,*) 'bal', hash%eval('bal') + 1,  iand(hash%eval('bal'), 31) + 1
!!$  write(*,*) 'Bal', hash%eval('Bal') + 1,  iand(hash%eval('Bal'), 31) + 1
!!$  write(*,*) 'Alysia Bal', hash%eval('Alysia Bal') + 1,  iand(hash%eval('Alysia Bal'), 31) + 1
!!$  write(*,*) 'vowel', hash%eval('vowel') + 1,  iand(hash%eval('vowel'), 31) + 1
!!$
!!$end program debug
