module quadrature_mod
  use GL2_1D_mod
  implicit none
  private

  type :: quadrature_def
     private
   contains
     generic, public :: GaussLegendre2 => GL2_1Dq
!     generic, public :: cumulative_GaussLegendre2 => GL2_1D_cumulativeq
     procedure, nopass :: GL2_1Dq => GL2_1D
!     procedure, nopass :: GL2_1D_cumulativeq => GL2_1D_cumulative
  end type quadrature_def

  type(quadrature_def), public :: quadrature
end module quadrature_mod
