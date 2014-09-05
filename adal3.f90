PROGRAM adal3

USE constants
USE mod_func
USE F90library

IMPLICIT NONE

INTEGER i,m,k,ierr
REAL(dp), ALLOCATABLE, DIMENSION(:,:) :: U
REAL(dp), ALLOCATABLE, DIMENSION(:)   :: V,dummy
INTEGER, ALLOCATABLE,DIMENSION(:)     :: perm
REAL(dp)                              :: determ
INTEGER, DIMENSION(3)                 :: l = [3,7,10]


DO k = 1, SIZE(l)
   m = 2**l(k)
   ALLOCATE(U(m,m),V(m),dummy(m),perm(m), STAT=ierr)
   CALL initialize(U,V,dummy,m)
   CALL lu_decompose(U,m,perm,determ)
   CALL lu_linear_equation(U,m,perm,V)
   DEALLOCATE(U,V,dummy,perm, STAT=ierr)
END DO

END PROGRAM adal3
