PROGRAM adal2

USE mod_init
USE mod_func
USE mod_method

IMPLICIT NONE

INTEGER i,m,k,ierr
REAL(dp), ALLOCATABLE, DIMENSION(:,:) :: U
REAL(dp), ALLOCATABLE, DIMENSION(:)   :: V,W
INTEGER, DIMENSION(3)                 :: l = [3,7,10]


DO k = 1, SIZE(l)
   m = 2**l(k)
   ALLOCATE(U(m,m),V(m),W(m), STAT=ierr)
   CALL initialize(U,V,W,m)
   CALL method(U,V,W,m)
   DEALLOCATE(U,V,W, STAT=ierr)
END DO

END PROGRAM adal2
