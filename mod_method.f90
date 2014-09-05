MODULE mod_method

USE mod_init

IMPLICIT NONE

CONTAINS
  SUBROUTINE method(A,B,X,m)
  INTEGER                  :: m, i 
  REAL(dp), DIMENSION(m,m) :: A
  REAL(dp), DIMENSION(m)   :: B,X
  REAL(dp)                 :: coefficient
  DO i = 1, m-1
     coefficient = A(i+1,i)/A(i,i)
     A(i+1,i+1)    = A(i+1,i+1) - coefficient*A(i,i+1)
     B(i+1)      = B(i+1)   - coefficient*B(i)
  END DO

  X(m) = B(m)/A(m,m)

  DO i = m-1, 1, -1
     X(i) = ( B(i) - A(i,i+1)*X(i+1) ) / A(i,i)
  END DO
  END SUBROUTINE method
END MODULE mod_method
