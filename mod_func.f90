MODULE mod_func

USE mod_init

IMPLICIT NONE

CONTAINS

  SUBROUTINE initialize(A,B,X,N)
  
    INTEGER                  :: N,i,j,k,ierr
    REAL(dp)                 :: dh
    REAL(dp), DIMENSION(N,N) :: A
    REAL(dp), DIMENSION(N)   :: B,X
    dh = 1.0_dp/FLOAT(N+1)

    DO i = 1, N            ! Zeroing
        X(i) = 0.0_dp
        DO j = 1, N
           A(i,j) = 0.0_dp
        END DO
     END DO

     DO i = 1, N            ! Initialize arrays
        B(i) = EXP( -10.0_dp*FLOAT(i)*dh )*100.0_dp*dh**2.0_dp
        IF (i .LT. N) THEN
           A(i,i+1) = -1.0_dp
           A(i+1,i) = -1.0_dp
           A(i,i)   =  2.0_dp
        ELSE
           A(i,i)   =  2.0_dp
        END IF
     END DO
  END SUBROUTINE
END MODULE mod_func
