PROGRAM adal

IMPLICIT NONE

INTEGER, PARAMETER                   :: dp = KIND(1.0D0)
INTEGER                              :: N,i,j,k,ierr
REAL(dp)                             :: coefficient
REAL(dp)                             :: dh
CHARACTER(len=1024)                  :: FMT = '(I2.2)'
CHARACTER(len=1024)                  :: stri
REAL(dp), ALLOCATABLE,DIMENSION(:,:) :: A
REAL(dp), ALLOCATABLE,DIMENSION(:)   :: X,B
INTEGER, PARAMETER                   :: l = 13

DO k = 1,l
   N = 2**k
   dh = 1.0_dp/FLOAT(N+1)
   WRITE(stri,FMT) 10+k
   OPEN(UNIT=10+k,FILE='sol'//TRIM(stri)//'.dat',STATUS='new')
   ALLOCATE(A(N,N),B(N),X(N), STAT=ierr)

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

   DO i = 1, N-1  
      coefficient = A(i+1,i)/A(i,i)
      A(i+1,i+1)  = A(i+1,i+1) - coefficient*A(i,i+1)
      B(i+1)    = B(i+1)   - coefficient*B(i)
   END DO

   X(N) = B(N)/A(N,N)
   WRITE(10+k,*) X(N)

   DO i = N-1, 1, -1
      X(i) = ( B(i) - A(i,i+1)*X(i+1) ) / A(i,i)
      WRITE(10+k,*) X(i)
   END DO

   CLOSE(10+k)

   DEALLOCATE(A,B,X, STAT=ierr)
   
END DO

END PROGRAM adal
