! 2020 - 17 - 5
! Kron.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! Este programa calculca la Función gamma para un numero z
! Por el metodo del limite hasta n = 10000

! Codificación del texto: ASCII text
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008
! Instrucciones de compilación: no requiere nada más
! gfortran -Wall -pedantic -std=f95 Kron.f90 -o kronecker
! ./kronecker

! para su caracterización
! /usr/bin/time -f "%e, %M, %P," ./kronecker

PROGRAM kron
  IMPLICIT NONE
	
  !Definimos variables importantes
  INTEGER(4) :: m,n,p,q,r,s ! Dimensiónes
  REAL(4), ALLOCATABLE :: A(:,:),B(:,:),C(:,:) ! Matrices
  INTEGER(4) :: alpha, beta

  ! Variables auxiliares
  INTEGER(4) :: i,j,k,l
  
  ! Variables para realizar la prueba
  ! Generamos matrices aleatorias
  INTEGER, DIMENSION(33) :: sem
  INTEGER(4) :: dim1, dim2
  
  ! Por simplicidad se realizaran las pruebas en matrices cuadradas.
  dim1 = 3
  dim2 = dim1**2
  sem = 18711208
  ! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  ! Este bloque es la prueba aleatoria
  CALL RANDOM_SEED( put=sem)
  ALLOCATE(A(dim1,dim1),B(dim1,dim1),C(dim2,dim2))
  
  CALL RANDOM_NUMBER(A)
  A=A*10
  CALL RANDOM_NUMBER(B)
  B=B*10
  
  m = dim1
  n = dim1
  p = dim1
  q = dim1
  
  ! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!~ 	!Prueba 1
!~ 	! Definimos las dimensiónes
!~     m = 2
!~     n = 2
!~     p = 2
!~     q = 2
!~     r = m*p
!~     s = n*q
!~     ALLOCATE(A(m,n),B(p,q),C(r,s))
	
!~ 	! Definimos las matrices
!~     A(1,1) = 1
!~     A(1,2) = 3
!~     A(2,1) = 2
!~     A(2,2) = 1
	
!~     B(1,1) = 0
!~     B(1,2) = 2
!~     B(2,1) = 3
!~     B(2,2) = 1

  ! FIN DE LA PRUEBA
  ! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    
  ! Esta parte imprime las matrices a calcular
  PRINT *, 'Calculando:'
  DO i=1, n
    PRINT *, '|',A(:,i),'|'
  END DO
  PRINT *, 'X'
  DO i=1, q
    PRINT *, '|',B(:,i),'|'
  END DO
  PRINT *, '='
    
  r = m*p
  s = n*q
    
  ! Esta parte calcula el producto de kroneker
  DO i=1, m
    DO j=1, n
      DO k=1, p
        DO l=1, q
          beta = p*(j-1)+l
          alpha = q*(i-1)+k
          C(alpha,beta) = A(i,j)*B(k,l)
        END DO
      END DO
    END DO
  END DO
    
  ! Esta parte imprime el resultado
  DO i=1, s
    PRINT *, '|',C(:,i),'|'
  END DO
  DEALLOCATE(A,B,C)  
END PROGRAM kron
