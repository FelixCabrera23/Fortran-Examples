! 2020 - 7 - 4
! Buffon.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! Este programa aproxima el numero pi por medio del problema
! de la aguja de buffon

! Codificación del texto: ASCII text
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008
! Instrucciones de compilación: no requiere nada más
! gfortran -Wall -pedantic -std=f95 Buffon.f90 -o buffon
! ./buffon

! para su caracterización
! /usr/bin/time -f "%e, %M, %P," ./buffon

PROGRAM buffon
  IMPLICIT NONE
  
  ! Declaramos algunas variables importatnes
  INTEGER(8) :: n, A
  REAL(8) :: o, x0, x1, pi1, pi2
  
  ! Variables auxiliares
  INTEGER, DIMENSION(33) :: sem, s1, s2, s3
  INTEGER(8) :: m, i, j, exp2
  REAL (8) :: r1, r2, r3, suma, exp1, x
 
  ! Iniciamos algunas variables 
  sem = 7052019
  
  CALL RANDOM_SEED (put = sem)
   
  n = 40
  m = 1
  x = 0
  o = 0
  suma = 0
    
  DO i = 1, m
    pi1 = 0
    A = 0
    DO j= 1, n
      ! Generamos 2 semillas independientes
      CALL RANDOM_NUMBER (r1) 
      s1 = INT(r1*10000)
    
      CALL RANDOM_NUMBER (r2)
      s2 = INT(r2*10000)
      
      CALL RANDOM_NUMBER (r3)
      s3 = INT(r3*10000)   
      ! Las usamos para generar dos numeros aleatorios independientes
      CALL RANDOM_SEED (put = s1)
      CALL RANDOM_NUMBER (x) 
    
      CALL RANDOM_SEED (put = s2)
      CALL RANDOM_NUMBER (o)     
      
      CALL RANDOM_SEED (put = s3)
      CALL RANDOM_NUMBER (exp1)
      
      exp2 = int(exp1*2)
      ! Hacemos uso de los numeros
      x0 = 2*o ! punto del origen de la aguja
      x1 = x0+(x*((-1)**exp2)) ! punto del final de la aguja
      IF ((x1 <= 0) .or. (x1 >=2)) THEN
        A = A + 1
      ELSE IF ((x0 <= 1) .and. (x1 >= 1)) THEN
        A = A + 1
      ELSE IF ((x0 >= 1) .and. (x1 <=1)) THEN
        A = A + 1
!~       IF ((x1 <= 0) .or. (x1 >= 1)) THEN
!~         A = A + 1
      END IF
    END DO
    ! Calculamos pi
  
    pi1 = (2*REAL(n))/(REAL(A))
    suma = suma + pi1
  END DO
  
  pi2 = suma/real(m)
  print *,'pi = ',pi2
  
END PROGRAM buffon
