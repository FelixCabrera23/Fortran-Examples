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
  REAL(8) :: x, ang, pi1
  
  ! Variables auxiliares
  INTEGER, DIMENSION(33) :: sem
  INTEGER(8) :: i, mod1, m
  REAL (8) :: x1
  
  ! Iniciamos algunas variables 
  sem = 7052019
  
  CALL RANDOM_SEED (put = sem)
   
  n = 1000000000
  m = 100
  x = 0
  pi1 = 0
  A = 1
  
  OPEN (1, file = 'Buffon.dat', status = 'new')
  WRITE (1,*) '# Resultados de la simulación de la aguja de buffon'
  WRITE (1,*) 'n pi'

  DO i= 1, n
  
    CALL RANDOM_NUMBER (x) 
    
    CALL RANDOM_NUMBER (ang)     
      
    ! Hacemos uso de los numeros
    x1 = x + SIN(ang*6.28318530718) ! punto del final de la aguja
    
    IF ((x1 <= 0) .or. (x1 >=1)) THEN
      A = A + 1
    END IF

  ! Calculamos pi
  
    pi1 = (2*REAL(i))/(REAL(A))
    
    mod1 = mod(i,m)
        
    IF ((i == 1) .or. (mod1 == 0)) THEN
      WRITE (1,*) i, pi1
    END IF
    
  END DO
  PRINT *, 'n =', n
  PRINT *,'pi =', pi1
  
END PROGRAM buffon
