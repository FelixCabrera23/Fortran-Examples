! 2020 - 10 - 4
! Distros.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! Este programa calcula las distintas ditribuciones de probabilidad
! usadas para resolver el problema de la aguja de buffon

! Codificación del texto: ASCII text
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008
! Instrucciones de compilación: no requiere nada más
! gfortran -Wall -pedantic -std=f95 Distros.f90 -o distros
! ./distros

PROGRAM distros
  IMPLICIT NONE
  
  ! Variables principales
  INTEGER(8) :: n
  REAL(8) :: x1, x2
  
  ! Variables auxiliares
  INTEGER, DIMENSION(33) :: semilla
  INTEGER(8) :: i, exp1
  REAL(8) :: r1, r2, r3, ang
  
  PRINT *, 'Procesando'
  
  semilla = 1209874
  CALL RANDOM_SEED (put=semilla)
  
  n = 100000
  
  OPEN (1, file = 'distros.dat', status = 'new')
  WRITE (1,*) '# Almacenamiento de numeros aleatorios'
  WRITE (1,*) '# MODELO 1, MODELO 2'
     
  DO i = 1, n
    CALL RANDOM_NUMBER (r1)
    CALL RANDOM_NUMBER (r2)
    CALL RANDOM_NUMBER (r3)
    ! Modelo 1
    exp1 = INT(r1*2)
    x1 = r2*((-1)**exp1)
    
    !Modelo 2
    ang = r3*6.28
    x2 = SIN(ang)
    
    WRITE (1,*) x1, x2
        
  END DO
  
  CLOSE (1)
  
  PRINT *, 'Terminado'

END PROGRAM distros
