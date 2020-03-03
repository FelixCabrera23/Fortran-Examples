! 2020-25-02
! Dado6.f95
! Félix Cabrera ( walberto.cabrera@gmail.com=
!
! Programa que simula un dado de 6 caras
!
! Codificación del texto: ASCII text
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008
! Instrucciones de compilación: no requiere nada más
! gfortran -Wall -pedantic -std=f95 Dado6.f95 -o Dado
! ./Dado

PROGRAM Dado

  IMPLICIT NONE
  
  !Variables principales
  REAL, DIMENSION(1):: R, R2
  INTEGER, DIMENSION(33):: sem 
  INTEGER :: i
  !Llamamos al modulo y lo ejecutamos para generar numero aleatoreos
  
  sem = 30101999

  
  CALL RANDOM_SEED(PUT=sem)
  DO i=1,100
    CALL RANDOM_NUMBER(R)
    R2 = INT(R*10)
    PRINT *, R2
  ENDDO
  
END PROGRAM Dado
