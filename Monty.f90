! 2020 - 28 - 2
! Monty.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! Programa que simula el concurso de Monty Hall

! Codificación del texto: ASCII text
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008
! Instrucciones de compilación: no requiere nada más
! gfortran -Wall -pedantic -std=f95 Monty.f90-o Monty
! ./Monty

PROGRAM Monty

  IMPLICIT NONE
  
  !Variables principales
  INTEGER :: Puerta, Eleccion
  !Variables necesarias para la estadistica
  INTEGER :: Gana_p, Gana_c
  REAL :: Prob_p, Prob_c
  
  INTEGER, DIMENSION (33) :: semilla
  
  !Variables auxiliares
  REAL :: p1,p2
  INTEGER :: i,n
  
  n = 10000000
  semilla = 07052019

  CALL RANDOM_SEED (put = semilla)
  
  ! inicializando variables
  Gana_c = 0
  Gana_p = 0
  
  DO i=1,n
    !Esta parte simula la elección de la puerta correcta
  
    CALL RANDOM_NUMBER (p1) !Elección de la puerta correcta
    CALL RANDOM_NUMBER (p2) !Elección del concursante
  
    Puerta = INT(p1*3)
    Eleccion = INT(p2*3)
    
    !Esta parte calcula la probabilidad si no se cambia de puerta
    IF (Puerta == Eleccion) THEN
      Gana_p = Gana_p + 1
    !Esta parte calcula la probabilidad si la eleccion cambia
    !Si la puerta elegida originalmente es la premida entonces 
    !No es necesario considerar esa opicon en el cambio de puerta
    ELSEIF (Puerta /= Eleccion) THEN
      Gana_c = Gana_c + 1
    ENDIF 
  ENDDO
  
  !Calculando la probabilidad
  Prob_p = (Gana_p/(n/100))
  Prob_c = (Gana_c/(n/100))
  
  PRINT *, 'La probabilidad de ganar si no se cambia es de: ', Prob_p,'%'
  PRINT *, 'La probabilidad de ganar si se cambia es de: ', Prob_c, '%'
  
END PROGRAM Monty
