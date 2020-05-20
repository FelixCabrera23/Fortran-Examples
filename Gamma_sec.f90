! 2020 - 15 - 4
! Gamma_sec.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! Este programa calculca la Función gamma para un numero z
! Por el metodo del limite hasta n = 10000

! Codificación del texto: ASCII text
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008
! Instrucciones de compilación: no requiere nada más
! gfortran -Wall -pedantic -std=f95 Gamma_sec.f90 -o Gamma1 -fall-intrinsics
! ./Gamma1

! para su caracterización
! /usr/bin/time -f "%e, %M, %P," ./Gamma1

PROGRAM Gamma1
  IMPLICIT NONE
  
  ! Declaramos variables importantes
  
  REAL(16) :: Z, N, zi
  REAL(16):: Gz, Factorial, Divisor, Gzreal, Error
  INTEGER(8) :: K
  
  ! Abrimos el programa para guardar los datos
  OPEN (5, file = 'Gamma_sec.dat')
  WRITE (5,*) '#Resultados de la función Gamma'
  WRITE (5,*) ' z    G(z)aprox    G(z)real    Error% '
  
  Z = 5.0   ! 35 es el limite de la función Gamma
  
  ! Iniciamos algunas variables
  N = 1700 ! 1750 Es el limite del ordenador de pruebas
  
  zi = 0.01
  DO WHILE (zi<Z)
    ! Calculamos el factorial
    Factorial = 1
    Divisor = zi
    
    DO k=1,(INT(N,4))
      Factorial = Factorial*(REAL(k,16))
      Divisor = Divisor*(zi+REAL(k,16))
    END DO
  
    ! Gamma
  
    Gz = (Factorial/Divisor)*(N**zi)
    Gzreal = gamma(zi)
    Error = ((Gzreal - Gz)/Gzreal)*100
  
    WRITE (5,*) zi, Gz, Gzreal, Error
    zi = zi + 0.01
  END DO
CLOSE(5)
END PROGRAM Gamma1
