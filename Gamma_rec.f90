! 2020 - 15 - 4
! Gamma_rec.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! Este programa calculca la Función gamma para un numero z
! Por el metodo del limite hasta n = 10000

! Codificación del texto: ASCII text
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008
! Instrucciones de compilación: no requiere nada más
! gfortran -Wall -pedantic -std=f95 Gamma_rec.f90 -o Gamma2 -fall-intrinsics
! ./Gamma2

! para su caracterización
! /usr/bin/time -f "%e, %M, %P," ./Gamma2

PROGRAM Gamma1
  IMPLICIT NONE
  
  ! Declaramos variables importantes
  
  REAL(16) :: Z, N
  INTEGER(8) :: i,j,k
  
  REAL(16):: Gzreal, Error
  REAL(16), ALLOCATABLE :: Gzi(:)
  
  OPEN (5, file = 'Gamma_rec.dat')
  WRITE (5,*) '#Resultados de la función Gamma'
  WRITE (5,*) ' z    G(z)aprox    G(z)real    Error% '
  WRITE (5,*) 1,1,1,0
  ! Iniciamos el numero hasta el cual encontraremos la funcioń gamma
  Z = 50.0   
    
  ! Iniciamos algunas variables
  N = 1700 ! 1750 Es el limite del ordenador de pruebas
 
  ! Calculamos el factorial
  
  k = INT(Z,8)
  ALLOCATE(Gzi(k))
  Gzi(1) = 1.0
  
  DO i=2, k
    j=i-1
    Gzi(i) = REAL(j,16)*Gzi(j)
    Gzreal = gamma(real(i,16))
    Error = ((Gzreal - Gzi(i))/Gzreal)*100
    WRITE (5,*) i, Gzi(i), Gzreal, Error
  END DO
  PRINT *,'Gamma(',int(Z),')=',Gzi(k)
  CLOSE(5)
END PROGRAM Gamma1
