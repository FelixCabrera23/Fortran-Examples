! 2020 - 15 - 4
! fibonacci_sec.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! Este programa calculca la sucecion de fibonacci hasta un número n
! Por medio del metodo de una serie infinita

! Codificación del texto: ASCII text
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008
! Instrucciones de compilación: no requiere nada más
! gfortran -Wall -pedantic -std=f95 fibonacci_sec.f90 -o fibonacci
! ./fibonacci

! para su caracterización
! /usr/bin/time -f "%e, %M, %P," ./fibonacci

PROGRAM fibonacci
  IMPLICIT NONE
  
  ! Declaramos variables importantes
  INTEGER :: i, j, k, n
  INTEGER, ALLOCATABLE :: fib(:)
  
  ! Definimos algunas variables
  n = 10
  
  ALLOCATE(fib(n))
  
  fib(1) = 0
  fib(2) = 1
  
  DO i=3, n+1
    j = i - 1
    k = i - 2
    fib(i) = fib(j) + fib(k)
  END DO
  
  print *, fib
ENDPROGRAM fibonacci
