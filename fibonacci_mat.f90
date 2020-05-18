! 2020 - 15 - 4
! fibonacci_sec.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! Este programa calculca la sucecion de fibonacci hasta un número n
! Por el metodo de multiplicación de matrices

! Codificación del texto: ASCII text
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008
! Instrucciones de compilación: no requiere nada más
! gfortran -Wall -pedantic -std=f95 fibonacci_mat.f90 -o fibonacciM
! ./fibonacciM

! para su caracterización
! /usr/bin/time -f "%e, %M, %P," ./fibonacciM

PROGRAM fibonacci
  IMPLICIT NONE
  
  ! Declaramos variables importantes
  INTEGER :: i, n
  INTEGER, ALLOCATABLE :: fib(:)
  INTEGER, DIMENSION(2,2) :: Gen, A
  
  ! Definimos algunas variables
  n = 6
  
  ALLOCATE(fib(n+1))
  ! iniciamos las variables requeridas
  fib(1) = 0
  fib(2) = 1
  
  gen(1,1) =1
  gen(1,2) =1
  gen(2,1) =1
  gen(2,2) =0
  
  A = Gen
  
  DO i=2, n
    A = MATMUL(A,Gen)
    fib(i+1)=A(2,1)
  END DO
  
  print *,A
ENDPROGRAM fibonacci
