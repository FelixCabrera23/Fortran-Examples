! 2020 - 19 - 3
! primos.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! Este programa devuelve los numeros primos desde 1 hasta n

! Codificación del texto: ASCII text
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008
! Instrucciones de compilación: no requiere nada más
! gfortran -Wall -pedantic -std=f95 primos.f90 -o primos
! ./primos

! para su caracterización
! /usr/bin/time -f "%e, %M, %P," ./primos


PROGRAM primos
  IMPLICIT NONE
  ! Variables auxiliares de conteo
  INTEGER (8) :: i, j, k, l, n, m, o
  
  ! Variables de las listas de numeros primos
  INTEGER(8), ALLOCATABLE:: primos1(:)
  
  ! Esta variables nos ayuda a sabe si un numero es primo o no
  LOGICAL :: primo
  
  ! iniciamos nuestras variables
  ! para hacer en orden ascendente l = 1,
  ! para hacerlos en orden descendente l = n
  n = 500000
  o = 0
  l = 2 ! Ascendente
  !l = n ! Descendente
  
  PRINT *,'Procesando',n
  
  ! Al principio no conocemos cuantos primos hay,
  ! pero sabemos que no pueden ser más que n
  ALLOCATE(primos1(n))
  primos1 = 0
  
  DO j = 1, (n)
    primo = .true.
    m = 2
    DO WHILE (m<l)
      k = MOD(l,m)
      IF (k == 0) THEN
        primo = .false.
        EXIT
      END IF
      m = m + 1
    END DO
    IF (primo) THEN
      o = o + 1
      primos1(o) = l
    END IF
    l = l + 1  !ascendente
    !l = l - 1 !descendente
    
  END DO
 
  ! Esta variable aqui ya sabe cuantos numeros primos hay
  
  OPEN (1, file = 'primos_sec.dat', status = 'new')
  WRITE (1,*) '# Numeros primos '
  
  DO i=1, (o)
    WRITE (1,*) primos1(i)
  END DO
  
  CLOSE(1)
  DEALLOCATE(primos1)
  
  PRINT *,'Terminado'
  
END PROGRAM primos
