! 2020 - 19 - 3
! primosD.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! Este programa devuelve los numeros primos desde 1 hasta n
! Utiliza el metodo de la división por tentativa

! Codificación del texto: ASCII text
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008
! Instrucciones de compilación: no requiere nada más
! gfortran -Wall -pedantic -std=f95 primosD.f90 -o primosD
! ./primosD

! para su caracterización
! /usr/bin/time -f "%e, %M, %P," ./primosD

PROGRAM primosD
  IMPLICIT NONE
  
  ! Variables principales
  INTEGER (8) :: i, j, k, l, m, n
  REAL (8) :: raiz
  
  ! Variables de las listas de numeros primos
  INTEGER(8), ALLOCATABLE:: primos1(:)
   
  ! Esta variables nos ayuda a sabe si un numero es primo o no
  LOGICAL :: primo
  
  ! Iniciamos las variables
  n = 500000
  l = 1
  
  ALLOCATE(primos1(n))
  primos1 = 0
  primos1(1) = 2
  
  PRINT *,'Procesando',n
  
  DO i = 3, n
    primo = .true.
    k = 1
    j = 2
    raiz = SQRT(REAL(i))
    DO WHILE (j<=raiz)
      m = MOD(i, primos1(k))
      IF (m == 0) THEN
        primo = .false.
        EXIT
      END IF
      j = primos1(k)
      k = k + 1        
    END DO
    
    IF (primo) THEN
      l = l + 1
      primos1(l) = i
    END IF    
  END DO  
    
  OPEN (1, file = 'primos_D.dat')
  WRITE (1,*) '# Numeros primos '
  
  DO i=1, (l)
    WRITE (1,*) primos1(i)
  END DO
  
  CLOSE(1)
  DEALLOCATE(primos1)

END PROGRAM primosD
