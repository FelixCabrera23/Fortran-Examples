! 2020 - 19 - 3
! GoldD.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! Este programa ayuda a probar la conjetura de goldbach

! Codificación del texto: ASCII text
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008
! Instrucciones de compilación: no requiere nada más
! gfortran -Wall -pedantic -std=f95 Gold_D.f90 -o goldD
! ./goldD

! Para caracterizar
! /usr/bin/time -f "%e, %M, %P," ./goldD


PROGRAM goldbach_D
  IMPLICIT NONE
  ! Variables auxiliares de conteo
  INTEGER (8) :: i, j, k, n, m, o
  INTEGER (8) :: suma1, suma2, m1, primo1, primo2
  ! Variables de las listas de numeros primos
  INTEGER(8), ALLOCATABLE:: primos1(:)
  INTEGER(8), ALLOCATABLE :: sumasv2v3(:,:)
  ! Variables para las distintas formas de sumar
  REAL(8):: v2, v3
  
  ! Esta variables nos ayuda a sabe si un numero es primo o no
  LOGICAL :: primo
  REAL (8) :: raiz
  
  ! iniciamos nuestras variables
  n = 50000
  o = 1 
  
  PRINT *, 'Procesando hasta', n
  
  ! Al principio no conocemos cuantos primos hay,
  ! pero sabemos que no pueden ser más que n/2
  ! ya que podemos descartar todos los numeros pares
  ALLOCATE(primos1(n/2+500))
  primos1 = 0
  primos1(1) = 2
 
 
  DO i = 3, n+1000
    primo = .true.
    k = 1
    j = 2
    raiz = SQRT(REAL(i))
    DO WHILE (j<=raiz)
      m1 = MOD(i, primos1(k))
      IF (m1 == 0) THEN
        primo = .false.
        EXIT
      END IF
      j = primos1(k)
      k = k + 1        
    END DO
    
    IF (primo) THEN
      o = o + 1
      primos1(o) = i
    END IF    
  END DO  
  ! Primero resolvemos para V2
  
  !Inicamos las variables necesarias
  
  ALLOCATE(sumasv2v3(3,n/2+10))
  sumasv2v3 = 0
  m = 4
  k = 1
  !El primer siclo ira barriendo todos los numeros pares
  DO WHILE (m <= n)
    v2 = 0
    v3 = 0
    i = 1
    primo1 = 2
    ! Este ciclo recorre la lista de numeros primos
    DO WHILE (primo1<m)
      j = 1
      primo2 = 2
      suma1 = primo1 + 2
      DO WHILE (suma1<m)
        
        suma1 = primo1 + primo2

        IF (suma1 == m) THEN
          IF (i == j) THEN
            v2 = v2 + 1 
          ELSE
            v2 = v2 + 0.5
          END IF
        END IF
        
        suma2 = suma1 + 2
        IF (suma2 == m) THEN
          IF (i == j) THEN
            v3 = v3 + 1 
          ELSE
            v3 = v3 + 0.5
          END IF
        END IF
        j = j + 1
        primo2 = primos1(j)
      END DO 
      i = i + 1  
      primo1 = primos1(i)
    END DO 
    
    sumasv2v3(1,k)= INT(m)
    sumasv2v3(2,k)= INT(v2)
    sumasv2v3(3,k)= INT(v3)

    m = m + 2
    k = k + 1
  END DO
  ! Con estos quedan los datos para v2 y v3 guardados en el arreglo 

  ! Ahora imprimimos los datos a un archivo status= 'new'
  
  OPEN (1, file = 'goldbach_D.dat')
  WRITE (1,*) '# Datos del programa que prueban la conjetura de goldbach hasta',n
  WRITE (1,*) 'N     V2     V3'
  
  DO i=1, (n/2-1)
  
    WRITE (1,*) sumasv2v3(1,i), sumasv2v3(2,i), sumasv2v3(3,i)
  
  END DO

  CLOSE(1)

  PRINT *, 'terminado'
  DEALLOCATE(primos1,sumasv2v3)
  
END PROGRAM goldbach_D
