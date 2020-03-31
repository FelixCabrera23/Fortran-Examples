! 2020 - 19 - 3
! Gold.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! Este programa ayuda a probar la conjetura de goldbach

! Codificación del texto: ASCII text
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008
! Instrucciones de compilación: no requiere nada más
! gfortran -Wall -pedantic -std=f95 Gold.f90 -o gold
! ./gold


PROGRAM goldbach
  IMPLICIT NONE
  ! Variables auxiliares de conteo
  INTEGER (8) :: i, j, k, l, n, m, o
  INTEGER (8) :: suma1, suma2
  ! Variables de las listas de numeros primos
  INTEGER(8), ALLOCATABLE:: primos1(:),primos2(:)
  INTEGER(8), ALLOCATABLE :: sumasv2v3(:,:)
  ! Variables para las distintas formas de sumar
  REAL(8):: v2, v3
  
  ! Esta variables nos ayuda a sabe si un numero es primo o no
  LOGICAL :: primo
  
  ! iniciamos nuestras variables
  n = 100
  o = 0
  l = 2 
  
  PRINT *, 'Procesando hasta', n
  
  ! Al principio no conocemos cuantos primos hay,
  ! pero sabemos que no pueden ser más que n
  ALLOCATE(primos1(n))
  primos1 = 0
  
  DO j = 1, (n)
    primo = .true.
    m = l
    DO i = 1, (l-2)
      m = m - 1
      k = MOD(l,m)
      IF (k == 0) THEN
        primo = .false.
      END IF
    END DO
    IF (primo) THEN
      o = o + 1
      primos1(o) = l
    END IF
    l = l + 1  !ascendente
  END DO
 
  ! Esta variable aqui ya sabe cuantos numeros primos hay
  ALLOCATE(primos2(o)) 
  DO i = 1, (o)
    primos2(i) = primos1(i)
  END DO 
  DEALLOCATE(primos1)
  
  ! Primero resolvemos para V2
  
  !Inicamos las variables necesarias
  
  ALLOCATE(sumasv2v3(3,n/2))
  m = 4
  !El primer siclo ira barriendo todos los numeros pares
  DO i=1, (n/2)
    v2 = 0
    v3 = 0
    ! Este ciclo recorre la lista de numeros primos
    DO j=1,(m+1)
      
      DO k=1,(m+1)
        suma1 = primos2(j) + primos2(k)
        IF (suma1 == m) THEN
          IF (j == k) THEN
            v2 = v2 + 1 
          ELSE
            v2 = v2 + 0.5
          END IF
        END IF
        
        !Aprobechamos los ciclos para hacer V3
        DO l=1,(m+1)
          suma2 = suma1+primos2(l)
          IF (suma2 == m) THEN
            IF ((j == k) .and. (k == l)) THEN
              v3 = v3 +1
            ELSEIF ((j /= k) .and. (k /= l)) THEN
              v3 = v3 + (1.0/6.0)
            ELSE
              v3 = v3 + (1.0/3.0)
            END IF
          END IF
        END DO
      END DO        
    END DO
    
    sumasv2v3(1,i)= INT(m)
    sumasv2v3(2,i)= INT(v2)
    sumasv2v3(3,i)= INT(v3)
    
    m = m + 2
  END DO
  ! Con estos quedan los datos para v2 y v3 guardados en el arreglo 

  ! Ahora imprimimos los datos a un archivo
  OPEN (1, file = 'goldbach.dat', status = 'new')
  WRITE (1,*) '# Datos del programa que prueban la conjetura de goldbach hasta',n
  WRITE (1,*) '# N, V2, V3'
  
  DO i=1, (n/2)
  
    WRITE (1,*) sumasv2v3(1,i), sumasv2v3(2,i), sumasv2v3(3,i)
  
  END DO

  CLOSE(1)

  PRINT *, 'terminado'
  DEALLOCATE(primos2,sumasv2v3)
  
END PROGRAM goldbach
