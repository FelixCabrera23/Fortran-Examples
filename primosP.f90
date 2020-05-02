! 2020 - 19 - 3
! primosP.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! Este programa ayuda a probar la conjetura de goldbach

! Codificación del texto: ASCII text
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008
! Instrucciones de compilación: no requiere nada más

! Codigo de programasion en paralelo tomado de las clases de 
! Física computacional dictadas el primer semestre del 2020
! por el Dr. Giovanmi Ramires
! en la ECFM - USAC

! Instrucciones de compilación: no requiere nada más
! mpifort -Wall -pedantic -std=f95 -c -o primosP.o primosP.f90
! mpifort -o primosP.x primosP.o
! mpirun -np 2 ./primosP.x

! Para caracterizar
! /usr/bin/time -f "%e, %M, %P," mpirun -np 2 ./primosP.x


PROGRAM primos_paralelo
  USE Mpi
  IMPLICIT NONE  
 
  ! Variables auxiliares de conteo
  INTEGER (4):: i, j, k, l, n, m, o
  INTEGER (4) :: total_o, contador_i, o1

  ! Variables de las listas de numeros primos
  INTEGER(4), ALLOCATABLE:: primos1(:),primos2(:), primos3(:)
  
  ! Esta variables nos ayuda a sabe si un numero es primo o no
  LOGICAL :: primo
  
  !Variables auxiliares para Mpi
  INTEGER :: err, rank, size, emisor
  INTEGER(4) :: m1,n1, n2, parte, res, mod1
  INTEGER(4),DIMENSION(1:MPI_STATUS_SIZE)::status
  
  !Esta parte llama a Mpi
  ! iniciando MPI, esto habilita el entorno de trabajo
  CALL MPI_Init(err)
  IF (err.NE.0) STOP 'MPI_Init error' ! esto es programación defensiva

  ! hay varios parámetros que el programa debe conocer en tiempo real, por
  ! ejemplo tiene que conocer de cuántos nodos se dispone y quién es cada nodo
  CALL MPI_Comm_rank(MPI_COMM_WORLD,rank,err) ! dice a cada nodo quién es
  IF (err.NE.0) STOP 'MPI_Comm_rank error' ! esto es programación defensiva

  CALL MPI_Comm_size(MPI_COMM_WORLD,size,err) ! dice cuántos nodos hay
  IF (err.NE.0) STOP 'MPI_Comm_size error' ! esto es programación defensiva
  
  ! Primero verificamos los parametros que debe cumplir mpí
  
  mod1 = MOD(size,2) ! esta parte revisa si el numero de procesadores es par
  IF (mod1 /= 0) THEN ! esto es programación defensiva 
    PRINT *, 'El numero de nodos debe ser par'
    STOP
  END IF
   
  ! Iniciamos las variables del calculo
  n = 500000
  o = 0
  o1 = 1
  ! Iniciamos las variables para MPI
  
  m1 = size**2  ! Este es el numero que dividira a n  
  res = MOD(n,m1)
  parte = n/m1
  
  k = 0
  DO i=1, size
    k = k + i
  END DO
  ! Con esto definimos la cantidad de iteraciónes que hace 0  
  n1 = (k*parte)+res 
  
  ! Con esto asignamos los valores con los que realizaran la busqueda
  ! de numeros primos cada nucleo
  IF (rank == 0) THEN
  PRINT *, 'Procesando', n
    n2 = n1
    l = 2
  ELSE
    n2 = (size-rank)*parte
    l = n
    DO i=1,(size-rank)
      l = l - parte*(i)
    END DO
  END IF
    
  PRINT *, 'Soy', rank, 'procesando',l,'n=',n2
  
  ! Al principio no conocemos cuantos primos hay,
  ! pero sabemos que no pueden ser más que n
  ALLOCATE(primos1(n1))
  primos1 = 0
  
  DO j = 1, (n2)
    primo = .true.
    m = 2
    DO WHILE(m < l)
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
    l = l + 1  
  END DO
 
  ! Esta variable aqui ya sabe cuantos numeros primos hay
  ALLOCATE(primos2(o)) 
  primos2 = 0
  DO i = 1, (o)
    primos2(i) = primos1(i)
  END DO 
  DEALLOCATE(primos1)
  
  ! Devemos mandar cuantos numeros primos encontro cada nucleo para
  ! que el nucleo principal los almacene todos en la misma variable.
  
  ! Esta parte nos dice el tamaño del array
  IF (rank /= 0) THEN
    CALL MPI_SEND(o,1,Mpi_int, 0, 0, MPI_COMM_WORLD, err)

  ELSE
    total_o = o
    contador_i = o
    DO emisor = 1, size -1
      CALL MPI_RECV(o,1,Mpi_int, emisor, 0, MPI_COMM_WORLD,status, err)
      total_o = total_o + o
    END DO

    ALLOCATE(primos3(total_o))
    primos3 = 0
    
    DO i = 1, (contador_i)
      primos3(i) = primos2(i)
    END DO
  END IF
  
  ! Esta parte llena el array
  IF (rank /= 0) THEN
    CALL MPI_SEND(o,1,Mpi_int, 0, 1, MPI_COMM_WORLD, err)
    CALL MPI_SEND(primos2(o1),o,Mpi_int,0,2,MPI_COMM_WORLD,err)
  ELSE
      DO emisor = 1, size -1
      DEALLOCATE(primos2)
      CALL MPI_RECV(o,1,Mpi_int, emisor, 1, MPI_COMM_WORLD,status, err)
      ALLOCATE(primos2(o))
      primos2 = 0
      CALL MPI_RECV(primos2(o1),o,Mpi_int, emisor, 2, MPI_COMM_WORLD,status, err)
      DO j = 1 , o
        contador_i= contador_i+1
        primos3(contador_i) = primos2(j)     
      END DO
    END DO
    
    OPEN (1, file = 'primos_P.dat', status = 'new')
    WRITE (1,*) '# Numeros primos '
  
    DO i=1, (contador_i)
      WRITE (1,*) primos3(i)
    END DO
    CLOSE(1)
    DEALLOCATE(primos2,primos3)
    
    PRINT *,'Terminado'
  END IF

  ! terminando MPI, esto deshabilita el entorno de trabajo 
  CALL MPI_Finalize(err)
  IF (err.NE.0) STOP 'MPI_Init error'
  
END PROGRAM primos_paralelo
