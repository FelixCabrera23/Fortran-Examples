! 2020 - 19 - 3
! GoldP.f90
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
! mpifort -Wall -pedantic -std=f95 -c -o GoldP.o GoldP.f90
! mpifort -o GoldP.x GoldP.o
! mpirun -np 2 ./GoldP.x


PROGRAM goldbach_paralelo
  USE Mpi
  IMPLICIT NONE
  
 
  ! Variables auxiliares de conteo
  INTEGER (4):: i, j, k, l, n, m, o
  INTEGER (4) :: total_o, contador_i, o1
  INTEGER (8) :: suma1, suma2
  ! Variables de las listas de numeros primos
  INTEGER(4), ALLOCATABLE:: primos1(:),primos2(:), primos3(:)
  INTEGER(4), ALLOCATABLE :: sumasv2(:),sumasv3(:), numeros(:)
  ! Variables para las distintas formas de sumar
  REAL(8):: v2, v3
  
  ! Esta variables nos ayuda a sabe si un numero es primo o no
  LOGICAL :: primo
  
  !Variables auxiliares para Mpi
  INTEGER :: err, rank, size, emisor, receptor
  INTEGER(4) :: m1,n1, n2, n3, l2, parte, res, mod1
  INTEGER(4),DIMENSION(1:MPI_STATUS_SIZE)::status
  INTEGER(8), ALLOCATABLE :: datos_par(:,:)
  
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
  n = 1000
  o = 0
  o1 = 1
  ! Iniciamos las variables para MPI
  
  m1 = size**2  ! Este es el numero que dividira a n  
  res = MOD(n,m1)
  parte = n/m1
  
  j = 0
  k = 0
  DO i=1, (size)
    j = j + 1
    k = k + j
  END DO
  ! Con esto definimos la cantidad de iteraciónes que hace 0  
  n1 = (k*parte) + res
  
  ! Con esto asignamos los valores con los que realizaran la busqueda
  ! de numeros primos cada nucleo
  IF (rank == 0) THEN
    n2 = n1
    l = 2
  ELSE
    n2 = (size-rank)*parte
    l = n
    DO i=1,(size-rank)
      l = l - parte*(i)
    END DO
  END IF
  
  ! Esta variable la necesitaremos para hacer goldbach
  ! Se toma aqui porque la l original se modifica más adelante
  l2 = l
  
  PRINT *, 'Soy', rank, 'procesando',l,'n=',n2
  
  ! Al principio no conocemos cuantos primos hay,
  ! pero sabemos que no pueden ser más que n
  ALLOCATE(primos1(n1))
  primos1 = 0
  
  DO j = 1, (n2)
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
  END IF
  DEALLOCATE(primos2) ! ya hemos dejado de usar esta variable
  
  ! Para empezar a resovler Goldbach en todos los nucleos necesitamos primero
  ! enviar la lista de nuemeros primos a todos los nucleos 
  ! y tambien la variable o que es la longitud de esta lista
  
  IF (rank == 0) THEN
    DO receptor=1, size-1
      CALL MPI_SEND(total_o,1,Mpi_int, receptor, 3, MPI_COMM_WORLD, err)
      CALL MPI_SEND(primos3(o1),total_o,Mpi_int,receptor,4,MPI_COMM_WORLD,err)
    END DO
  ELSE
    CALL MPI_RECV(total_o,1,Mpi_int,0,3,MPI_COMM_WORLD,status, err)
    ALLOCATE(primos3(total_o))
    CALL MPI_RECV(primos3(o1),total_o,Mpi_int,0,4,MPI_COMM_WORLD,status,err)
  END IF
  
  !Inicamos las variables necesarias
  ! Necesitamos un contador que sea par
  
  
  IF (MOD(n2,2) /= 0) THEN
    n3 = (n2 + 1)/2 - 2
  ELSE
    n3 = n2/2 -2
  END IF
  
  IF (MOD(l2,2) /= 0) THEN
    l2 = l2 - 1
  END IF
  
  ALLOCATE(sumasv2(n3))
  ALLOCATE(sumasv3(n3))
  ALLOCATE(numeros(n3))
  
  IF (rank == 0) THEN
    m = 4
  ELSE
    m = l2
  END IF  
  
  !El primer siclo ira barriendo todos los numeros pares
  DO i=1, (n3)
    v2 = 0
    v3 = 0
    ! Este ciclo recorre la lista de numeros primos
    DO j=1,(m+1)
      
      DO k=1,(m+1)
        suma1 = primos3(j) + primos3(k)
        IF (suma1 == m) THEN
          IF (j == k) THEN
            v2 = v2 + 1 
          ELSE
            v2 = v2 + 0.5
          END IF
        END IF
        
        !Aprobechamos los ciclos para hacer V3
        DO l=1,(m+1)
          suma2 = suma1+primos3(l)
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

    numeros(i)= m    
    sumasv2(i)= INT(v2)
    sumasv3(i)= INT(v3)
    
    m = m + 2
  END DO
  
  DEALLOCATE(primos3) ! ya la terminamos de usar
  
  ! Con estos quedan los datos para v2 y v3 guardados en el arreglo 
  ! Cada procesador tiene los propios en un array de distinta dimensión
  ! Ahora debemos guardar todos en el arreglo maestro
    
  IF (rank /= 0) THEN
    ! Los subprocesos envian las dimensiones de sus datos
    CALL MPI_SEND(n3,1,Mpi_int,0,5,MPI_COMM_WORLD, err)
    CALL MPI_SEND(l2,1,Mpi_int,0,6,MPI_COMM_WORLD, err)
    ! Los subprocesos envian resultados al maestro3
    CALL MPI_SEND(sumasv2(o1),n3,Mpi_int,0,7,MPI_COMM_WORLD,err)
    CALL MPI_SEND(sumasv3(o1),n3,Mpi_int,0,8,MPI_COMM_WORLD,err)
    CALL MPI_SEND(numeros(o1),n3,Mpi_int,0,9,MPI_COMM_WORLD,err)
    DEALLOCATE(sumasv2,sumasv3,numeros)
  ELSE
    ! El maestro añade sus resultados a los datos finales
    m1 = (n/2)
    ALLOCATE(datos_par(3,m1))
    contador_i = n3
    DO i = 1, n3
      datos_par(1,i) = numeros(i)
      datos_par(2,i) = sumasv2(i)
      datos_par(3,i) = sumasv3(i)
    END DO
    DEALLOCATE(sumasv2,sumasv3,numeros)
    ! Luego recive todos los datos
    DO emisor = 1, (size -1)
      n3 = 0
      l2 = 0
      o1 = 1
      CALL MPI_RECV(n3,1,Mpi_int,emisor,5,MPI_COMM_WORLD,status,err)
      CALL MPI_RECV(l2,1,Mpi_int,emisor,6,MPI_COMM_WORLD,status,err)
      ALLOCATE(sumasv2(n3),sumasv3(n3),numeros(n3))
      CALL MPI_RECV(sumasv2(o1),n3,Mpi_int,emisor,7,MPI_COMM_WORLD,status, err)
      CALL MPI_RECV(sumasv3(o1),n3,Mpi_int,emisor,8,MPI_COMM_WORLD,status, err)
      CALL MPI_RECV(numeros(o1),n3,Mpi_int,emisor,9,MPI_COMM_WORLD,status, err)
      DO i = 1, n3
        contador_i = contador_i + 1
        datos_par(1,contador_i) = numeros(i)
        datos_par(2,contador_i) = sumasv2(i)
        datos_par(3,contador_i) = sumasv3(i)
      END DO
      DEALLOCATE(sumasv2,sumasv3,numeros)
    END DO
  END IF
  
  ! Ahora imprimimos los datos a un archivo
  ! Solamente el maestro
  
  IF (rank == 0) THEN
  
    OPEN (10, file = 'goldbach.dat', status = 'new')
    WRITE (10,*) '# Datos del programa que prueban la conjetura de goldbach hasta',n
    WRITE (10,*) '# N, V2, V3'
  
    DO i=1, (m1)
  
      WRITE (10,*) datos_par(1,i), datos_par(2,i), datos_par(3,i)
  
    END DO

    CLOSE(10)

    PRINT *, 'terminado'
    DEALLOCATE(datos_par)

    ! terminando MPI, esto deshabilita el entorno de trabajo
  
  END IF
  CALL MPI_Finalize(err)
  IF (err.NE.0) STOP 'MPI_Init error'
  
END PROGRAM goldbach_paralelo
