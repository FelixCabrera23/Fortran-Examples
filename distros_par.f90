! 2020 - 15 - 4
! distros_sec.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! Este programa genera numeros aleatorios siguiendo la distribución 
! Exponencial y la distribución normal, por el método inverso.

! Codificación del texto: ASCII text
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008
! Instrucciones de compilación: no requiere nada más

! Codigo de programasion en paralelo tomado de las clases de 
! Física computacional dictadas el primer semestre del 2020
! por el Dr. Giovanmi Ramires
! en la ECFM - USAC

! Instrucciones de compilación: no requiere nada más
! mpifort -Wall -pedantic -std=f95 -c -o distros_par.o distros_par.f90
! mpifort -o distros_par.x distros_par.o
! mpirun -np 2 ./distros_par.x

! Para caracterizar
! /usr/bin/time -f "%e, %M, %P," mpirun -np 2 ./distros_par.x

PROGRAM distros
  USE Mpi
  IMPLICIT NONE
  ! Declaramos variables importantes
  ! Estas variables seran los numeros aleatorios
  REAL (4) :: exp1, nor1, uni1
  
  ! Declaramos variables auxiliares
  INTEGER (4) :: i, j, n
  REAL (4) :: delta
  INTEGER, DIMENSION(33) :: semilla
  REAL (4), ALLOCATABLE :: exp2(:), nor2(:), uni2(:)
  REAL (4), ALLOCATABLE :: datos_par(:,:)
  
  !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  
  !Variables auxiliares para Mpi
  INTEGER :: err, rank, size, emisor
  INTEGER(4) :: n1
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
  
  !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  
  ! Determinamos la cantida de numeros que queremos generar
  n = 50000000
  j = 1
   
  ! iniciamos variables auxiliares
  delta = 3
  ALLOCATE(datos_par(3,n+size))
  datos_par = 0
  ! Separamos la tarea
  IF (rank == 0) THEN
    n1 = (n/size) + MOD(n,size)
    PRINT *, 'Generando', n, 'números aleatorios'
  ELSE
    n1 = n/size
  END IF
  ALLOCATE(exp2(n1),nor2(n1),uni2(n1))
  PRINT *, 'rank=', rank, 'generando',n1,'números'
  
  ! Generamos la semilla prinipal
  semilla = 23052020*(497041*rank)
  CALL RANDOM_SEED(put = semilla) 
  
  ! Este cilco generará todos los numeros aleatorios.
  DO i=1, n1
    
    ! Primero generamos el numero aleatorio de distribución uniforme
    CALL RANDOM_NUMBER(uni1)
    
    ! Generamos el número aleatorio de distribución exponencial
    exp1 = -(1/delta)*LOG(uni1)
       
    ! Generamos el número aleatorio de distribución normal
    nor1 = SQRT(LOG(1/(uni1**2)))
    
    ! Guardamos todos los resultados en la variable
    IF (rank==0) THEN
      datos_par(1,i) = uni1
      datos_par(2,i) = exp1
      datos_par(3,i) = nor1
    ELSE
      uni2(i) = uni1
      exp2(i) = exp1
      nor2(i) = nor1
    END IF
      
    ! Llevamos un conteo de cuantos numeros genera el proceso maestro
  END DO
  
  IF (rank /= 0) THEN
    ! Los subprocesos envian las dimensiones de sus datos
    CALL MPI_SEND(n1,1,Mpi_int,0,1,MPI_COMM_WORLD, err)
    ! Los subprocesos envian resultados al maestro3
    CALL MPI_SEND(uni2(1),n1,Mpi_real,0,2,MPI_COMM_WORLD,err)
    CALL MPI_SEND(exp2(1),n1,Mpi_real,0,3,MPI_COMM_WORLD,err)
    CALL MPI_SEND(nor2(1),n1,Mpi_real,0,4,MPI_COMM_WORLD,err)
    DEALLOCATE(uni2,exp2,nor2)

  ELSE
    j = n1
    DO emisor = 1, (size-1)
      DEALLOCATE(uni2,exp2,nor2)
      n1 = 0
      CALL MPI_RECV(n1,1,Mpi_int,emisor,1,MPI_COMM_WORLD,status,err)
      ALLOCATE(uni2(n1),exp2(n1),nor2(n1))
      CALL MPI_RECV(uni2(1),n1,Mpi_real,emisor,2,MPI_COMM_WORLD,status, err)
      CALL MPI_RECV(exp2(1),n1,Mpi_real,emisor,3,MPI_COMM_WORLD,status, err)
      CALL MPI_RECV(nor2(1),n1,Mpi_real,emisor,4,MPI_COMM_WORLD,status, err)
      DO i=1, n1
        j=j+1       
        datos_par(1,j) = uni2(i)
        datos_par(2,j) = exp2(i)
        datos_par(3,j) = nor2(i)
        
      END DO
     
    END DO
    DEALLOCATE(uni2,exp2,nor2)

  END IF

  IF (rank==0) THEN
    ! Abrimos archivos donde se guardaran los datos
    OPEN (5, file = 'ditribuciones_par.dat')
    WRITE (5,*) '# Resultados de distribuciónes exponencial y potencial'
    WRITE (5,*) 'x1      exp(x1)      norm(x1)'
    DO i=1, n
      WRITE (5,*) datos_par(1,i),datos_par(2,i),datos_par(3,i)
    END DO
  
    PRINT *, 'Terminado'
    CLOSE(5)
  END IF
  DEALLOCATE(datos_par)
  CALL MPI_Finalize(err)
  IF (err.NE.0) STOP 'MPI_Init error'
ENDPROGRAM distros











