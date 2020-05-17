! 2020 - 7 - 4
! Buffon.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! Este programa aproxima el numero pi por medio del problema
! de la aguja de buffon

! Codificación del texto: ASCII text
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008

! Codigo de programasion en paralelo tomado de las clases de 
! Física computacional dictadas el primer semestre del 2020
! por el Dr. Giovanmi Ramires
! en la ECFM - USAC

! Instrucciones de compilación: no requiere nada más
! mpifort -Wall -pedantic -std=f95 -c -o Buffon_p.o Buffon_v16_par.f90
! mpifort -o Buffon_p.x Buffon_p.o
! mpirun -np 2 ./Buffon_p.x

! Para caracterizar
! /usr/bin/time -f "%e, %M, %P," mpirun -np 2 ./Buffon_p.x

PROGRAM buffon_par
  USE Mpi
  IMPLICIT NONE
  
  ! Declaramos algunas variables importatnes
  REAL(16) :: x, ang, pi1, n, A
  
  ! Variables auxiliares
  INTEGER, DIMENSION(33) :: sem
  INTEGER(16) :: i, mod1, m
  REAL (16) :: x1
  
  !Variables auxiliares para Mpi
  INTEGER :: err, rank, size, emisor, tag1, tag2
  INTEGER(16) :: n1, res, A1, B1, B2
  INTEGER(16),DIMENSION(1:MPI_STATUS_SIZE)::status
  
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

  
  ! Iniciamos algunas variables 
  sem = 12081948*(497041*rank)
  
  CALL RANDOM_SEED (put = sem)
  
  ! para mayor numero de agujas se recomienda la formula
  ! n = rank*10.0**9
  n = 100000000.0
  x = 0
  pi1 = 0
  A = 1
  A1 = 0
  B1 = 0
  
  n1 = INT(n/size,8)
  res = INT( MOD(n,REAL(size,16)), 8)
  m = n1/1000
  
  IF (rank == 0 ) THEN
    
    ! Esto es programación defensiva
    IF (n1 > 10**9) THEN
      PRINT *, 'n es muy grande, por farvor utilize un umero mas pequeño o añada mas nucleos'
      STOP
    ELSE IF (res /= 0) THEN
      PRINT *, 'n debe ser un multiplo de size'
      STOP
    END IF
  
    OPEN (1, file = 'Buffon_quad_par.dat', status = 'new')
    WRITE (1,*) '# Resultados de la simulación de la aguja de buffon'
    WRITE (1,*) 'n pi'
  END IF
  
  PRINT *, 'soy', rank, 'n=',n1


  DO i= 1, n1
    
    B1 = B1 + 1
    tag1 = INT(i)
    tag2 = INT(1+i)
    
    CALL RANDOM_NUMBER (x) 
    
    CALL RANDOM_NUMBER (ang)     
      
    ! Hacemos uso de los numeros
    x1 = x + SIN(ang*6.28318530718) ! punto del final de la aguja
    
    IF ((x1 <= 0) .or. (x1 >=1)) THEN
      A1 = A1 + 1
    END IF

  ! Calculamos pi
  
    
    
    mod1 = mod(i,m)
        
    IF ((i == 1) .or. (mod1 == 0)) THEN
      IF (rank /= 0) THEN
        CALL MPI_SEND(A1,1,Mpi_int, 0, tag1, MPI_COMM_WORLD, err)
        CALL MPI_SEND(B1,1,Mpi_int, 0, tag2, MPI_COMM_WORLD, err)
      ELSE
        A = A1
        B2 = B1
         DO emisor = 1, size -1
          CALL MPI_RECV(A1,1,Mpi_int, emisor, tag1, MPI_COMM_WORLD,status, err)
          CALL MPI_RECV(B1,1,Mpi_int, emisor, tag2, MPI_COMM_WORLD,status, err)
          A = A + A1
          B2 = B2 + B1
        END DO
        pi1 = (2*REAL(B2))/(REAL(A))
        WRITE (1,*) B2, pi1
      END IF
    END IF
    
  END DO

  IF (rank == 0) THEN
    PRINT *, 'n =', n
    PRINT *,'pi =', pi1
    CLOSE (1)
  END IF
END PROGRAM buffon_par
