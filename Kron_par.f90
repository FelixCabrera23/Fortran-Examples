! 2020 - 17 - 5
! Kron_par.f90
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
! mpifort -Wall -pedantic -std=f95 -c -o kron_par.o Kron_par.f90
! mpifort -o Kron_p.x kron_par.o
! mpirun -np 2 ./Kron_p.x

! Para caracterizar
! /usr/bin/time -f "%e, %M, %P," mpirun -np 2 ./Kron_p.x

PROGRAM kron_par
  USE MPI
  IMPLICIT NONE
  
  ! Declaramos variables y variables auxiliares
  INTEGER(4) :: m,n,p,q,r,s ! Dimensiónes
  REAL(4), ALLOCATABLE :: A(:,:),B(:,:),C(:,:) ! Matrices
  INTEGER(4) :: alpha, beta
  INTEGER(4) :: i,j,k,l
  INTEGER(4) :: counter
  
  ! Variables para realizar la prueba
  ! Generamos matrices aleatorias
  INTEGER, DIMENSION(33) :: sem
  INTEGER(4) :: dim1, dim2
  
  !Variables auxiliares para Mpi
  INTEGER :: err, rank, size, tag1, tag2, tag3
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

  IF (size .NE.2)STOP 'MPI size must be 2' ! este codigo  solo funciona con 2 nucleos
  

  ! Por simplicidad se realizaran las pruebas en matrices cuadradas.
  dim1 = 10
  dim2 = dim1**2
  sem = 18711208

  ! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  ! Este bloque es la prueba aleatoria
!~   CALL RANDOM_SEED( put=sem)
!~   ALLOCATE(A(dim1,dim1),B(dim1,dim1),C(dim2,dim2))
  
!~   CALL RANDOM_NUMBER(A)
!~   A=A*10
!~   CALL RANDOM_NUMBER(B)
!~   B=B*10
  
!~   m = dim1
!~   n = dim1
!~   p = dim1
!~   q = dim1

  ! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

  !Prueba 1
  ! Definimos las dimensiónes
  m = 2
  n = 2
  p = 2
  q = 2
  r = m*p
  s = n*q
  ALLOCATE(A(m,n),B(p,q),C(r,s))
	
  !Definimos las matrices
  A(1,1) = 1
  A(1,2) = 3
  A(2,1) = 2
  A(2,2) = 1
	
  B(1,1) = 0
  B(1,2) = 2
  B(2,1) = 3
  B(2,2) = 1
  ! FIN DE LA PRUEBA
  ! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx



  IF (rank /= 0) THEN
    PRINT *, 'Calculando:'
    DO i=1, n
      PRINT *, '|',A(:,i),'|'
    END DO
    PRINT *, 'X'
    DO i=1, q
      PRINT *, '|',B(:,i),'|'
    END DO
    PRINT *, '='
  END IF
  
  r = m*p
  s = n*q
  counter = 1
  ! Separamos las tareas
  IF (rank /= 0) THEN
    DO i=1, m
      DO k=1, p
        alpha = q*(i-1)+k
        tag1 = counter
        tag2 = counter*2
        tag3 = counter*3
        CALL MPI_SEND(alpha,1,Mpi_int, 0, tag1, MPI_COMM_WORLD, err)
        CALL MPI_SEND(i,1,Mpi_int, 0, tag2, MPI_COMM_WORLD, err)
        CALL MPI_SEND(k,1,Mpi_int, 0, tag3, MPI_COMM_WORLD, err)
        counter = counter + 1
      END DO
    END DO
  ELSE
    DO counter = 1, r
      tag1 = counter
      tag2 = counter*2
      tag3 = counter*3
      CALL MPI_RECV(alpha,1,Mpi_int,1,tag1,MPI_COMM_WORLD,status, err)
      CALL MPI_RECV(i,1,Mpi_int,1,tag2,MPI_COMM_WORLD,status, err)
      CALL MPI_RECV(k,1,Mpi_int,1,tag3,MPI_COMM_WORLD,status, err)
      DO j=1, n
        DO l=1, q
          beta= p*(j-1)+l
          C(alpha,beta) = A(i,j)*B(k,l)
        END DO
      END DO
    END DO
    
    ! Imprimimos los resultados
    DO i=1, s
      PRINT *, '|',C(:,I),'|'
    END DO
  END IF
  DEALLOCATE(A,B,C)

END PROGRAM kron_par
