! 2020 - 28 - 2
! Monty.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! Programa que simula el concurso de Monty Hall

! Codificación del texto: ASCII text
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008
! Instrucciones de compilación: no requiere nada más
! mpifort -Wall -pedantic -std=f95 -c -o MontyP.o MontyP.f90
! mpifort -o MontyP.x MontyP.o
! mpirun -np 2 ./MontyP.x

! Notese que en la ultima instrucción el numero depede de la cantidad 
! de procesadores fisicos que disponga el sistema.

! Codigo de programasion en paralelo tomado de las clases de 
! Física computacional dictadas el primer semestre del 2020
! por el Dr. Giovanmi Ramires
! en la ECFM - USAC

PROGRAM Monty_Paralelo
  USE Mpi
  IMPLICIT NONE
  
  !Variables principales
  INTEGER :: Puerta, Eleccion
  !Variables necesarias para la estadistica
  INTEGER :: Gana_p, Gana_c,Total_p,Total_c
  REAL(4) :: Prob_p, Prob_c
  
  INTEGER, DIMENSION (33) :: semilla
  
  !Variables auxiliares
  REAL(4) :: p1,p2,m 
  INTEGER(8) :: i,n
  !Variables auxiliares para Mpi
  INTEGER :: err, rank, size, emisor

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
  
  ! inicializando variables
  
  n = 1000000000 !Esta es la cantidad de iteraciones que hara cada nucleo, el maximo es 1000000000
  
  m = n*size
  
  semilla = 07052019 + (rank*7)

  CALL RANDOM_SEED (put = semilla)
  
  
  Gana_c = 0
  Gana_p = 0
  
  DO i=1,n
    !Esta parte simula la elección de la puerta correcta
  
    CALL RANDOM_NUMBER (p1) !Elección de la puerta correcta
    CALL RANDOM_NUMBER (p2) !Elección del concursante
  
    Puerta = INT(p1*3)
    Eleccion = INT(p2*3)
    
    !Esta parte calcula la probabilidad si no se cambia de puerta
    IF (Puerta == Eleccion) THEN
      Gana_p = Gana_p + 1
    !Esta parte calcula la probabilidad si la eleccion cambia
    !Si la puerta elegida originalmente es la premida entonces 
    !No es necesario considerar esa opicon en el cambio de puerta
    ELSEIF (Puerta /= Eleccion) THEN
      Gana_c = Gana_c + 1
    ENDIF 
  ENDDO

  PRINT *, 'soy ', rank,'semilla:', semilla (1)
  PRINT *, 'cantidad de juegos ganados p', Gana_p 
  PRINT *, 'cantidad de juegos ganados c', Gana_c

! Resivimos los calculos que hacen los otros nucleos

  IF (rank /= 0) THEN
    CALL MPI_SEND(Gana_c,1,Mpi_int, 0, 0, MPI_COMM_WORLD, err)
    CALL MPI_SEND(Gana_p,1,Mpi_int, 0, 0, MPI_COMM_WORLD, err)
  ELSE
    Total_c = Gana_c
    Total_p = Gana_p
    DO emisor = 1, size -1
      CALL MPI_RECV(Gana_c,1,Mpi_int, emisor, 0,MPI_COMM_WORLD,MPI_STATUS_IGNORE, err)
      CALL MPI_RECV(Gana_p,1,Mpi_int, emisor, 0,MPI_COMM_WORLD,MPI_STATUS_IGNORE, err)
      Total_c = Total_c + Gana_c
      Total_p = Total_p + Gana_p
    ENDDO
  ENDIF

  !Calculando la probabilidad
  IF ( rank == 0 ) THEN
    
    PRINT *, 'Numero de iteraciones: ',m
!    PRINT *, 'Juegos ganados totales p: ', Total_p
!    PRINT *, 'Juegos ganados totales c: ', Total_c
    Prob_p = (Total_p/(m/100.0))
    Prob_c = (Total_c/(m/100.0))
  
    PRINT *, 'La probabilidad de ganar si no se cambia es de: ', Prob_p,'%'
    PRINT *, 'La probabilidad de ganar si se cambia es de: ', Prob_c, '%'
  ENDIF  

  ! terminando MPI, esto deshabilita el entorno de trabajo
  CALL MPI_Finalize(err)
  IF (err.NE.0) STOP 'MPI_Init error'
  
END PROGRAM Monty_Paralelo
