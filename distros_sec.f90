! 2020 - 15 - 4
! distros_sec.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! Este programa genera numeros aleatorios siguiendo la distribución 
! Exponencial y la distribución normal, por el método inverso.

! Codificación del texto: ASCII text
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008
! Instrucciones de compilación: no requiere nada más
! gfortran -Wall -pedantic -std=f95 distros_sec.f90 -o distros1
! ./distros1

! para su caracterización
! /usr/bin/time -f "%e, %M, %P," ./distros1

PROGRAM distros
  IMPLICIT NONE
  ! Declaramos variables importantes
  REAL (4) :: exp1, nor1, uni1
  
  ! Declaramos variables auxiliares
  INTEGER (4) :: i, n
  REAL (4) :: delta
  INTEGER, DIMENSION(33) :: semilla
  
  ! Determinamos la cantida de numeros que queremos generar
  n = 50000
  
  ! iniciamos variables auxiliares
  delta = 3
   
  ! Abrimos archivos donde se guardaran los datos
  OPEN (1, file = 'ditribuciones.dat',status='new')
  WRITE (1,*) '# Resultados de distribuciónes exponencial y potencial'
  WRITE (1,*) 'x1      exp(x1)      norm(x1)'
  
  ! Generamos la semilla prinipal
  semilla = 23052020
  CALL RANDOM_SEED(put = semilla)
  
  PRINT *, 'Generando', n, 'números aleatorios'
  
  
  ! Este cilco generará todos los numeros aleatorios.
  DO i=1, n
    
    ! Primero generamos el numero aleatorio de distribución uniforme
    CALL RANDOM_NUMBER(uni1)
    
    ! Generamos el número aleatorio de distribución exponencial
    exp1 = -(1/delta)*LOG(uni1)
       
    ! Generamos el número aleatorio de distribución normal
    nor1 = SQRT(LOG(1/(uni1**2)))
    
    ! Guardamos todos los resultados en el archivo
    WRITE (1,*) uni1, exp1, nor1
    
  END DO
  
  PRINT *, 'Terminado'
  CLOSE(1)

ENDPROGRAM distros











