! 2020 - 15 - 4
! Hanoi.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! Este programa calculca la Función gamma para un numero z
! Por el metodo del limite hasta n = 10000

! Codificación del texto: ASCII text
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008
! Instrucciones de compilación: no requiere nada más
! gfortran -Wall -pedantic -std=f95 Hanoi.f90 -o Hanoi
! ./Hanoi

! para su caracterización
! /usr/bin/time -f "%e, %M, %P," ./Hanoi

PROGRAM Hanoi
  IMPLICIT NONE
  
  ! Definimos variables importantes
  CHARACTER(*), PARAMETER :: A='A', B='B', C='C'
  ! Variables auxiliares
  INTEGER(4):: n, paso
     
  n = 3
  
  paso = 1

  ! Abrimos archivo de instrucciónes
  OPEN(5, file ='Instrucciones.txt', status='new')
  WRITE(5,*) 'Instrucciónes para completar las torres de Hanoi',n,'discos'
  PRINT *, 'Generando instrucciónes para',n, 'torres'
  CALL mover(paso,n,A,B,C)
  
  CLOSE(5)
  
  CONTAINS
    RECURSIVE SUBROUTINE mover (paso,n,origen,aux,destino)
      IMPLICIT NONE
      INTEGER(4), INTENT(INOUT) :: paso
      INTEGER(4), INTENT(IN) :: n
      CHARACTER(*), INTENT(IN):: origen,aux,destino
      
      IF (n==1) THEN
        WRITE(5,*) paso,' Mover de ',origen,' a ',destino
        paso = paso + 1
      ELSE
        CALL mover(paso, n-1, origen,destino,aux)
        CALL mover(paso, 1, origen, aux, destino)
        CALL mover(paso, n-1, aux, origen, destino)
      END IF
    END SUBROUTINE mover
END PROGRAM Hanoi
