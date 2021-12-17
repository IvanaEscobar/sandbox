! ------------------------------------------------------
! Compute the area of a triangle using Heron's formula
! ------------------------------------------------------

      PROGRAM  HeronFormula
         USE, INTRINSIC :: iso_fortran_env, ONLY: dp=>real64
         IMPLICIT NONE
      
         
         REAL(dp)     b, c            ! three sides
         INTEGER  a
         REAL     s                   ! half of perimeter
         REAL     Area                ! triangle area
         LOGICAL  Cond_1, Cond_2      ! two logical conditions

         ! Arrays
         REAL(dp) arr(10)
      
         READ(*,*)  a, b, c
      
         WRITE(*,*)  "a = ", a
         WRITE(*,*)  "b = ", b
         WRITE(*,*)  "c = ", c
         WRITE(*,*)
      
         Cond_1 =   (a > 0.) .AND. (b > 0.)
     &              .AND. (c > 0.)
         Cond_2 =   (a + b > c) .AND. (a + c > b) 
     &              .AND. (b + c > a)
      
         IF (Cond_1 .AND. Cond_2) THEN
            s    = (a + b + c) / 2.0
            Area = SQRT(s * (s - a) * (s - b) * (s - c))
            WRITE(*,*) "Triangle area = ", Area
         ELSE
            WRITE(*,*) "ERROR: this is not a triangle!"
         END IF
      
      END PROGRAM  HeronFormula
