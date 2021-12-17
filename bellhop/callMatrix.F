      program call_sub
        implicit none
      
        real :: mat(4, 3)
      
        mat(:,:) = 0.0
      
        call print_matrix(4, 3, mat)
      
      end program call_sub
