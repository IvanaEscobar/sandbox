program drive_f_tl
    implicit none
    real :: a, x, y, x_tl , y_tl
    
    ! initialisation
    read *, a, x
    print *, ' y = a xˆ4, with a, x =', a, x 
    x_tl=1.

    ! evaluation of function and derivative
    call f_tl(a,x,x_tl,y,y_tl) ! postprocessing
    print *, ' values of f and df (forward): ',y, y_tl 
end
