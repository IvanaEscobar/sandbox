program drive_f_ad
    implicit none
    real :: a, x, y, x_ad , y_ad
    
    ! initialisation
    read *, a, x
    print *, ' y = a xˆ4, with a, x =', a, x 
    x_ad=0.
    y_ad=1.

    ! evaluation of function and derivative
    call f_ad(a,x,x_ad,y,y_ad) ! postprocessing
    print *, ' values of f and df (reverse): ',y, x_ad
end
