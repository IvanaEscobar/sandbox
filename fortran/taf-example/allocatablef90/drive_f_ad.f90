program drive_f_ad
    implicit none
    integer :: i, ii
    real (kind=8) :: a
    real (kind=8), allocatable :: x(:), y(:), x_ad(:), y_ad(:)
    
    ! reset memory if already allocated
    if (allocated(x)) DEALLOCATE(x)
    if (allocated(y)) DEALLOCATE(y)
    if (allocated(x_ad)) DEALLOCATE(x_ad)
    if (allocated(y_ad)) DEALLOCATE(y_ad)

    read *, a, i
    print *, ' y = a*xˆ4, with a, i =', a, i 

    ALLOCATE( x(i), y(i) )
    ALLOCATE( x_ad(i), y_ad(i) )

    ! set x to some values
    do ii = 1,i
        x(ii) = ii + i
    end do

    ! initialisation
    x_ad=0.
    y_ad=1.


    ! evaluation of function and derivative
    call f_ad(a,x,x_ad,y,y_ad,i) ! postprocessing
    print *, ' values of f: ',y
    print *, ' and df (reverse): ',x_ad
end
