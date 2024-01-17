program simple
    implicit none
    integer :: i, ii
    real (kind=8) :: a
    real (kind=8), ALLOCATABLE :: x(:), y(:)

    i=3
    a=0.1

    ! reset memory if already allocated
    if (allocated(x)) DEALLOCATE(x)
    if (allocated(y)) DEALLOCATE(y)

    ALLOCATE( x(i), y(i) )

    ! set x to some values
    do ii = 1,i
        x(ii) = ii + i
    end do

    ! evaluate fc output
    call f(a,x,y, i)
    print '(10F10.3)', y
end

subroutine f( a,x,y,i )
    implicit none
    integer :: i,ii
    real (kind=8) , intent(in ) :: a,x(i)
    real (kind=8) , intent(out) :: y(i)

    do ii = 1,i
        y(ii)=a*x(ii)**4
    end do
end
