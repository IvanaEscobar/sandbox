program simple
    implicit none
    real :: a,x,y

    a=2
    x=1.4

    call f(a,x,y)
    print '(F10.3)', y
end

subroutine f( a,x,y)
    implicit none
    real, intent(in ) :: a,x
    real, intent(out) :: y

    y=a*x**4
end
