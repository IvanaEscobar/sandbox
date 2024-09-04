subroutine f( n, x, y ) 
    implicit none
    integer , intent(in) :: n
    real , intent(in) :: x
    real , intent(out) :: y
    
    real :: h, a
    integer :: i
    
    a=x 
    h=1. 
    do i=1,n
        h = a * sin(h) 
    enddo
    y=h

end
