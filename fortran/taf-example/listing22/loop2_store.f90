subroutine f( n, x, y ) 
    implicit none
    integer , intent(in) :: n
    real , intent(in) :: x
    real , intent(out) :: y
    
    real :: h, a
    integer :: i
    
!$TAF INIT tape_h = 'intermediate'
    a=x 
    h=1. 
    do i=1,n
!$TAF STORE h = tape_h
        h = a * sin(h) 
    enddo
    y=h

end
