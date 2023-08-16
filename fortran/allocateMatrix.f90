program allocateMatrix

    implicit none 
    integer, allocatable :: array(:,:)

    integer :: rows, cols, iallocstat, i,j

    rows=2
    cols=4
    
    ALLOCATE( array(rows, cols), stat=iallocstat )
    if ( iallocstat /= 0 ) then
        print *, "not allocated"
        stop
    endif


    do i=1,rows
        do j=1,cols
            array(i,j) = i+j
        enddo
    enddo

    do i=1,rows
        do j=1,cols
            print *, array(i,j)
        enddo
        print *
    enddo

    DEALLOCATE( array )
end program allocateMatrix
