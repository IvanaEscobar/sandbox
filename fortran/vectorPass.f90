










PROGRAM vectorPass
    use mpi
    implicit none

    integer :: myThid, num_procs, n, i, tag
    integer, parameter :: n_elements=20
    real(8) :: vectorA(n_elements), vectorB(n_elements), vectorOut(n_elements)
    integer :: ierr, status(MPI_STATUS_SIZE)

    call MPI_INIT(ierr)
    call MPI_COMM_RANK( MPI_COMM_WORLD, myThid, ierr )
    call MPI_COMM_SIZE( MPI_COMM_WORLD, num_procs, ierr )

    do i = 1, n_elements
        vectorA(i) = i * 1.0
        vectorB(i) = i * 10.0
    enddo

    do i = 1, n_elements
        vectorOut(i) = vectorA(i) + vectorB(i)
    enddo

    do n = 1,num_procs
        print *, "myThid = ", n
        do i = 1,n_elements
            print *, vectorOut(i)
        enddo
    enddo

    call MPI_BARRIER( MPI_COMM_WORLD, ierr )

    if (myThid == 0) then
        print *, "vector out:" 
        do i = 1,n_elements
            print *, vectorOut(i)
        enddo
    endif

    call MPI_FINALIZE(ierr)

END PROGRAM vectorPass
