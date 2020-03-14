program transfer
    implicit none
    integer i, j
    real, dimension(3,2) :: A
    real, dimension(2,3) :: B
    do i=1,3
        do j=1,2
            read(*,*) A(i, j)
        end do
    end do
    do i=1,2
        do j=1,3
            B(i, j) = A(j, i)
        end do
    end do
    do i=1,2
        write(*,*) B(i,:)
    end do
end program transfer