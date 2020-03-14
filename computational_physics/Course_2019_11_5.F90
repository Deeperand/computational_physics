program main
    implicit none
    real h, tau, r
    real :: Tmax = 0.5
    real, parameter :: pi = 4.0 * atan(1.0)
    integer, parameter :: M = 50, N = 200
    real, dimension(0:M) :: xpos
    real, dimension(0:N) :: time_array
    real, dimension(0:M, 0:N) :: u
    integer i, j

    h = 1.0/M ! segment
    u(0, 0:N) = 0.0
    tau = Tmax / N
    u(0, 0:M) = 0.0
    do i = 0, M ! space lattice
        xpos(i) = i * h
        u(i, 0) = sin(pi*xpos(i))
    end do
    do j = 0, N ! time lattice
        time_array(j) = j* tau
    end do
    r = tau / h**2
    write(*,*) h, tau, r

    do j = 0, N-1
        do i = 1, M - 1
            u(i, j+1) = (1.0 - 2.0*r) * u(i, j) + r*(u(i-1,j) + u(i+1, j))
        end do
    end do

    open(unit = 20, file = 'u.dat')
    do j = 0, N-1
        do i = 1, M - 1
            write(20, *) xpos(i), j, u(i, j)
        end do
        write(20, *)
    end do
    close(20)
end program main