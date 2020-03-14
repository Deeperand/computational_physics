! SOR method
program main
    implicit none
    integer, parameter :: N = 100
    real(8), parameter :: myeps = 1.D-4
    real(8), dimension(0:N) :: V, rho
    integer i, j, iter 
    real(8) h, diff, myerr, V_GS, omega

    h = 1.D0 / N
    V(0:N) = 0.D0
    do i = 1, N-1
        rho(i) = -12.D0 * (i * h) ** 2.D0
    end do

    do iter = 1, 5000
        myerr = 0.D0
        do i = 1, N-1
            V_GS = 0.5D0 * (V(i-1) + V(i+1) - h*h*rho(i))
            diff = V_GS - V(i)
            V(i) = V(i) + omega * diff
            myerr = myerr + abs(diff)
        end do
        if (mod(iter, 20) == 1) write(*, *) iter, myerr
        if (myerr <= myeps) then
            exit
        end if
    end do 
end program main