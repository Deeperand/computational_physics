program main
    implicit none
    integer :: N = 10**7, i
    real(8), dimension(1:4) :: x
    real(8) :: integrate = 0.D0, ref

    open(unit = 22, file = 'EX_8_data.txt')
    write(22,*) 'Monto Carlo & Reference \\'
    write(*,'(2A12)') 'Monto Carlo', 'Reference'
    ref = 1/2.D0 + 1/4.D0 + 1/6.D0 + 1/8.D0
    do i = 1, N
        call random_number(x)
        x = dble(x)
        integrate = integrate + x(1) + x(2)**3 + x(3)**5 + x(4)**7
    end do
    integrate = integrate / N
    write(*,'(2F12.5)') integrate, ref
    write(22,'(F12.5, A2, F12.5)') integrate, '&' ,ref 
end program main
