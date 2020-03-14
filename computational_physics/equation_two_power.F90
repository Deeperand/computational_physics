program equation_two_power
    implicit none
    real, parameter :: myeps = 1.0E-4
    real a, b, c, delta, result_1, result_2
    read(*,*) a, b, c
    delta = b**2 - 4*a*c
    if ( delta > myeps ) then
        result_1 = 0.5*(-b + sqrt(delta))/a
        result_2 = 0.5*(b + sqrt(delta))/a
        write(*,*) 'two value', result_1, result_2
    else if( delta < -myeps) then
        write(*,*) 'no real root'
    else
        result_1 = 0.5*(-b)/a
        write(*,*) 'one root'
    end if
end program equation_two_power