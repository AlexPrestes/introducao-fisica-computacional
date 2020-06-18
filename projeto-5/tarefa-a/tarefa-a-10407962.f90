program tarefaa
    implicit real(8) (a-h,o-z)
    parameter (pi = 4d0*atan(1d0))

    g = 9.8d0
    a_l = 9.8d0
    a_m = 1d0
    dt = 1d-3
    tmax = 120d0
    omega_i = 0d0
    theta_i = pi/15
    i = 0

    open(10, file='saida-a1-10407962.dat')

    do while ( i*dt.le.tmax )
        
        theta_i = mod(theta_i, 2*pi)

        omega_j = omega_i - g*theta_i*dt/a_l
        theta_j = theta_i + omega_i*dt

        omega_i = omega_j
        theta_i = theta_j

        Ec = 0.5*a_m*(omega_i*a_l)**2
        Ep = a_m*g*a_l*(1-cos(theta_i))

        write(10, '(F0.16,3(" ",F0.16))') i*dt, theta_i, Ec+Ep
        i = i+1
    end do

    close(10)
    

    omega_i = 0d0
    theta_i = pi/15
    i = 0
    
    open(10, file='saida-a2-10407962.dat')
    
    do while ( i*dt.le.tmax )
        
        theta_i = mod(theta_i, 2*pi)

        omega_i = omega_i - g*theta_i*dt/a_l
        theta_i = theta_i + omega_i*dt

        Ec = 0.5*a_m*(omega_i*a_l)**2
        Ep = a_m*g*a_l*(1-cos(theta_i))

        write(10, '(F0.16,2(" ",F0.16))') i*dt, theta_i, Ec+Ep
        i = i+1
    end do

    close(10)
end program tarefaa
