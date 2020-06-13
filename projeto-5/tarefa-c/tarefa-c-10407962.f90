program tarefac
    implicit real(8) (a-h,o-z)
    parameter (pi = 4d0*atan(1d0))

    dt = 0.001d0
    tmax = 60d0

    g = 9.8d0
    a_l = 9.8d0
    F0max = 4.0d0
    gama = 0.5d0
    Omega = 2d0/3d0
    
    write(*,*) 'Entre com o valor de F0:'
    read(*,*) F0

    open(10, file='saida-c-10407962.dat')

    theta_i1 = 0.2d0
    omega_i1 = 0d0
    theta_i2 = theta_i1 + 0.001d0
    omega_i2 = omega_i1
    i = 0

    do while ( i*dt.le.tmax )

        omega_i1 = omega_i1 +dw(g, a_l, theta_i1, gama, omega_i1, F0, Omega, dt*i)*dt
        theta_i1 = theta_i1 +omega_i1*dt
        omega_i2 = omega_i2 +dw(g, a_l, theta_i2, gama, omega_i2, F0, Omega, dt*i)*dt
        theta_i2 = theta_i2 +omega_i2*dt

        deltaTheta = abs(theta_i2-theta_i1)
            
        write(10, '(F0.6," ",F0.6)') i*dt, deltaTheta
        
        i = i+1
    end do

    F0 = F0 + 0.01d0

    close(10)

contains
    function dw(g, a_l, theta, gama, dTheta, F0, Omega, t)
        implicit real(8) (a-h,o-z)
        dw = -(g/a_l)*sin(theta) +gama*dTheta -F0*sin(Omega*t)
        return
    end function

end program tarefac
