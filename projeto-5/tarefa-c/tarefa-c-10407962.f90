program tarefac
    implicit real(8) (a-h,o-z)

    dt = 0.001d0
    tmax = 120d0

    g = 9.8d0
    a_l = 9.8d0
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
    n = 0
    a_lyapunov = 0d0

    do while ( i*dt.le.tmax )

        dw_i1 = dw(g, a_l, theta_i1, gama, omega_i1, F0, Omega, dt*i)
        call oscilacao(theta_i1, omega_i1, dw_i1, dt)
        
        dw_i2 = dw(g, a_l, theta_i2, gama, omega_i2, F0, Omega, dt*i)
        call oscilacao(theta_i2, omega_i2, dw_i2, dt)

        deltaTheta = abs(theta_i2-theta_i1)
        a_lyapunov_i = log(deltaTheta)/(i*dt)

        if (((i*dt).gt.(tmax/2)).and.(abs(a_lyapunov_i).lt.9d300)) then
            a_lyapunov = a_lyapunov + a_lyapunov_i
            n = n + 1
        end if
            
        write(10, *) i*dt, deltaTheta
        
        i = i+1
    end do

    close(10)
    
    print *, F0, a_lyapunov/n

contains
    function dw(g, a_l, theta, gama, dTheta, F0, Omega, t)
        implicit real(8) (a-h,o-z)
        dw = -(g/a_l)*sin(theta) -gama*dTheta +F0*sin(Omega*t)
        return
    end function

    subroutine oscilacao(theta_i, omega_i, dw_i, dt)
        implicit real(8) (a-h,o-z)
        parameter (pi = 4d0*atan(1d0))
        omega_i = omega_i +dw_i*dt
        theta_i = theta_i +omega_i*dt

        do while (abs(theta_i).ge.pi)
            sinal = theta_i/abs(theta_i)
            theta_i = theta_i -sinal*2*pi
        end do
        return
    end subroutine

end program tarefac
