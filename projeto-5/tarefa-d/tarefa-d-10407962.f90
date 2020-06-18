program tarefac
    implicit real(8) (a-h,o-z)

    dt = 0.001d0
    tmax = 240d0

    g = 9.8d0
    a_l = 9.8d0
    gama = 0.5d0
    Omega = 2d0/3d0
    
    write(*,*) 'Entre com o valor de F0:'
    read(*,*) F0

    open(10, file='saida-d-10407962.dat')

    theta_i = 0.2d0
    omega_i = 0d0
    i = 0
    n = 0
    a_lyapunov = 0d0

    do while ( i*dt.le.tmax )

        dw_i = dw(g, a_l, theta_i, gama, omega_i, F0, Omega, dt*i)
        call oscilacao(theta_i, omega_i, dw_i, dt)
        
        write(10, *) theta_i, omega_i
        
        i = i+1
    end do

    close(10)
    
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
