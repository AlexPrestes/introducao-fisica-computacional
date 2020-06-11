program tarefab3
    implicit real(8) (a-h,o-z)
    parameter (pi = 4d0*atan(1d0))

    dt = 1d-4
    tmax = 30d0
    i = 0

    g = 9.8d0
    a_l = 9.8d0
    theta_i = pi/6
    omega_i = 0d0
    F0 = 0d0
    gama = 0.5d0
    Omega = 0d0
    
    open(10, file='saida-b3-10407962.dat')
    
    write(10,'(F0.6,2(" ",F0.6))') dt*i, theta_i

    do while ( i*dt.le.tmax )
        
        omega_j = omega_i +dw(g, a_l, theta_i, gama, omega_i, F0, Omega, dt*i)*dt
        theta_j = theta_i +omega_j*dt

        theta_i = theta_j
        omega_i = omega_j
        
        write(10,'(F0.6,2(" ",F0.6))') dt*i, theta_i
        
        i = i+1
    end do

    close(10)

    freqnat = sqrt(g/a_l)
    write(*,'(A,F8.6)') 'Freq. Natural: ', freqnat
    write(*,'(A,F8.6)') 'gamma/2: ', gama/2

contains
    function dw(g, a_l, theta, gama, dTheta, F0, Omega, t)
        implicit real(8) (a-h,o-z)
        dw = -(g/a_l)*sin(theta) -gama*dTheta +F0*sin(Omega*t)
        return
    end function

end program tarefab3
