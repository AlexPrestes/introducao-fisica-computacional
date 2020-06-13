program tarefab4
    implicit real(8) (a-h,o-z)
    character(70) filename
    parameter (pi = 4d0*atan(1d0))
    dimension F0(3)

    dt = 0.03d0
    tmax = 480d0

    g = 9.8d0
    a_l = 9.8d0
    F0 = (/0d0, 0.5d0, 1.2d0/)
    gama = 0.5d0
    Omega = 2d0/3d0
        
    do j = 1,3
        theta_i = 0.2d0
        omega_i = 0d0
        i = 0

        write(filename, '(A,I0,A)') 'saida-b4-', j, '-10407962.dat'
        
        open(10, file=filename)

        write(10,'(F0.6,2(" ",F0.6))') dt*i, theta_i, omega_i

        do while ( i*dt.le.tmax )

            omega_i = omega_i +dw(g, a_l, theta_i, gama, omega_i, F0(j), Omega, dt*i)*dt
            theta_i = theta_i +omega_i*dt
            if (abs(theta_i).ge.pi) then
                theta_i = -theta_i
            end if

            write(10,'(F0.6,2(" ",F0.6))') dt*i, theta_i, omega_i
        
            i = i+1
        end do

        close(10)

    end do

contains
    function dw(g, a_l, theta, gama, dTheta, F0, Omega, t)
        implicit real(8) (a-h,o-z)
        dw = -(g/a_l)*sin(theta) -gama*dTheta +F0*sin(Omega*t)
        return
    end function

end program tarefab4
