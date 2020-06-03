program tarefab1
    implicit real(8) (a-h,o-z)
    parameter (pi=4d0*atan(1d0))

    g = 9.8d0
    a_l = 9.8d0
    a_m = 1d0
    dt = 1d-4
    tmax = 120d0
    e = 1e-4
    
    open(10, file='saida-b1-10407962.dat')

    do j = 1, 10
        omega_i = 0d0
        theta_0 = rand()
        theta_i = theta_0
        i = 0
        t0 = 0d0
        n = 0
        sumT = 0d0

        do while ( i*dt.le.tmax )
        
            omega_j = omega_i - g*theta_i*dt/a_l
            theta_j = theta_i + omega_j*dt

            if (omega_i*omega_j.lt.0) then
                if (t0.ge.0d0) then
                    sumT = sumT + i*dt-t0
                    n = n + 1
                end if
                t0 = i*dt
            end if

            theta_i = theta_j
            omega_i = omega_j
        
            i = i+1
        end do
        sumT = 2*sumT/n
        solucaoNum = simpson(f, -theta_0+e, theta_0-e, i, theta_0)
        solucaoAna = analitico(theta_0, a_l, g, e)
        T = 2*pi*sqrt(a_l/g)

        write(10,'(F0.6,3(" ",F0.6))') theta_0, sumT, solucaoNum + solucaoAna, T
    end do

    close(10)

contains
    function f(theta, theta_0)
        implicit real(8) (a-h,o-z)
        g = 9.8d0
        a_l = 9.8d0
        f = sqrt(2*a_l/g)/sqrt(cos(theta)-cos(theta_0))
        return
    end function

    function analitico(theta_0, a_l, g, e)
        implicit real(8) (a-h,o-z)
        A = sqrt(2*a_l/g)*sqrt(e)*0.5/sqrt(theta_0)
        analitico = 2*A
        return
    end function

    function simpson(f, a, b, n, y)
        implicit real(8) (a-h,o-z)
        h = (b-a)/n
        simpson = 0d0

        do i = 1, n-1, 2
            x = a + i*h
            simpson = simpson + ( f(x -h, y) +4*f(x, y) +f(x +h, y) ) * h/3
        end do

        return
    end function simpson

end program tarefab1
