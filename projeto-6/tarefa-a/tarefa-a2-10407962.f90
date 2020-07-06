program tarefaa2
    implicit real(8) (a-h,o-z)
    character(70) c_filename
    character(10) c_planeta
    parameter (pi=4d0*atan(1d0))
    dimension ri(2), rmi(2), d2rdt2(2), v(2)

    n = 5d4

    open(20, file='entrada-a2-10407962.dat')

    do j = 1, 9
        read(20,*) c_planeta, a_Mp, a_raio, a_exce, v0

        write(c_filename, '(A,A,A)') 'saida-a2-', trim(c_planeta), '-10407962.dat'

        Tp = sqrt(a_raio**3d0)
        dt = Tp/n
        GMs = 4*pi**2

!        v0 = sqrt(GMs*(1-a_exce)/(a_raio*(1+a_exce)))
        v = (/0d0, v0 /)

        write(*,*) c_planeta, Tp**2/a_raio**3, v0

        ri = (/ a_raio*(1+a_exce), 0d0 /)
        rmi = ri +v*dt

        open(10, file=c_filename)

        write(10, *) ri

        do i = 1, n
            d2rdt2 = -GMs*ri/dnorm(ri)**3

            call verlet(rmi, ri, d2rdt2, dt)

            write(10, *) ri
        end do

        close(10)
    end do

contains
    function dnorm(r)
        implicit real(8) (a-h,o-z)
        dimension r(2)
        dnorm = sqrt(r(1)**2 + r(2)**2)
        return
    end function

    subroutine verlet(ri, rmi, d2rdt2, dt)
        implicit real(8) (a-h,o-z)
        dimension ri(2), rmi(2), raux(2), d2rdt2(2)

        raux = ri
        ri = 2*ri - rmi + d2rdt2*dt**2
        rmi = raux
    end subroutine

end program tarefaa2
