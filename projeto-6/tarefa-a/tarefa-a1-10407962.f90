program tarefaa1
    implicit real(8) (a-h,o-z)
    character(70) filename
    character(10) c_planeta
    parameter (pi=4d0*atan(1d0))
    dimension ri(2), rmi(2), d2rdt2(2), v(2)

    n = 100
    Tt = 1d0
    a_Ms = 2d30

    open(20, file='entrada-a-10407962.dat')

    do j = 1, 9
        read(20,*) c_planeta, a_massa, a_raio, a_exce

        write(filename, '(A,A,A)') 'saida-a1-', trim(c_planeta), '-10407962.dat'

        Tp = Tt*sqrt(a_raio**3d0)
        dt = Tp/n
        GMs = ((2*pi)**2)*(a_raio**3)/(Tp**2)

        v0 = 2*pi*a_raio/Tp
        print *, v0
        v0 = sqrt(GMs)*sqrt((1/a_raio) * (1 + a_massa/a_Ms))
        print *, v0
        v = (/0d0, v0 /)

        write(*,*) c_planeta, dnorm(v), Tp/Tt
    
        rmi = (/ a_raio, 0d0 /)
        ri = rmi +v*dt

        open(10, file=filename)

        write(10, *) ri

        do i = 1, n+1
            d2rdt2 = -GMs*ri/dnorm(ri)**3

            call verlet(ri, rmi, d2rdt2, dt)

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

    function ac_gravit(G, aM, r)
        implicit real(8) (a-h,o-z)
        dimension r(2), ac_gravit(2)

        ac_gravit = G*aM*r/dnorm(r)**3
        return
    end function

    subroutine verlet(ri, rmi, d2rdt2, dt)
        implicit real(8) (a-h,o-z)
        dimension ri(2), rmi(2), raux(2), d2rdt2(2)

        raux = ri
        ri = 2*ri - rmi + d2rdt2*dt**2
        rmi = raux
        
    end subroutine

end program tarefaa1
