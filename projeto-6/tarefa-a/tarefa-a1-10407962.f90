program tarefaa1
    implicit real(8) (a-h,o-z)
    character(70) filename
    character(10) dplaneta
    parameter (pi=4d0*atan(1d0))
    dimension ri(2), rmi(2), d2rdt2(2), v(2)

    dt = 1d0

    Tt = 365

    open(20, file='entrada-a1-10407962.dat')

    do j = 1, 9
        read(20,*) dplaneta, dmassa, draio, dexce

        write(filename, '(A,A,A)') 'saida-a1-', trim(dplaneta), '-10407962.dat'

        Tp = Tt*sqrt(draio**3d0)

        v = (/0d0, 2*pi*draio/Tp /)

        write(*,*) dplaneta, dnorm(v)*Tt, Tp/Tt
    
        rmi = (/ draio, 0d0 /)
        ri = rmi +v*dt

        GMs = ((2*pi)**2)*(draio**3)/(Tp**2)

        open(10, file=filename)

        write(10, *) ri

        do i = 1, int(Tp)+1
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
