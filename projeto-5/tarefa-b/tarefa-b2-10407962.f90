program tarefab2
    implicit real(8) (a-h,o-z)

    g = 9.8d0
    a_l = 9.8d0
    
    open(10, file='saida-b2-10407962.dat')

    do j = 1, 20
        theta_0 = rand()
        Tb2 = periodob2(g, a_l, theta_0)

        write(10,'(F0.8," ",F0.8)') theta_0, Tb2
    end do

    close(10)

contains
    function periodob2(g, a_l, theta_0)
        implicit real(8) (a-h,o-z)
        parameter (pi = 4d0*atan(1d0))
        periodob2 = 2*pi*sqrt(a_l/g)*(1 + theta_0**2/16d0)
        return
    end function
end program tarefab2
