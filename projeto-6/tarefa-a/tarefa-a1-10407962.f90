program tarefaa1
    implicit real(8) (a-h,o-z)
    dimension r(2), v(2)

    r = (/ 0d0, 0d0 /) ! posição Terra
    v = (/ 0d0, 0d0 /) ! velocidade Terra

contains
    subroutine verlet(ri, rmi, d2rdt2, dt)
        implicit real(8) (a-h,o-z)

        raux = ri
        ri = 2*ri - rmi + d2rdt2*dt**2
        rmi = raux
        
        return
    end subroutine

end program tarefaa1
