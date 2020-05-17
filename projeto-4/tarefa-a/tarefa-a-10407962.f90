program tarefaa
    implicit real*8 (a-h,o-z)

    parameter (pi = 4d0*datan(1d0))

    real(8), dimension(2) :: x, v, a

    x = (/0d0, 100d0/)
    v0 = 10d0
    theta = pi/4
    v = v0*(/dcos(theta), dsin(theta)/)
    a = (/0d0, -10d0/)
    h = 0.01

    do while (x(2).ge.0d0)
        v = v + h*a
        x = x + h*v
        print *, x
    end do

end program tarefaa
