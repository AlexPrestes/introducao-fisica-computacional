program tarefaa
    implicit real*8 (a-h,o-z)

    write(*,*) 'Escreva o valor de x:'
    read(*,*) x

    do i = 2, 10
        
        h = 1d0/2d0**i
        write(*,*) h, dfrente2p(f, x, h)-df(x), dtras2p(f, x, h)-df( x), dsimetrica3p(f, x, h)-df(x), dsimetrica5p(f, x, h)-df(x)

    end do

contains

    function f(x)
        f = dsinh(2*x)*dsin(x/4)
        return
    end function

    function df(x)
        df = 2*dcosh(2*x)*dsin(x/4) + dcos(x/4)*dsinh(2*x)/4
        return
    end function

    function dfrente2p(f, x, h)
        dfrente2p = ( f(x+h) - f(x) ) / h
        return
    end function

    function dtras2p(f, x, h)
        dtras2p = ( f(x) - f(x-h) ) / h
        return
    end function

    function dsimetrica3p(f, x, h)
        dsimetrica3p = ( f(x+h) - f(x-h) ) / ( 2*h )
        return
    end function

    function dsimetrica5p(f, x, h)
        dsimetrica5p = ( f(x-2*h) -8*f(x-h) +8*f(x+h) -f(x-2*h) ) / ( 12*h )
        return
    end function

end program tarefaa
