program tarefab
    implicit real*8 (a-h,o-z)

    open(10, file='saida-b-10407962')

    a = 0d0
    b = 1d0

    exato = fzao(b)-fzao(a)
    
    do j = 2, 12
        n = 2**j
        desvio_trapezio = abs(exato - trapezio(f, a, b, n))
        desvio_simpson = abs(exato - simpson(f, a, b, n))
        desvio_bode = abs(exato - bode(f, a, b, n))
        write(10,*) (b-a)/n, desvio_trapezio, desvio_simpson, desvio_bode
    end do

    close(10)

contains

    function f(x)
        parameter (pi = 4*datan(1d0))
        f = dexp(x/2)*dcos(pi*x)
        return
    end function f

    function fzao(x)
        parameter (pi = 4*datan(1d0))
        fzao = (2*dexp(x/2)*(dcos(pi*x) + 2*pi*dsin(pi*x))) / (1 + 4*pi**2)
        return
    end function fzao

    function trapezio(f, a, b, n)
        h = (b-a)/n
        trapezio = 0d0

        do i = 1, n-1, 2
            x = a + i*h
            trapezio = trapezio + ( f(x -h) +2*f(x) +f(x +h) ) * h/2
        end do

        return
    end function trapezio

    function simpson(f, a, b, n)
        h = (b-a)/n
        simpson = 0d0

        do i = 1, n-1, 2
            x = a + i*h
            simpson = simpson + ( f(x -h) +4*f(x) +f(x +h) ) * h/3
        end do

        return
    end function simpson

    function bode(f, a, b, n)
        h = (b-a)/n
        bode = 0d0

        do i = 0, n-4, 4
            x = a + i*h
            bode = bode + ( 7*f(x) +32*f(x +h) +12*f(x +2*h) +32*f(x +3*h) +7*f(x +4*h) ) * 2*h/45
        end do

        return
    end function bode

end program tarefab
