program tarefaa
    implicit real*8 (a-h,o-z)

    f(x) = dsinh(2*x)*dsin(x/4)
    df(x) = 2*dcosh(2*x)*dsin(x/4) + dcos(x/4)*dsinh(2*x)/4

    dfrente2p(x, h) = ( f(x+h) - f(x) ) / h
    dtras2p(x, h) = ( f(x) - f(x-h) ) / h
    dsimetrica3p(x, h) = ( f(x+h) - f(x-h) ) / ( 2*h )
    dsimetrica5p(x, h) = ( f(x-2*h) -8*f(x-h) +8*f(x+h) -f(x-2*h) ) / ( 12*h )

    write(*,*) 'Escreva o valor de x:'
    read(*,*) x

    df_x = df(x)

    do i = 2, 10
        h = 1d0/2d0**i

        dfrente2p_x_h = dfrente2p(x, h)
        dtras2p_x_h = dtras2p(x, h)
        dsimetrica3p_x_h = dsimetrica3p(x, h)
        dsimetrica5p_x_h = dsimetrica5p(x, h)
        
        write(*,*) h, dfrente2p_x_h-df_x, dtras2p_x_h-df_x, dsimetrica3p_x_h-df_x, dsimetrica5p_x_h-df_x
    end do

end program tarefaa
