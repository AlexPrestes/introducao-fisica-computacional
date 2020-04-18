program tarefaa
    implicit real*8 (a-h,o-z)

    dimension h(14)

    ! Funções exatas
    f(x) = dsinh(2*x)*dsin(x/4)
    df(x) = 2*dcosh(2*x)*dsin(x/4) + dcos(x/4)*dsinh(2*x)/4
    d2f(x) = dcos(x/4)*dcosh(2*x) + 63*dsin(x/4)*dsinh(2*x)/16
    d3f(x) = 61*dcosh(2*x)*dsin(x/4)/8 + 191*dcos(x/4)*dsinh(2*x)/64 

    ! Funções aproximaximadas
    dfrente2p(x, h_i) = ( f(x+h_i) - f(x) ) / h_i
    dtras2p(x, h_i) = ( f(x) - f(x-h_i) ) / h_i
    dsimetrica3p(x, h_i) = ( f(x+h_i) - f(x-h_i) ) / ( 2*h_i )
    dsimetrica5p(x, h_i) = ( f(x-2*h_i) -8*f(x-h_i) +8*f(x+h_i) -f(x+2*h_i) ) / ( 12*h_i )
    d2simetrica5p(x, h_i) = ( -f(x-2*h_i) +16*f(x-h_i) -30*f(x) +16*f(x+h_i) -f(x+2*h_i) ) / ( 12*h_i**2 )
    d3antisimetrica5p(x, h_i) = ( -f(x-2*h_i) +2*f(x-h_i) -2*f(x+h_i) +f(x+2*h_i) ) / ( 2*h_i**3 )

    ! vetor com os valor de h
    h = (/0.5, 0.2, 0.1, 0.05, 0.01, 0.005, 0.001, 0.0005, 0.0001, 0.00005, 0.00001, 0.000001, 0.0000001, 0.00000001/)

    ! 
    open(10, file='saida-a-10407962')

    write(*,*) 'Escreva o valor de x:'
    read(*,*) x

    df_x  = df(x)
    d2f_x = d2f(x)
    d3f_x = d3f(x)

    do i = 1, 14

        dfrente2p_x_h = dfrente2p(x, h(i))
        dtras2p_x_h = dtras2p(x, h(i))
        dsimetrica3p_x_h = dsimetrica3p(x, h(i))
        dsimetrica5p_x_h = dsimetrica5p(x, h(i))
        d2simetrica5p_x_h = d2simetrica5p(x, h(i))
        d3antisimetrica5p_x_h = d3antisimetrica5p(x, h(i))

        
        write(10,*) h(i), abs(dfrente2p_x_h-df_x), abs(dtras2p_x_h-df_x), abs(dsimetrica3p_x_h-df_x), &
                    abs(dsimetrica5p_x_h-df_x), abs(d2simetrica5p_x_h-d2f_x), abs(d3antisimetrica5p_x_h-d3f_x)
    end do

    write(*,'(A,F0.11)') 'df/dx: ', df_x
    write(*,'(A,F0.11)') 'd^2f/dx^2: ', d2f_x
    write(*,'(A,F0.11)') 'd^3f/dx^3: ', d3f_x

end program tarefaa
