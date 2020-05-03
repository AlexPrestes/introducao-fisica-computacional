program tarefaa
    implicit real*8 (a-h,o-z)

    dimension h(14)

    ! Funções exatas
    f(x) = sinh(2*x)*sin(x/4)
    df(x) = 2*cosh(2*x)*sin(x/4) + cos(x/4)*sinh(2*x)/4
    d2f(x) = cos(x/4)*cosh(2*x) + 63*sin(x/4)*sinh(2*x)/16
    d3f(x) = 61*cosh(2*x)*sin(x/4)/8 + 191*cos(x/4)*sinh(2*x)/64 

    ! Funções aproximaximadas
    dfrente2p(x, h_i) = ( f(x+h_i) - f(x) ) / h_i
    dtras2p(x, h_i) = ( f(x) - f(x-h_i) ) / h_i
    dsimetrica3p(x, h_i) = ( f(x+h_i) - f(x-h_i) ) / ( 2*h_i )
    dsimetrica5p(x, h_i) = ( f(x-2*h_i) -8*f(x-h_i) +8*f(x+h_i) -f(x+2*h_i) ) / ( 12*h_i )
    d2simetrica5p(x, h_i) = ( -f(x-2*h_i) +16*f(x-h_i) -30*f(x) +16*f(x+h_i) -f(x+2*h_i) ) / ( 12*h_i**2 )
    d3antisimetrica5p(x, h_i) = ( -f(x-2*h_i) +2*f(x-h_i) -2*f(x+h_i) +f(x+2*h_i) ) / ( 2*h_i**3 )

    ! vetor com os valor de h
    h = (/0.5, 0.2, 0.1, 0.05, 0.01, 0.005, 0.001, 0.0005, 0.0001, 0.00005, 0.00001, 0.000001, 0.0000001, 0.00000001/)

    ! Arquivo de saida
    open(10, file='saida-a-10407962')

    ! x para calcular a derivada
    x = 1d0

    ! valor de cada derivada analitica
    df_x  = df(x)
    d2f_x = d2f(x)
    d3f_x = d3f(x)

    ! loop para alterar o valor de h
    do i = 1, 14

        ! desvio de cada método
        erro1 = abs(dfrente2p(x, h(i)) - df_x)
        erro2 = abs(dtras2p(x, h(i)) - df_x)
        erro3 = abs(dsimetrica3p(x, h(i)) - df_X)
        erro4 = abs(dsimetrica5p(x, h(i)) - df_x)
        erro5 = abs(d2simetrica5p(x, h(i)) - d2f_x)
        erro6 = abs(d3antisimetrica5p(x, h(i)) - d3f_x)
        
        ! escreve no arquivo de saida
        write(10,*) h(i), erro1, erro2, erro3, erro4, erro5, erro6
    end do

    ! escreve na tela os valores exatos
    write(*,'(A,F0.11)') 'df/dx: ', df_x
    write(*,'(A,F0.11)') 'd^2f/dx^2: ', d2f_x
    write(*,'(A,F0.11)') 'd^3f/dx^3: ', d3f_x

end program tarefaa
