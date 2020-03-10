program tarefa8

    real*16 :: Vd, VA, VB

    ! Definindo o valor de pi
    pi = 4e0*atan(1e0)
    
    ! Definindo unit para o arquivo de saida da tarefa 8
    open(unit=10, file='saida-8-10407962')

    ! Definindo unit para o arquivo de saida do item A
    open(unit=11, file='saida-8a-10407962')

    ! Definindo unit para o arquivo de saida do item B
    open(unit=12, file='saida-8b-10407962')

    ! Recebe o valor do raio
    print *, 'Digite o raio R:'
    read(*,*) R

    ! Recebe o valor da dimensão
    print *, 'Digite o número de dimensões d:'
    read(*,*) id
    
    ! loop que para mudar a dimensão de 2 até d
    do i = 2, id
        
        ! definindo arg e dgamma inicial
        ! aqui está sem as redundancia da tarefa-7
        arg = i/2e0
        dgamma = 1e0

        ! loop que calcula o dgamma
        do while (arg.gt.0e0)
            if (arg.ge.1e0) then
                dgamma = arg*dgamma
                arg = arg-1e0
            else
                dgamma = arg*sqrt(pi)*dgamma
                arg = 0e0
            end if
        end do
    
        ! calculo do volume d
        Vd = (pi**(i/2e0) * R**i) / dgamma

        ! escreve a dimensão e o volume no arquivo de saida
        write(10,'(I0,", ",F0.6)') i, Vd

        ! calcula e escreve a dimensão e a razão do volume do cudo raio 1 mili e esfera 1 mili
        VA = 1e0/Vd
        write(11, '(I0,", ",F0.6)') i, VA

        ! calcula e escreve a dimensão e a razão do volume do cubo raio 1 micro e esfera 1 angström
        VB = 1e0/(Vd*10e-4**i)
        write(12, '(I0,", ",F0.6)') i, VB

    end do

    ! fecha o arquivo de saida
    close(10)
    close(11)
    close(12)
    
end program tarefa8
