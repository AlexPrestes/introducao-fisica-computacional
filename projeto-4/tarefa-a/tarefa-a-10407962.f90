program tarefaa
    character(70) filename
    dimension v0(2), e0(3)

    e0 = (/0.01, 0.001, 0.0001/)
    v0 = (/0.0, 10.0/)

    do i = 1, 2
        do j = 1, 3
        
            ! Parametros iniciais
            r = 100e0       ! Posição
            v = v0(i)       ! Velocidade
            g = 10e0        ! Aceleração
            t = 0e0         ! Tempo
            e = e0(j)       ! Incremento temporal

            write(filename,'(A,2(I0,A))') 'saida-a-v',i,'-e',j,'-10407962.dat'

            open(10, file=filename)

            do while (r.ge.0)
                write(10, '(F0.6,2(" ",F0.6))') t, r, v
                a = -g
                v = v + e*a
                r = r + e*v
                t = t + e
            end do

            close(10)
        end do
    end do

end program tarefaa
