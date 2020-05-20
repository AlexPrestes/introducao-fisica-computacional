program tarefab
    implicit real(8) (a-h,o-z)
    character(70) filename
    dimension v0(2), gama0(2)

    v0 = (/0.0, 10.0/)
    gama0 = (/0.1, 0.01/)

    do i = 1, 2
        do j = 1, 2
        
            ! Parametros iniciais
            r = 100e0           ! Posição
            v = v0(i)           ! Velocidade
            g = 10e0            ! Aceleração
            t = 0e0             ! Tempo
            e = 0.001           ! Incremento temporal
            gama = gama0(j)     ! resistência do ar gamma

            write(filename,'(A,2(I0,A))') 'saida-b-v',i,'-g',j,'-10407962.dat'

            open(10, file=filename)

            do while (r.ge.0)
                write(10, '(F0.6,2(" ",F0.6))') t, r, v
                a = -g -gama*v
                v = v + e*a/2
                r = r + e*v
                t = t + e
            end do

            close(10)
        end do
    end do

end program tarefab
