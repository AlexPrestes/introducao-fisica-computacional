program tarefaa
    implicit real(8) (a-h,o-z)
    character(70) filename
    dimension v0(2), e0(3)

    e0 = (/0.01, 0.001, 0.0001/)
    v0 = (/0.0, 10.0/)

    do i = 1, 2
        do j = 1, 3
        
            ! Parametros iniciais
            e = e0(j)           ! Incremento temporal
            g = 10d0            ! gravidade
            a = -g              ! Aceleração
            r = 100d0           ! Posição
            v = v0(i) +e*a/2    ! Velocidade
            t = 0d0             ! Tempo
            rexato = f(r, v, a, 0d0)

            write(filename,'(A,2(I0,A))') 'saida-a-v',i,'-e',j,'-10407962.dat'

            open(10, file=filename)

            do while (r.ge.0)
                write(10, '(F0.6,2(" ",F0.6)," ",4E0.5)') t, r, v, abs(rexato-r)
                rexato = f(r, v, a, e)
                a = -g
                v = v + e*a
                r = r + e*v
                t = t + e
            end do

            close(10)
        end do
    end do

contains

    function f(r0, v0, a0, t)
        implicit real(8) (a-h,o-z)
        f = r0 + v0*t + 0.5*a0*t**2
        return
    end function

end program tarefaa
