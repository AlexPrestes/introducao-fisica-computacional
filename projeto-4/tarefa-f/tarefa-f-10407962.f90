program tarefaf
    parameter (pi = 4e0*atan(1e0))
    character(70) filename
    dimension alfa0(3), r0(2)
    dimension r(2), v(2), a(2), g(2), d(2), gama(2)

    e0 = 0.001
    gama0 = 0.1
    dx = 0.3
    dy = 0.3
    alfa0 = (/-pi/4, 0e0, pi/4/)
    g0 = 10e0
    r0 = (/0e0, 100e0/)
    v0 = 10e0
    t0 = 0e0


    do i = 1, 3
        ! Parametros iniciais
        gama = (/gama0, gama0/)             ! Fator gamma
        alfa = alfa0(i)                     ! Angulo alfa
        d = (/dx, dy/)                      ! fator de choque
        g = (/0e0, g0/)                     ! gravidade
        r = r0                              ! Posição
        v = v0*(/cos(alfa), sin(alfa)/)     ! Velocidade
        t = t0                              ! Tempo
        e = e0                              ! Incremento temporal

        write(filename,'(A,1(I0,A))') 'saida-e-al',i,'-10407962.dat'

        open(10, file=filename)

        do while (norm2(d*v).ge.1e-3)
            write(10, '(F0.6,4(" ",F0.6))') t, r, v
            a = -g -gama*v
            v = v + e*a
            r = r + e*v
            t = t + e
            if (r(2).lt.0) then
                r(2) = abs(r(2))
                v = abs(v) - d*abs(v)
            end if
        end do

        close(10)
    end do

end program tarefaf
