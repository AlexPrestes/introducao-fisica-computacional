program tarefae
    implicit real(8) (a-h,o-z)
    parameter (pi = 4d0*datan(1d0))
    character(70) filename
    dimension alfa0(3), r0(2)
    dimension r(2), v(2), a(2), g(2), d(2)

    e0 = 0.001
    dx = 0.3
    dy = 0.3
    alfa0 = (/-pi/4, 0d0, pi/4/)
    g0 = 10d0
    r0 = (/0d0, 100d0/)
    v0 = 10d0
    t0 = 0d0


    do i = 1, 3
        ! Parametros iniciais
        e = e0                              ! Incremento temporal
        alfa = alfa0(i)                     ! Angulo alfa
        d = (/dx, dy/)                      ! fator de choque
        g = (/0d0, g0/)                     ! gravidade
        a = -g                              ! Aceleração
        r = r0                              ! Posição
        v = v0*(/dcos(alfa), dsin(alfa)/)   ! Velocidade Decomposta
        v = v + e*a/2                       ! Velocidade
        t = t0                              ! Tempo

        write(filename,'(A,1(I0,A))') 'saida-e-al',i,'-10407962.dat'

        open(10, file=filename)

        do while (norm2(d*v).ge.1e-3)
            write(10, '(F0.6,4(" ",F0.6))') t, r, v
            a = -g
            v = v + e*a/2
            r = r + e*v
            t = t + e
            if (r(2).lt.0) then
                r(2) = abs(r(2))
                v = abs(v) - d*abs(v)
            end if
        end do

        close(10)
    end do

end program tarefae
