program tarefac
    implicit real(8) (a-h,o-z)
    parameter (pi = 4d0*datan(1d0))
    character(70) filename
    dimension alfa0(3), r0(2)
    dimension r(2), v(2), a(2), g(2)

    e0 = 0.001
    alfa0 = (/-pi/4, 0d0, pi/4/)
    g0 = 10d0
    r0 = (/0d0, 100d0/)
    v0 = 10.0
    t0 = 0d0


    do i = 1, 3
        ! Parametros iniciais
        t = t0                              ! Tempo
        e = e0                              ! Incremento temporal
        alfa = alfa0(i)                     ! Angulo alfa
        g = (/0d0, g0/)                     ! gravidade
        r = r0                              ! Posição
        v = v0*(/dcos(alfa), dsin(alfa)/)   ! Velocidade decomposição
        v = v + e*a/2
        t = t0                              ! Tempo

        write(filename,'(A,1(I0,A))') 'saida-c-al',i,'-10407962.dat'

        open(10, file=filename)

        do while (r(2).ge.0)
            write(10, '(F0.6,4(" ",F0.6))') t, r, v
            a = -g
            v = v + e*a
            r = r + e*v
            t = t + e
        end do

        close(10)
    end do

end program tarefac
