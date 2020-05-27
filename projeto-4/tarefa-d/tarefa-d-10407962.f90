program tarefad
    implicit real(8) (a-h,o-z)
    parameter (pi = 4d0*datan(1d0))
    character(70) filename
    dimension alfa0(3), r0(2)
    dimension r(2), v(2), a(2), g(2), gama(2)

    e0 = 0.001
    alfa0 = (/-pi/4, 0d0, pi/4/)
    g0 = 10d0
    gama0 = 0.1
    r0 = (/0d0, 100d0/)
    v0 = 10d0
    t0 = 0d0


    do i = 1, 3
        ! Parametros iniciais
        e = e0                              ! Incremento temporal
        alfa = alfa0(i)                     ! angulo alfa
        gama = (/gama0, gama0/)             ! fator gamma
        g = (/0d0, g0/)                     ! gravidade
        a = -g                              ! Aceleração
        r = r0                              ! Posição
        v = v0*(/dcos(alfa), dsin(alfa)/)   ! Velocidade descomposta
        v = v + e*a/2                       ! Velocidade
        t = t0                              ! Tempo

        write(filename,'(A,1(I0,A))') 'saida-d-al',i,'-10407962.dat'

        open(10, file=filename)

        do while (r(2).ge.0)
            write(10, '(F0.6,4(" ",F0.6))') t, r, v
            a = -g -gama*v
            v = v + e*a/2
            r = r + e*v
            t = t + e
        end do

        close(10)
    end do

end program tarefad
