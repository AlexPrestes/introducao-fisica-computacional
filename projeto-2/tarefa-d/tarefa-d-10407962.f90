program tarefad

    parameter (M = 400, N = 1000000, pi = 4e0*atan(1e0))
    dimension I_AndPos(M,2), I_Passo(2), Prob(0:5000*5000)

    I_AndPos = 0
    Prob = 0

    do j = 1, N
        do i = 1, M

            i_rand = 4*rand()
            arg = i_rand*pi/2
            I_Passo = (/cos(arg), sin(arg)/)
            I_AndPos(i,:) = I_AndPos(i,:) + I_Passo

        end do
    end do

    do i = 1, M

        i_x = abs(I_AndPos(i,1))
        i_y = abs(I_AndPos(i,2))
        iglob = i_x + i_y*N
        Prob(iglob) = Prob(iglob) + 1
    
    end do

    Prob = Prob/M

    write(*,*) Prob

end program tarefad
