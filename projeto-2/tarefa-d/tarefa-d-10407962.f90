program tarefad

    parameter (M = 400, N = 1000000, Nmax=3000, pi = 4e0*atan(1e0))
    dimension I_AndPos(M,2), I_Passo(2), Prob(0:Nmax*Nmax)

    open(10, file="saida-d-10407962")

    I_AndPos = 0
    Prob = 0

    do j = 1, N
        S = 0
        do i = 1, M

            i_rand = 4*rand()
            arg = i_rand*pi/2
            I_Passo = (/cos(arg), sin(arg)/)
            I_AndPos(i,:) = I_AndPos(i,:) + I_Passo

        end do

        do i = 1, M

            i_x = abs(I_AndPos(i,1))
            i_y = abs(I_AndPos(i,2))
            iglob = i_x + i_y*Nmax
            Prob(iglob) = Prob(iglob) + 1
    
        end do

        Prob = Prob/M

        do i = 0, Nmax*Nmax
            
            if (Prob(i).ne.0) then
                S = S - Prob(i)*log(Prob(i))
            end if
        end do

        write(10,'(I0," ",1F0.3)') j, S
    end do

    close(10)

end program tarefad
