program prog8

    pi = 4e0*atan(1e0)
    
    print *, 'Digite o número de dimensões d:'
    read (*,*) n
    
    r = 1e0
    arg = (n/2e0 + 1e0)
    ugamma = 1e0
    gamma2 = gamma(arg)

    arg = arg-1e0
    
    do while (arg > 0e0)
        ugamma = arg*ugamma
        if (arg >= 1e0) then
            arg = arg-1e0
        else
            ugamma = sqrt(pi)*ugamma
            arg = 0e0
        end if
    end do
    
    V = (pi**(n/2e0) * r**n) / ugamma
    
    print *, V
    
end program prog8
