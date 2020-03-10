program tarefa1

    print *,'Digite a, b e c, respectivamente:'
    !Recebe os valores de a, b e c
    read(*,*) a, b, c
    
    !Calcula o discriminante
    delta = (b**2) - (4*a*c)
    
    !Verifica cada condição do discriminante
    if (delta.eq.0) then
        x1 = (-b + sqrt(delta))/(2*a)
        print '(A10,I1)', "#Raízes:", 1
        print '(A4,F8.5)', "X1: ", x1
    else if (delta.gt.0) then
        x1 = (-b + sqrt(delta))/(2*a)
        x2 = (-b - sqrt(delta))/(2*a)

        print '(A10,I1)', "#Raízes: ", 2
        print '(A4,F8.5)', "X1: ", x1
        print '(A4,F8.5)', "X2: ", x2
    else
        print '(A10,I1)', "#Raízes: ", 0
    end if

end program tarefa1
