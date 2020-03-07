program tarefa2
    
    print *, 'Digite V1: (x1, y1, z1)'
    read(*,*) x1, y1, z1
    print *, 'Digite V2: (x2, y2, z2)'
    read(*,*) x2, y2, z2
    
    area = (y1*z2 - y2*z1)**2 + (x2*z1 - x1*z2)**2 + (x1*y2 - x2*y1)**2
    area = sqrt(area)/2.d0
    
    print '(A, F8.5)', "Área triângulo: ", area

end program tarefa2
