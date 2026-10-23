program cal
    implicit none
    real :: a, b, c, c1, c2
    integer:: d, i 
    c1 = 3.14159265358979323846
    c2 = 2.71828182845904523536
    do
    print *, "do you want to continue (1 for yes, 0 for no):"
    read *, i
    if (i == 0) then
        print *, "Exiting the program."
        exit
    end if
    print *, "Enter first number: "
    read *, a
    print *, "Enter an operation (1-8): "
    read *, d
    print *, "Enter second number: "
    read *, b     
    if (d == 1) then
      c = a - b
    else if (d == 2) then
        c = a + b
    else if (d == 3) then
        c = a * b
    else if (d == 4) then
        if (b /= 0.0) then
            c = a / b
        else
            print *, "Error: Division by zero"
            exit
        end if
    else if (d == 5) then
        c = a ** b
    else if (d == 6) then
        print *, "in 6"
       if (b /= 0.0) then
          c = a ** (1.0 / b)
       else
          print *, "Error: Root degree cannot be zero"
          exit
        end if
    else if (d ==7) then
        if (a > 0.0 .and. b > 0.0 .and. b /= 1.0) then
            c= log(a) / log(b)
        else
            print *, "Error: Invalid logarithm arguments"
            exit
        end if
    else if (d == 8) then
        if (b == 0.0) then
            c = a * c1
        else if (b == 1.0) then
            c = a * c2
        else
            print *, "Error: Invalid constant code"
            exit
        end if
    else if (d==9)then
        if (b==1)then
            c=sin(a*c1/180.0)
        else if (b==2)then
            c=cos(a*c1/180.0)
        else if (b==3)then
            c=tan(a*c1/180.0)
        else
            print *, "Error: Invalid trigonometric function code"
            exit
        end if
    else
        print *, "Invalid operation code."
        exit
    end if
    print *, "it equals: ", c
end do
end program cal