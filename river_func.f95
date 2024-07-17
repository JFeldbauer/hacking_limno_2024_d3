module functions
implicit none
contains

double precision function monod (x, h)
double precision, intent(in):: x, h
monod = x / (x + h)
end function

end module