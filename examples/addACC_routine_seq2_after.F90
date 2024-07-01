!#PYFT transfo: --addACC_routine_seq --stopScopes sub:FOO1

SUBROUTINE FOO1(P)
!$acc routine (FOO1) seq
    REAL, DIMENSION(2) :: P
END SUBROUTINE FOO1
