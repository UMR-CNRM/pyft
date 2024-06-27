!#PYFT transfo: --addACC_routine_seq --stopScopes sub:FOO

SUBROUTINE FOO(P)
!$acc routine (FOO) seq
    REAL, DIMENSION(2) :: P
END SUBROUTINE FOO
