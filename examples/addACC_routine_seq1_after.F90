!#PYFT transfo: --addACC_routine_seq --stopScopes sub:FOO

MODULE MODFOO
  INTERFACE
    SUBROUTINE FOO(P)
!$acc routine (FOO) seq
      REAL, DIMENSION(2) :: P
    END SUBROUTINE FOO
  END INTERFACE
END MODULE MODFOO
