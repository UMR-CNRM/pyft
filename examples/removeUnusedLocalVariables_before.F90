!#PYFT transfo: --removeUnusedLocalVariables sub:FOO NONE

SUBROUTINE FOO(P, Q)
  REAL, INTENT(IN) :: P, Q
  REAL :: Z1, Z2
  Z1 = P
END SUBROUTINE FOO
