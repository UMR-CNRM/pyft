!#PYFT transfo: --addArrayParentheses --addExplicitArrayBounds

SUBROUTINE FOO2
  INTEGER, DIMENSION(5, 4) :: I
  PRINT*, I(1:5, 1:4)
END SUBROUTINE FOO2
