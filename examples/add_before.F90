!#PYFT transfo: --addVariable sub:ADD PBUF TYPE(BUF_t),INTENT(IN)::PBUF -1 --addModuleVariable sub:ADD MODD_BUF BUF_t

MODULE MODD_BUF
  TYPE BUF_t
    REAL :: XX
  END TYPE BUF_t
END MODULE MODD_BUF

SUBROUTINE ADD(Z1, Z2)
IMPLICIT NONE
REAL, INTENT(IN) :: Z1, Z2
END SUBROUTINE ADD
