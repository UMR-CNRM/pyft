!#PYFT transfo: --removeUnusedLocalVariables JK

SUBROUTINE FOO(P, Q)
  REAL, INTENT(IN) :: P, Q
  INTEGER :: JIJ, JK
  REAL :: Z1, Z2
  !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  A(:,:)=0
  !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  Z1 = P
END SUBROUTINE FOO
