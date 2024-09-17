!#PYFT transfo: --mnhExpandConcurrent

SUBROUTINE EXPAND7
IMPLICIT NONE

INTEGER :: JI, JJ
INTEGER :: IOPT
INTEGER, DIMENSION(5) :: ICASE
REAL, DIMENSION(5) :: ZZ
REAL, DIMENSION(5, 6) :: ZZZ

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Exemple le plus simple

!$acc loop independent collapse(1)
DO CONCURRENT (JI=1:5)
  ZZ(JI)=1.
END DO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Le même avec les bornes définies dans les expressions

!$acc loop independent collapse(1)
DO CONCURRENT (JI=1:5)
  ZZ(JI)=1.
END DO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Une varainte avec deux dimensions

!$acc loop independent collapse(2)
DO CONCURRENT (JJ=1:6, JI=1:5)
  ZZZ(JI, JJ)=1.
END DO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Exemples avec des IF
IOPT=1

!$acc loop independent collapse(1)
DO CONCURRENT (JI=1:5)
  IF(IOPT==1) THEN
    ZZ(JI)=1.
  ELSEIF(IOPT==2) THEN
    ZZ(JI)=2.
  ELSE IF(IOPT==3) THEN
    ZZ(JI)=3.
  ELSE
    ZZ(JI)=4.
  ENDIF
END DO

!$acc loop independent collapse(1)
DO CONCURRENT (JI=1:5)
  ZZ(JI)=1.
  IF (IOPT==1) ZZ(JI)=2.
END DO


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Exemple avec des WHERE

DO JI=1, 5
  ICASE(JI)=JI
ENDDO

!$acc loop independent collapse(1)
DO CONCURRENT (JI=1:5)
  IF (ICASE(JI)==1) THEN
    ZZ(JI)=1.
  ELSE IF (ICASE(JI)==2) THEN
    ZZ(JI)=2.
  ELSE IF (ICASE(JI)==3) THEN
    ZZ(JI)=3.
  ELSE
    ZZ(JI)=4.
  END IF
END DO

!$acc loop independent collapse(1)
DO CONCURRENT (JI=1:5)
  ZZ(JI)=1.
  IF (ICASE(JI)==1) ZZ(JI)=2.
END DO

END SUBROUTINE EXPAND7
