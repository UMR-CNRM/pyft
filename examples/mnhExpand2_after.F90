!#PYFT transfo: --mnhExpand

SUBROUTINE EXPAND6
IMPLICIT NONE

INTEGER :: JI, JJ
INTEGER :: IOPT
INTEGER, DIMENSION(5) :: ICASE
REAL, DIMENSION(5) :: ZZ
REAL, DIMENSION(5, 6) :: ZZZ

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Exemple le plus simple

DO JI=1, 5
  ZZ(JI)=1.
END DO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Le même avec les bornes définies dans les expressions

DO JI=1, 5
  ZZ(JI)=1.
END DO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Une varainte avec deux dimensions

DO JJ=1, 6
  DO JI=1, 5
    ZZZ(JI, JJ)=1.
  END DO
END DO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Exemples avec des IF
IOPT=1

DO JI=1, 5
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

DO JI=1, 5
  ZZ(JI)=1.
  IF (IOPT==1) ZZ(JI)=2.
END DO


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Exemple avec des WHERE

DO JI=1, 5
  ICASE(JI)=JI
ENDDO

DO JI=1, 5
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

DO JI=1, 5
  ZZ(JI)=1.
  IF (ICASE(JI)==1) ZZ(JI)=2.
END DO

END SUBROUTINE EXPAND6
