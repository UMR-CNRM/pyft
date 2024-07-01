!#PYFT transfo: --checkINTENT Err

SUBROUTINE CHECK4(ARG1, ARG2, &
                 !La ligne suivante est commentée, Z1 n'est donc pas un argument
                 !Z1, &
                 ARG3)
   
IMPLICIT NONE   

REAL, INTENT(IN) :: ARG1, ARG3

!Tous les arguments sont déclarés avec INTENT, l'outil ne doit pas générer de
!warning ou une d'erreur si la vérification est demandé
REAL, INTENT(IN) :: ARG2

!Déclaration des arguments locaux

!L'argument suivant est local et ne doit pas avoir d'INTENT
REAL :: Z1

END SUBROUTINE CHECK4
