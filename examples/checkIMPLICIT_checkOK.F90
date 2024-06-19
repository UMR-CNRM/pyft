!#PYFT transfo: --checkIMPLICIT Err

SUBROUTINE CHECK(ARG1, ARG2, &
                 !La ligne suivante est commentée, Z1 n'est donc pas un argument
                 !Z1, &
                 ARG3)
   
!L'instruction n'est pas commentée, l'outil ne doit pas générer de warning ou
!d'erreur si la vérification est demandée
IMPLICIT NONE   

REAL, INTENT(IN) :: ARG1, ARG2, ARG3

END SUBROUTINE CHECK
