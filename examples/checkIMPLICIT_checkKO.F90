!#PYFT transfo: --checkIMPLICIT Err

SUBROUTINE CHECK1(ARG1, ARG2, &
                 !La ligne suivante est commentée, Z1 n'est donc pas un argument
                 !Z1, &
                 ARG3)
   
!L'instruction est ici commentée, l'outil doit générer un warning ou une
!erreur si la vérification est demandée
!IMPLICIT NONE   

REAL, INTENT(IN) :: ARG1, ARG2, ARG3

END SUBROUTINE CHECK1
