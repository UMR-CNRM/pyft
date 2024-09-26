!#PYFT transfo: --dryRun --plotCompilTree plotCompilTree_trans.dot
! This is a hack to produce a png file instead of source-2source transformation


SUBROUTINE PLOTEXEC
USE MODE_COMPUTE_UPDRAFT3, ONLY: COMPUTE_UPDRAFT
CALL COMPUTE_UPDRAFT
END SUBROUTINE PLOTEXEC
