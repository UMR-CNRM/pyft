!#PYFT transfo: --dryRun --plotExecTree plotExecTree_trans.dot
! Thi is a hack to produce a png file instead of source-2source transformation


SUBROUTINE PLOTEXEC
USE MODE_COMPUTE_UPDRAFT3, ONLY: COMPUTE_UPDRAFT
CALL COMPUTE_UPDRAFT
END SUBROUTINE PLOTEXEC
