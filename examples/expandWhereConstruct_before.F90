!#PYFT transfo: --expandAllArraysPHYEX

!MNH_LIC Copyright 1994-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
MODULE MODE_ICE4_COMPUTE_PDF
IMPLICIT NONE
CONTAINS
SUBROUTINE ICE4_COMPUTE_PDF(D, CST, ICEP, ICED, KSIZE, HSUBG_AUCV_RC, HSUBG_AUCV_RI, HSUBG_PR_PDF, &
                            PRHODREF, PRCT, PRIT, PCF, PT, PSIGMA_RC,&
                            PHLC_HCF, PHLC_LCF, PHLC_HRC, PHLC_LRC, &
                            PHLI_HCF, PHLI_LCF, PHLI_HRI, PHLI_LRI, PRF)
!!
!!**  PURPOSE
!!    -------
!!      Computes the pdf used to split cloud into high and low content parts
!!
!!    AUTHOR
!!    ------
!!      S. Riette from the plitting of rain_ice source code (nov. 2014)
!!
!!    MODIFICATIONS
!!    -------------
!!
!
!
!*      0. DECLARATIONS
!          ------------
!
!
USE MODD_DIMPHYEX,       ONLY: DIMPHYEX_t
USE MODD_CST,            ONLY: CST_t
USE MODD_RAIN_ICE_DESCR_n, ONLY: RAIN_ICE_DESCR_t
USE MODD_RAIN_ICE_PARAM_n, ONLY: RAIN_ICE_PARAM_t
USE YOMHOOK , ONLY : LHOOK, DR_HOOK, JPHOOK
!
USE MODE_MSG
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
TYPE(DIMPHYEX_t),         INTENT(IN)    :: D
TYPE(CST_t),              INTENT(IN)    :: CST
TYPE(RAIN_ICE_PARAM_t),   INTENT(IN)    :: ICEP
TYPE(RAIN_ICE_DESCR_t),   INTENT(IN)    :: ICED
INTEGER,                INTENT(IN)  :: KSIZE
CHARACTER(LEN=4),       INTENT(IN)  :: HSUBG_AUCV_RC     ! Kind of Subgrid autoconversion method for cloud water
CHARACTER(LEN=80),      INTENT(IN)  :: HSUBG_AUCV_RI     ! Kind of Subgrid autoconversion method for cloud ice
CHARACTER(LEN=80),      INTENT(IN)  :: HSUBG_PR_PDF   ! pdf for subgrid precipitation
REAL, DIMENSION(KSIZE), INTENT(IN)  :: PRHODREF   ! Reference density
REAL, DIMENSION(KSIZE), INTENT(IN)  :: PRCT       ! Cloud water m.r. at t
REAL, DIMENSION(KSIZE), INTENT(IN)  :: PRIT       ! Ice Crystal m.r. at t
REAL, DIMENSION(KSIZE), INTENT(IN)  :: PCF        ! Cloud fraction
REAL, DIMENSION(KSIZE), INTENT(IN)  :: PT         ! Temperature
REAL, DIMENSION(KSIZE), INTENT(IN)  :: PSIGMA_RC  ! Standard deviation of rc at time t
!Note for INTENT STATUS: in 'ADJU' case the PHL?_??? variables must be able to "cross" the subroutine untouched
REAL, DIMENSION(KSIZE), INTENT(INOUT) :: PHLC_HCF   ! HLCLOUDS : fraction of High Cloud Fraction in grid
REAL, DIMENSION(KSIZE), INTENT(INOUT) :: PHLC_LCF   ! HLCLOUDS : fraction of Low  Cloud Fraction in grid
                                                  !    note that PCF = PHLC_HCF + PHLC_LCF
REAL, DIMENSION(KSIZE), INTENT(INOUT) :: PHLC_HRC   ! HLCLOUDS : LWC that is High LWC in grid
REAL, DIMENSION(KSIZE), INTENT(INOUT) :: PHLC_LRC   ! HLCLOUDS : LWC that is Low  LWC in grid
                                                  !    note that PRC = PHLC_HRC + PHLC_LRC
REAL, DIMENSION(KSIZE), INTENT(INOUT) :: PHLI_HCF
REAL, DIMENSION(KSIZE), INTENT(INOUT) :: PHLI_LCF
REAL, DIMENSION(KSIZE), INTENT(INOUT) :: PHLI_HRI
REAL, DIMENSION(KSIZE), INTENT(INOUT) :: PHLI_LRI
REAL, DIMENSION(KSIZE), INTENT(OUT) :: PRF        ! Rain fraction
!
!*       0.2  declaration of local variables
!
REAL, DIMENSION(KSIZE) :: ZRCRAUTC,      & !RC value to begin rain formation =XCRIAUTC/RHODREF
                          ZCRIAUTI,      & !RI value to begin snow formation
                          ZHLC_RCMAX,    & !HLCLOUDS : maximum value for RC in distribution
                          ZHLC_LRCLOCAL, & !HLCLOUDS : LWC that is Low  LWC local in LCF
                          ZHLC_HRCLOCAL, & !HLCLOUDS : LWC that is High LWC local in HCF
                                                    !    note that ZRC/CF = ZHLC_HRCLOCAL+ ZHLC_LRCLOCAL
                                                    !                     = PHLC_HRC/HCF+ PHLC_LRC/LCF
                          ZSUMRC, ZSUMRI
REAL :: ZCOEFFRCM
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
INTEGER :: JL
!-------------------------------------------------------------------------------
!
IF (LHOOK) THEN
  CALL DR_HOOK('ICE4_COMPUTE_PDF', 0, ZHOOK_HANDLE)
END IF!

!Cloud water split between high and low content part is done according to autoconversion option
ZRCRAUTC(:)=ICEP%XCRIAUTC/PRHODREF(:) ! Autoconversion rc threshold
IF(HSUBG_AUCV_RC=='NONE') THEN
  !Cloud water is entirely in low or high part
 !$mnh_expand_where(JL=1:KSIZE)
  WHERE(PRCT(:)>ZRCRAUTC(:))
    PHLC_HCF(:)=1.
    PHLC_LCF(:)=0.
    PHLC_HRC(:)=PRCT(:)
    PHLC_LRC(:)=0.
  ELSEWHERE(PRCT(:)>ICED%XRTMIN(2))
    PHLC_HCF(:)=0.
    PHLC_LCF(:)=1.
    PHLC_HRC(:)=0.
    PHLC_LRC(:)=PRCT(:)
  ELSEWHERE
    PHLC_HCF(:)=0.
    PHLC_LCF(:)=0.
    PHLC_HRC(:)=0.
    PHLC_LRC(:)=0.
  END WHERE
 !$mnh_end_expand_where(JL=1:KSIZE)

ELSEIF(HSUBG_AUCV_RC=='CLFR') THEN
  !Cloud water is only in the cloudy part and entirely in low or high part
 !$mnh_expand_where(JL=1:KSIZE)
  WHERE(PCF(:)>0. .AND. PRCT(:)>ZRCRAUTC(:)*PCF(:))
    PHLC_HCF(:)=PCF(:)
    PHLC_LCF(:)=0.
    PHLC_HRC(:)=PRCT(:)
    PHLC_LRC(:)=0.
  ELSEWHERE(PCF(:)>0. .AND. PRCT(:)>ICED%XRTMIN(2))
    PHLC_HCF(:)=0.
    PHLC_LCF(:)=PCF(:)
    PHLC_HRC(:)=0.0
    PHLC_LRC(:)=PRCT(:)
  ELSEWHERE
    PHLC_HCF(:)=0.
    PHLC_LCF(:)=0.
    PHLC_HRC(:)=0.
    PHLC_LRC(:)=0.
  END WHERE
 !$mnh_end_expand_where(JL=1:KSIZE)
ELSEIF(HSUBG_AUCV_RC=='ADJU') THEN
 !$mnh_expand_where(JL=1:KSIZE)
  ZSUMRC(:)=PHLC_LRC(:)+PHLC_HRC(:)
  WHERE(ZSUMRC(:) .GT. 0.)
    PHLC_LRC(:)=PHLC_LRC(:)*PRCT(:)/ZSUMRC(:)
    PHLC_HRC(:)=PHLC_HRC(:)*PRCT(:)/ZSUMRC(:)
  ELSEWHERE
    PHLC_LRC(:)=0.
    PHLC_HRC(:)=0.
  ENDWHERE
 !$mnh_end_expand_where(JL=1:KSIZE)
ELSEIF(HSUBG_AUCV_RC=='PDF ') THEN
  !Cloud water is split between high and low part according to a PDF
  !    'HLCRECTPDF'    : rectangular PDF form
  !    'HLCTRIANGPDF'  : triangular PDF form
  !    'HLCQUADRAPDF'  : second order quadratic PDF form
  !    'HLCISOTRIPDF'  : isocele triangular PDF
  !    'SIGM'          : Redelsperger and Sommeria (1986)
  IF(HSUBG_PR_PDF=='SIGM') THEN
    ! Redelsperger and Sommeria (1986) but organised according to Turner (2011, 2012)
   !$mnh_expand_where(JL=1:KSIZE)
    WHERE (PRCT(:)>ZRCRAUTC(:)+PSIGMA_RC(:))
      PHLC_HCF(:)=1.
      PHLC_LCF(:)=0.
      PHLC_HRC(:)=PRCT(:)
      PHLC_LRC(:)=0.
    ELSEWHERE(PRCT(:)> (ZRCRAUTC(:)-PSIGMA_RC(:)) .AND. PRCT(:)<=(ZRCRAUTC(:)+PSIGMA_RC(:))       )
      PHLC_HCF(:)=(PRCT(:)+PSIGMA_RC(:)-ZRCRAUTC(:))/ &
                  &(2.*PSIGMA_RC(:))
      PHLC_LCF(:)=MAX(0., PCF(:)-PHLC_HCF(:))
      PHLC_HRC(:)=(PRCT(:)+PSIGMA_RC(:)-ZRCRAUTC(:))* &
                  &(PRCT(:)+PSIGMA_RC(:)+ZRCRAUTC(:))/ &
                  &(4.*PSIGMA_RC(:))
      PHLC_LRC(:)=MAX(0., PRCT(:)-PHLC_HRC(:))
    ELSEWHERE(PRCT(:)>ICED%XRTMIN(2) .AND. PCF(:)>0.)
      PHLC_HCF(:)=0.
      PHLC_LCF(:)=PCF(:)
      PHLC_HRC(:)=0.
      PHLC_LRC(:)=PRCT(:)
    ELSEWHERE
      PHLC_HCF(:)=0.
      PHLC_LCF(:)=0.
      PHLC_HRC(:)=0.
      PHLC_LRC(:)=0.
    END WHERE
   !$mnh_end_expand_where(JL=1:KSIZE)
  ELSEIF(HSUBG_PR_PDF=='HLCRECTPDF' .OR. HSUBG_PR_PDF=='HLCISOTRIPDF' .OR. &
         &HSUBG_PR_PDF=='HLCTRIANGPDF' .OR. HSUBG_PR_PDF=='HLCQUADRAPDF') THEN
    ! Turner (2011, 2012)
    ! Calculate maximum value r_cM from PDF forms
    IF(HSUBG_PR_PDF=='HLCRECTPDF' .OR. HSUBG_PR_PDF=='HLCISOTRIPDF') THEN
      ZCOEFFRCM=2.
    ELSE IF(HSUBG_PR_PDF=='HLCTRIANGPDF') THEN
      ZCOEFFRCM=3.
    ELSE IF(HSUBG_PR_PDF=='HLCQUADRAPDF') THEN
      ZCOEFFRCM=4.
    END IF
   !$mnh_expand_where(JL=1:KSIZE)
    WHERE(PRCT(:).GT.0. .AND. PCF(:).GT.0.)
      ZHLC_RCMAX(:)=ZCOEFFRCM*PRCT(:)/PCF(:)
    END WHERE
    ! Split available water and cloud fraction in two parts
    ! Calculate local mean values int he low and high parts for the 3 PDF forms:
    IF(HSUBG_PR_PDF=='HLCRECTPDF') THEN
      WHERE(PRCT(:).GT.0. .AND. PCF(:).GT.0. .AND. ZHLC_RCMAX(:).GT.ZRCRAUTC(:))
        ZHLC_LRCLOCAL(:)=0.5*ZRCRAUTC(:)
        ZHLC_HRCLOCAL(:)=( ZHLC_RCMAX(:) + ZRCRAUTC(:))/2.0
      END WHERE
    ELSE IF(HSUBG_PR_PDF=='HLCTRIANGPDF') THEN
      WHERE(PRCT(:).GT.0. .AND. PCF(:).GT.0. .AND. ZHLC_RCMAX(:).GT.ZRCRAUTC(:))
        ZHLC_LRCLOCAL(:)=( ZRCRAUTC(:) *(3.0 * ZHLC_RCMAX(:) - 2.0 * ZRCRAUTC(:) ) ) &
                        / (3.0 * (2.0 * ZHLC_RCMAX(:) - ZRCRAUTC(:)  ) )
        ZHLC_HRCLOCAL(:)=(ZHLC_RCMAX(:) + 2.0*ZRCRAUTC(:)) / 3.0
      END WHERE
    ELSE IF(HSUBG_PR_PDF=='HLCQUADRAPDF') THEN
      WHERE(PRCT(:).GT.0. .AND. PCF(:).GT.0. .AND. ZHLC_RCMAX(:).GT.ZRCRAUTC(:))
        ZHLC_LRCLOCAL(:)=(3.0 *ZRCRAUTC(:)**3 - 8.0 *ZRCRAUTC(:)**2 * ZHLC_RCMAX(:) &
                        + 6.0*ZRCRAUTC(:) *ZHLC_RCMAX(:)**2 ) &
                        / &
                        (4.0* ZRCRAUTC(:)**2 -12.0*ZRCRAUTC(:) *ZHLC_RCMAX(:) &
                        + 12.0 * ZHLC_RCMAX(:)**2 )
        ZHLC_HRCLOCAL(:)=(ZHLC_RCMAX(:) + 3.0*ZRCRAUTC(:))/4.0
      END WHERE
    ELSE IF(HSUBG_PR_PDF=='HLCISOTRIPDF') THEN
    DO JL=1,KSIZE  
     IF(PRCT(JL).GT.0. .AND. PCF(JL).GT.0. .AND. ZHLC_RCMAX(JL).GT.ZRCRAUTC(JL)) THEN
        IF((PRCT(JL) / PCF(JL)).LE.ZRCRAUTC(JL)) THEN
          ZHLC_LRCLOCAL(JL)=( (ZHLC_RCMAX(JL))**3 &
                          -(12.0 * (ZHLC_RCMAX(JL))*(ZRCRAUTC(JL))**2) &
                          +(8.0 * ZRCRAUTC(JL)**3) ) &
                          /( (6.0 * (ZHLC_RCMAX(JL))**2) &
                          -(24.0 * (ZHLC_RCMAX(JL)) * ZRCRAUTC(JL)) &
                          +(12.0 * ZRCRAUTC(JL)**2) )
          ZHLC_HRCLOCAL(JL)=( ZHLC_RCMAX(JL) + 2.0 * ZRCRAUTC(JL) )/3.0
        ELSE
          ZHLC_LRCLOCAL(JL)=(2.0/3.0) * ZRCRAUTC(JL)
          ZHLC_HRCLOCAL(JL)=(3.0*ZHLC_RCMAX(JL)**3 - 8.0*ZRCRAUTC(JL)**3) &
                          / (6.0 * ZHLC_RCMAX(JL)**2 - 12.0*ZRCRAUTC(JL)**2)
        END IF
      END IF
    END DO
    END IF
    ! Compare r_cM  to r_cR to know if cloud water content is high enough to split in two parts or not
    WHERE (PRCT(:).GT.0. .AND. PCF(:).GT.0. .AND. ZHLC_RCMAX(:).GT.ZRCRAUTC(:))
      ! Calculate final values for LCF and HCF:
      PHLC_LCF(:)=PCF(:) &
                    *(ZHLC_HRCLOCAL(:)- &
                    (PRCT(:) / PCF(:))) &
                    / (ZHLC_HRCLOCAL(:)-ZHLC_LRCLOCAL(:))
      PHLC_HCF(:)=MAX(0., PCF(:)-PHLC_LCF(:))
      !
      ! Calculate final values for LRC and HRC:
      PHLC_LRC(:)=ZHLC_LRCLOCAL(:)*PHLC_LCF(:)
      PHLC_HRC(:)=MAX(0., PRCT(:)-PHLC_LRC(:))
    ELSEWHERE (PRCT(:).GT.0. .AND. PCF(:).GT.0. .AND. ZHLC_RCMAX(:).LE.ZRCRAUTC(:))
      ! Put all available cloud water and his fraction in the low part
      PHLC_LCF(:)=PCF(:)
      PHLC_HCF(:)=0.
      PHLC_LRC(:)=PRCT(:)
      PHLC_HRC(:)=0.
    ELSEWHERE
      PHLC_LCF(:)=0.
      PHLC_HCF(:)=0.
      PHLC_LRC(:)=0.
      PHLC_HRC(:)=0.
    END WHERE
   !$mnh_end_expand_where(JL=1:KSIZE)
  ELSE
    CALL PRINT_MSG(NVERB_FATAL,'GEN','ICE4_COMPUTE_PDF','wrong HSUBG_PR_PDF case')
  ENDIF
ELSE
  CALL PRINT_MSG(NVERB_FATAL,'GEN','ICE4_COMPUTE_PDF','wrong HSUBG_AUCV case')
ENDIF
!
!Ice water split between high and low content part is done according to autoconversion option
!$mnh_expand_where(JL=1:KSIZE)
  ZCRIAUTI(:)=MIN(ICEP%XCRIAUTI,10**(ICEP%XACRIAUTI*(PT(:)-CST%XTT)+ICEP%XBCRIAUTI)) ! Autoconversion ri threshold
!$mnh_end_expand_where(JL=1:KSIZE)
IF(HSUBG_AUCV_RI=='NONE') THEN
 !$mnh_expand_where(JL=1:KSIZE)
  !Cloud water is entirely in low or high part
  WHERE(PRIT(:)>ZCRIAUTI(:))
    PHLI_HCF(:)=1.
    PHLI_LCF(:)=0.
    PHLI_HRI(:)=PRIT(:)
    PHLI_LRI(:)=0.
  ELSEWHERE(PRIT(:)>ICED%XRTMIN(4))
    PHLI_HCF(:)=0.
    PHLI_LCF(:)=1.
    PHLI_HRI(:)=0.
    PHLI_LRI(:)=PRIT(:)
  ELSEWHERE
    PHLI_HCF(:)=0.
    PHLI_LCF(:)=0.
    PHLI_HRI(:)=0.
    PHLI_LRI(:)=0.
  END WHERE
 !$mnh_end_expand_where(JL=1:KSIZE)
ELSEIF(HSUBG_AUCV_RI=='CLFR') THEN
  !Cloud water is only in the cloudy part and entirely in low or high part
 !$mnh_expand_where(JL=1:KSIZE)
  WHERE(PCF(:)>0. .AND. PRIT(:)>ZCRIAUTI(:)*PCF(:))
    PHLI_HCF(:)=PCF(:)
    PHLI_LCF(:)=0.
    PHLI_HRI(:)=PRIT(:)
    PHLI_LRI(:)=0.
  ELSEWHERE(PCF(:)>0. .AND. PRIT(:)>ICED%XRTMIN(4))
    PHLI_HCF(:)=0.
    PHLI_LCF(:)=PCF(:)
    PHLI_HRI(:)=0.0
    PHLI_LRI(:)=PRIT(:)
  ELSEWHERE
    PHLI_HCF(:)=0.
    PHLI_LCF(:)=0.
    PHLI_HRI(:)=0.
    PHLI_LRI(:)=0.
  END WHERE
 !$mnh_end_expand_where(JL=1:KSIZE)
ELSEIF(HSUBG_AUCV_RI=='ADJU') THEN
 !$mnh_expand_where(JL=1:KSIZE)
  ZSUMRI(:)=PHLI_LRI(:)+PHLI_HRI(:)
  WHERE(ZSUMRI(:) .GT. 0.)
    PHLI_LRI(:)=PHLI_LRI(:)*PRIT(:)/ZSUMRI(:)
    PHLI_HRI(:)=PHLI_HRI(:)*PRIT(:)/ZSUMRI(:)
  ELSEWHERE
    PHLI_LRI(:)=0.
    PHLI_HRI(:)=0.
  ENDWHERE
 !$mnh_end_expand_where(JL=1:KSIZE)
ELSE
  !wrong HSUBG_AUCV_RI case
  CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'ICE4_COMPUTE_PDF', 'wrong HSUBG_AUCV_RI case' )
ENDIF
!
!$mnh_expand_where(JI=1:KSIZE)
  PRF(:)=MAX(PHLC_HCF(:),PHLI_HCF(:))
!$mnh_end_expand_where(JI=1:KSIZE)
!
IF (LHOOK) THEN
  CALL DR_HOOK('ICE4_COMPUTE_PDF', 1, ZHOOK_HANDLE)
END IF
END SUBROUTINE ICE4_COMPUTE_PDF
END MODULE MODE_ICE4_COMPUTE_PDF
