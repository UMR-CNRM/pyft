!#PYFT transfo: --inlineContainedSubroutinesPHYEX

!MNH_LIC Copyright 2004-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ######spl
     MODULE MODE_COMPUTE_UPDRAFT
!    ###########################
!
IMPLICIT NONE
CONTAINS
      SUBROUTINE COMPUTE_UPDRAFT(D,CST,NEBN,PARAMMF,TURBN,CSTURB, &
                                 KSV,                             &
                                 OENTR_DETR,                      &
                                 ONOMIXLG,KSV_LGBEG,KSV_LGEND,    &
                                 PZZ,PDZZ,                        &
                                 PSFTH,PSFRV,                     &
                                 PPABSM,PRHODREF,PUM,PVM, PTKEM,  &
                                 PTHM,PRVM,PTHLM,PRTM,            &
                                 PSVM,PTHL_UP,PRT_UP,             &
                                 PRV_UP,PRC_UP,PRI_UP,PTHV_UP,    &
                                 PW_UP,PU_UP, PV_UP, PSV_UP,      &
                                 PFRAC_UP,PFRAC_ICE_UP,PRSAT_UP,  &
                                 PEMF,PDETR,PENTR,                &
                                 PBUO_INTEG,KKLCL,KKETL,KKCTL,    &
                                 PDEPTH, PDX, PDY     )

!     #################################################################
!!
!!****  *COMPUTE_UPDRAFT* - calculates caracteristics of the updraft 
!!                         
!!
!!    PURPOSE
!!    -------
!!****  The purpose of this routine is to build the updraft model 
!!
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!      
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      !!     REFERENCE
!!     ---------
!!       Book 1 of Meso-NH documentation (chapter Turbulence)
!!       Soares et al. 2004 QJ
!!
!!     AUTHOR
!!     ------
!!     J.Pergaud
!!     V.Masson : Optimization 07/2010
!!     S. Riette : 07/2010 : modification for reproducibility  
!!     S. Riette may 2011: ice added, interface modified
!!     S. Riette Jan 2012: support for both order of vertical levels
!!     V.Masson, C.Lac : 02/2011 : SV_UP initialized by a non-zero value
!!     S. Riette Apr 2013: improvement of continuity at the condensation level
!!     R.Honnert Oct 2016 : Add ZSURF and Update with AROME
!!     Q.Rodier  01/2019 : support RM17 mixing length
!!     R.Honnert 01/2019 : add LGZ (reduction of the mass-flux surface closure with the resolution)
!!     S. Riette 06/2022: compute_entr_detr is inlined
!! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_DIMPHYEX,        ONLY: DIMPHYEX_t
USE MODD_CST,             ONLY: CST_t
USE MODD_NEB_n,           ONLY: NEB_t
USE MODD_PARAM_MFSHALL_n, ONLY: PARAM_MFSHALL_t
USE MODD_TURB_n,          ONLY: TURB_t
USE MODD_CTURB,           ONLY: CSTURB_t
!
USE MODI_SHUMAN_MF, ONLY: MZM_MF, MZF_MF, GZ_M_W_MF

USE MODE_COMPUTE_BL89_ML, ONLY: COMPUTE_BL89_ML
USE MODE_MSG, ONLY: PRINT_MSG, NVERB_FATAL
USE YOMHOOK , ONLY : LHOOK, DR_HOOK, JPHOOK

IMPLICIT NONE

!*                    1.1  Declaration of Arguments
!
!
!
TYPE(DIMPHYEX_t),       INTENT(IN)   :: D
TYPE(CST_t),            INTENT(IN)   :: CST
TYPE(NEB_t),            INTENT(IN)   :: NEBN
TYPE(PARAM_MFSHALL_t),  INTENT(IN)   :: PARAMMF
TYPE(TURB_t),           INTENT(IN)   :: TURBN
TYPE(CSTURB_t),         INTENT(IN)   :: CSTURB
INTEGER,                INTENT(IN)   :: KSV
LOGICAL,                INTENT(IN) :: OENTR_DETR! flag to recompute entrainment, detrainment and mass flux
LOGICAL,                INTENT(IN)   :: ONOMIXLG  ! False if mixing of lagrangian tracer
INTEGER,                INTENT(IN)   :: KSV_LGBEG ! first index of lag. tracer
INTEGER,                INTENT(IN)   :: KSV_LGEND ! last  index of lag. tracer
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)   :: PZZ       !  Height at the flux point
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)   :: PDZZ      !  Metrics coefficient
 
REAL, DIMENSION(D%NIJT),   INTENT(IN)   ::  PSFTH,PSFRV
! normal surface fluxes of theta,rv,(u,v) parallel to the orography
!
REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(IN) ::  PPABSM     ! Pressure at t-dt
REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(IN) ::  PRHODREF   ! dry density of the
                                                  ! reference state
REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(IN) ::  PUM        ! u mean wind
REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(IN) ::  PVM        ! v mean wind
REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(IN) ::  PTKEM      ! TKE at t-dt
!
REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(IN)   ::  PTHM           ! liquid pot. temp. at t-dt
REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(IN)   ::  PRVM           ! vapor mixing ratio at t-dt
REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(IN)   ::  PTHLM,PRTM     ! cons. var. at t-dt

REAL, DIMENSION(D%NIJT,D%NKT,KSV), INTENT(IN)   ::  PSVM           ! scalar var. at t-dt

REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(OUT)  ::  PTHL_UP,PRT_UP   ! updraft properties
REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(OUT)  ::  PU_UP, PV_UP     ! updraft wind components
REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(INOUT)::  PRV_UP,PRC_UP, & ! updraft rv, rc
                                         PRI_UP,PTHV_UP,& ! updraft ri, THv
                                         PW_UP,PFRAC_UP,& ! updraft w, fraction
                                         PFRAC_ICE_UP,&   ! liquid/solid fraction in updraft
                                         PRSAT_UP         ! Rsat

REAL, DIMENSION(D%NIJT,D%NKT,KSV), INTENT(OUT)  ::  PSV_UP           ! updraft scalar var. 
                                         
REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(INOUT)::  PEMF,PDETR,PENTR ! Mass_flux,
                                                          ! detrainment,entrainment
REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(INOUT) :: PBUO_INTEG       ! Integrated Buoyancy 
INTEGER, DIMENSION(D%NIJT),  INTENT(INOUT) :: KKLCL,KKETL,KKCTL! LCL, ETL, CTL
REAL, DIMENSION(D%NIJT),     INTENT(OUT)   :: PDEPTH           ! Deepness of cloud
REAL,                   INTENT(IN)    :: PDX, PDY
!                       1.2  Declaration of local variables
!
!
! Mean environment variables at t-dt at flux point
REAL, DIMENSION(D%NIJT,D%NKT) ::    &
                        ZTHM_F,ZRVM_F                 ! Theta,rv of
                                                      ! updraft environnement
REAL, DIMENSION(D%NIJT,D%NKT) ::    &
                        ZRTM_F, ZTHLM_F, ZTKEM_F,&    ! rt, thetal,TKE,pressure,
                        ZUM_F,ZVM_F,ZRHO_F,      &    ! density,momentum
                        ZPRES_F,ZTHVM_F,ZTHVM,   &    ! interpolated at the flux point
                        ZG_O_THVREF,             &    ! g*ThetaV ref
                        ZW_UP2,                  &    ! w**2  of the updraft
                        ZBUO_INTEG_DRY, ZBUO_INTEG_CLD,&! Integrated Buoyancy
                        ZENTR_CLD,ZDETR_CLD           ! wet entrainment and detrainment

REAL, DIMENSION(D%NIJT,D%NKT,KSV) :: &
                        ZSVM_F ! scalar variables 

                        
REAL, DIMENSION(D%NIJT,D%NKT) ::  &
                        ZTH_UP,                  &    ! updraft THETA 
                        ZRC_MIX, ZRI_MIX              ! guess of Rc and Ri for KF mixture

REAL, DIMENSION(D%NIJT,D%NKT) ::  ZCOEF  ! diminution coefficient for too high clouds 
                        
REAL, DIMENSION(D%NIJT)            ::  ZWTHVSURF  ! Surface w'thetav'

REAL  :: ZRDORV       ! RD/RV
REAL  :: ZRVORD       ! RV/RD


REAL, DIMENSION(D%NIJT) :: ZMIX1,ZMIX2,ZMIX3_CLD,ZMIX2_CLD

REAL, DIMENSION(D%NIJT) :: ZLUP         ! Upward Mixing length from the ground

INTEGER  :: JK,JIJ,JSV          ! loop counters

LOGICAL, DIMENSION(D%NIJT) ::  GTEST,GTESTLCL,GTESTETL
                               ! Test if the ascent continue, if LCL or ETL is reached
LOGICAL                          ::  GLMIX 
                               ! To choose upward or downward mixing length
LOGICAL, DIMENSION(D%NIJT)              :: GWORK1
LOGICAL, DIMENSION(D%NIJT,D%NKT) :: GWORK2

INTEGER  :: ITEST

REAL, DIMENSION(D%NIJT) :: ZRC_UP, ZRI_UP, ZRV_UP,&
                                 ZRSATW, ZRSATI,&
                                 ZPART_DRY

REAL  :: ZDEPTH_MAX1, ZDEPTH_MAX2 ! control auto-extinction process

REAL  :: ZTMAX,ZRMAX  ! control value

REAL, DIMENSION(D%NIJT) :: ZSURF
REAL, DIMENSION(D%NIJT,D%NKT) :: ZSHEAR,ZDUDZ,ZDVDZ ! vertical wind shear
!
REAL, DIMENSION(D%NIJT,D%NKT) :: ZWK
REAL, DIMENSION(D%NIJT,16) :: ZBUF
!
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
!
!                       1.3  Declaration of additional local variables for compute_entr_detr
!
! Variables for cloudy part
REAL, DIMENSION(D%NIJT) :: ZKIC, ZKIC_F2  ! fraction of env. mass in the muxtures
REAL, DIMENSION(D%NIJT) :: ZEPSI,ZDELTA   ! factor entrainment detrainment
REAL                   :: ZEPSI_CLOUD    ! factor entrainment detrainment
REAL                   :: ZCOEFFMF_CLOUD ! factor for compputing entr. detr.
REAL, DIMENSION(D%NIJT) :: ZMIXTHL,ZMIXRT ! Thetal and rt in the mixtures
REAL, DIMENSION(D%NIJT) :: ZTHMIX         ! Theta and Thetav  of mixtures
REAL, DIMENSION(D%NIJT) :: ZRVMIX,ZRCMIX,ZRIMIX ! mixing ratios in mixtures
REAL, DIMENSION(D%NIJT) :: ZTHVMIX, ZTHVMIX_F2 ! Theta and Thetav  of mixtures
REAL, DIMENSION(D%NIJT) :: ZTHV_UP_F2     ! thv_up at flux point kk+kkl
REAL, DIMENSION(D%NIJT) :: ZRSATW_ED, ZRSATI_ED ! working arrays (mixing ratio at saturation)
REAL, DIMENSION(D%NIJT) :: ZTHV           ! theta V of environment at the bottom of cloudy part  
REAL                   :: ZKIC_INIT      !Initial value of ZKIC
REAL                   :: ZCOTHVU              ! Variation of Thvup between bottom and top of cloudy part

! Variables for dry part
REAL                   :: ZFOESW, ZFOESI       ! saturating vapor pressure
REAL                   :: ZDRSATODP            ! d.Rsat/dP
REAL                   :: ZT                   ! Temperature
REAL                   :: ZWK0D                ! Work array

! Variables for dry and cloudy parts
REAL, DIMENSION(D%NIJT) :: ZCOEFF_MINUS_HALF,&  ! Variation of Thv between mass points kk-kkl and kk
                                  ZCOEFF_PLUS_HALF     ! Variation of Thv between mass points kk and kk+kkl
REAL, DIMENSION(D%NIJT) :: ZPRE                 ! pressure at the bottom of the cloudy part
REAL, DIMENSION(D%NIJT) :: ZG_O_THVREF_ED
REAL, DIMENSION(D%NIJT) :: ZFRAC_ICE            ! fraction of ice
REAL, DIMENSION(D%NIJT) :: ZDZ_STOP,&           ! Exact Height of the LCL above flux level KK
                          ZTHV_MINUS_HALF,&    ! Thv at flux point(kk)  
                          ZTHV_PLUS_HALF       ! Thv at flux point(kk+kkl)
REAL                   :: ZDZ                  ! Delta Z used in computations
INTEGER :: JKLIM
INTEGER :: IIJB,IIJE ! physical horizontal domain indices
INTEGER :: IKT,IKB,IKE,IKL
INTEGER :: II
INTEGER :: JITER
INTEGER :: JIJ_1
INTEGER :: IIJB_1
INTEGER :: IIJE_1
INTEGER, PARAMETER :: IEXN_1=1
INTEGER, PARAMETER :: IRVSAT_1=2
INTEGER, PARAMETER :: ICPH_1=3
INTEGER, PARAMETER :: IRLTEMP_1=4
INTEGER, PARAMETER :: ICPH2_1=5
INTEGER, PARAMETER :: IT_1=6
INTEGER, PARAMETER :: ILVOCPEXN_1=7
INTEGER, PARAMETER :: ILSOCPEXN_1=8
INTEGER, PARAMETER :: IDRSATODT_1=9
INTEGER, PARAMETER :: IDRSATODTW_1=10
INTEGER, PARAMETER :: IDRSATODTI_1=11
INTEGER, PARAMETER :: IFOESW_1=12
INTEGER, PARAMETER :: IFOESI_1=13
INTEGER, PARAMETER :: ILOGT_1=14
INTEGER, PARAMETER :: I99PP_1=15
INTEGER, PARAMETER :: I1PRT_1=16
REAL :: ZVAR1
REAL :: ZVAR2
REAL :: ZTPOW2
REAL :: ZDELT
INTEGER :: II_3
INTEGER :: JITER_3
INTEGER :: JIJ_4
INTEGER :: IIJB_4
INTEGER :: IIJE_4
INTEGER, PARAMETER :: IEXN_4=1
INTEGER, PARAMETER :: IRVSAT_4=2
INTEGER, PARAMETER :: ICPH_4=3
INTEGER, PARAMETER :: IRLTEMP_4=4
INTEGER, PARAMETER :: ICPH2_4=5
INTEGER, PARAMETER :: IT_4=6
INTEGER, PARAMETER :: ILVOCPEXN_4=7
INTEGER, PARAMETER :: ILSOCPEXN_4=8
INTEGER, PARAMETER :: IDRSATODT_4=9
INTEGER, PARAMETER :: IDRSATODTW_4=10
INTEGER, PARAMETER :: IDRSATODTI_4=11
INTEGER, PARAMETER :: IFOESW_4=12
INTEGER, PARAMETER :: IFOESI_4=13
INTEGER, PARAMETER :: ILOGT_4=14
INTEGER, PARAMETER :: I99PP_4=15
INTEGER, PARAMETER :: I1PRT_4=16
REAL :: ZVAR1_3
REAL :: ZVAR2_3
REAL :: ZTPOW2_3
REAL :: ZDELT_3
INTEGER :: II_1
INTEGER :: JITER_1
INTEGER :: JIJ_2
INTEGER :: IIJB_2
INTEGER :: IIJE_2
INTEGER, PARAMETER :: IEXN_2=1
INTEGER, PARAMETER :: IRVSAT_2=2
INTEGER, PARAMETER :: ICPH_2=3
INTEGER, PARAMETER :: IRLTEMP_2=4
INTEGER, PARAMETER :: ICPH2_2=5
INTEGER, PARAMETER :: IT_2=6
INTEGER, PARAMETER :: ILVOCPEXN_2=7
INTEGER, PARAMETER :: ILSOCPEXN_2=8
INTEGER, PARAMETER :: IDRSATODT_2=9
INTEGER, PARAMETER :: IDRSATODTW_2=10
INTEGER, PARAMETER :: IDRSATODTI_2=11
INTEGER, PARAMETER :: IFOESW_2=12
INTEGER, PARAMETER :: IFOESI_2=13
INTEGER, PARAMETER :: ILOGT_2=14
INTEGER, PARAMETER :: I99PP_2=15
INTEGER, PARAMETER :: I1PRT_2=16
REAL :: ZVAR1_1
REAL :: ZVAR2_1
REAL :: ZTPOW2_1
REAL :: ZDELT_1
INTEGER :: II_2
INTEGER :: JITER_2
INTEGER :: JIJ_3
INTEGER :: IIJB_3
INTEGER :: IIJE_3
INTEGER, PARAMETER :: IEXN_3=1
INTEGER, PARAMETER :: IRVSAT_3=2
INTEGER, PARAMETER :: ICPH_3=3
INTEGER, PARAMETER :: IRLTEMP_3=4
INTEGER, PARAMETER :: ICPH2_3=5
INTEGER, PARAMETER :: IT_3=6
INTEGER, PARAMETER :: ILVOCPEXN_3=7
INTEGER, PARAMETER :: ILSOCPEXN_3=8
INTEGER, PARAMETER :: IDRSATODT_3=9
INTEGER, PARAMETER :: IDRSATODTW_3=10
INTEGER, PARAMETER :: IDRSATODTI_3=11
INTEGER, PARAMETER :: IFOESW_3=12
INTEGER, PARAMETER :: IFOESI_3=13
INTEGER, PARAMETER :: ILOGT_3=14
INTEGER, PARAMETER :: I99PP_3=15
INTEGER, PARAMETER :: I1PRT_3=16
REAL :: ZVAR1_2
REAL :: ZVAR2_2
REAL :: ZTPOW2_2
REAL :: ZDELT_2
INTEGER :: II_4
INTEGER :: JITER_4
INTEGER :: JIJ_5
INTEGER :: IIJB_5
INTEGER :: IIJE_5
INTEGER, PARAMETER :: IEXN_5=1
INTEGER, PARAMETER :: IRVSAT_5=2
INTEGER, PARAMETER :: ICPH_5=3
INTEGER, PARAMETER :: IRLTEMP_5=4
INTEGER, PARAMETER :: ICPH2_5=5
INTEGER, PARAMETER :: IT_5=6
INTEGER, PARAMETER :: ILVOCPEXN_5=7
INTEGER, PARAMETER :: ILSOCPEXN_5=8
INTEGER, PARAMETER :: IDRSATODT_5=9
INTEGER, PARAMETER :: IDRSATODTW_5=10
INTEGER, PARAMETER :: IDRSATODTI_5=11
INTEGER, PARAMETER :: IFOESW_5=12
INTEGER, PARAMETER :: IFOESI_5=13
INTEGER, PARAMETER :: ILOGT_5=14
INTEGER, PARAMETER :: I99PP_5=15
INTEGER, PARAMETER :: I1PRT_5=16
REAL :: ZVAR1_4
REAL :: ZVAR2_4
REAL :: ZTPOW2_4
REAL :: ZDELT_4
INTEGER, PARAMETER :: IEXN=1, IRVSAT=2, ICPH=3, IRLTEMP=4, ICPH2=5, IT=6, ILVOCPEXN=7, ILSOCPEXN=8, &
                    & IDRSATODT=9, IDRSATODTW=10, IDRSATODTI=11, IFOESW=12, IFOESI=13, &
                    & ILOGT=14, I99PP=15, I1PRT=16
!
IF (LHOOK) CALL DR_HOOK('COMPUTE_UPDRAFT',0,ZHOOK_HANDLE)
!
IIJE=D%NIJE
IIJB=D%NIJB
IKT=D%NKT
IKB=D%NKB
IKE=D%NKE
IKL=D%NKL
!
! Thresholds for the  perturbation of
! theta_l and r_t at the first level of the updraft
ZTMAX=2.0
ZRMAX=1.E-3
!------------------------------------------------------------------------

!                     INITIALISATION

! Initialisation of the constants   
ZRDORV   = CST%XRD / CST%XRV   !=0.622
ZRVORD   = (CST%XRV / CST%XRD) 

ZDEPTH_MAX1=3000. ! clouds with depth inferior to this value are keeped untouched
ZDEPTH_MAX2=4000. ! clouds with depth superior to this value are suppressed

!                 Local variables, internal domain

IF (OENTR_DETR) THEN
  ! Initialisation of intersesting Level :LCL,ETL,CTL
  KKLCL(:)=IKE
  KKETL(:)=IKE
  KKCTL(:)=IKE

  !
  ! Initialisation
  !* udraft governing variables
  PEMF(:,:)=0.
  PDETR(:,:)=0.
  PENTR(:,:)=0.

  ! Initialisation
  !* updraft core variables
  PRV_UP(:,:)=0.
  PRC_UP(:,:)=0.
  PRI_UP(:,:)=0.
  PW_UP(:,:)=0.
  ZTH_UP(:,:)=0.
  PFRAC_UP(:,:)=0.
  PTHV_UP(:,:)=0.

  PBUO_INTEG=0.

  PFRAC_ICE_UP(:,:)=0.
  !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  PRSAT_UP(IIJB:IIJE,1:IKT)=PRVM(IIJB:IIJE,1:IKT) ! should be initialised correctly but is (normaly) not used
  !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)

  !cloud/dry air mixture cloud content
  ZRC_MIX = 0.
  ZRI_MIX = 0.

END IF

! Initialisation of environment variables at t-dt
! variables at flux level
CALL MZM_MF(D, PTHLM(:,:), ZTHLM_F(:,:))
CALL MZM_MF(D, PRTM(:,:), ZRTM_F (:,:))
CALL MZM_MF(D, PUM(:,:), ZUM_F  (:,:))
CALL MZM_MF(D, PVM(:,:), ZVM_F  (:,:))
CALL MZM_MF(D, PTKEM(:,:), ZTKEM_F(:,:))

DO JSV=1,KSV
  IF (ONOMIXLG .AND. JSV >= KSV_LGBEG .AND. JSV<= KSV_LGEND) CYCLE
  CALL MZM_MF(D, PSVM(:,:,JSV), ZSVM_F(:,:,JSV))
END DO
!                     
!          Initialisation of updraft characteristics 
!$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
PTHL_UP(IIJB:IIJE,1:IKT)=ZTHLM_F(IIJB:IIJE,1:IKT)
PRT_UP(IIJB:IIJE,1:IKT)=ZRTM_F(IIJB:IIJE,1:IKT)
PU_UP(IIJB:IIJE,1:IKT)=ZUM_F(IIJB:IIJE,1:IKT)
PV_UP(IIJB:IIJE,1:IKT)=ZVM_F(IIJB:IIJE,1:IKT)
!$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
!$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT,JSV=1:KSV)
PSV_UP(IIJB:IIJE,1:IKT,:)=ZSVM_F(IIJB:IIJE,1:IKT,:)
!$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT,JSV=1:KSV)

! Computation or initialisation of updraft characteristics at the KKB level
! thetal_up,rt_up,thetaV_up, w2,Buoyancy term and mass flux (PEMF)
!$mnh_expand_array(JIJ=IIJB:IIJE)
PTHL_UP(IIJB:IIJE,IKB)= ZTHLM_F(IIJB:IIJE,IKB)+ &
                            & MAX(0.,MIN(ZTMAX,(PSFTH(IIJB:IIJE)/SQRT(ZTKEM_F(IIJB:IIJE,IKB)))* PARAMMF%XALP_PERT))
PRT_UP(IIJB:IIJE,IKB) = ZRTM_F(IIJB:IIJE,IKB)+ &
                            & MAX(0.,MIN(ZRMAX,(PSFRV(IIJB:IIJE)/SQRT(ZTKEM_F(IIJB:IIJE,IKB)))* PARAMMF%XALP_PERT)) 
!$mnh_end_expand_array(JIJ=IIJB:IIJE)

IF (OENTR_DETR) THEN
  CALL MZM_MF(D, PTHM (:,:), ZTHM_F (:,:))
  CALL MZM_MF(D, PPABSM(:,:), ZPRES_F(:,:))
  CALL MZM_MF(D, PRHODREF(:,:), ZRHO_F (:,:))
  CALL MZM_MF(D, PRVM(:,:), ZRVM_F (:,:))

  !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  ! thetav at mass and flux levels
  ZTHVM_F(IIJB:IIJE,1:IKT)=ZTHM_F(IIJB:IIJE,1:IKT)* &
                                    &((1.+ZRVORD*ZRVM_F(IIJB:IIJE,1:IKT))/(1.+ZRTM_F(IIJB:IIJE,1:IKT)))
  ZTHVM(IIJB:IIJE,1:IKT)=PTHM(IIJB:IIJE,1:IKT)* &
                                    &((1.+ZRVORD*PRVM(IIJB:IIJE,1:IKT))/(1.+PRTM(IIJB:IIJE,1:IKT)))

  PTHV_UP(IIJB:IIJE,1:IKT)=ZTHVM_F(IIJB:IIJE,1:IKT)
  !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)

  ZW_UP2(:,:)=0.
  !$mnh_expand_array(JIJ=IIJB:IIJE)
  ZW_UP2(IIJB:IIJE,IKB) = MAX(0.0001,(2./3.)*ZTKEM_F(IIJB:IIJE,IKB))

  ! Computation of non conservative variable for the KKB level of the updraft
  ! (all or nothing ajustement)
  PRC_UP(:,IKB)=0.
  PRI_UP(:,IKB)=0.
  !$mnh_end_expand_array(JIJ=IIJB:IIJE)
  IIJB_1=MERGE(D%NIJB, 1, .TRUE.)
IIJE_1=MERGE(D%NIJE, D%NIJT, .TRUE.)
!Number of iterations
JITER=2
!
!Computation of PBUF(IIJB:IIJE, ICPH2) depending on dummy arguments received
ZBUF(IIJB_1:IIJE_1, ICPH2_1)=0
!
!Computation of an approximate state thanks to PRL and PRI guess
ZBUF(IIJB_1:IIJE_1, IEXN_1)=(ZPRES_F(IIJB_1:IIJE_1, IKB)/CST%XP00) ** CST%RDSCPD

DO JIJ_1=IIJB_1,IIJE_1
  ZBUF(JIJ_1, I99PP_1)=0.99*ZPRES_F(JIJ_1, IKB)
  PRV_UP(JIJ_1, IKB)=PRT_UP(JIJ_1, IKB)-PRC_UP(JIJ_1, IKB)-PRI_UP(JIJ_1, IKB)
  ZBUF(JIJ_1, ICPH_1)=CST%XCPD+ CST%XCPV * PRV_UP(JIJ_1, IKB)+ CST%XCL * PRC_UP(JIJ_1, IKB) + CST%XCI * PRI_UP(JIJ_1, IKB) + ZBUF(JIJ_1, ICPH2_1)
  ZVAR2=ZBUF(JIJ_1, ICPH_1)*ZBUF(JIJ_1, IEXN_1)
  ZDELT=(PTHL_UP(JIJ_1, IKB)*ZBUF(JIJ_1, IEXN_1))-CST%XTT
  ZBUF(JIJ_1, ILVOCPEXN_1) = (CST%XLVTT + (CST%XCPV-CST%XCL) * ZDELT) /ZVAR2
  ZBUF(JIJ_1, ILSOCPEXN_1) = (CST%XLSTT + (CST%XCPV-CST%XCI) * ZDELT) /ZVAR2 
  ZTH_UP(JIJ_1, IKB)=PTHL_UP(JIJ_1, IKB)+ZBUF(JIJ_1, ILVOCPEXN_1)*PRC_UP(JIJ_1, IKB)+ZBUF(JIJ_1, ILSOCPEXN_1)*PRI_UP(JIJ_1, IKB)
  ZBUF(JIJ_1, I1PRT_1)=1+PRT_UP(JIJ_1, IKB)
ENDDO
!
!
!       2 Iteration
!         ---------

DO II=1,JITER
  IF (.FALSE.) THEN
    ZBUF(IIJB_1:IIJE_1, IT_1)=ZTH_UP(IIJB_1:IIJE_1, IKB)
  ELSE
    ZBUF(IIJB_1:IIJE_1, IT_1)=ZTH_UP(IIJB_1:IIJE_1, IKB)*ZBUF(IIJB_1:IIJE_1, IEXN_1)
  END IF
  !Computation of liquid/ice fractions
  PFRAC_ICE_UP(IIJB_1:IIJE_1, IKB) = 0.
  DO JIJ_1=IIJB_1, IIJE_1
    IF(PRC_UP(JIJ_1, IKB)+PRI_UP(JIJ_1, IKB) > 1.E-20) THEN
      PFRAC_ICE_UP(JIJ_1, IKB) = PRI_UP(JIJ_1, IKB) / (PRC_UP(JIJ_1, IKB)+PRI_UP(JIJ_1, IKB))
    ENDIF
  ENDDO
  DO JIJ_1=IIJB_1, IIJE_1
  !$acc Meso-NH-champignon
SELECT CASE(NEBN%CFRAC_ICE_SHALLOW_MF)
  CASE ('T') !using Temperature
    PFRAC_ICE_UP(JIJ_1, IKB) = MAX( 0., MIN(1., (( NEBN%XTMAXMIX - ZBUF(JIJ_1, IT_1) ) / ( NEBN%XTMAXMIX - NEBN%XTMINMIX )) ) ) ! freezing interval
  CASE ('O') !using Temperature with old formulae
    PFRAC_ICE_UP(JIJ_1, IKB) = MAX( 0., MIN(1., (( CST%XTT - ZBUF(JIJ_1, IT_1) ) / 40.) ) ) ! freezing interval
  CASE ('N') !No ice
    PFRAC_ICE_UP(JIJ_1, IKB) = 0.
  CASE ('S') !Same as previous
    ! (almost) nothing to do
    PFRAC_ICE_UP(JIJ_1, IKB) = MAX( 0., MIN(1., PFRAC_ICE_UP(JIJ_1, IKB) ) )
  CASE DEFAULT    
END SELECT



  END DO
!Computation of Rvsat and dRsat/dT
  !In this version QSAT, QSATI, DQSAT and DQASATI functions are not used
  !due to performance issue

  ! Log does not vectorize on all compilers:
  ZBUF(IIJB_1:IIJE_1, ILOGT_1)=LOG(ZBUF(IIJB_1:IIJE_1, IT_1))

  DO JIJ_1=IIJB_1, IIJE_1
    ZBUF(JIJ_1, IFOESW_1) = MIN(EXP( CST%XALPW - CST%XBETAW/ZBUF(JIJ_1, IT_1) - CST%XGAMW*ZBUF(JIJ_1, ILOGT_1)  ), ZBUF(JIJ_1, I99PP_1))
    ZBUF(JIJ_1, IFOESI_1) = MIN(EXP( CST%XALPI - CST%XBETAI/ZBUF(JIJ_1, IT_1) - CST%XGAMI*ZBUF(JIJ_1, ILOGT_1)  ), ZBUF(JIJ_1, I99PP_1))
    ZRSATW(JIJ_1) = CST%XRD/CST%XRV*ZBUF(JIJ_1, IFOESW_1)/ZPRES_F(JIJ_1, IKB) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ_1, IFOESW_1)/ZPRES_F(JIJ_1, IKB))
    ZRSATI(JIJ_1) = CST%XRD/CST%XRV*ZBUF(JIJ_1, IFOESI_1)/ZPRES_F(JIJ_1, IKB) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ_1, IFOESI_1)/ZPRES_F(JIJ_1, IKB))
    ZTPOW2=ZBUF(JIJ_1, IT_1)**2
    ZBUF(JIJ_1, IDRSATODTW_1) = ZRSATW(JIJ_1) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ_1, IFOESW_1)/ZPRES_F(JIJ_1, IKB) ) &
                     * (CST%XBETAW/ZTPOW2 - CST%XGAMW/ZBUF(JIJ_1, IT_1))*ZBUF(JIJ_1, I1PRT_1)
    ZBUF(JIJ_1, IDRSATODTI_1) = ZRSATI(JIJ_1) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ_1, IFOESI_1)/ZPRES_F(JIJ_1, IKB) ) &
                     * (CST%XBETAI/ZTPOW2 - CST%XGAMI/ZBUF(JIJ_1, IT_1))*ZBUF(JIJ_1, I1PRT_1)
    !PRSATW(JIJ) =  QSAT(PBUF(JIJ, IT),PP(JIJ)) !qsatw
    !PRSATI(JIJ) = QSATI(PBUF(JIJ, IT),PP(JIJ)) !qsati
    !PBUF(JIJ, IDRSATODTW) =  DQSAT(PBUF(JIJ, IT),PP(JIJ),PRSATW(JIJ))*PBUF(JIJ, I1PRT)
    !PBUF(JIJ, IDRSATODTI) = DQSATI(PBUF(JIJ, IT),PP(JIJ),PRSATI(JIJ))*PBUF(JIJ, I1PRT)
    ZRSATW(JIJ_1) = ZRSATW(JIJ_1)*ZBUF(JIJ_1, I1PRT_1)
    ZRSATI(JIJ_1) = ZRSATI(JIJ_1)*ZBUF(JIJ_1, I1PRT_1)
    ZBUF(JIJ_1, IRVSAT_1) = ZRSATW(JIJ_1)*(1-PFRAC_ICE_UP(JIJ_1, IKB)) + ZRSATI(JIJ_1)*PFRAC_ICE_UP(JIJ_1, IKB)
    ZBUF(JIJ_1, IDRSATODT_1) = (ZBUF(JIJ_1, IDRSATODTW_1)*(1-PFRAC_ICE_UP(JIJ_1, IKB))+ &
              & ZBUF(JIJ_1, IDRSATODTI_1)*PFRAC_ICE_UP(JIJ_1, IKB))

    !Computation of new PRL, PRI and PRV
    !Correction term applied to (PRV(JIJ)-PBUF(JIJ, IRVSAT)) is computed assuming that
    !PBUF(JIJ, ILVOCPEXN), PBUF(JIJ, ILSOCPEXN) and PBUF(JIJ, ICPH) don't vary to much with T. It takes into account
    !the variation (estimated linear) of Qsat with T
    ZBUF(JIJ_1, IRLTEMP_1)=(PRV_UP(JIJ_1, IKB)-ZBUF(JIJ_1, IRVSAT_1))/ &
                  &(1 + ZBUF(JIJ_1, IDRSATODT_1)*ZBUF(JIJ_1, IEXN_1)* &
                  &     (ZBUF(JIJ_1, ILVOCPEXN_1)*(1-PFRAC_ICE_UP(JIJ_1, IKB))+ZBUF(JIJ_1, ILSOCPEXN_1)*PFRAC_ICE_UP(JIJ_1, IKB)))
    ZBUF(JIJ_1, IRLTEMP_1)=MIN(MAX(-PRC_UP(JIJ_1, IKB)-PRI_UP(JIJ_1, IKB), ZBUF(JIJ_1, IRLTEMP_1)),PRV_UP(JIJ_1, IKB))
    PRV_UP(JIJ_1, IKB)=PRV_UP(JIJ_1, IKB)-ZBUF(JIJ_1, IRLTEMP_1)
    PRC_UP(JIJ_1, IKB)=PRC_UP(JIJ_1, IKB)+PRI_UP(JIJ_1, IKB)+ZBUF(JIJ_1, IRLTEMP_1)
    PRI_UP(JIJ_1, IKB)=PFRAC_ICE_UP(JIJ_1, IKB)     * (PRC_UP(JIJ_1, IKB))
    PRC_UP(JIJ_1, IKB)=(1-PFRAC_ICE_UP(JIJ_1, IKB)) * (PRT_UP(JIJ_1, IKB) - PRV_UP(JIJ_1, IKB))

    !Computation of Cph (as defined in Meso-NH doc, equation 2.2, to be used with mixing ratios)
    ZBUF(JIJ_1, ICPH_1)=CST%XCPD+ CST%XCPV * PRV_UP(JIJ_1, IKB)+ CST%XCL * PRC_UP(JIJ_1, IKB) + CST%XCI * PRI_UP(JIJ_1, IKB) + ZBUF(JIJ_1, ICPH2_1)

    !Computation of L/Cph/EXN, then new PTH
    ZVAR2=ZBUF(JIJ_1, ICPH_1)*ZBUF(JIJ_1, IEXN_1)
    ZBUF(JIJ_1, ILVOCPEXN_1) = (CST%XLVTT + (CST%XCPV-CST%XCL) * (ZBUF(JIJ_1, IT_1)-CST%XTT)) /ZVAR2
    ZBUF(JIJ_1, ILSOCPEXN_1) = (CST%XLSTT + (CST%XCPV-CST%XCI) * (ZBUF(JIJ_1, IT_1)-CST%XTT)) /ZVAR2
    ZTH_UP(JIJ_1, IKB)=PTHL_UP(JIJ_1, IKB)+ZBUF(JIJ_1, ILVOCPEXN_1)*PRC_UP(JIJ_1, IKB)+ZBUF(JIJ_1, ILSOCPEXN_1)*PRI_UP(JIJ_1, IKB)

    !Computation of estimated mixing ration at saturation
    !To compute the adjustement a first order development was used
    ZVAR1=ZTH_UP(JIJ_1, IKB)*ZBUF(JIJ_1, IEXN_1)-ZBUF(JIJ_1, IT_1)
    ZRSATW(JIJ_1)=ZRSATW(JIJ_1) + ZBUF(JIJ_1, IDRSATODTW_1)*ZVAR1
    ZRSATI(JIJ_1)=ZRSATI(JIJ_1) + ZBUF(JIJ_1, IDRSATODTI_1)*ZVAR1
  ENDDO
ENDDO



  !$mnh_expand_array(JIJ=IIJB:IIJE)
  ! compute updraft thevav and buoyancy term at KKB level
  PTHV_UP(IIJB:IIJE,IKB) = ZTH_UP(IIJB:IIJE,IKB)*&
                               & ((1+ZRVORD*PRV_UP(IIJB:IIJE,IKB))/(1+PRT_UP(IIJB:IIJE,IKB)))
  ! compute mean rsat in updraft
  PRSAT_UP(IIJB:IIJE,IKB) = ZRSATW(IIJB:IIJE)*(1-PFRAC_ICE_UP(IIJB:IIJE,IKB)) + &
                              & ZRSATI(IIJB:IIJE)*PFRAC_ICE_UP(IIJB:IIJE,IKB)
  !$mnh_end_expand_array(JIJ=IIJB:IIJE)
  ! Closure assumption for mass flux at KKB level
  !

  !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  ZG_O_THVREF(IIJB:IIJE,1:IKT)=CST%XG/ZTHVM_F(IIJB:IIJE,1:IKT)
  !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)

  ! compute L_up
  GLMIX=.TRUE.
  !$mnh_expand_array(JIJ=IIJB:IIJE)
  ZTKEM_F(IIJB:IIJE,IKB)=0.
  !$mnh_end_expand_array(JIJ=IIJB:IIJE)
  !
  IF(TURBN%CTURBLEN=='RM17') THEN
    CALL GZ_M_W_MF(D, PUM, PDZZ, ZWK)
    CALL MZF_MF(D, ZWK, ZDUDZ)
    CALL GZ_M_W_MF(D, PVM, PDZZ, ZWK)
    CALL MZF_MF(D, ZWK, ZDVDZ)
    !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
    ZSHEAR(IIJB:IIJE,1:IKT) = SQRT(ZDUDZ(IIJB:IIJE,1:IKT)**2 + ZDVDZ(IIJB:IIJE,1:IKT)**2)
    !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  ELSE
    ZSHEAR = 0. !no shear in bl89 mixing length
  END IF
  !
  CALL COMPUTE_BL89_ML(D, CST, CSTURB, PDZZ,ZTKEM_F(:,IKB),&
                      &ZG_O_THVREF(:,IKB),ZTHVM,IKB,GLMIX,.TRUE.,ZSHEAR,ZLUP)
  !$mnh_expand_array(JIJ=IIJB:IIJE)
  ZLUP(IIJB:IIJE)=MAX(ZLUP(IIJB:IIJE),1.E-10)

  ! Compute Buoyancy flux at the ground
  ZWTHVSURF(IIJB:IIJE) = (ZTHVM_F(IIJB:IIJE,IKB)/ZTHM_F(IIJB:IIJE,IKB))*PSFTH(IIJB:IIJE)+     &
                (0.61*ZTHM_F(IIJB:IIJE,IKB))*PSFRV(IIJB:IIJE)
  !$mnh_end_expand_array(JIJ=IIJB:IIJE)

  ! Mass flux at KKB level (updraft triggered if PSFTH>0.)
  IF (PARAMMF%LGZ) THEN
    IF(PDX==0. .OR. PDY==0.) THEN                                                                                                   
      CALL PRINT_MSG(NVERB_FATAL, 'GEN', 'COMPUTE_UPDRAFT', 'PDX or PDY is NULL with option LGZ!')                                  
    ENDIF
    !$mnh_expand_array(JIJ=IIJB:IIJE)
    ZSURF(IIJB:IIJE)=TANH(PARAMMF%XGZ*SQRT(PDX*PDY)/ZLUP(IIJB:IIJE))
    !$mnh_end_expand_array(JIJ=IIJB:IIJE)
  ELSE
    ZSURF(IIJB:IIJE)=1.
  END IF
  !$mnh_expand_where(JIJ=IIJB:IIJE)
  WHERE (ZWTHVSURF(IIJB:IIJE)>0.)
    PEMF(IIJB:IIJE,IKB) = PARAMMF%XCMF * ZSURF(IIJB:IIJE) * ZRHO_F(IIJB:IIJE,IKB) *  &
            ((ZG_O_THVREF(IIJB:IIJE,IKB))*ZWTHVSURF(IIJB:IIJE)*ZLUP(IIJB:IIJE))**(1./3.)
    PFRAC_UP(IIJB:IIJE,IKB)=MIN(PEMF(IIJB:IIJE,IKB)/(SQRT(ZW_UP2(IIJB:IIJE,IKB))*ZRHO_F(IIJB:IIJE,IKB)), &
                                   &PARAMMF%XFRAC_UP_MAX)
    ZW_UP2(IIJB:IIJE,IKB)=(PEMF(IIJB:IIJE,IKB)/(PFRAC_UP(IIJB:IIJE,IKB)*ZRHO_F(IIJB:IIJE,IKB)))**2
    GTEST(IIJB:IIJE)=.TRUE.
  ELSEWHERE
    PEMF(IIJB:IIJE,IKB) =0.
    GTEST(IIJB:IIJE)=.FALSE.
  ENDWHERE
  !$mnh_end_expand_where(JIJ=IIJB:IIJE)
ELSE
  !$mnh_expand_array(JIJ=IIJB:IIJE)
  GTEST(IIJB:IIJE)=PEMF(IIJB:IIJE,IKB+IKL)>0.
  !$mnh_end_expand_array(JIJ=IIJB:IIJE)
END IF

!--------------------------------------------------------------------------

!                        3. Vertical ascending loop
!                           -----------------------
!
! If GTEST = T the updraft starts from the KKB level and stops when GTEST becomes F
!
!
GTESTLCL(:)=.FALSE.
GTESTETL(:)=.FALSE.

!       Loop on vertical level

DO JK=IKB,IKE-IKL,IKL

  ! IF the updraft top is reached for all column, stop the loop on levels
  ITEST=0
  DO JIJ=IIJB,IIJE
    IF(GTEST(JIJ)) ITEST = ITEST + 1
  END DO
  IF (ITEST==0) CYCLE

  !       Computation of entrainment and detrainment with KF90
  !       parameterization in clouds and LR01 in subcloud layer


  ! to find the LCL (check if JK is LCL or not)
  !$mnh_expand_where(JIJ=IIJB:IIJE)
  WHERE ((PRC_UP(IIJB:IIJE,JK)+PRI_UP(IIJB:IIJE,JK)>0.).AND.(.NOT.(GTESTLCL(IIJB:IIJE))))
      KKLCL(IIJB:IIJE) = JK           
      GTESTLCL(IIJB:IIJE)=.TRUE.
  ENDWHERE
  !$mnh_end_expand_where(JIJ=IIJB:IIJE)

  ! COMPUTE PENTR and PDETR at mass level JK
  IF (OENTR_DETR) THEN
    IF(JK/=IKB) THEN
      !$mnh_expand_array(JIJ=IIJB:IIJE)
      ZRC_MIX(IIJB:IIJE,JK) = ZRC_MIX(IIJB:IIJE,JK-IKL) ! guess of Rc of mixture
      ZRI_MIX(IIJB:IIJE,JK) = ZRI_MIX(IIJB:IIJE,JK-IKL) ! guess of Ri of mixture
      !$mnh_end_expand_array(JIJ=IIJB:IIJE)
    ENDIF
    ZCOEFFMF_CLOUD=PARAMMF%XENTR_MF * CST%XG / PARAMMF%XCRAD_MF
!$mnh_expand_array(JIJ=IIJB:IIJE)
ZG_O_THVREF_ED(IIJB:IIJE)=CST%XG/ZTHVM(IIJB:IIJE,JK)

ZFRAC_ICE(IIJB:IIJE)=PFRAC_ICE_UP(IIJB:IIJE, JK) ! to not modify fraction of ice

ZPRE(IIJB:IIJE)=ZPRES_F(IIJB:IIJE, JK)
!$mnh_end_expand_array(JIJ=IIJB:IIJE)

!                1.4 Estimation of PPART_DRY
DO JIJ=IIJB,IIJE
  IF(GTEST(JIJ) .AND. GTESTLCL(JIJ)) THEN
    !No dry part when condensation level is reached
    ZPART_DRY(JIJ)=0.
    ZDZ_STOP(JIJ)=0.
    ZPRE(JIJ)=ZPRES_F(JIJ, JK)
  ELSE IF (GTEST(JIJ) .AND. .NOT. GTESTLCL(JIJ)) THEN
    !Temperature at flux level KK
    ZT=ZTH_UP(JIJ, JK)*(ZPRES_F(JIJ, JK)/CST%XP00) ** (CST%XRD/CST%XCPD)
    !Saturating vapor pressure at flux level KK
    ZFOESW = MIN(EXP( CST%XALPW - CST%XBETAW/ZT - CST%XGAMW*LOG(ZT)  ), 0.99*ZPRES_F(JIJ, JK))
    ZFOESI = MIN(EXP( CST%XALPI - CST%XBETAI/ZT - CST%XGAMI*LOG(ZT)  ), 0.99*ZPRES_F(JIJ, JK))
    !Computation of d.Rsat / dP (partial derivations with respect to P and T
    !and use of T=Theta*(P/P0)**(R/Cp) to transform dT into dP with theta_up
    !constant at the vertical)
    ZDRSATODP=(CST%XBETAW/ZT-CST%XGAMW)*(1-ZFRAC_ICE(JIJ))+(CST%XBETAI/ZT-CST%XGAMI)*ZFRAC_ICE(JIJ)
    ZDRSATODP=((CST%XRD/CST%XCPD)*ZDRSATODP-1.)*PRSAT_UP(JIJ, JK)/ &
                &(ZPRES_F(JIJ, JK)-(ZFOESW*(1-ZFRAC_ICE(JIJ)) + ZFOESI*ZFRAC_ICE(JIJ)))
    !Use of d.Rsat / dP and pressure at flux level KK to find pressure (ZPRE)
    !where Rsat is equal to PRT_UP
    ZPRE(JIJ)=ZPRES_F(JIJ, JK)+(PRT_UP(JIJ, JK)-PRSAT_UP(JIJ, JK))/ZDRSATODP
    !Fraction of dry part (computed with pressure and used with heights, no
    !impact found when using log function here and for pressure on flux levels
    !computation)
    ZPART_DRY(JIJ)=MAX(0., MIN(1., (ZPRES_F(JIJ, JK)-ZPRE(JIJ))/(ZPRES_F(JIJ, JK)-ZPRES_F(JIJ, JK+IKL))))
    !Height above flux level KK of the cloudy part
    ZDZ_STOP(JIJ) = (PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK))*ZPART_DRY(JIJ)
  ELSE
    ZPART_DRY(JIJ)=0. ! value does not matter, here
  END IF
END DO

!               1.5 Gradient and flux values of thetav
!$mnh_expand_array(JIJ=IIJB:IIJE)
IF(JK/=IKB)THEN
  ZCOEFF_MINUS_HALF(IIJB:IIJE)=((ZTHVM(IIJB:IIJE,JK)-ZTHVM(IIJB:IIJE,JK-IKL))/PDZZ(IIJB:IIJE,JK))
  ZTHV_MINUS_HALF(IIJB:IIJE) = ZTHVM(IIJB:IIJE,JK) - &
                               & ZCOEFF_MINUS_HALF(IIJB:IIJE)*0.5*(PZZ(IIJB:IIJE,JK+IKL)-PZZ(IIJB:IIJE,JK))
ELSE
  ZCOEFF_MINUS_HALF(IIJB:IIJE)=0.
  ZTHV_MINUS_HALF(IIJB:IIJE) = ZTHVM(IIJB:IIJE,JK)
ENDIF
ZCOEFF_PLUS_HALF(IIJB:IIJE)  = ((ZTHVM(IIJB:IIJE,JK+IKL)-ZTHVM(IIJB:IIJE,JK))/PDZZ(IIJB:IIJE,JK+IKL))
ZTHV_PLUS_HALF(IIJB:IIJE)  = ZTHVM(IIJB:IIJE,JK) + &
                             & ZCOEFF_PLUS_HALF(IIJB:IIJE)*0.5*(PZZ(IIJB:IIJE,JK+IKL)-PZZ(IIJB:IIJE,JK))
!$mnh_end_expand_array(JIJ=IIJB:IIJE)

!               2  Dry part computation:
!                  Integral buoyancy and computation of PENTR and PDETR for dry part
!               --------------------------------------------------------------------

DO JIJ=IIJB,IIJE
  IF (GTEST(JIJ) .AND. ZPART_DRY(JIJ)>0.) THEN
    !Buoyancy computation in two parts to use change of gradient of theta v of environment
    !Between flux level KK and min(mass level, bottom of cloudy part)
    ZDZ=MIN(ZDZ_STOP(JIJ),(PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK))*0.5)
    ZBUO_INTEG_DRY(JIJ, JK) = ZG_O_THVREF_ED(JIJ)*ZDZ*&
                (0.5 * (  - ZCOEFF_MINUS_HALF(JIJ))*ZDZ  &
                  - ZTHV_MINUS_HALF(JIJ) + PTHV_UP(JIJ, JK) )

    !Between mass flux KK and bottom of cloudy part (if above mass flux)
    ZDZ=MAX(0., ZDZ_STOP(JIJ)-(PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK))*0.5)
    ZBUO_INTEG_DRY(JIJ, JK) = ZBUO_INTEG_DRY(JIJ, JK) + ZG_O_THVREF_ED(JIJ)*ZDZ*&
                (0.5 * (  - ZCOEFF_PLUS_HALF(JIJ))*ZDZ &
                  - ZTHVM(JIJ,JK) + PTHV_UP(JIJ, JK) )

    !Entr//Detr. computation
    IF (ZBUO_INTEG_DRY(JIJ, JK)>=0.) THEN
      PENTR(JIJ, JK) = 0.5/(PARAMMF%XABUO-PARAMMF%XBENTR*PARAMMF%XENTR_DRY)*&
                 LOG(1.+ (2.*(PARAMMF%XABUO-PARAMMF%XBENTR*PARAMMF%XENTR_DRY)/ZW_UP2(JIJ,JK))* &
                 ZBUO_INTEG_DRY(JIJ, JK))
      PDETR(JIJ, JK) = 0.
    ELSE
      PENTR(JIJ, JK) = 0.
      PDETR(JIJ, JK) = 0.5/(PARAMMF%XABUO)*&
                 LOG(1.+ (2.*(PARAMMF%XABUO)/ZW_UP2(JIJ,JK))* &
                 (-ZBUO_INTEG_DRY(JIJ, JK)))
    ENDIF
    PENTR(JIJ, JK) = PARAMMF%XENTR_DRY*PENTR(JIJ, JK)/(PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK))    
    PDETR(JIJ, JK) = PARAMMF%XDETR_DRY*PDETR(JIJ, JK)/(PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK))
    !Minimum value of detrainment
    ZWK0D=ZLUP(JIJ)-0.5*(PZZ(JIJ,JK)+PZZ(JIJ,JK+IKL))
    ZWK0D=SIGN(MAX(1., ABS(ZWK0D)), ZWK0D) ! ZWK0D must not be zero
    PDETR(JIJ, JK) = MAX(ZPART_DRY(JIJ)*PARAMMF%XDETR_LUP/ZWK0D, PDETR(JIJ, JK))
  ELSE
    !No dry part, condensation reached (OTESTLCL)
    ZBUO_INTEG_DRY(JIJ, JK) = 0.
    PENTR(JIJ, JK)=0.
    PDETR(JIJ, JK)=0.
  ENDIF
ENDDO

!               3  Wet part computation
!               -----------------------

!               3.1 Integral buoyancy for cloudy part

! Compute theta_v of updraft at flux level KK+KKL                   
!MIX variables are used to avoid declaring new variables
!but we are dealing with updraft and not mixture
!$mnh_expand_array(JIJ=IIJB:IIJE)
ZRCMIX(IIJB:IIJE)=PRC_UP(IIJB:IIJE, JK)
ZRIMIX(IIJB:IIJE)=PRI_UP(IIJB:IIJE, JK)
!$mnh_end_expand_array(JIJ=IIJB:IIJE)
IIJB_4=MERGE(D%NIJB, 1, .TRUE.)
IIJE_4=MERGE(D%NIJE, D%NIJT, .TRUE.)
!Number of iterations
JITER_3=2
!
!Computation of PBUF(IIJB:IIJE, ICPH2) depending on dummy arguments received
ZBUF(IIJB_4:IIJE_4, ICPH2_4)=0
!
!Computation of an approximate state thanks to PRL and PRI guess
ZBUF(IIJB_4:IIJE_4, IEXN_4)=(ZPRES_F(IIJB_4:IIJE_4, JK+IKL)/CST%XP00) ** CST%RDSCPD

DO JIJ_4=IIJB_4,IIJE_4
  ZBUF(JIJ_4, I99PP_4)=0.99*ZPRES_F(JIJ_4, JK+IKL)
  ZRVMIX(JIJ_4)=PRT_UP(JIJ_4, JK)-ZRCMIX(JIJ_4)-ZRIMIX(JIJ_4)
  ZBUF(JIJ_4, ICPH_4)=CST%XCPD+ CST%XCPV * ZRVMIX(JIJ_4)+ CST%XCL * ZRCMIX(JIJ_4) + CST%XCI * ZRIMIX(JIJ_4) + ZBUF(JIJ_4, ICPH2_4)
  ZVAR2_3=ZBUF(JIJ_4, ICPH_4)*ZBUF(JIJ_4, IEXN_4)
  ZDELT_3=(PTHL_UP(JIJ_4, JK)*ZBUF(JIJ_4, IEXN_4))-CST%XTT
  ZBUF(JIJ_4, ILVOCPEXN_4) = (CST%XLVTT + (CST%XCPV-CST%XCL) * ZDELT_3) /ZVAR2_3
  ZBUF(JIJ_4, ILSOCPEXN_4) = (CST%XLSTT + (CST%XCPV-CST%XCI) * ZDELT_3) /ZVAR2_3 
  ZTHMIX(JIJ_4)=PTHL_UP(JIJ_4, JK)+ZBUF(JIJ_4, ILVOCPEXN_4)*ZRCMIX(JIJ_4)+ZBUF(JIJ_4, ILSOCPEXN_4)*ZRIMIX(JIJ_4)
  ZBUF(JIJ_4, I1PRT_4)=1+PRT_UP(JIJ_4, JK)
ENDDO
!
!
!       2 Iteration
!         ---------

DO II_3=1,JITER_3
  IF (.FALSE.) THEN
    ZBUF(IIJB_4:IIJE_4, IT_4)=ZTHMIX(IIJB_4:IIJE_4)
  ELSE
    ZBUF(IIJB_4:IIJE_4, IT_4)=ZTHMIX(IIJB_4:IIJE_4)*ZBUF(IIJB_4:IIJE_4, IEXN_4)
  END IF
  !Computation of liquid/ice fractions
  ZFRAC_ICE(IIJB_4:IIJE_4) = 0.
  DO JIJ_4=IIJB_4, IIJE_4
    IF(ZRCMIX(JIJ_4)+ZRIMIX(JIJ_4) > 1.E-20) THEN
      ZFRAC_ICE(JIJ_4) = ZRIMIX(JIJ_4) / (ZRCMIX(JIJ_4)+ZRIMIX(JIJ_4))
    ENDIF
  ENDDO
  DO JIJ_4=IIJB_4, IIJE_4
  !$acc Meso-NH-champignon
SELECT CASE(NEBN%CFRAC_ICE_SHALLOW_MF)
  CASE ('T') !using Temperature
    ZFRAC_ICE(JIJ_4) = MAX( 0., MIN(1., (( NEBN%XTMAXMIX - ZBUF(JIJ_4, IT_4) ) / ( NEBN%XTMAXMIX - NEBN%XTMINMIX )) ) ) ! freezing interval
  CASE ('O') !using Temperature with old formulae
    ZFRAC_ICE(JIJ_4) = MAX( 0., MIN(1., (( CST%XTT - ZBUF(JIJ_4, IT_4) ) / 40.) ) ) ! freezing interval
  CASE ('N') !No ice
    ZFRAC_ICE(JIJ_4) = 0.
  CASE ('S') !Same as previous
    ! (almost) nothing to do
    ZFRAC_ICE(JIJ_4) = MAX( 0., MIN(1., ZFRAC_ICE(JIJ_4) ) )
  CASE DEFAULT    
END SELECT



  END DO
!Computation of Rvsat and dRsat/dT
  !In this version QSAT, QSATI, DQSAT and DQASATI functions are not used
  !due to performance issue

  ! Log does not vectorize on all compilers:
  ZBUF(IIJB_4:IIJE_4, ILOGT_4)=LOG(ZBUF(IIJB_4:IIJE_4, IT_4))

  DO JIJ_4=IIJB_4, IIJE_4
    ZBUF(JIJ_4, IFOESW_4) = MIN(EXP( CST%XALPW - CST%XBETAW/ZBUF(JIJ_4, IT_4) - CST%XGAMW*ZBUF(JIJ_4, ILOGT_4)  ), ZBUF(JIJ_4, I99PP_4))
    ZBUF(JIJ_4, IFOESI_4) = MIN(EXP( CST%XALPI - CST%XBETAI/ZBUF(JIJ_4, IT_4) - CST%XGAMI*ZBUF(JIJ_4, ILOGT_4)  ), ZBUF(JIJ_4, I99PP_4))
    ZRSATW_ED(JIJ_4) = CST%XRD/CST%XRV*ZBUF(JIJ_4, IFOESW_4)/ZPRES_F(JIJ_4, JK+IKL) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ_4, IFOESW_4)/ZPRES_F(JIJ_4, JK+IKL))
    ZRSATI_ED(JIJ_4) = CST%XRD/CST%XRV*ZBUF(JIJ_4, IFOESI_4)/ZPRES_F(JIJ_4, JK+IKL) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ_4, IFOESI_4)/ZPRES_F(JIJ_4, JK+IKL))
    ZTPOW2_3=ZBUF(JIJ_4, IT_4)**2
    ZBUF(JIJ_4, IDRSATODTW_4) = ZRSATW_ED(JIJ_4) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ_4, IFOESW_4)/ZPRES_F(JIJ_4, JK+IKL) ) &
                     * (CST%XBETAW/ZTPOW2_3 - CST%XGAMW/ZBUF(JIJ_4, IT_4))*ZBUF(JIJ_4, I1PRT_4)
    ZBUF(JIJ_4, IDRSATODTI_4) = ZRSATI_ED(JIJ_4) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ_4, IFOESI_4)/ZPRES_F(JIJ_4, JK+IKL) ) &
                     * (CST%XBETAI/ZTPOW2_3 - CST%XGAMI/ZBUF(JIJ_4, IT_4))*ZBUF(JIJ_4, I1PRT_4)
    !PRSATW(JIJ) =  QSAT(PBUF(JIJ, IT),PP(JIJ)) !qsatw
    !PRSATI(JIJ) = QSATI(PBUF(JIJ, IT),PP(JIJ)) !qsati
    !PBUF(JIJ, IDRSATODTW) =  DQSAT(PBUF(JIJ, IT),PP(JIJ),PRSATW(JIJ))*PBUF(JIJ, I1PRT)
    !PBUF(JIJ, IDRSATODTI) = DQSATI(PBUF(JIJ, IT),PP(JIJ),PRSATI(JIJ))*PBUF(JIJ, I1PRT)
    ZRSATW_ED(JIJ_4) = ZRSATW_ED(JIJ_4)*ZBUF(JIJ_4, I1PRT_4)
    ZRSATI_ED(JIJ_4) = ZRSATI_ED(JIJ_4)*ZBUF(JIJ_4, I1PRT_4)
    ZBUF(JIJ_4, IRVSAT_4) = ZRSATW_ED(JIJ_4)*(1-ZFRAC_ICE(JIJ_4)) + ZRSATI_ED(JIJ_4)*ZFRAC_ICE(JIJ_4)
    ZBUF(JIJ_4, IDRSATODT_4) = (ZBUF(JIJ_4, IDRSATODTW_4)*(1-ZFRAC_ICE(JIJ_4))+ &
              & ZBUF(JIJ_4, IDRSATODTI_4)*ZFRAC_ICE(JIJ_4))

    !Computation of new PRL, PRI and PRV
    !Correction term applied to (PRV(JIJ)-PBUF(JIJ, IRVSAT)) is computed assuming that
    !PBUF(JIJ, ILVOCPEXN), PBUF(JIJ, ILSOCPEXN) and PBUF(JIJ, ICPH) don't vary to much with T. It takes into account
    !the variation (estimated linear) of Qsat with T
    ZBUF(JIJ_4, IRLTEMP_4)=(ZRVMIX(JIJ_4)-ZBUF(JIJ_4, IRVSAT_4))/ &
                  &(1 + ZBUF(JIJ_4, IDRSATODT_4)*ZBUF(JIJ_4, IEXN_4)* &
                  &     (ZBUF(JIJ_4, ILVOCPEXN_4)*(1-ZFRAC_ICE(JIJ_4))+ZBUF(JIJ_4, ILSOCPEXN_4)*ZFRAC_ICE(JIJ_4)))
    ZBUF(JIJ_4, IRLTEMP_4)=MIN(MAX(-ZRCMIX(JIJ_4)-ZRIMIX(JIJ_4), ZBUF(JIJ_4, IRLTEMP_4)),ZRVMIX(JIJ_4))
    ZRVMIX(JIJ_4)=ZRVMIX(JIJ_4)-ZBUF(JIJ_4, IRLTEMP_4)
    ZRCMIX(JIJ_4)=ZRCMIX(JIJ_4)+ZRIMIX(JIJ_4)+ZBUF(JIJ_4, IRLTEMP_4)
    ZRIMIX(JIJ_4)=ZFRAC_ICE(JIJ_4)     * (ZRCMIX(JIJ_4))
    ZRCMIX(JIJ_4)=(1-ZFRAC_ICE(JIJ_4)) * (PRT_UP(JIJ_4, JK) - ZRVMIX(JIJ_4))

    !Computation of Cph (as defined in Meso-NH doc, equation 2.2, to be used with mixing ratios)
    ZBUF(JIJ_4, ICPH_4)=CST%XCPD+ CST%XCPV * ZRVMIX(JIJ_4)+ CST%XCL * ZRCMIX(JIJ_4) + CST%XCI * ZRIMIX(JIJ_4) + ZBUF(JIJ_4, ICPH2_4)

    !Computation of L/Cph/EXN, then new PTH
    ZVAR2_3=ZBUF(JIJ_4, ICPH_4)*ZBUF(JIJ_4, IEXN_4)
    ZBUF(JIJ_4, ILVOCPEXN_4) = (CST%XLVTT + (CST%XCPV-CST%XCL) * (ZBUF(JIJ_4, IT_4)-CST%XTT)) /ZVAR2_3
    ZBUF(JIJ_4, ILSOCPEXN_4) = (CST%XLSTT + (CST%XCPV-CST%XCI) * (ZBUF(JIJ_4, IT_4)-CST%XTT)) /ZVAR2_3
    ZTHMIX(JIJ_4)=PTHL_UP(JIJ_4, JK)+ZBUF(JIJ_4, ILVOCPEXN_4)*ZRCMIX(JIJ_4)+ZBUF(JIJ_4, ILSOCPEXN_4)*ZRIMIX(JIJ_4)

    !Computation of estimated mixing ration at saturation
    !To compute the adjustement a first order development was used
    ZVAR1_3=ZTHMIX(JIJ_4)*ZBUF(JIJ_4, IEXN_4)-ZBUF(JIJ_4, IT_4)
    ZRSATW_ED(JIJ_4)=ZRSATW_ED(JIJ_4) + ZBUF(JIJ_4, IDRSATODTW_4)*ZVAR1_3
    ZRSATI_ED(JIJ_4)=ZRSATI_ED(JIJ_4) + ZBUF(JIJ_4, IDRSATODTI_4)*ZVAR1_3
  ENDDO
ENDDO


!$mnh_expand_array(JIJ=IIJB:IIJE)
ZTHV_UP_F2(IIJB:IIJE) = ZTHMIX(IIJB:IIJE)*(1.+ZRVORD*ZRVMIX(IIJB:IIJE))/(1.+PRT_UP(IIJB:IIJE, JK))
!$mnh_end_expand_array(JIJ=IIJB:IIJE)

! Integral buoyancy for cloudy part
DO JIJ=IIJB,IIJE
  IF(GTEST(JIJ) .AND. ZPART_DRY(JIJ)<1.) THEN
    !Gradient of Theta V updraft over the cloudy part, assuming that thetaV updraft don't change
    !between flux level KK and bottom of cloudy part
    ZCOTHVU=(ZTHV_UP_F2(JIJ)-PTHV_UP(JIJ, JK))/((PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK))*(1-ZPART_DRY(JIJ)))

    !Computation in two parts to use change of gradient of theta v of environment
    !Between bottom of cloudy part (if under mass level) and mass level KK
    ZDZ=MAX(0., 0.5*(PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK))-ZDZ_STOP(JIJ))
    ZBUO_INTEG_CLD(JIJ, JK) = ZG_O_THVREF_ED(JIJ)*ZDZ*&
            (0.5*( ZCOTHVU - ZCOEFF_MINUS_HALF(JIJ))*ZDZ &
              - (ZTHVM(JIJ,JK)-ZDZ*ZCOEFF_MINUS_HALF(JIJ)) + PTHV_UP(JIJ, JK) )

    !Between max(mass level, bottom of cloudy part) and flux level KK+KKL
    ZDZ=(PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK))-MAX(ZDZ_STOP(JIJ),0.5*(PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK)))
    ZBUO_INTEG_CLD(JIJ, JK) = ZBUO_INTEG_CLD(JIJ, JK)+ZG_O_THVREF_ED(JIJ)*ZDZ*&
                      (0.5*( ZCOTHVU - ZCOEFF_PLUS_HALF(JIJ))*ZDZ&
              - (ZTHVM(JIJ,JK)+(0.5*((PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK)))-ZDZ)*ZCOEFF_PLUS_HALF(JIJ)) +&
              PTHV_UP(JIJ, JK) )

  ELSE
    !No cloudy part
    ZBUO_INTEG_CLD(JIJ, JK)=0.
  END IF
END DO

!               3.2 Critical mixed fraction for KK+KKL flux level (ZKIC_F2) and
!                   for bottom of cloudy part (ZKIC), then a mean for the cloudy part
!                   (put also in ZKIC)
!
!                   computation by estimating unknown  
!                   T^mix r_c^mix and r_i^mix from enthalpy^mix and r_w^mix
!                   We determine the zero crossing of the linear curve
!                   evaluating the derivative using ZMIXF=0.1
                
ZKIC_INIT=0.1  ! starting value for critical mixed fraction for CLoudy Part

!  Compute thetaV of environment at the bottom of cloudy part
!    and cons then non cons. var. of mixture at the bottom of cloudy part

!   JKLIM computed to avoid KKL(KK-KKL) being < KKL*KKB
JKLIM=IKL*MAX(IKL*(JK-IKL),IKL*IKB)
DO JIJ=IIJB,IIJE
  IF(GTEST(JIJ) .AND. ZPART_DRY(JIJ)>0.5) THEN
    ZDZ=ZDZ_STOP(JIJ)-0.5*(PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK))
    ZTHV(JIJ)= ZTHVM(JIJ,JK)+ZCOEFF_PLUS_HALF(JIJ)*ZDZ
    ZMIXTHL(JIJ) = ZKIC_INIT * &
               (PTHLM(JIJ,JK)+ZDZ*(PTHLM(JIJ,JK+IKL)-PTHLM(JIJ,JK))/PDZZ(JIJ,JK+IKL)) + &
               (1. - ZKIC_INIT)*PTHL_UP(JIJ, JK)
    ZMIXRT(JIJ)  = ZKIC_INIT * &
               (PRTM(JIJ,JK)+ZDZ*(PRTM(JIJ,JK+IKL)-PRTM(JIJ,JK))/PDZZ(JIJ,JK+IKL)) +   &
               (1. - ZKIC_INIT)*PRT_UP(JIJ, JK)
  ELSEIF(GTEST(JIJ)) THEN
    ZDZ=0.5*(PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK))-ZDZ_STOP(JIJ)
    ZTHV(JIJ)= ZTHVM(JIJ,JK)-ZCOEFF_MINUS_HALF(JIJ)*ZDZ
    ZMIXTHL(JIJ) = ZKIC_INIT * &
               (PTHLM(JIJ,JK)-ZDZ*(PTHLM(JIJ,JK)-PTHLM(JIJ,JKLIM))/PDZZ(JIJ,JK)) + &
               (1. - ZKIC_INIT)*PTHL_UP(JIJ, JK)
    ZMIXRT(JIJ)  = ZKIC_INIT * &
               (PRTM(JIJ,JK)-ZDZ*(PRTM(JIJ,JK)-PRTM(JIJ,JKLIM))/PDZZ(JIJ,JK)) + &
               (1. - ZKIC_INIT)*PRT_UP(JIJ, JK)
  ELSE
    ZMIXTHL(JIJ) = 300.
    ZMIXRT(JIJ) = 0.1
  ENDIF
ENDDO
IIJB_2=MERGE(D%NIJB, 1, .TRUE.)
IIJE_2=MERGE(D%NIJE, D%NIJT, .TRUE.)
!Number of iterations
JITER_1=2
!
!Computation of PBUF(IIJB:IIJE, ICPH2) depending on dummy arguments received
ZBUF(IIJB_2:IIJE_2, ICPH2_2)=0
!
!Computation of an approximate state thanks to PRL and PRI guess
ZBUF(IIJB_2:IIJE_2, IEXN_2)=(ZPRE(IIJB_2:IIJE_2)/CST%XP00) ** CST%RDSCPD

DO JIJ_2=IIJB_2,IIJE_2
  ZBUF(JIJ_2, I99PP_2)=0.99*ZPRE(JIJ_2)
  ZRVMIX(JIJ_2)=ZMIXRT(JIJ_2)-ZRC_MIX(JIJ_2, JK)-ZRI_MIX(JIJ_2, JK)
  ZBUF(JIJ_2, ICPH_2)=CST%XCPD+ CST%XCPV * ZRVMIX(JIJ_2)+ CST%XCL * ZRC_MIX(JIJ_2, JK) + CST%XCI * ZRI_MIX(JIJ_2, JK) + ZBUF(JIJ_2, ICPH2_2)
  ZVAR2_1=ZBUF(JIJ_2, ICPH_2)*ZBUF(JIJ_2, IEXN_2)
  ZDELT_1=(ZMIXTHL(JIJ_2)*ZBUF(JIJ_2, IEXN_2))-CST%XTT
  ZBUF(JIJ_2, ILVOCPEXN_2) = (CST%XLVTT + (CST%XCPV-CST%XCL) * ZDELT_1) /ZVAR2_1
  ZBUF(JIJ_2, ILSOCPEXN_2) = (CST%XLSTT + (CST%XCPV-CST%XCI) * ZDELT_1) /ZVAR2_1 
  ZTHMIX(JIJ_2)=ZMIXTHL(JIJ_2)+ZBUF(JIJ_2, ILVOCPEXN_2)*ZRC_MIX(JIJ_2, JK)+ZBUF(JIJ_2, ILSOCPEXN_2)*ZRI_MIX(JIJ_2, JK)
  ZBUF(JIJ_2, I1PRT_2)=1+ZMIXRT(JIJ_2)
ENDDO
!
!
!       2 Iteration
!         ---------

DO II_1=1,JITER_1
  IF (.FALSE.) THEN
    ZBUF(IIJB_2:IIJE_2, IT_2)=ZTHMIX(IIJB_2:IIJE_2)
  ELSE
    ZBUF(IIJB_2:IIJE_2, IT_2)=ZTHMIX(IIJB_2:IIJE_2)*ZBUF(IIJB_2:IIJE_2, IEXN_2)
  END IF
  !Computation of liquid/ice fractions
  ZFRAC_ICE(IIJB_2:IIJE_2) = 0.
  DO JIJ_2=IIJB_2, IIJE_2
    IF(ZRC_MIX(JIJ_2, JK)+ZRI_MIX(JIJ_2, JK) > 1.E-20) THEN
      ZFRAC_ICE(JIJ_2) = ZRI_MIX(JIJ_2, JK) / (ZRC_MIX(JIJ_2, JK)+ZRI_MIX(JIJ_2, JK))
    ENDIF
  ENDDO
  DO JIJ_2=IIJB_2, IIJE_2
  !$acc Meso-NH-champignon
SELECT CASE(NEBN%CFRAC_ICE_SHALLOW_MF)
  CASE ('T') !using Temperature
    ZFRAC_ICE(JIJ_2) = MAX( 0., MIN(1., (( NEBN%XTMAXMIX - ZBUF(JIJ_2, IT_2) ) / ( NEBN%XTMAXMIX - NEBN%XTMINMIX )) ) ) ! freezing interval
  CASE ('O') !using Temperature with old formulae
    ZFRAC_ICE(JIJ_2) = MAX( 0., MIN(1., (( CST%XTT - ZBUF(JIJ_2, IT_2) ) / 40.) ) ) ! freezing interval
  CASE ('N') !No ice
    ZFRAC_ICE(JIJ_2) = 0.
  CASE ('S') !Same as previous
    ! (almost) nothing to do
    ZFRAC_ICE(JIJ_2) = MAX( 0., MIN(1., ZFRAC_ICE(JIJ_2) ) )
  CASE DEFAULT    
END SELECT



  END DO
!Computation of Rvsat and dRsat/dT
  !In this version QSAT, QSATI, DQSAT and DQASATI functions are not used
  !due to performance issue

  ! Log does not vectorize on all compilers:
  ZBUF(IIJB_2:IIJE_2, ILOGT_2)=LOG(ZBUF(IIJB_2:IIJE_2, IT_2))

  DO JIJ_2=IIJB_2, IIJE_2
    ZBUF(JIJ_2, IFOESW_2) = MIN(EXP( CST%XALPW - CST%XBETAW/ZBUF(JIJ_2, IT_2) - CST%XGAMW*ZBUF(JIJ_2, ILOGT_2)  ), ZBUF(JIJ_2, I99PP_2))
    ZBUF(JIJ_2, IFOESI_2) = MIN(EXP( CST%XALPI - CST%XBETAI/ZBUF(JIJ_2, IT_2) - CST%XGAMI*ZBUF(JIJ_2, ILOGT_2)  ), ZBUF(JIJ_2, I99PP_2))
    ZRSATW_ED(JIJ_2) = CST%XRD/CST%XRV*ZBUF(JIJ_2, IFOESW_2)/ZPRE(JIJ_2) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ_2, IFOESW_2)/ZPRE(JIJ_2))
    ZRSATI_ED(JIJ_2) = CST%XRD/CST%XRV*ZBUF(JIJ_2, IFOESI_2)/ZPRE(JIJ_2) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ_2, IFOESI_2)/ZPRE(JIJ_2))
    ZTPOW2_1=ZBUF(JIJ_2, IT_2)**2
    ZBUF(JIJ_2, IDRSATODTW_2) = ZRSATW_ED(JIJ_2) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ_2, IFOESW_2)/ZPRE(JIJ_2) ) &
                     * (CST%XBETAW/ZTPOW2_1 - CST%XGAMW/ZBUF(JIJ_2, IT_2))*ZBUF(JIJ_2, I1PRT_2)
    ZBUF(JIJ_2, IDRSATODTI_2) = ZRSATI_ED(JIJ_2) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ_2, IFOESI_2)/ZPRE(JIJ_2) ) &
                     * (CST%XBETAI/ZTPOW2_1 - CST%XGAMI/ZBUF(JIJ_2, IT_2))*ZBUF(JIJ_2, I1PRT_2)
    !PRSATW(JIJ) =  QSAT(PBUF(JIJ, IT),PP(JIJ)) !qsatw
    !PRSATI(JIJ) = QSATI(PBUF(JIJ, IT),PP(JIJ)) !qsati
    !PBUF(JIJ, IDRSATODTW) =  DQSAT(PBUF(JIJ, IT),PP(JIJ),PRSATW(JIJ))*PBUF(JIJ, I1PRT)
    !PBUF(JIJ, IDRSATODTI) = DQSATI(PBUF(JIJ, IT),PP(JIJ),PRSATI(JIJ))*PBUF(JIJ, I1PRT)
    ZRSATW_ED(JIJ_2) = ZRSATW_ED(JIJ_2)*ZBUF(JIJ_2, I1PRT_2)
    ZRSATI_ED(JIJ_2) = ZRSATI_ED(JIJ_2)*ZBUF(JIJ_2, I1PRT_2)
    ZBUF(JIJ_2, IRVSAT_2) = ZRSATW_ED(JIJ_2)*(1-ZFRAC_ICE(JIJ_2)) + ZRSATI_ED(JIJ_2)*ZFRAC_ICE(JIJ_2)
    ZBUF(JIJ_2, IDRSATODT_2) = (ZBUF(JIJ_2, IDRSATODTW_2)*(1-ZFRAC_ICE(JIJ_2))+ &
              & ZBUF(JIJ_2, IDRSATODTI_2)*ZFRAC_ICE(JIJ_2))

    !Computation of new PRL, PRI and PRV
    !Correction term applied to (PRV(JIJ)-PBUF(JIJ, IRVSAT)) is computed assuming that
    !PBUF(JIJ, ILVOCPEXN), PBUF(JIJ, ILSOCPEXN) and PBUF(JIJ, ICPH) don't vary to much with T. It takes into account
    !the variation (estimated linear) of Qsat with T
    ZBUF(JIJ_2, IRLTEMP_2)=(ZRVMIX(JIJ_2)-ZBUF(JIJ_2, IRVSAT_2))/ &
                  &(1 + ZBUF(JIJ_2, IDRSATODT_2)*ZBUF(JIJ_2, IEXN_2)* &
                  &     (ZBUF(JIJ_2, ILVOCPEXN_2)*(1-ZFRAC_ICE(JIJ_2))+ZBUF(JIJ_2, ILSOCPEXN_2)*ZFRAC_ICE(JIJ_2)))
    ZBUF(JIJ_2, IRLTEMP_2)=MIN(MAX(-ZRC_MIX(JIJ_2, JK)-ZRI_MIX(JIJ_2, JK), ZBUF(JIJ_2, IRLTEMP_2)),ZRVMIX(JIJ_2))
    ZRVMIX(JIJ_2)=ZRVMIX(JIJ_2)-ZBUF(JIJ_2, IRLTEMP_2)
    ZRC_MIX(JIJ_2, JK)=ZRC_MIX(JIJ_2, JK)+ZRI_MIX(JIJ_2, JK)+ZBUF(JIJ_2, IRLTEMP_2)
    ZRI_MIX(JIJ_2, JK)=ZFRAC_ICE(JIJ_2)     * (ZRC_MIX(JIJ_2, JK))
    ZRC_MIX(JIJ_2, JK)=(1-ZFRAC_ICE(JIJ_2)) * (ZMIXRT(JIJ_2) - ZRVMIX(JIJ_2))

    !Computation of Cph (as defined in Meso-NH doc, equation 2.2, to be used with mixing ratios)
    ZBUF(JIJ_2, ICPH_2)=CST%XCPD+ CST%XCPV * ZRVMIX(JIJ_2)+ CST%XCL * ZRC_MIX(JIJ_2, JK) + CST%XCI * ZRI_MIX(JIJ_2, JK) + ZBUF(JIJ_2, ICPH2_2)

    !Computation of L/Cph/EXN, then new PTH
    ZVAR2_1=ZBUF(JIJ_2, ICPH_2)*ZBUF(JIJ_2, IEXN_2)
    ZBUF(JIJ_2, ILVOCPEXN_2) = (CST%XLVTT + (CST%XCPV-CST%XCL) * (ZBUF(JIJ_2, IT_2)-CST%XTT)) /ZVAR2_1
    ZBUF(JIJ_2, ILSOCPEXN_2) = (CST%XLSTT + (CST%XCPV-CST%XCI) * (ZBUF(JIJ_2, IT_2)-CST%XTT)) /ZVAR2_1
    ZTHMIX(JIJ_2)=ZMIXTHL(JIJ_2)+ZBUF(JIJ_2, ILVOCPEXN_2)*ZRC_MIX(JIJ_2, JK)+ZBUF(JIJ_2, ILSOCPEXN_2)*ZRI_MIX(JIJ_2, JK)

    !Computation of estimated mixing ration at saturation
    !To compute the adjustement a first order development was used
    ZVAR1_1=ZTHMIX(JIJ_2)*ZBUF(JIJ_2, IEXN_2)-ZBUF(JIJ_2, IT_2)
    ZRSATW_ED(JIJ_2)=ZRSATW_ED(JIJ_2) + ZBUF(JIJ_2, IDRSATODTW_2)*ZVAR1_1
    ZRSATI_ED(JIJ_2)=ZRSATI_ED(JIJ_2) + ZBUF(JIJ_2, IDRSATODTI_2)*ZVAR1_1
  ENDDO
ENDDO


!$mnh_expand_array(JIJ=IIJB:IIJE)
ZTHVMIX(IIJB:IIJE) = ZTHMIX(IIJB:IIJE)*(1.+ZRVORD*ZRVMIX(IIJB:IIJE))/(1.+ZMIXRT(IIJB:IIJE))

!  Compute cons then non cons. var. of mixture at the flux level KK+KKL  with initial ZKIC
ZMIXTHL(IIJB:IIJE) = ZKIC_INIT * 0.5*(PTHLM(IIJB:IIJE,JK)+PTHLM(IIJB:IIJE,JK+IKL))+&
                       & (1. - ZKIC_INIT)*PTHL_UP(IIJB:IIJE, JK)
ZMIXRT(IIJB:IIJE)  = ZKIC_INIT * 0.5*(PRTM(IIJB:IIJE,JK)+PRTM(IIJB:IIJE,JK+IKL))+&
                       & (1. - ZKIC_INIT)*PRT_UP(IIJB:IIJE, JK)
!$mnh_end_expand_array(JIJ=IIJB:IIJE)
IIJB_3=MERGE(D%NIJB, 1, .TRUE.)
IIJE_3=MERGE(D%NIJE, D%NIJT, .TRUE.)
!Number of iterations
JITER_2=2
!
!Computation of PBUF(IIJB:IIJE, ICPH2) depending on dummy arguments received
ZBUF(IIJB_3:IIJE_3, ICPH2_3)=0
!
!Computation of an approximate state thanks to PRL and PRI guess
ZBUF(IIJB_3:IIJE_3, IEXN_3)=(ZPRES_F(IIJB_3:IIJE_3, JK+IKL)/CST%XP00) ** CST%RDSCPD

DO JIJ_3=IIJB_3,IIJE_3
  ZBUF(JIJ_3, I99PP_3)=0.99*ZPRES_F(JIJ_3, JK+IKL)
  ZRVMIX(JIJ_3)=ZMIXRT(JIJ_3)-ZRC_MIX(JIJ_3, JK)-ZRI_MIX(JIJ_3, JK)
  ZBUF(JIJ_3, ICPH_3)=CST%XCPD+ CST%XCPV * ZRVMIX(JIJ_3)+ CST%XCL * ZRC_MIX(JIJ_3, JK) + CST%XCI * ZRI_MIX(JIJ_3, JK) + ZBUF(JIJ_3, ICPH2_3)
  ZVAR2_2=ZBUF(JIJ_3, ICPH_3)*ZBUF(JIJ_3, IEXN_3)
  ZDELT_2=(ZMIXTHL(JIJ_3)*ZBUF(JIJ_3, IEXN_3))-CST%XTT
  ZBUF(JIJ_3, ILVOCPEXN_3) = (CST%XLVTT + (CST%XCPV-CST%XCL) * ZDELT_2) /ZVAR2_2
  ZBUF(JIJ_3, ILSOCPEXN_3) = (CST%XLSTT + (CST%XCPV-CST%XCI) * ZDELT_2) /ZVAR2_2 
  ZTHMIX(JIJ_3)=ZMIXTHL(JIJ_3)+ZBUF(JIJ_3, ILVOCPEXN_3)*ZRC_MIX(JIJ_3, JK)+ZBUF(JIJ_3, ILSOCPEXN_3)*ZRI_MIX(JIJ_3, JK)
  ZBUF(JIJ_3, I1PRT_3)=1+ZMIXRT(JIJ_3)
ENDDO
!
!
!       2 Iteration
!         ---------

DO II_2=1,JITER_2
  IF (.FALSE.) THEN
    ZBUF(IIJB_3:IIJE_3, IT_3)=ZTHMIX(IIJB_3:IIJE_3)
  ELSE
    ZBUF(IIJB_3:IIJE_3, IT_3)=ZTHMIX(IIJB_3:IIJE_3)*ZBUF(IIJB_3:IIJE_3, IEXN_3)
  END IF
  !Computation of liquid/ice fractions
  ZFRAC_ICE(IIJB_3:IIJE_3) = 0.
  DO JIJ_3=IIJB_3, IIJE_3
    IF(ZRC_MIX(JIJ_3, JK)+ZRI_MIX(JIJ_3, JK) > 1.E-20) THEN
      ZFRAC_ICE(JIJ_3) = ZRI_MIX(JIJ_3, JK) / (ZRC_MIX(JIJ_3, JK)+ZRI_MIX(JIJ_3, JK))
    ENDIF
  ENDDO
  DO JIJ_3=IIJB_3, IIJE_3
  !$acc Meso-NH-champignon
SELECT CASE(NEBN%CFRAC_ICE_SHALLOW_MF)
  CASE ('T') !using Temperature
    ZFRAC_ICE(JIJ_3) = MAX( 0., MIN(1., (( NEBN%XTMAXMIX - ZBUF(JIJ_3, IT_3) ) / ( NEBN%XTMAXMIX - NEBN%XTMINMIX )) ) ) ! freezing interval
  CASE ('O') !using Temperature with old formulae
    ZFRAC_ICE(JIJ_3) = MAX( 0., MIN(1., (( CST%XTT - ZBUF(JIJ_3, IT_3) ) / 40.) ) ) ! freezing interval
  CASE ('N') !No ice
    ZFRAC_ICE(JIJ_3) = 0.
  CASE ('S') !Same as previous
    ! (almost) nothing to do
    ZFRAC_ICE(JIJ_3) = MAX( 0., MIN(1., ZFRAC_ICE(JIJ_3) ) )
  CASE DEFAULT    
END SELECT



  END DO
!Computation of Rvsat and dRsat/dT
  !In this version QSAT, QSATI, DQSAT and DQASATI functions are not used
  !due to performance issue

  ! Log does not vectorize on all compilers:
  ZBUF(IIJB_3:IIJE_3, ILOGT_3)=LOG(ZBUF(IIJB_3:IIJE_3, IT_3))

  DO JIJ_3=IIJB_3, IIJE_3
    ZBUF(JIJ_3, IFOESW_3) = MIN(EXP( CST%XALPW - CST%XBETAW/ZBUF(JIJ_3, IT_3) - CST%XGAMW*ZBUF(JIJ_3, ILOGT_3)  ), ZBUF(JIJ_3, I99PP_3))
    ZBUF(JIJ_3, IFOESI_3) = MIN(EXP( CST%XALPI - CST%XBETAI/ZBUF(JIJ_3, IT_3) - CST%XGAMI*ZBUF(JIJ_3, ILOGT_3)  ), ZBUF(JIJ_3, I99PP_3))
    ZRSATW_ED(JIJ_3) = CST%XRD/CST%XRV*ZBUF(JIJ_3, IFOESW_3)/ZPRES_F(JIJ_3, JK+IKL) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ_3, IFOESW_3)/ZPRES_F(JIJ_3, JK+IKL))
    ZRSATI_ED(JIJ_3) = CST%XRD/CST%XRV*ZBUF(JIJ_3, IFOESI_3)/ZPRES_F(JIJ_3, JK+IKL) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ_3, IFOESI_3)/ZPRES_F(JIJ_3, JK+IKL))
    ZTPOW2_2=ZBUF(JIJ_3, IT_3)**2
    ZBUF(JIJ_3, IDRSATODTW_3) = ZRSATW_ED(JIJ_3) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ_3, IFOESW_3)/ZPRES_F(JIJ_3, JK+IKL) ) &
                     * (CST%XBETAW/ZTPOW2_2 - CST%XGAMW/ZBUF(JIJ_3, IT_3))*ZBUF(JIJ_3, I1PRT_3)
    ZBUF(JIJ_3, IDRSATODTI_3) = ZRSATI_ED(JIJ_3) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ_3, IFOESI_3)/ZPRES_F(JIJ_3, JK+IKL) ) &
                     * (CST%XBETAI/ZTPOW2_2 - CST%XGAMI/ZBUF(JIJ_3, IT_3))*ZBUF(JIJ_3, I1PRT_3)
    !PRSATW(JIJ) =  QSAT(PBUF(JIJ, IT),PP(JIJ)) !qsatw
    !PRSATI(JIJ) = QSATI(PBUF(JIJ, IT),PP(JIJ)) !qsati
    !PBUF(JIJ, IDRSATODTW) =  DQSAT(PBUF(JIJ, IT),PP(JIJ),PRSATW(JIJ))*PBUF(JIJ, I1PRT)
    !PBUF(JIJ, IDRSATODTI) = DQSATI(PBUF(JIJ, IT),PP(JIJ),PRSATI(JIJ))*PBUF(JIJ, I1PRT)
    ZRSATW_ED(JIJ_3) = ZRSATW_ED(JIJ_3)*ZBUF(JIJ_3, I1PRT_3)
    ZRSATI_ED(JIJ_3) = ZRSATI_ED(JIJ_3)*ZBUF(JIJ_3, I1PRT_3)
    ZBUF(JIJ_3, IRVSAT_3) = ZRSATW_ED(JIJ_3)*(1-ZFRAC_ICE(JIJ_3)) + ZRSATI_ED(JIJ_3)*ZFRAC_ICE(JIJ_3)
    ZBUF(JIJ_3, IDRSATODT_3) = (ZBUF(JIJ_3, IDRSATODTW_3)*(1-ZFRAC_ICE(JIJ_3))+ &
              & ZBUF(JIJ_3, IDRSATODTI_3)*ZFRAC_ICE(JIJ_3))

    !Computation of new PRL, PRI and PRV
    !Correction term applied to (PRV(JIJ)-PBUF(JIJ, IRVSAT)) is computed assuming that
    !PBUF(JIJ, ILVOCPEXN), PBUF(JIJ, ILSOCPEXN) and PBUF(JIJ, ICPH) don't vary to much with T. It takes into account
    !the variation (estimated linear) of Qsat with T
    ZBUF(JIJ_3, IRLTEMP_3)=(ZRVMIX(JIJ_3)-ZBUF(JIJ_3, IRVSAT_3))/ &
                  &(1 + ZBUF(JIJ_3, IDRSATODT_3)*ZBUF(JIJ_3, IEXN_3)* &
                  &     (ZBUF(JIJ_3, ILVOCPEXN_3)*(1-ZFRAC_ICE(JIJ_3))+ZBUF(JIJ_3, ILSOCPEXN_3)*ZFRAC_ICE(JIJ_3)))
    ZBUF(JIJ_3, IRLTEMP_3)=MIN(MAX(-ZRC_MIX(JIJ_3, JK)-ZRI_MIX(JIJ_3, JK), ZBUF(JIJ_3, IRLTEMP_3)),ZRVMIX(JIJ_3))
    ZRVMIX(JIJ_3)=ZRVMIX(JIJ_3)-ZBUF(JIJ_3, IRLTEMP_3)
    ZRC_MIX(JIJ_3, JK)=ZRC_MIX(JIJ_3, JK)+ZRI_MIX(JIJ_3, JK)+ZBUF(JIJ_3, IRLTEMP_3)
    ZRI_MIX(JIJ_3, JK)=ZFRAC_ICE(JIJ_3)     * (ZRC_MIX(JIJ_3, JK))
    ZRC_MIX(JIJ_3, JK)=(1-ZFRAC_ICE(JIJ_3)) * (ZMIXRT(JIJ_3) - ZRVMIX(JIJ_3))

    !Computation of Cph (as defined in Meso-NH doc, equation 2.2, to be used with mixing ratios)
    ZBUF(JIJ_3, ICPH_3)=CST%XCPD+ CST%XCPV * ZRVMIX(JIJ_3)+ CST%XCL * ZRC_MIX(JIJ_3, JK) + CST%XCI * ZRI_MIX(JIJ_3, JK) + ZBUF(JIJ_3, ICPH2_3)

    !Computation of L/Cph/EXN, then new PTH
    ZVAR2_2=ZBUF(JIJ_3, ICPH_3)*ZBUF(JIJ_3, IEXN_3)
    ZBUF(JIJ_3, ILVOCPEXN_3) = (CST%XLVTT + (CST%XCPV-CST%XCL) * (ZBUF(JIJ_3, IT_3)-CST%XTT)) /ZVAR2_2
    ZBUF(JIJ_3, ILSOCPEXN_3) = (CST%XLSTT + (CST%XCPV-CST%XCI) * (ZBUF(JIJ_3, IT_3)-CST%XTT)) /ZVAR2_2
    ZTHMIX(JIJ_3)=ZMIXTHL(JIJ_3)+ZBUF(JIJ_3, ILVOCPEXN_3)*ZRC_MIX(JIJ_3, JK)+ZBUF(JIJ_3, ILSOCPEXN_3)*ZRI_MIX(JIJ_3, JK)

    !Computation of estimated mixing ration at saturation
    !To compute the adjustement a first order development was used
    ZVAR1_2=ZTHMIX(JIJ_3)*ZBUF(JIJ_3, IEXN_3)-ZBUF(JIJ_3, IT_3)
    ZRSATW_ED(JIJ_3)=ZRSATW_ED(JIJ_3) + ZBUF(JIJ_3, IDRSATODTW_3)*ZVAR1_2
    ZRSATI_ED(JIJ_3)=ZRSATI_ED(JIJ_3) + ZBUF(JIJ_3, IDRSATODTI_3)*ZVAR1_2
  ENDDO
ENDDO


!$mnh_expand_array(JIJ=IIJB:IIJE)
ZTHVMIX_F2(IIJB:IIJE) = ZTHMIX(IIJB:IIJE)*(1.+ZRVORD*ZRVMIX(IIJB:IIJE))/(1.+ZMIXRT(IIJB:IIJE))
!$mnh_end_expand_array(JIJ=IIJB:IIJE)

!Computation of mean ZKIC over the cloudy part
DO JIJ=IIJB,IIJE
  IF (GTEST(JIJ)) THEN
    ! Compute ZKIC at the bottom of cloudy part
    ! Thetav_up at bottom is equal to Thetav_up at flux level KK
    IF (ABS(PTHV_UP(JIJ, JK)-ZTHVMIX(JIJ))<1.E-10) THEN
      ZKIC(JIJ)=1.
    ELSE
      ZKIC(JIJ) = MAX(0.,PTHV_UP(JIJ, JK)-ZTHV(JIJ))*ZKIC_INIT /  &  
                 (PTHV_UP(JIJ, JK)-ZTHVMIX(JIJ))
    END IF
    ! Compute ZKIC_F2 at flux level KK+KKL
    IF (ABS(ZTHV_UP_F2(JIJ)-ZTHVMIX_F2(JIJ))<1.E-10) THEN
      ZKIC_F2(JIJ)=1.
    ELSE
      ZKIC_F2(JIJ) = MAX(0.,ZTHV_UP_F2(JIJ)-ZTHV_PLUS_HALF(JIJ))*ZKIC_INIT /  &  
                 (ZTHV_UP_F2(JIJ)-ZTHVMIX_F2(JIJ))
    END IF
    !Mean ZKIC over the cloudy part
    ZKIC(JIJ)=MAX(MIN(0.5*(ZKIC(JIJ)+ZKIC_F2(JIJ)),1.),0.)
  END IF
END DO

!               3.3 Integration of PDF
!                   According to Kain and Fritsch (1990), we replace delta Mt
!                   in eq. (7) and (8) using eq. (5). Here we compute the ratio
!                   of integrals without computing delta Me

!Constant PDF
!For this PDF, eq. (5) is delta Me=0.5*delta Mt
DO JIJ=IIJB,IIJE
  IF(GTEST(JIJ)) THEN
    ZEPSI(JIJ) = ZKIC(JIJ)**2. !integration multiplied by 2
    ZDELTA(JIJ) = (1.-ZKIC(JIJ))**2. !idem
  ENDIF
ENDDO

!Triangular PDF
!Calculus must be verified before activating this part, but in this state,
!results on ARM case are almost identical
!For this PDF, eq. (5) is also delta Me=0.5*delta Mt
!WHERE(OTEST(IIJB:IIJE))
!  !Integration multiplied by 2
!  WHERE(ZKIC<0.5)
!    ZEPSI(IIJB:IIJE)=8.*ZKIC(IIJB:IIJE)**3/3.
!    ZDELTA(IIJB:IIJE)=1.-4.*ZKIC(IIJB:IIJE)**2+8.*ZKIC(IIJB:IIJE)**3/3.
!  ELSEWHERE
!    ZEPSI(IIJB:IIJE)=5./3.-4*ZKIC(IIJB:IIJE)**2+8.*ZKIC(IIJB:IIJE)**3/3.
!    ZDELTA(IIJB:IIJE)=8.*(1.-ZKIC(IIJB:IIJE))**3/3.
!  ENDWHERE
!ENDWHERE

!               3.4 Computation of PENTR and PDETR
DO JIJ=IIJB,IIJE
  IF(GTEST(JIJ)) THEN
    ZEPSI_CLOUD=MIN(ZDELTA(JIJ), ZEPSI(JIJ))
    ZENTR_CLD(JIJ, JK) = (1.-ZPART_DRY(JIJ))*ZCOEFFMF_CLOUD*PRHODREF(JIJ, JK)*ZEPSI_CLOUD
    ZDETR_CLD(JIJ, JK) = (1.-ZPART_DRY(JIJ))*ZCOEFFMF_CLOUD*PRHODREF(JIJ, JK)*ZDELTA(JIJ)
    PENTR(JIJ, JK) = PENTR(JIJ, JK)+ZENTR_CLD(JIJ, JK)
    PDETR(JIJ, JK) = PDETR(JIJ, JK)+ZDETR_CLD(JIJ, JK)
  ELSE
    ZENTR_CLD(JIJ, JK) = 0.
    ZDETR_CLD(JIJ, JK) = 0.
  ENDIF
ENDDO


    !$mnh_expand_where(JIJ=IIJB:IIJE)
    PBUO_INTEG(IIJB:IIJE,JK)=ZBUO_INTEG_DRY(IIJB:IIJE,JK)+ZBUO_INTEG_CLD(IIJB:IIJE,JK)

    IF (JK==IKB) THEN
       PDETR(IIJB:IIJE,JK)=0.
       ZDETR_CLD(IIJB:IIJE,JK)=0.
    ENDIF   
 
    !       Computation of updraft characteristics at level JK+KKL
    WHERE(GTEST(IIJB:IIJE))
      ZMIX1(IIJB:IIJE)=0.5*(PZZ(IIJB:IIJE,JK+IKL)-PZZ(IIJB:IIJE,JK))*&
                          &(PENTR(IIJB:IIJE,JK)-PDETR(IIJB:IIJE,JK))
      PEMF(IIJB:IIJE,JK+IKL)=PEMF(IIJB:IIJE,JK)*EXP(2*ZMIX1(IIJB:IIJE))
    ENDWHERE
    !$mnh_end_expand_where(JIJ=IIJB:IIJE)
  ELSE !OENTR_DETR
    !$mnh_expand_array(JIJ=IIJB:IIJE)
    GTEST(IIJB:IIJE) = (PEMF(IIJB:IIJE,JK+IKL)>0.)
    !$mnh_end_expand_array(JIJ=IIJB:IIJE)
  END IF !OENTR_DETR
  
  ! stop the updraft if MF becomes negative
  !$mnh_expand_where(JIJ=IIJB:IIJE)
  WHERE (GTEST(IIJB:IIJE).AND.(PEMF(IIJB:IIJE,JK+IKL)<=0.))
    PEMF(IIJB:IIJE,JK+IKL)=0.
    KKCTL(IIJB:IIJE) = JK+IKL
    GTEST(IIJB:IIJE)=.FALSE.
    PFRAC_ICE_UP(IIJB:IIJE,JK+IKL)=PFRAC_ICE_UP(IIJB:IIJE,JK)
    PRSAT_UP(IIJB:IIJE,JK+IKL)=PRSAT_UP(IIJB:IIJE,JK)
  ENDWHERE
  !$mnh_end_expand_where(JIJ=IIJB:IIJE)

  ! If the updraft did not stop, compute cons updraft characteritics at jk+KKL
  DO JIJ=IIJB,IIJE
    IF(GTEST(JIJ)) THEN
      ZMIX2(JIJ) = (PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK))*PENTR(JIJ,JK) !&
      ZMIX3_CLD(JIJ) = (PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK))*(1.-ZPART_DRY(JIJ))*ZDETR_CLD(JIJ,JK) !&                   
      ZMIX2_CLD(JIJ) = (PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK))*(1.-ZPART_DRY(JIJ))*ZENTR_CLD(JIJ,JK)
      PTHL_UP(JIJ,JK+IKL)=(PTHL_UP(JIJ,JK)*(1.-0.5*ZMIX2(JIJ)) + PTHLM(JIJ,JK)*ZMIX2(JIJ)) &
                            /(1.+0.5*ZMIX2(JIJ))   
      PRT_UP(JIJ,JK+IKL) =(PRT_UP (JIJ,JK)*(1.-0.5*ZMIX2(JIJ)) + PRTM(JIJ,JK)*ZMIX2(JIJ))  &
                            /(1.+0.5*ZMIX2(JIJ))
    ENDIF
  ENDDO
  
  IF(PARAMMF%LMIXUV) THEN
    IF(JK/=IKB) THEN
      !$mnh_expand_where(JIJ=IIJB:IIJE)
      WHERE(GTEST(IIJB:IIJE))
        PU_UP(IIJB:IIJE,JK+IKL) = (PU_UP(IIJB:IIJE,JK)*(1-0.5*ZMIX2(IIJB:IIJE)) + &
                                        &PUM(IIJB:IIJE,JK)*ZMIX2(IIJB:IIJE)+ &
                          0.5*PARAMMF%XPRES_UV*(PZZ(IIJB:IIJE,JK+IKL)-PZZ(IIJB:IIJE,JK))*&
                          ((PUM(IIJB:IIJE,JK+IKL)-PUM(IIJB:IIJE,JK))/PDZZ(IIJB:IIJE,JK+IKL)+&
                           (PUM(IIJB:IIJE,JK)-PUM(IIJB:IIJE,JK-IKL))/PDZZ(IIJB:IIJE,JK))        )   &
                          /(1+0.5*ZMIX2(IIJB:IIJE))
        PV_UP(IIJB:IIJE,JK+IKL) = (PV_UP(IIJB:IIJE,JK)*(1-0.5*ZMIX2(IIJB:IIJE)) + &
                                        &PVM(IIJB:IIJE,JK)*ZMIX2(IIJB:IIJE)+ &
                          0.5*PARAMMF%XPRES_UV*(PZZ(IIJB:IIJE,JK+IKL)-PZZ(IIJB:IIJE,JK))*&
                          ((PVM(IIJB:IIJE,JK+IKL)-PVM(IIJB:IIJE,JK))/PDZZ(IIJB:IIJE,JK+IKL)+&
                           (PVM(IIJB:IIJE,JK)-PVM(IIJB:IIJE,JK-IKL))/PDZZ(IIJB:IIJE,JK))    )   &
                          /(1+0.5*ZMIX2(IIJB:IIJE))
      ENDWHERE
      !$mnh_end_expand_where(JIJ=IIJB:IIJE)
    ELSE
      !$mnh_expand_where(JIJ=IIJB:IIJE)
      WHERE(GTEST(IIJB:IIJE))
        PU_UP(IIJB:IIJE,JK+IKL) = (PU_UP(IIJB:IIJE,JK)*(1-0.5*ZMIX2(IIJB:IIJE)) + &
                                        &PUM(IIJB:IIJE,JK)*ZMIX2(IIJB:IIJE)+ &
                          0.5*PARAMMF%XPRES_UV*(PZZ(IIJB:IIJE,JK+IKL)-PZZ(IIJB:IIJE,JK))*&
                          ((PUM(IIJB:IIJE,JK+IKL)-PUM(IIJB:IIJE,JK))/PDZZ(IIJB:IIJE,JK+IKL))        )   &
                          /(1+0.5*ZMIX2(IIJB:IIJE))
        PV_UP(IIJB:IIJE,JK+IKL) = (PV_UP(IIJB:IIJE,JK)*(1-0.5*ZMIX2(IIJB:IIJE)) + &
                                        &PVM(IIJB:IIJE,JK)*ZMIX2(IIJB:IIJE)+ &
                          0.5*PARAMMF%XPRES_UV*(PZZ(IIJB:IIJE,JK+IKL)-PZZ(IIJB:IIJE,JK))*&
                          ((PVM(IIJB:IIJE,JK+IKL)-PVM(IIJB:IIJE,JK))/PDZZ(IIJB:IIJE,JK+IKL))    )   &
                          /(1+0.5*ZMIX2(IIJB:IIJE))
      ENDWHERE
      !$mnh_end_expand_where(JIJ=IIJB:IIJE)
    ENDIF
  ENDIF !PARAMMF%LMIXUV
  DO JSV=1,KSV 
    IF (ONOMIXLG .AND. JSV >= KSV_LGBEG .AND. JSV<= KSV_LGEND) CYCLE
    !$mnh_expand_where(JIJ=IIJB:IIJE)
    WHERE(GTEST(IIJB:IIJE)) 
      PSV_UP(IIJB:IIJE,JK+IKL,JSV) = (PSV_UP(IIJB:IIJE,JK,JSV)*(1-0.5*ZMIX2(IIJB:IIJE)) + &
                   PSVM(IIJB:IIJE,JK,JSV)*ZMIX2(IIJB:IIJE))  /(1+0.5*ZMIX2(IIJB:IIJE))
    ENDWHERE
    !$mnh_end_expand_where(JIJ=IIJB:IIJE)
  END DO  
  
  IF (OENTR_DETR) THEN

    ! Compute non cons. var. at level JK+KKL
    !$mnh_expand_array(JIJ=IIJB:IIJE)
    ZRC_UP(IIJB:IIJE)=PRC_UP(IIJB:IIJE,JK) ! guess = level just below
    ZRI_UP(IIJB:IIJE)=PRI_UP(IIJB:IIJE,JK) ! guess = level just below
    !$mnh_end_expand_array(JIJ=IIJB:IIJE)
    IIJB_5=MERGE(D%NIJB, 1, .TRUE.)
IIJE_5=MERGE(D%NIJE, D%NIJT, .TRUE.)
!Number of iterations
JITER_4=2
!
!Computation of PBUF(IIJB:IIJE, ICPH2) depending on dummy arguments received
ZBUF(IIJB_5:IIJE_5, ICPH2_5)=0
!
!Computation of an approximate state thanks to PRL and PRI guess
ZBUF(IIJB_5:IIJE_5, IEXN_5)=(ZPRES_F(IIJB_5:IIJE_5, JK+IKL)/CST%XP00) ** CST%RDSCPD

DO JIJ_5=IIJB_5,IIJE_5
  ZBUF(JIJ_5, I99PP_5)=0.99*ZPRES_F(JIJ_5, JK+IKL)
  ZRV_UP(JIJ_5)=PRT_UP(JIJ_5, JK+IKL)-ZRC_UP(JIJ_5)-ZRI_UP(JIJ_5)
  ZBUF(JIJ_5, ICPH_5)=CST%XCPD+ CST%XCPV * ZRV_UP(JIJ_5)+ CST%XCL * ZRC_UP(JIJ_5) + CST%XCI * ZRI_UP(JIJ_5) + ZBUF(JIJ_5, ICPH2_5)
  ZVAR2_4=ZBUF(JIJ_5, ICPH_5)*ZBUF(JIJ_5, IEXN_5)
  ZDELT_4=(PTHL_UP(JIJ_5, JK+IKL)*ZBUF(JIJ_5, IEXN_5))-CST%XTT
  ZBUF(JIJ_5, ILVOCPEXN_5) = (CST%XLVTT + (CST%XCPV-CST%XCL) * ZDELT_4) /ZVAR2_4
  ZBUF(JIJ_5, ILSOCPEXN_5) = (CST%XLSTT + (CST%XCPV-CST%XCI) * ZDELT_4) /ZVAR2_4 
  ZTH_UP(JIJ_5, JK+IKL)=PTHL_UP(JIJ_5, JK+IKL)+ZBUF(JIJ_5, ILVOCPEXN_5)*ZRC_UP(JIJ_5)+ZBUF(JIJ_5, ILSOCPEXN_5)*ZRI_UP(JIJ_5)
  ZBUF(JIJ_5, I1PRT_5)=1+PRT_UP(JIJ_5, JK+IKL)
ENDDO
!
!
!       2 Iteration
!         ---------

DO II_4=1,JITER_4
  IF (.FALSE.) THEN
    ZBUF(IIJB_5:IIJE_5, IT_5)=ZTH_UP(IIJB_5:IIJE_5, JK+IKL)
  ELSE
    ZBUF(IIJB_5:IIJE_5, IT_5)=ZTH_UP(IIJB_5:IIJE_5, JK+IKL)*ZBUF(IIJB_5:IIJE_5, IEXN_5)
  END IF
  !Computation of liquid/ice fractions
  PFRAC_ICE_UP(IIJB_5:IIJE_5, JK+IKL) = 0.
  DO JIJ_5=IIJB_5, IIJE_5
    IF(ZRC_UP(JIJ_5)+ZRI_UP(JIJ_5) > 1.E-20) THEN
      PFRAC_ICE_UP(JIJ_5, JK+IKL) = ZRI_UP(JIJ_5) / (ZRC_UP(JIJ_5)+ZRI_UP(JIJ_5))
    ENDIF
  ENDDO
  DO JIJ_5=IIJB_5, IIJE_5
  !$acc Meso-NH-champignon
SELECT CASE(NEBN%CFRAC_ICE_SHALLOW_MF)
  CASE ('T') !using Temperature
    PFRAC_ICE_UP(JIJ_5, JK+IKL) = MAX( 0., MIN(1., (( NEBN%XTMAXMIX - ZBUF(JIJ_5, IT_5) ) / ( NEBN%XTMAXMIX - NEBN%XTMINMIX )) ) ) ! freezing interval
  CASE ('O') !using Temperature with old formulae
    PFRAC_ICE_UP(JIJ_5, JK+IKL) = MAX( 0., MIN(1., (( CST%XTT - ZBUF(JIJ_5, IT_5) ) / 40.) ) ) ! freezing interval
  CASE ('N') !No ice
    PFRAC_ICE_UP(JIJ_5, JK+IKL) = 0.
  CASE ('S') !Same as previous
    ! (almost) nothing to do
    PFRAC_ICE_UP(JIJ_5, JK+IKL) = MAX( 0., MIN(1., PFRAC_ICE_UP(JIJ_5, JK+IKL) ) )
  CASE DEFAULT    
END SELECT



  END DO
!Computation of Rvsat and dRsat/dT
  !In this version QSAT, QSATI, DQSAT and DQASATI functions are not used
  !due to performance issue

  ! Log does not vectorize on all compilers:
  ZBUF(IIJB_5:IIJE_5, ILOGT_5)=LOG(ZBUF(IIJB_5:IIJE_5, IT_5))

  DO JIJ_5=IIJB_5, IIJE_5
    ZBUF(JIJ_5, IFOESW_5) = MIN(EXP( CST%XALPW - CST%XBETAW/ZBUF(JIJ_5, IT_5) - CST%XGAMW*ZBUF(JIJ_5, ILOGT_5)  ), ZBUF(JIJ_5, I99PP_5))
    ZBUF(JIJ_5, IFOESI_5) = MIN(EXP( CST%XALPI - CST%XBETAI/ZBUF(JIJ_5, IT_5) - CST%XGAMI*ZBUF(JIJ_5, ILOGT_5)  ), ZBUF(JIJ_5, I99PP_5))
    ZRSATW(JIJ_5) = CST%XRD/CST%XRV*ZBUF(JIJ_5, IFOESW_5)/ZPRES_F(JIJ_5, JK+IKL) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ_5, IFOESW_5)/ZPRES_F(JIJ_5, JK+IKL))
    ZRSATI(JIJ_5) = CST%XRD/CST%XRV*ZBUF(JIJ_5, IFOESI_5)/ZPRES_F(JIJ_5, JK+IKL) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ_5, IFOESI_5)/ZPRES_F(JIJ_5, JK+IKL))
    ZTPOW2_4=ZBUF(JIJ_5, IT_5)**2
    ZBUF(JIJ_5, IDRSATODTW_5) = ZRSATW(JIJ_5) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ_5, IFOESW_5)/ZPRES_F(JIJ_5, JK+IKL) ) &
                     * (CST%XBETAW/ZTPOW2_4 - CST%XGAMW/ZBUF(JIJ_5, IT_5))*ZBUF(JIJ_5, I1PRT_5)
    ZBUF(JIJ_5, IDRSATODTI_5) = ZRSATI(JIJ_5) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ_5, IFOESI_5)/ZPRES_F(JIJ_5, JK+IKL) ) &
                     * (CST%XBETAI/ZTPOW2_4 - CST%XGAMI/ZBUF(JIJ_5, IT_5))*ZBUF(JIJ_5, I1PRT_5)
    !PRSATW(JIJ) =  QSAT(PBUF(JIJ, IT),PP(JIJ)) !qsatw
    !PRSATI(JIJ) = QSATI(PBUF(JIJ, IT),PP(JIJ)) !qsati
    !PBUF(JIJ, IDRSATODTW) =  DQSAT(PBUF(JIJ, IT),PP(JIJ),PRSATW(JIJ))*PBUF(JIJ, I1PRT)
    !PBUF(JIJ, IDRSATODTI) = DQSATI(PBUF(JIJ, IT),PP(JIJ),PRSATI(JIJ))*PBUF(JIJ, I1PRT)
    ZRSATW(JIJ_5) = ZRSATW(JIJ_5)*ZBUF(JIJ_5, I1PRT_5)
    ZRSATI(JIJ_5) = ZRSATI(JIJ_5)*ZBUF(JIJ_5, I1PRT_5)
    ZBUF(JIJ_5, IRVSAT_5) = ZRSATW(JIJ_5)*(1-PFRAC_ICE_UP(JIJ_5, JK+IKL)) + ZRSATI(JIJ_5)*PFRAC_ICE_UP(JIJ_5, JK+IKL)
    ZBUF(JIJ_5, IDRSATODT_5) = (ZBUF(JIJ_5, IDRSATODTW_5)*(1-PFRAC_ICE_UP(JIJ_5, JK+IKL))+ &
              & ZBUF(JIJ_5, IDRSATODTI_5)*PFRAC_ICE_UP(JIJ_5, JK+IKL))

    !Computation of new PRL, PRI and PRV
    !Correction term applied to (PRV(JIJ)-PBUF(JIJ, IRVSAT)) is computed assuming that
    !PBUF(JIJ, ILVOCPEXN), PBUF(JIJ, ILSOCPEXN) and PBUF(JIJ, ICPH) don't vary to much with T. It takes into account
    !the variation (estimated linear) of Qsat with T
    ZBUF(JIJ_5, IRLTEMP_5)=(ZRV_UP(JIJ_5)-ZBUF(JIJ_5, IRVSAT_5))/ &
                  &(1 + ZBUF(JIJ_5, IDRSATODT_5)*ZBUF(JIJ_5, IEXN_5)* &
                  &     (ZBUF(JIJ_5, ILVOCPEXN_5)*(1-PFRAC_ICE_UP(JIJ_5, JK+IKL))+ZBUF(JIJ_5, ILSOCPEXN_5)*PFRAC_ICE_UP(JIJ_5, JK+IKL)))
    ZBUF(JIJ_5, IRLTEMP_5)=MIN(MAX(-ZRC_UP(JIJ_5)-ZRI_UP(JIJ_5), ZBUF(JIJ_5, IRLTEMP_5)),ZRV_UP(JIJ_5))
    ZRV_UP(JIJ_5)=ZRV_UP(JIJ_5)-ZBUF(JIJ_5, IRLTEMP_5)
    ZRC_UP(JIJ_5)=ZRC_UP(JIJ_5)+ZRI_UP(JIJ_5)+ZBUF(JIJ_5, IRLTEMP_5)
    ZRI_UP(JIJ_5)=PFRAC_ICE_UP(JIJ_5, JK+IKL)     * (ZRC_UP(JIJ_5))
    ZRC_UP(JIJ_5)=(1-PFRAC_ICE_UP(JIJ_5, JK+IKL)) * (PRT_UP(JIJ_5, JK+IKL) - ZRV_UP(JIJ_5))

    !Computation of Cph (as defined in Meso-NH doc, equation 2.2, to be used with mixing ratios)
    ZBUF(JIJ_5, ICPH_5)=CST%XCPD+ CST%XCPV * ZRV_UP(JIJ_5)+ CST%XCL * ZRC_UP(JIJ_5) + CST%XCI * ZRI_UP(JIJ_5) + ZBUF(JIJ_5, ICPH2_5)

    !Computation of L/Cph/EXN, then new PTH
    ZVAR2_4=ZBUF(JIJ_5, ICPH_5)*ZBUF(JIJ_5, IEXN_5)
    ZBUF(JIJ_5, ILVOCPEXN_5) = (CST%XLVTT + (CST%XCPV-CST%XCL) * (ZBUF(JIJ_5, IT_5)-CST%XTT)) /ZVAR2_4
    ZBUF(JIJ_5, ILSOCPEXN_5) = (CST%XLSTT + (CST%XCPV-CST%XCI) * (ZBUF(JIJ_5, IT_5)-CST%XTT)) /ZVAR2_4
    ZTH_UP(JIJ_5, JK+IKL)=PTHL_UP(JIJ_5, JK+IKL)+ZBUF(JIJ_5, ILVOCPEXN_5)*ZRC_UP(JIJ_5)+ZBUF(JIJ_5, ILSOCPEXN_5)*ZRI_UP(JIJ_5)

    !Computation of estimated mixing ration at saturation
    !To compute the adjustement a first order development was used
    ZVAR1_4=ZTH_UP(JIJ_5, JK+IKL)*ZBUF(JIJ_5, IEXN_5)-ZBUF(JIJ_5, IT_5)
    ZRSATW(JIJ_5)=ZRSATW(JIJ_5) + ZBUF(JIJ_5, IDRSATODTW_5)*ZVAR1_4
    ZRSATI(JIJ_5)=ZRSATI(JIJ_5) + ZBUF(JIJ_5, IDRSATODTI_5)*ZVAR1_4
  ENDDO
ENDDO


    !$mnh_expand_where(JIJ=IIJB:IIJE)
    WHERE(GTEST(IIJB:IIJE))
      PRC_UP(IIJB:IIJE,JK+IKL)=ZRC_UP(IIJB:IIJE)
      PRV_UP(IIJB:IIJE,JK+IKL)=ZRV_UP(IIJB:IIJE)
      PRI_UP(IIJB:IIJE,JK+IKL)=ZRI_UP(IIJB:IIJE)
      PRSAT_UP(IIJB:IIJE,JK+IKL) = ZRSATW(IIJB:IIJE)*(1-PFRAC_ICE_UP(IIJB:IIJE,JK+IKL)) + &
                                     & ZRSATI(IIJB:IIJE)*PFRAC_ICE_UP(IIJB:IIJE,JK+IKL)
    ENDWHERE

    ! Compute the updraft theta_v, buoyancy and w**2 for level JK+KKL
  DO JIJ=IIJB,IIJE
    IF(GTEST(JIJ)) THEN
      PTHV_UP(JIJ,JK+IKL) = ZTH_UP(JIJ,JK+IKL)* &
                                    & ((1+ZRVORD*PRV_UP(JIJ,JK+IKL))/(1+PRT_UP(JIJ,JK+IKL)))
      IF (ZBUO_INTEG_DRY(JIJ,JK)>0.) THEN
        ZW_UP2(JIJ,JK+IKL)  = ZW_UP2(JIJ,JK) + 2.*(PARAMMF%XABUO-PARAMMF%XBENTR*PARAMMF%XENTR_DRY)* &
                                                                &ZBUO_INTEG_DRY(JIJ,JK)
      ELSE
        ZW_UP2(JIJ,JK+IKL)  = ZW_UP2(JIJ,JK) + 2.*PARAMMF%XABUO* ZBUO_INTEG_DRY(JIJ,JK)
      END IF
      ZW_UP2(JIJ,JK+IKL)  = ZW_UP2(JIJ,JK+IKL)*(1.-(PARAMMF%XBDETR*ZMIX3_CLD(JIJ)+ &
                                                                       &PARAMMF%XBENTR*ZMIX2_CLD(JIJ)))&
              /(1.+(PARAMMF%XBDETR*ZMIX3_CLD(JIJ)+PARAMMF%XBENTR*ZMIX2_CLD(JIJ))) &
              +2.*(PARAMMF%XABUO)*ZBUO_INTEG_CLD(JIJ,JK)/ &
              &(1.+(PARAMMF%XBDETR*ZMIX3_CLD(JIJ)+PARAMMF%XBENTR*ZMIX2_CLD(JIJ)))
    END IF
  END DO

    ! Test if the updraft has reach the ETL
    WHERE (GTEST(IIJB:IIJE).AND.(PBUO_INTEG(IIJB:IIJE,JK)<=0.))
      KKETL(IIJB:IIJE) = JK+IKL
      GTESTETL(IIJB:IIJE)=.TRUE.
    ELSEWHERE
      GTESTETL(IIJB:IIJE)=.FALSE.
    ENDWHERE

    ! Test is we have reached the top of the updraft
    WHERE (GTEST(IIJB:IIJE).AND.((ZW_UP2(IIJB:IIJE,JK+IKL)<=0.).OR.(PEMF(IIJB:IIJE,JK+IKL)<=0.)))
        ZW_UP2(IIJB:IIJE,JK+IKL)=0.
        PEMF(IIJB:IIJE,JK+IKL)=0.
        GTEST(IIJB:IIJE)=.FALSE.
        PTHL_UP(IIJB:IIJE,JK+IKL)=ZTHLM_F(IIJB:IIJE,JK+IKL)
        PRT_UP(IIJB:IIJE,JK+IKL)=ZRTM_F(IIJB:IIJE,JK+IKL)
        PRC_UP(IIJB:IIJE,JK+IKL)=0.
        PRI_UP(IIJB:IIJE,JK+IKL)=0.
        PRV_UP(IIJB:IIJE,JK+IKL)=0.
        PTHV_UP(IIJB:IIJE,JK+IKL)=ZTHVM_F(IIJB:IIJE,JK+IKL)
        PFRAC_UP(IIJB:IIJE,JK+IKL)=0.
        KKCTL(IIJB:IIJE)=JK+IKL
    ENDWHERE
 
    ! compute frac_up at JK+KKL
    WHERE (GTEST(IIJB:IIJE))
      PFRAC_UP(IIJB:IIJE,JK+IKL)=PEMF(IIJB:IIJE,JK+IKL)/&
                                      &(SQRT(ZW_UP2(IIJB:IIJE,JK+IKL))*ZRHO_F(IIJB:IIJE,JK+IKL))
    ENDWHERE

    ! Updraft fraction must be smaller than XFRAC_UP_MAX
    WHERE (GTEST(IIJB:IIJE))
      PFRAC_UP(IIJB:IIJE,JK+IKL)=MIN(PARAMMF%XFRAC_UP_MAX,PFRAC_UP(IIJB:IIJE,JK+IKL))
    ENDWHERE

    ! When cloudy and non-buoyant, updraft fraction must decrease
    WHERE ((GTEST(IIJB:IIJE).AND.GTESTETL(IIJB:IIJE)).AND.GTESTLCL(IIJB:IIJE))
      PFRAC_UP(IIJB:IIJE,JK+IKL)=MIN(PFRAC_UP(IIJB:IIJE,JK+IKL),PFRAC_UP(IIJB:IIJE,JK))
    ENDWHERE

    ! Mass flux is updated with the new updraft fraction
    IF (OENTR_DETR) PEMF(IIJB:IIJE,JK+IKL)=PFRAC_UP(IIJB:IIJE,JK+IKL)*SQRT(ZW_UP2(IIJB:IIJE,JK+IKL))* &
                                              &ZRHO_F(IIJB:IIJE,JK+IKL)
    !$mnh_end_expand_where(JIJ=IIJB:IIJE)

  END IF !OENTR_DETR
ENDDO

IF(OENTR_DETR) THEN

  !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  PW_UP(IIJB:IIJE,1:IKT)=SQRT(ZW_UP2(IIJB:IIJE,1:IKT))
  !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)

  !$mnh_expand_array(JIJ=IIJB:IIJE)
  PEMF(IIJB:IIJE,IKB) =0.
  !$mnh_end_expand_array(JIJ=IIJB:IIJE)

  ! Limits the shallow convection scheme when cloud heigth is higher than 3000m.
  ! To do this, mass flux is multiplied by a coefficient decreasing linearly
  ! from 1 (for clouds of ZDEPTH_MAX1 m of depth) to 0 (for clouds of ZDEPTH_MAX2 m of depth).
  ! This way, all MF fluxes are diminished by this amount.
  ! Diagnosed cloud fraction is also multiplied by the same coefficient.
  !
  DO JIJ=IIJB,IIJE
     PDEPTH(JIJ) = MAX(0., PZZ(JIJ,KKCTL(JIJ)) -  PZZ(JIJ,KKLCL(JIJ)) )
  END DO

  !$mnh_expand_array(JIJ=IIJB:IIJE)
  GWORK1(IIJB:IIJE)= (GTESTLCL(IIJB:IIJE) .AND. (PDEPTH(IIJB:IIJE) > ZDEPTH_MAX1) )
  !$mnh_end_expand_array(JIJ=IIJB:IIJE)
  DO JK=1,IKT
    !$mnh_expand_array(JIJ=IIJB:IIJE)
    GWORK2(IIJB:IIJE,JK) = GWORK1(IIJB:IIJE)
    ZCOEF(IIJB:IIJE,JK) = (1.-(PDEPTH(IIJB:IIJE)-ZDEPTH_MAX1)/(ZDEPTH_MAX2-ZDEPTH_MAX1))
    ZCOEF(IIJB:IIJE,JK)=MIN(MAX(ZCOEF(IIJB:IIJE,JK),0.),1.)
    !$mnh_end_expand_array(JIJ=IIJB:IIJE)
  ENDDO
  !$mnh_expand_where(JIJ=IIJB:IIJE,JK=1:IKT)
  WHERE (GWORK2(IIJB:IIJE,1:IKT)) 
    PEMF(IIJB:IIJE,1:IKT)     = PEMF(IIJB:IIJE,1:IKT)     * ZCOEF(IIJB:IIJE,1:IKT)
    PFRAC_UP(IIJB:IIJE,1:IKT) = PFRAC_UP(IIJB:IIJE,1:IKT) * ZCOEF(IIJB:IIJE,1:IKT)
  ENDWHERE
  !$mnh_end_expand_where(JIJ=IIJB:IIJE,JK=1:IKT)
ENDIF

IF (LHOOK) CALL DR_HOOK('COMPUTE_UPDRAFT',1,ZHOOK_HANDLE)
CONTAINS
          !MNH_LIC Copyright 2006-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
      !MNH_LIC Copyright 2006-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
      END SUBROUTINE COMPUTE_UPDRAFT
END MODULE MODE_COMPUTE_UPDRAFT

