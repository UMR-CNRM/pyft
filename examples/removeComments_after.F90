






      SUBROUTINE TURB4(CST,CSTURB,BUCONF,TURBN,NEBN,D,TLES,            &
              & KMI,KRR,KRRL,KRRI,HLBCX,HLBCY,KGRADIENTS,KHALO,       &
              & KSPLIT,KMODEL_CL,KSV,KSV_LGBEG,KSV_LGEND,             &
              & KSV_LIMA_NR, KSV_LIMA_NS, KSV_LIMA_NG, KSV_LIMA_NH,   &
              & O2D,ONOMIXLG,OFLAT,OCOUPLES,OBLOWSNOW,OIBM,OFLYER,    &
              & OCOMPUTE_SRC, PRSNOW,                                 &
              & OOCEAN,ODEEPOC,ODIAG_IN_RUN,                          &
              & HTURBLEN_CL,HCLOUD,                                   &
              & PTSTEP,TPFILE,                                        &
              & PDXX,PDYY,PDZZ,PDZX,PDZY,PZZ,                         &
              & PDIRCOSXW,PDIRCOSYW,PDIRCOSZW,PCOSSLOPE,PSINSLOPE,    &
              & PRHODJ,PTHVREF,PHGRAD,PZS,                            &
              & PSFTH,PSFRV,PSFSV,PSFU,PSFV,                          &
              & PPABST,PUT,PVT,PWT,PTKET,PSVT,PSRCT,                  &
              & PLENGTHM,PLENGTHH,MFMOIST,                            &
              & PBL_DEPTH,PSBL_DEPTH,                                 &
              & PCEI,PCEI_MIN,PCEI_MAX,PCOEF_AMPL_SAT,                &
              & PTHLT,PRT,                                            &
              & PRUS,PRVS,PRWS,PRTHLS,PRRS,PRSVS,PRTKES,              &
              & PSIGS,                                                &
              & PFLXZTHVMF,PWTH,PWRC,PWSV,PDP,PTP,PTDIFF,PTDISS,      &
              & TBUDGETS, KBUDGETS,                                   &
              & PEDR,PLEM,PRTKEMS,PTPMF,                              &
              & PDRUS_TURB,PDRVS_TURB,                                &
              & PDRTHLS_TURB,PDRRTS_TURB,PDRSVS_TURB,PTR,PDISS,       &
              & PIBM_LS, PIBM_XMUT,                                   &
              & PCURRENT_TKE_DISS, PSSTFL, PSSTFL_C, PSSRFL_C,        &
              & PSSUFL_C, PSSVFL_C,PSSUFL,PSSVFL                      )
















































































































































































































USE MODE_SHUMAN_PHY, ONLY: MZF_PHY,MXF_PHY,MYF_PHY
USE YOMHOOK ,   ONLY: LHOOK, DR_HOOK, JPHOOK

USE MODD_BUDGET,     ONLY:  NBUDGET_U,  NBUDGET_V,  NBUDGET_W,  NBUDGET_TH, NBUDGET_RV, NBUDGET_RC,  &
                             NBUDGET_RI, NBUDGET_SV1, &
                            TBUDGETDATA, TBUDGETCONF_t
USE MODD_CST,        ONLY: CST_t
USE MODD_CTURB,      ONLY: CSTURB_t
USE MODD_DIMPHYEX,   ONLY: DIMPHYEX_t
USE MODD_FIELD,      ONLY: TFIELDMETADATA, TYPEREAL
USE MODD_IO,         ONLY: TFILEDATA
USE MODD_LES,        ONLY: TLES_t
USE MODD_PARAMETERS, ONLY: JPVEXT_TURB, XUNDEF
USE MODD_TURB_n,     ONLY: TURB_t
USE MODD_NEB_n,      ONLY: NEB_t

USE MODE_BL89,                ONLY: BL89
USE MODE_BUDGET_PHY,              ONLY: BUDGET_STORE_INIT_PHY, BUDGET_STORE_END_PHY
USE MODE_EMOIST,              ONLY: EMOIST
USE MODE_ETHETA,              ONLY: ETHETA
USE MODE_GRADIENT_U_PHY,      ONLY: GZ_U_UW_PHY
USE MODE_GRADIENT_V_PHY,      ONLY: GZ_V_VW_PHY
USE MODE_GRADIENT_W_PHY,      ONLY: GZ_W_M_PHY
USE MODE_GRADIENT_M_PHY,      ONLY: GZ_M_W_PHY
USE MODE_IBM_MIXINGLENGTH,    ONLY: IBM_MIXINGLENGTH
USE MODE_IO_FIELD_WRITE_PHY,      ONLY: IO_FIELD_WRITE_PHY
USE MODE_RMC01,               ONLY: RMC01
USE MODE_ROTATE_WIND,         ONLY: ROTATE_WIND, UPDATE_ROTATE_WIND
USE MODE_SBL_PHY,             ONLY: LMO
USE MODE_SOURCES_NEG_CORRECT, ONLY: SOURCES_NEG_CORRECT_PHY
USE MODE_TM06,                ONLY: TM06
USE MODE_TKE_EPS_SOURCES,     ONLY: TKE_EPS_SOURCES
USE MODE_TURB_HOR_SPLT,       ONLY: TURB_HOR_SPLT
USE MODE_TURB_VER,            ONLY: TURB_VER
USE MODE_UPDATE_LM,           ONLY: UPDATE_LM

USE MODI_LES_MEAN_SUBGRID_PHY


IMPLICIT NONE






TYPE(DIMPHYEX_t),       INTENT(IN)   :: D             
TYPE(CST_t),            INTENT(IN)   :: CST           
TYPE(CSTURB_t),         INTENT(IN)   :: CSTURB        
TYPE(TBUDGETCONF_t),    INTENT(IN)   :: BUCONF        
TYPE(TURB_t),           INTENT(IN)   :: TURBN         
TYPE(NEB_t),            INTENT(IN)   :: NEBN          
TYPE(TLES_t),           INTENT(INOUT)   :: TLES          
INTEGER,                INTENT(IN)   :: KGRADIENTS    
INTEGER,                INTENT(IN)   :: KMI           
INTEGER,                INTENT(IN)   :: KRR           
INTEGER,                INTENT(IN)   :: KRRL          
INTEGER,                INTENT(IN)   :: KRRI          
INTEGER,                INTENT(IN)   :: KSV, KSV_LGBEG, KSV_LGEND 
INTEGER,                INTENT(IN)   :: KSV_LIMA_NR,KSV_LIMA_NS,KSV_LIMA_NG,KSV_LIMA_NH
CHARACTER(LEN=4),DIMENSION(2),INTENT(IN):: HLBCX, HLBCY  
INTEGER,                INTENT(IN)   :: KSPLIT        
INTEGER,                INTENT(IN)   :: KMODEL_CL     
INTEGER,                INTENT(IN)   ::  KHALO        
LOGICAL,                INTENT(IN)   ::  OCOMPUTE_SRC 
LOGICAL,                INTENT(IN)   ::  OOCEAN       
LOGICAL,                INTENT(IN)   ::  ODEEPOC      
LOGICAL,                INTENT(IN)   ::  OFLYER       
LOGICAL,                INTENT(IN)   ::  OFLAT        
LOGICAL,                INTENT(IN)   ::  OCOUPLES     
LOGICAL,                INTENT(IN)   ::  OBLOWSNOW    
LOGICAL,                INTENT(IN)   ::  ODIAG_IN_RUN 
LOGICAL,                INTENT(IN)   ::  OIBM         
CHARACTER(LEN=4),       INTENT(IN)   ::  HTURBLEN_CL  
CHARACTER (LEN=4),      INTENT(IN)   ::  HCLOUD       
REAL,                   INTENT(IN)   ::  PRSNOW       
REAL,                   INTENT(IN)   ::  PTSTEP       
TYPE(TFILEDATA),        INTENT(IN)   ::  TPFILE       

REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)   :: PDXX,PDYY,PDZZ,PDZX,PDZY
                                        
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)   :: PZZ       

REAL, DIMENSION(D%NIJT),   INTENT(IN)      ::  PDIRCOSXW, PDIRCOSYW, PDIRCOSZW

REAL, DIMENSION(D%NIJT),   INTENT(IN)   ::  PCOSSLOPE       
                                 
REAL, DIMENSION(D%NIJT),   INTENT(IN)   ::  PSINSLOPE       
                                 
REAL, DIMENSION(D%NIJT),   INTENT(IN)   ::  PZS 
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)      ::  PRHODJ    
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)      ::  MFMOIST 
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)      ::  PTHVREF   
                                        
REAL, DIMENSION(D%NIJT,D%NKT,KGRADIENTS),   INTENT(IN) ::  PHGRAD      

REAL, DIMENSION(D%NIJT),   INTENT(IN)      ::  PSFTH,PSFRV,   &

                                            PSFU,PSFV

REAL, DIMENSION(D%NIJT,KSV), INTENT(IN)      ::  PSFSV



REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(IN) ::  PPABST      
REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(IN) ::  PUT,PVT,PWT 
REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(IN) ::  PTKET       
REAL, DIMENSION(D%NIJT,D%NKT,KSV), INTENT(IN) ::  PSVT        
REAL, DIMENSION(MERGE(D%NIJT,0,OCOMPUTE_SRC),&
                MERGE(D%NKT,0,OCOMPUTE_SRC)),   INTENT(IN) ::  PSRCT       
                      
REAL, DIMENSION(D%NIJT),INTENT(INOUT) :: PBL_DEPTH  
REAL, DIMENSION(D%NIJT),INTENT(INOUT) :: PSBL_DEPTH 


REAL, DIMENSION(MERGE(D%NIJT,0,KMODEL_CL==KMI .AND. HTURBLEN_CL/='NONE'),&
                MERGE(D%NKT,0,KMODEL_CL==KMI .AND. HTURBLEN_CL/='NONE')),INTENT(IN)      ::  PCEI
                                                 
                                                 
                                                 
REAL, INTENT(IN)      ::  PCEI_MIN 
REAL, INTENT(IN)      ::  PCEI_MAX 
REAL, INTENT(IN)      ::  PCOEF_AMPL_SAT 


REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(INOUT) ::  PTHLT       
REAL, DIMENSION(D%NIJT,D%NKT,KRR), INTENT(INOUT) ::  PRT         
                             



REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(INOUT) ::  PRUS,PRVS,PRWS,PRTHLS,PRTKES


REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(IN),OPTIONAL    ::  PRTKEMS
REAL, DIMENSION(D%NIJT,D%NKT,KRR), INTENT(INOUT) ::  PRRS

REAL, DIMENSION(D%NIJT,D%NKT,KSV), INTENT(INOUT) ::  PRSVS


REAL, DIMENSION(MERGE(D%NIJT,0,OCOMPUTE_SRC),&
                MERGE(D%NKT,0,OCOMPUTE_SRC)), INTENT(OUT)     ::  PSIGS
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(OUT),OPTIONAL     ::  PDRUS_TURB   
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(OUT),OPTIONAL     ::  PDRVS_TURB   
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(OUT),OPTIONAL     ::  PDRTHLS_TURB 
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(OUT),OPTIONAL     ::  PDRRTS_TURB  
REAL, DIMENSION(D%NIJT,D%NKT,KSV), INTENT(OUT),OPTIONAL ::  PDRSVS_TURB  
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)      ::  PFLXZTHVMF


REAL, DIMENSION(D%NIJT,D%NKT), INTENT(OUT)  :: PWTH       
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(OUT)  :: PWRC       
REAL, DIMENSION(D%NIJT,D%NKT,KSV),INTENT(OUT) :: PWSV       
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(OUT)  :: PTP        
                                                   
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(OUT),OPTIONAL  :: PTPMF      
                                                   
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(OUT)  :: PDP        
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(OUT)  :: PTDIFF     
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(OUT)  :: PTDISS     

TYPE(TBUDGETDATA), DIMENSION(KBUDGETS), INTENT(INOUT) :: TBUDGETS
INTEGER, INTENT(IN) :: KBUDGETS

LOGICAL, INTENT(IN) :: ONOMIXLG          
LOGICAL, INTENT(IN) :: O2D               


REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)    :: PLENGTHM, PLENGTHH

REAL, DIMENSION(D%NIJT,D%NKT), INTENT(OUT), OPTIONAL  :: PEDR  
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(OUT), OPTIONAL  :: PLEM  
REAL, DIMENSION(D%NIJT,D%NKT),  INTENT(OUT), OPTIONAL  ::  PTR          
REAL, DIMENSION(D%NIJT,D%NKT),  INTENT(OUT), OPTIONAL  ::  PDISS        
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(INOUT), OPTIONAL  ::  PCURRENT_TKE_DISS 
REAL, DIMENSION(D%NIJT), INTENT(IN),OPTIONAL   ::  PSSTFL        
REAL, DIMENSION(D%NIJT), INTENT(IN),OPTIONAL   ::  PSSTFL_C  
REAL, DIMENSION(D%NIJT), INTENT(IN),OPTIONAL   ::  PSSRFL_C  
REAL, DIMENSION(D%NIJT), INTENT(IN),OPTIONAL   ::  PSSUFL_C        
REAL, DIMENSION(D%NIJT), INTENT(IN),OPTIONAL   ::  PSSVFL_C  
REAL, DIMENSION(D%NIJT), INTENT(IN),OPTIONAL   ::  PSSUFL   
REAL, DIMENSION(D%NIJT), INTENT(IN),OPTIONAL   ::  PSSVFL  

REAL, DIMENSION(D%NIJT,D%NKT), INTENT(OUT), OPTIONAL :: PIBM_XMUT 
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN), OPTIONAL  :: PIBM_LS 






REAL, DIMENSION(D%NIJT,D%NKT) ::     &
          ZCP,                        &  
          ZEXN,                       &  
          ZT,                         &  
          ZLOCPEXNM,                  &  
          ZLM,ZLMW,                   &  
          ZLEPS,                      &  
          ZTRH,                       &  
          ZATHETA,ZAMOIST,            &  
          ZCOEF_DISS,                 &  
          ZFRAC_ICE,                  &  
          ZMWTH,ZMWR,ZMTH2,ZMR2,ZMTHR,&  
          ZFWTH,ZFWR,ZFTH2,ZFR2,ZFTHR,&  
          ZTHLM,ZRTKEMS,              &  
          ZSHEAR, ZDUDZ, ZDVDZ,       &  
          ZLVOCPEXNM,ZLSOCPEXNM,      &  
          ZATHETA_ICE,ZAMOIST_ICE,    &  
          ZRVSAT, ZDRVSATDT,          &  
          ZWORK1,ZWORK2,              &  
          ZETHETA,ZEMOIST,            &  
          ZDTHLDZ,ZDRTDZ,             &  
          ZCOEF_AMPL,                 &  
                                         
          ZLM_CLOUD                      


REAL, DIMENSION(D%NIJT,D%NKT,KRR) :: ZRM 
REAL, DIMENSION(D%NIJT) ::  ZTAU11M,ZTAU12M,  &
                                                 ZTAU22M,ZTAU33M,  &
            
                                                 ZUSLOPE,ZVSLOPE,  &
            
            
                                                 ZCDUEFF,          &
            
            
                                                 ZUSTAR, ZLMO,     &
                                                 ZRVM, ZSFRV,ZWORK2D
            

            
            


REAL, DIMENSION(D%NIJT,D%NKT,KSV) :: ZRSVS

REAL                :: ZEXPL        
REAL                :: ZRVORD       
REAL                :: ZEPS         
REAL                :: ZD           
REAL                :: ZVAR         
REAL                :: ZPENTE       
REAL                :: ZCOEF_AMPL_CEI_NUL
                                         

INTEGER             :: IIJB,IIJE,IKB,IKE      

INTEGER             :: IKT,IKA,IKU  
INTEGER             :: IKL
INTEGER             :: IKTB,IKTE    
INTEGER             :: JRR,JK,JSV   
INTEGER             :: JIJ          
REAL                :: ZL0          
REAL                :: ZALPHA       
                                    


REAL :: ZTIME1, ZTIME2
TYPE(TFIELDMETADATA) :: TZFIELD







REAL(KIND=JPHOOK) :: ZHOOK_HANDLE,ZHOOK_HANDLE2
IF (LHOOK) THEN
  CALL DR_HOOK('TURB',0,ZHOOK_HANDLE)
END IF

IF (TURBN%LHARAT .AND. TURBN%CTURBDIM /= '1DIM') THEN
  CALL ABOR1('TURBN%LHARATU only implemented for option TURBN%CTURBDIM=1DIM!')
ENDIF
IF (TURBN%LHARAT .AND. TLES%LLES_CALL) THEN
  CALL ABOR1('TURBN%LHARATU not implemented for option LLES_CALL')
ENDIF

IKT=D%NKT
IKTB=D%NKTB
IKTE=D%NKTE
IKB=D%NKB
IKE=D%NKE
IKA=D%NKA
IKU=D%NKU
IKL=D%NKL
IIJE=D%NIJE
IIJB=D%NIJB

ZEXPL = 1.- TURBN%XIMPL
ZRVORD= CST%XRV / CST%XRD


IF (TURBN%CTURBLEN=='BL89' .OR. TURBN%CTURBLEN=='RM17' .OR. TURBN%CTURBLEN=='HM21' .OR. TURBN%LRMC01) THEN
  ZTHLM(IIJB:IIJE,1:IKT) = PTHLT(IIJB:IIJE,1:IKT)
  ZRM(IIJB:IIJE,1:IKT,:) = PRT(IIJB:IIJE,1:IKT,:)
END IF


ZRSVS(IIJB:IIJE,1:IKT,1:KSV)=PRSVS(IIJB:IIJE,1:IKT,1:KSV)









!$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
ZCP(IIJB:IIJE,1:IKT)=CST%XCPD

IF (KRR > 0) THEN
  ZCP(IIJB:IIJE,1:IKT) = ZCP(IIJB:IIJE,1:IKT) + CST%XCPV * PRT(IIJB:IIJE,1:IKT,1)
END IF
!$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
DO JRR = 2,1+KRRL                          
!$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)  
  ZCP(IIJB:IIJE,1:IKT)  = ZCP(IIJB:IIJE,1:IKT) + CST%XCL * PRT(IIJB:IIJE,1:IKT,JRR)
!$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
END DO

DO JRR = 2+KRRL,1+KRRL+KRRI                
!$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  ZCP(IIJB:IIJE,1:IKT)  = ZCP(IIJB:IIJE,1:IKT)  + CST%XCI * PRT(IIJB:IIJE,1:IKT,JRR)
!$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
END DO



IF (OOCEAN) THEN
!$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  ZEXN(IIJB:IIJE,1:IKT) = 1.
!$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
ELSE
!$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  ZEXN(IIJB:IIJE,1:IKT) = (PPABST(IIJB:IIJE,1:IKT)/CST%XP00) ** (CST%XRD/CST%XCPD)
!$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
END IF



!$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
ZCOEF_DISS(IIJB:IIJE,1:IKT) = 1/(ZCP(IIJB:IIJE,1:IKT) * ZEXN(IIJB:IIJE,1:IKT))
!$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)


ZFRAC_ICE(IIJB:IIJE,1:IKT) = 0.0
ZATHETA(IIJB:IIJE,1:IKT) = 0.0
ZAMOIST(IIJB:IIJE,1:IKT) = 0.0

IF (KRRL >=1) THEN



  !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  ZT(IIJB:IIJE,1:IKT) =  PTHLT(IIJB:IIJE,1:IKT) * ZEXN(IIJB:IIJE,1:IKT)
  !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)



  IF ( KRRI >= 1 ) THEN
    IF (NEBN%LSTATNW) THEN
    
       CALL COMPUTE_FUNCTION_THERMO_NEW_STAT(CST%XALPW,CST%XBETAW,CST%XGAMW,CST%XLVTT,CST%XCL,ZT,ZEXN,ZCP, &
                                 ZLVOCPEXNM,ZAMOIST,ZATHETA)
       CALL COMPUTE_FUNCTION_THERMO_NEW_STAT(CST%XALPI,CST%XBETAI,CST%XGAMI,CST%XLSTT,CST%XCI,ZT,ZEXN,ZCP, &
                                 ZLSOCPEXNM,ZAMOIST_ICE,ZATHETA_ICE)
    ELSE
      CALL COMPUTE_FUNCTION_THERMO(CST%XALPW,CST%XBETAW,CST%XGAMW,CST%XLVTT,CST%XCL,ZT,ZEXN,ZCP, &
                                 ZLVOCPEXNM,ZAMOIST,ZATHETA)
      CALL COMPUTE_FUNCTION_THERMO(CST%XALPI,CST%XBETAI,CST%XGAMI,CST%XLSTT,CST%XCI,ZT,ZEXN,ZCP, &
                                 ZLSOCPEXNM,ZAMOIST_ICE,ZATHETA_ICE)
    ENDIF

    !$mnh_expand_where(JIJ=IIJB:IIJE,JK=1:IKT)
    WHERE(PRT(IIJB:IIJE,1:IKT,2)+PRT(IIJB:IIJE,1:IKT,4)>0.0)
      ZFRAC_ICE(IIJB:IIJE,1:IKT) = PRT(IIJB:IIJE,1:IKT,4) / ( PRT(IIJB:IIJE,1:IKT,2) &
                                          +PRT(IIJB:IIJE,1:IKT,4) )
    END WHERE
    !$mnh_end_expand_where(JIJ=IIJB:IIJE,JK=1:IKT)

    !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
    ZLOCPEXNM(IIJB:IIJE,1:IKT) = (1.0-ZFRAC_ICE(IIJB:IIJE,1:IKT))*ZLVOCPEXNM(IIJB:IIJE,1:IKT) &
                           +ZFRAC_ICE(IIJB:IIJE,1:IKT) *ZLSOCPEXNM(IIJB:IIJE,1:IKT)
    ZAMOIST(IIJB:IIJE,1:IKT) = (1.0-ZFRAC_ICE(IIJB:IIJE,1:IKT))*ZAMOIST(IIJB:IIJE,1:IKT) &
                         +ZFRAC_ICE(IIJB:IIJE,1:IKT) *ZAMOIST_ICE(IIJB:IIJE,1:IKT)
    ZATHETA(IIJB:IIJE,1:IKT) = (1.0-ZFRAC_ICE(IIJB:IIJE,1:IKT))*ZATHETA(IIJB:IIJE,1:IKT) &
                         +ZFRAC_ICE(IIJB:IIJE,1:IKT) *ZATHETA_ICE(IIJB:IIJE,1:IKT)
    !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  ELSE
    
    IF (NEBN%LSTATNW) THEN
      CALL COMPUTE_FUNCTION_THERMO_NEW_STAT(CST%XALPW,CST%XBETAW,CST%XGAMW,CST%XLVTT,CST%XCL,ZT,ZEXN,ZCP, &
                                 ZLOCPEXNM,ZAMOIST,ZATHETA)
    ELSE
      CALL COMPUTE_FUNCTION_THERMO(CST%XALPW,CST%XBETAW,CST%XGAMW,CST%XLVTT,CST%XCL,ZT,ZEXN,ZCP, &
                                   ZLOCPEXNM,ZAMOIST,ZATHETA)
    ENDIF
  END IF


  IF ( TPFILE%LOPENED .AND. TURBN%LTURB_DIAG ) THEN
    TZFIELD = TFIELDMETADATA(      &
      CMNHNAME   = 'ATHETA',       &
      CSTDNAME   = '',             &
      CLONGNAME  = 'ATHETA',       &
      CUNITS     = 'm',            &
      CDIR       = 'XY',           &
      CCOMMENT   = 'X_Y_Z_ATHETA', &
      NGRID      = 1,              &
      NTYPE      = TYPEREAL,       &
      NDIMS      = 3,              &
      LTIMEDEP   = .TRUE.          )
    CALL IO_FIELD_WRITE_PHY(D,TPFILE,TZFIELD,ZATHETA)

    TZFIELD = TFIELDMETADATA(      &
      CMNHNAME   = 'AMOIST',       &
      CSTDNAME   = '',             &
      CLONGNAME  = 'AMOIST',       &
      CUNITS     = 'm',            &
      CDIR       = 'XY',           &
      CCOMMENT   = 'X_Y_Z_AMOIST', &
      NGRID      = 1,              &
      NTYPE      = TYPEREAL,       &
      NDIMS      = 3,              &
      LTIMEDEP   = .TRUE.          )
    CALL IO_FIELD_WRITE_PHY(D,TPFILE,TZFIELD,ZAMOIST)
  END IF

ELSE
  ZLOCPEXNM(IIJB:IIJE,1:IKT)=0.
END IF              



IF ( KRRL >= 1 ) THEN
  IF ( KRRI >= 1 ) THEN
    !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
    
    PRT(IIJB:IIJE,1:IKT,1)  = PRT(IIJB:IIJE,1:IKT,1)  + PRT(IIJB:IIJE,1:IKT,2)  & 
                                    + PRT(IIJB:IIJE,1:IKT,4)
    PRRS(IIJB:IIJE,1:IKT,1) = PRRS(IIJB:IIJE,1:IKT,1) + PRRS(IIJB:IIJE,1:IKT,2) & 
                                    + PRRS(IIJB:IIJE,1:IKT,4)
    
    PTHLT(IIJB:IIJE,1:IKT)  = PTHLT(IIJB:IIJE,1:IKT)  - ZLVOCPEXNM(IIJB:IIJE,1:IKT) &
                                    * PRT(IIJB:IIJE,1:IKT,2) &
                                  - ZLSOCPEXNM(IIJB:IIJE,1:IKT) * PRT(IIJB:IIJE,1:IKT,4)
    PRTHLS(IIJB:IIJE,1:IKT) = PRTHLS(IIJB:IIJE,1:IKT) - ZLVOCPEXNM(IIJB:IIJE,1:IKT) &
                                    * PRRS(IIJB:IIJE,1:IKT,2) &
                                  - ZLSOCPEXNM(IIJB:IIJE,1:IKT) * PRRS(IIJB:IIJE,1:IKT,4)
    !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  ELSE
    !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
    
    PRT(IIJB:IIJE,1:IKT,1)  = PRT(IIJB:IIJE,1:IKT,1)  + PRT(IIJB:IIJE,1:IKT,2)
    PRRS(IIJB:IIJE,1:IKT,1) = PRRS(IIJB:IIJE,1:IKT,1) + PRRS(IIJB:IIJE,1:IKT,2)
    
    PTHLT(IIJB:IIJE,1:IKT)  = PTHLT(IIJB:IIJE,1:IKT)  - ZLOCPEXNM(IIJB:IIJE,1:IKT) &
                                    * PRT(IIJB:IIJE,1:IKT,2)
    PRTHLS(IIJB:IIJE,1:IKT) = PRTHLS(IIJB:IIJE,1:IKT) - ZLOCPEXNM(IIJB:IIJE,1:IKT) &
                                    * PRRS(IIJB:IIJE,1:IKT,2)
    !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  END IF
END IF


IF(PRESENT(PDRUS_TURB)) THEN
  PDRUS_TURB(:,:) = PRUS(:,:)
  PDRVS_TURB(:,:) = PRVS(:,:)
  PDRTHLS_TURB(:,:) = PRTHLS(:,:)
  PDRRTS_TURB(:,:)  = PRRS(:,:,1)
  PDRSVS_TURB(:,:,:)  = PRSVS(:,:,:)
END IF






IF (.NOT. TURBN%LHARAT) THEN

SELECT CASE (TURBN%CTURBLEN)




  CASE ('BL89')
    ZSHEAR(:,:)=0.
    CALL BL89(D,CST,CSTURB,TURBN,PZZ,PDZZ,PTHVREF,ZTHLM,KRR,ZRM,PTKET,ZSHEAR,ZLM,OOCEAN)




  CASE ('RM17')
    CALL GZ_U_UW_PHY(D,PUT,PDZZ,ZWORK1)
    CALL MZF_PHY(D,ZWORK1,ZWORK2)
    CALL MXF_PHY(D,ZWORK2,ZDUDZ)
    
    CALL GZ_V_VW_PHY(D,PVT,PDZZ,ZWORK1)
    CALL MZF_PHY(D,ZWORK1,ZWORK2)
    CALL MYF_PHY(D,ZWORK2,ZDVDZ)
    
    !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
    ZSHEAR(IIJB:IIJE,1:IKT) = SQRT(ZDUDZ(IIJB:IIJE,1:IKT)*ZDUDZ(IIJB:IIJE,1:IKT) &
                                    + ZDVDZ(IIJB:IIJE,1:IKT)*ZDVDZ(IIJB:IIJE,1:IKT))
    !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
    CALL BL89(D,CST,CSTURB,TURBN,PZZ,PDZZ,PTHVREF,ZTHLM,KRR,ZRM,PTKET,ZSHEAR,ZLM,OOCEAN)




  CASE ('HM21')
    CALL GZ_U_UW_PHY(D,PUT,PDZZ,ZWORK1)
    CALL MZF_PHY(D,ZWORK1,ZWORK2)
    CALL MXF_PHY(D,ZWORK2,ZDUDZ)
    
    CALL GZ_V_VW_PHY(D,PVT,PDZZ,ZWORK1)
    CALL MZF_PHY(D,ZWORK1,ZWORK2)
    CALL MYF_PHY(D,ZWORK2,ZDVDZ)
    
    !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
    ZSHEAR(IIJB:IIJE,1:IKT) = SQRT(ZDUDZ(IIJB:IIJE,1:IKT)*ZDUDZ(IIJB:IIJE,1:IKT) &
                                    + ZDVDZ(IIJB:IIJE,1:IKT)*ZDVDZ(IIJB:IIJE,1:IKT))
    !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
    CALL BL89(D,CST,CSTURB,TURBN,PZZ,PDZZ,PTHVREF,ZTHLM,KRR,ZRM,PTKET,ZSHEAR,ZLM,OOCEAN)

    CALL DELT(ZLMW,ODZ=.FALSE.)
    
    
    
    
    
    
    !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
    ZLM(IIJB:IIJE,1:IKT) = MIN(ZLM(IIJB:IIJE,1:IKT),TURBN%XCADAP*ZLMW(IIJB:IIJE,1:IKT))
    !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)




  CASE ('DELT')
    CALL DELT(ZLM,ODZ=.TRUE.)




  CASE ('DEAR')
    CALL DEAR(ZLM)




  CASE ('BLKR')
   ZL0 = 100.
   !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
   ZLM(IIJB:IIJE,1:IKT) = ZL0
   !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)

   ZALPHA=0.5**(-1.5)
   
   DO JK=IKTB,IKTE
     !$mnh_expand_array(JIJ=IIJB:IIJE)
     ZLM(IIJB:IIJE,JK) = ( 0.5*(PZZ(IIJB:IIJE,JK)+PZZ(IIJB:IIJE,JK+IKL)) - &
     & PZZ(IIJB:IIJE,IKA+JPVEXT_TURB*IKL) ) * PDIRCOSZW(IIJB:IIJE)
     ZLM(IIJB:IIJE,JK) = ZALPHA  * ZLM(IIJB:IIJE,JK) * ZL0 / ( ZL0 + ZALPHA*ZLM(IIJB:IIJE,JK) )
     !$mnh_end_expand_array(JIJ=IIJB:IIJE)
   END DO

   !$mnh_expand_array(JIJ=IIJB:IIJE)
   ZLM(IIJB:IIJE,IKTB-1) = ZLM(IIJB:IIJE,IKTB)
   ZLM(IIJB:IIJE,IKTE+1) = ZLM(IIJB:IIJE,IKTE)
   !$mnh_end_expand_array(JIJ=IIJB:IIJE)



END SELECT



IF (KMODEL_CL==KMI .AND. HTURBLEN_CL/='NONE') THEN
  CALL CLOUD_MODIF_LM
END IF
ENDIF  





IF (TURBN%LHARAT) THEN
  !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  ZLEPS(IIJB:IIJE,1:IKT)=PLENGTHM(IIJB:IIJE,1:IKT)*(3.75**2.)
  !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
ELSE
  ZLEPS(IIJB:IIJE,1:IKT)=ZLM(IIJB:IIJE,1:IKT)
ENDIF




!$mnh_expand_array(JIJ=IIJB:IIJE)
ZLMO(IIJB:IIJE)=XUNDEF
!$mnh_end_expand_array(JIJ=IIJB:IIJE)
IF (TURBN%LRMC01) THEN
  !$mnh_expand_array(JIJ=IIJB:IIJE)
  ZUSTAR(IIJB:IIJE)=(PSFU(IIJB:IIJE)**2+PSFV(IIJB:IIJE)**2)**(0.25)
  !$mnh_end_expand_array(JIJ=IIJB:IIJE)
  IF (KRR>0) THEN
    CALL LMO(D,CST,ZUSTAR,ZTHLM(:,IKB),ZRM(:,IKB,1),PSFTH,PSFRV,ZLMO)
  ELSE
    ZRVM(:)=0.
    ZSFRV(:)=0.
    CALL LMO(D,CST,ZUSTAR,ZTHLM(:,IKB),ZRVM,PSFTH,ZSFRV,ZLMO)
  END IF
  CALL RMC01(D,CST,CSTURB,TURBN,PZZ,PDXX,PDYY,PDZZ,PDIRCOSZW,PSBL_DEPTH,ZLMO,ZLM,ZLEPS)
END IF


IF (TURBN%CTURBLEN=='HM21') THEN
  !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  ZLEPS(IIJB:IIJE,1:IKT) = MIN(ZLEPS(IIJB:IIJE,1:IKT),ZLMW(IIJB:IIJE,1:IKT)*TURBN%XCADAP)
  !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
END IF




IF (TURBN%CTURBDIM=="3DIM") THEN
  CALL UPDATE_LM(D,HLBCX,HLBCY,ZLM,ZLEPS)
END IF




IF (OIBM) THEN
   CALL IBM_MIXINGLENGTH(D,ZLM,ZLEPS,PIBM_XMUT,PIBM_LS,PTKET)
ENDIF










IF (TURBN%LROTATE_WIND) THEN
  CALL ROTATE_WIND(D,PUT,PVT,PWT,                       &
                     PDIRCOSXW, PDIRCOSYW, PDIRCOSZW,   &
                     PCOSSLOPE,PSINSLOPE,               &
                     PDXX,PDYY,PDZZ,                    &
                     ZUSLOPE,ZVSLOPE                    )

  CALL UPDATE_ROTATE_WIND(D,ZUSLOPE,ZVSLOPE,HLBCX,HLBCY)
ELSE
  ZUSLOPE(IIJB:IIJE)=PUT(IIJB:IIJE,IKA)
  ZVSLOPE(IIJB:IIJE)=PVT(IIJB:IIJE,IKA)
END IF
IF (OOCEAN) THEN
  ZUSLOPE(IIJB:IIJE)=PUT(IIJB:IIJE,IKU-1)
  ZVSLOPE(IIJB:IIJE)=PVT(IIJB:IIJE,IKU-1)
END IF




!$mnh_expand_array(JIJ=IIJB:IIJE)
ZCDUEFF(IIJB:IIJE) =-SQRT ( (PSFU(IIJB:IIJE)**2 + PSFV(IIJB:IIJE)**2) /               &
                    (CST%XMNH_TINY + ZUSLOPE(IIJB:IIJE)**2 + ZVSLOPE(IIJB:IIJE)**2 ) )
!$mnh_end_expand_array(JIJ=IIJB:IIJE)



IF (OOCEAN) THEN
  ZTAU11M(IIJB:IIJE)=0.
ELSE
  !$mnh_expand_array(JIJ=IIJB:IIJE)        
  ZTAU11M(IIJB:IIJE) =2./3.*(  (1.+ (PZZ(IIJB:IIJE,IKB+IKL)-PZZ(IIJB:IIJE,IKB))  &
                           /(PDZZ(IIJB:IIJE,IKB+IKL)+PDZZ(IIJB:IIJE,IKB))  &
                       )   *PTKET(IIJB:IIJE,IKB)                   &
                     -0.5  *PTKET(IIJB:IIJE,IKB+IKL)                 &
                    )
  !$mnh_end_expand_array(JIJ=IIJB:IIJE)
END IF
ZTAU12M(IIJB:IIJE) =0.0
ZTAU22M(IIJB:IIJE) =ZTAU11M(IIJB:IIJE)
ZTAU33M(IIJB:IIJE) =ZTAU11M(IIJB:IIJE)





ZMWTH(:,:) = 0.     
ZMWR(:,:)  = 0.     
ZMTH2(:,:) = 0.     
ZMR2(:,:)  = 0.     
ZMTHR(:,:) = 0.     

IF (TURBN%CTOM=='TM06') THEN
  CALL TM06(D,CST,PTHVREF,PBL_DEPTH,PZZ,PSFTH,ZMWTH,ZMTH2)

   CALL GZ_M_W_PHY(D,ZMWTH,PDZZ,ZWORK1)    
   CALL GZ_W_M_PHY(D,ZMTH2,PDZZ,ZWORK2)    
   !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
   ZFWTH(IIJB:IIJE,1:IKT) = -ZWORK1(IIJB:IIJE,1:IKT)
   ZFTH2(IIJB:IIJE,1:IKT) = -ZWORK2(IIJB:IIJE,1:IKT)
   !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)

  ZFWTH(:,IKTE:) = 0.
  ZFWTH(:,:IKTB) = 0.
  ZFWR(:,:)  = 0.
  ZFTH2(:,IKTE:) = 0.
  ZFTH2(:,:IKTB) = 0.
  ZFR2(:,:)  = 0.
  ZFTHR(:,:) = 0.
ELSE
  ZFWTH(:,:) = 0.
  ZFWR(:,:)  = 0.
  ZFTH2(:,:) = 0.
  ZFR2(:,:)  = 0.
  ZFTHR(:,:) = 0.
ENDIF






IF( BUCONF%LBUDGET_U )  THEN
  CALL BUDGET_STORE_INIT_PHY(D, TBUDGETS(NBUDGET_U ), 'VTURB', PRUS(:,:)    )
END IF
IF( BUCONF%LBUDGET_V )  THEN
  CALL BUDGET_STORE_INIT_PHY(D, TBUDGETS(NBUDGET_V ), 'VTURB', PRVS(:,:)    )
END IF
IF( BUCONF%LBUDGET_W )  THEN
  CALL BUDGET_STORE_INIT_PHY(D, TBUDGETS(NBUDGET_W ), 'VTURB', PRWS(:,:)    )
END IF

IF( BUCONF%LBUDGET_TH ) THEN
  IF( KRRI >= 1 .AND. KRRL >= 1 ) THEN
    CALL BUDGET_STORE_INIT_PHY(D, TBUDGETS(NBUDGET_TH), 'VTURB', PRTHLS(:,:) + ZLVOCPEXNM(:,:) * PRRS(:,:, 2) &
                                                                          + ZLSOCPEXNM(:,:) * PRRS(:,:, 4) )
  ELSE IF( KRRL >= 1 ) THEN
    CALL BUDGET_STORE_INIT_PHY(D, TBUDGETS(NBUDGET_TH), 'VTURB', PRTHLS(:,:) + ZLOCPEXNM(:,:) * PRRS(:,:, 2) )
  ELSE
    CALL BUDGET_STORE_INIT_PHY(D, TBUDGETS(NBUDGET_TH), 'VTURB', PRTHLS(:,:) )
  END IF
END IF

IF( BUCONF%LBUDGET_RV ) THEN
  IF( KRRI >= 1 .AND. KRRL >= 1 ) THEN
    CALL BUDGET_STORE_INIT_PHY(D, TBUDGETS(NBUDGET_RV), 'VTURB', PRRS(:,:, 1) - PRRS(:,:, 2) - PRRS(:,:, 4) )
  ELSE IF( KRRL >= 1 ) THEN
    CALL BUDGET_STORE_INIT_PHY(D, TBUDGETS(NBUDGET_RV), 'VTURB', PRRS(:,:, 1) - PRRS(:,:, 2) )
  ELSE
    CALL BUDGET_STORE_INIT_PHY(D, TBUDGETS(NBUDGET_RV), 'VTURB', PRRS(:,:, 1) )
  END IF
END IF

IF( BUCONF%LBUDGET_RC ) THEN
  CALL BUDGET_STORE_INIT_PHY(D, TBUDGETS(NBUDGET_RC), 'VTURB', PRRS  (:,:, 2) )
END IF
IF( BUCONF%LBUDGET_RI ) THEN
  CALL BUDGET_STORE_INIT_PHY(D, TBUDGETS(NBUDGET_RI), 'VTURB', PRRS  (:,:, 4) )
END IF

IF( BUCONF%LBUDGET_SV ) THEN
  DO JSV = 1, KSV
    CALL BUDGET_STORE_INIT_PHY(D, TBUDGETS(NBUDGET_SV1 - 1 + JSV), 'VTURB', PRSVS(:,:, JSV) )
  END DO
END IF

CALL TURB_VER(D,CST,CSTURB,TURBN,NEBN,TLES,              &
          KRR,KRRL,KRRI,KGRADIENTS,                      &
          OOCEAN, ODEEPOC, OCOMPUTE_SRC,                 &
          KSV,KSV_LGBEG,KSV_LGEND,                       &
          ZEXPL, O2D, ONOMIXLG, OFLAT,                   &
          OCOUPLES,OBLOWSNOW,OFLYER, PRSNOW,             &
          PTSTEP,TPFILE,                                 &
          PDXX,PDYY,PDZZ,PDZX,PDZY,PDIRCOSZW,PZZ,        &
          PCOSSLOPE,PSINSLOPE,                           &
          PRHODJ,PTHVREF,PSFU,PSFV,                      &
          PSFTH,PSFRV,PSFSV,PSFTH,PSFRV,PSFSV,           &
          ZCDUEFF,ZTAU11M,ZTAU12M,ZTAU33M,               &
          PUT,PVT,PWT,ZUSLOPE,ZVSLOPE,PTHLT,PRT,PSVT,    &
          PTKET,ZLM,PLENGTHM,PLENGTHH,ZLEPS,MFMOIST,     &
          ZLOCPEXNM,ZATHETA,ZAMOIST,PSRCT,ZFRAC_ICE,     &
          ZFWTH,ZFWR,ZFTH2,ZFR2,ZFTHR,PBL_DEPTH,         &
          PSBL_DEPTH,ZLMO,PHGRAD,PZS,                    &
          PRUS,PRVS,PRWS,PRTHLS,PRRS,PRSVS,              &
          PDP,PTP,PSIGS,PWTH,PWRC,PWSV,                  &
          PSSTFL, PSSTFL_C, PSSRFL_C,PSSUFL_C,PSSVFL_C,  &
          PSSUFL,PSSVFL                                  )








IF( BUCONF%LBUDGET_U ) THEN
  CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_U), 'VTURB', PRUS(:,:) )
END IF
IF( BUCONF%LBUDGET_V ) THEN
  CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_V), 'VTURB', PRVS(:,:) )
END IF
IF( BUCONF%LBUDGET_W ) THEN
  CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_W), 'VTURB', PRWS(:,:) )
END IF

IF( BUCONF%LBUDGET_TH ) THEN
  IF( KRRI >= 1 .AND. KRRL >= 1 ) THEN
    CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_TH), 'VTURB', PRTHLS(:,:) + ZLVOCPEXNM(:,:) * PRRS(:,:, 2) &
                                                                          + ZLSOCPEXNM(:,:) * PRRS(:,:, 4) )
  ELSE IF( KRRL >= 1 ) THEN
    CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_TH), 'VTURB', PRTHLS(:,:) + ZLOCPEXNM(:,:) * PRRS(:,:, 2) )
  ELSE
    CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_TH), 'VTURB', PRTHLS(:,:) )
  END IF
END IF

IF( BUCONF%LBUDGET_RV ) THEN
  IF( KRRI >= 1 .AND. KRRL >= 1 ) THEN
    CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_RV), 'VTURB', PRRS(:,:, 1) - PRRS(:,:, 2) - PRRS(:,:, 4) )
  ELSE IF( KRRL >= 1 ) THEN
    CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_RV), 'VTURB', PRRS(:,:, 1) - PRRS(:,:, 2) )
  ELSE
    CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_RV), 'VTURB', PRRS(:,:, 1) )
  END IF
END IF

IF( BUCONF%LBUDGET_RC ) THEN
  CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_RC), 'VTURB', PRRS(:,:, 2) )
END IF
IF( BUCONF%LBUDGET_RI ) THEN
  CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_RI), 'VTURB', PRRS(:,:, 4) )
END IF

IF( BUCONF%LBUDGET_SV )  THEN
  DO JSV = 1, KSV
    CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_SV1 - 1 + JSV), 'VTURB', PRSVS(:,:, JSV) )
  END DO
END IF
    CALL TURB_HOR_SPLT(D,CST,CSTURB, TURBN, NEBN, TLES,        &
          KSPLIT, KRR, KRRL, KRRI, KSV,KSV_LGBEG,KSV_LGEND,    & 
          PTSTEP,HLBCX,HLBCY, OFLAT,O2D, ONOMIXLG,             & 
          OOCEAN,OCOMPUTE_SRC,OBLOWSNOW,PRSNOW,                &
          TPFILE, KHALO,                                       &
          PDXX,PDYY,PDZZ,PDZX,PDZY,PZZ,                        &
          PDIRCOSXW,PDIRCOSYW,PDIRCOSZW,                       &
          PCOSSLOPE,PSINSLOPE,                                 &
          PRHODJ,PTHVREF,                                      &
          PSFTH,PSFRV,PSFSV,                                   &
          ZCDUEFF,ZTAU11M,ZTAU12M,ZTAU22M,ZTAU33M,             &
          PUT,PVT,PWT,ZUSLOPE,ZVSLOPE,PTHLT,PRT,PSVT,          &
          PTKET,ZLM,ZLEPS,                                     &
          ZLOCPEXNM,ZATHETA,ZAMOIST,PSRCT,ZFRAC_ICE,           &
          PDP,PTP,PSIGS,                                       &
          ZTRH,                                                &
          PRUS,PRVS,PRWS,PRTHLS,PRRS,PRSVS                     )
 






  
  IF( BUCONF%LBUDGET_U ) THEN
    CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_U), 'HTURB', PRUS(:,:) )
  END IF
  IF( BUCONF%LBUDGET_V ) THEN
    CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_V), 'HTURB', PRVS(:,:) )
  END IF
  IF( BUCONF%LBUDGET_W ) THEN
    CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_W), 'HTURB', PRWS(:,:) )
  END IF

  IF( BUCONF%LBUDGET_TH ) THEN
    IF( KRRI >= 1 .AND. KRRL >= 1 ) THEN
      CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_TH), 'HTURB', PRTHLS(:,:) + ZLVOCPEXNM(:,:) * PRRS(:,:, 2) &
                                                                            + ZLSOCPEXNM(:,:) * PRRS(:,:, 4) )
    ELSE IF( KRRL >= 1 ) THEN
      CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_TH), 'HTURB', PRTHLS(:,:) + ZLOCPEXNM(:,:) * PRRS(:,:, 2) )
    ELSE
      CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_TH), 'HTURB', PRTHLS(:,:) )
    END IF
  END IF

  IF( BUCONF%LBUDGET_RV ) THEN
    IF( KRRI >= 1 .AND. KRRL >= 1 ) THEN
      CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_RV), 'HTURB', PRRS(:,:, 1) - PRRS(:,:, 2) - PRRS(:,:, 4) )
    ELSE IF( KRRL >= 1 ) THEN
      CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_RV), 'HTURB', PRRS(:,:, 1) - PRRS(:,:, 2) )
    ELSE
      CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_RV), 'HTURB', PRRS(:,:, 1) )
    END IF
  END IF

  IF( BUCONF%LBUDGET_RC ) THEN
    CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_RC), 'HTURB', PRRS(:,:, 2) )
  END IF
  IF( BUCONF%LBUDGET_RI ) THEN
    CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_RI), 'HTURB', PRRS(:,:, 4) )
  END IF

  IF( BUCONF%LBUDGET_SV )  THEN
    DO JSV = 1, KSV
      CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_SV1 - 1 + JSV), 'HTURB', PRSVS(:,:, JSV) )
    END DO
  END IF







CALL MZF_PHY(D,PFLXZTHVMF,ZWORK1)
!$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
PTP(IIJB:IIJE,1:IKT) = PTP(IIJB:IIJE,1:IKT) &
                             + CST%XG / PTHVREF(IIJB:IIJE,1:IKT) * ZWORK1(IIJB:IIJE,1:IKT)
!$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)

IF(PRESENT(PTPMF))  THEN
  !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  PTPMF(IIJB:IIJE,1:IKT)=CST%XG / PTHVREF(IIJB:IIJE,1:IKT) * ZWORK1(IIJB:IIJE,1:IKT)
  !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
END IF


IF (.NOT. TURBN%LHARAT) THEN

IF (BUCONF%LBUDGET_TH)  THEN
  IF ( KRRI >= 1 .AND. KRRL >= 1 ) THEN
    CALL BUDGET_STORE_INIT_PHY(D, TBUDGETS(NBUDGET_TH), 'DISSH', PRTHLS(:,:)+ ZLVOCPEXNM(:,:) * PRRS(:,:,2) &
                                                          & + ZLSOCPEXNM(:,:) * PRRS(:,:,4) )
  ELSE IF ( KRRL >= 1 ) THEN
    CALL BUDGET_STORE_INIT_PHY(D, TBUDGETS(NBUDGET_TH), 'DISSH', PRTHLS(:,:) + ZLOCPEXNM(:,:) * PRRS(:,:,2) )
  ELSE
    CALL BUDGET_STORE_INIT_PHY(D, TBUDGETS(NBUDGET_TH), 'DISSH', PRTHLS(:,:) )
  END IF
END IF

IF(PRESENT(PRTKEMS)) THEN
  ZRTKEMS(:,:)=PRTKEMS(:,:)
ELSE
  ZRTKEMS(:,:)=0.
END IF

CALL TKE_EPS_SOURCES(D,CST,CSTURB,BUCONF,TURBN,TLES,                    &
                   & PTKET,ZLM,ZLEPS,PDP,ZTRH,                          &
                   & PRHODJ,PDZZ,PDXX,PDYY,PDZX,PDZY,PZZ,               &
                   & PTSTEP,ZEXPL,                                      &
                   & TPFILE,ODIAG_IN_RUN,OOCEAN,                        &
                   & PSFU,PSFV,                                         &
                   & PTP,PRTKES,PRTHLS,ZCOEF_DISS,PTDIFF,PTDISS,ZRTKEMS,&
                   & TBUDGETS,KBUDGETS, PEDR=PEDR, PTR=PTR,PDISS=PDISS, &
                   & PCURRENT_TKE_DISS=PCURRENT_TKE_DISS                )
IF (BUCONF%LBUDGET_TH)  THEN
  IF ( KRRI >= 1 .AND. KRRL >= 1 ) THEN
    CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_TH), 'DISSH', PRTHLS(:,:)+ ZLVOCPEXNM(:,:) * PRRS(:,:,2) &
                                                          & + ZLSOCPEXNM(:,:) * PRRS(:,:,4) )
  ELSE IF ( KRRL >= 1 ) THEN
    CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_TH), 'DISSH', PRTHLS(:,:)+ ZLOCPEXNM(:,:) * PRRS(:,:,2) )
  ELSE
    CALL BUDGET_STORE_END_PHY(D, TBUDGETS(NBUDGET_TH), 'DISSH', PRTHLS(:,:) )
  END IF
END IF

ENDIF






IF ( TURBN%LTURB_DIAG .AND. TPFILE%LOPENED ) THEN



  TZFIELD = TFIELDMETADATA(       &
    CMNHNAME   = 'LM',            &
    CSTDNAME   = '',              &
    CLONGNAME  = 'LM',            &
    CUNITS     = 'm',             &
    CDIR       = 'XY',            &
    CCOMMENT   = 'Mixing length', &
    NGRID      = 1,               &
    NTYPE      = TYPEREAL,        &
    NDIMS      = 3,               &
    LTIMEDEP   = .TRUE.           )
  CALL IO_FIELD_WRITE_PHY(D,TPFILE,TZFIELD,ZLM)

  IF (KRR /= 0) THEN



    TZFIELD = TFIELDMETADATA(                          &
    CMNHNAME   = 'THLM',                               &
    CSTDNAME   = '',                                   &
    CLONGNAME  = 'THLM',                               &
    CUNITS     = 'K',                                  &
    CDIR       = 'XY',                                 &
    CCOMMENT   = 'Conservative potential temperature', &
    NGRID      = 1,                                    &
    NTYPE      = TYPEREAL,                             &
    NDIMS      = 3,                                    &
    LTIMEDEP   = .TRUE.                                )
    CALL IO_FIELD_WRITE_PHY(D,TPFILE,TZFIELD,PTHLT)



    TZFIELD = TFIELDMETADATA(                &
    CMNHNAME   = 'RNPM',                     &
    CSTDNAME   = '',                         &
    CLONGNAME  = 'RNPM',                     &
    CUNITS     = 'kg kg-1',                  &
    CDIR       = 'XY',                       &
    CCOMMENT   = 'Conservative mixing ratio',&
    NGRID      = 1,                          &
    NTYPE      = TYPEREAL,                   &
    NDIMS      = 3,                          &
    LTIMEDEP   = .TRUE.                      )
    CALL IO_FIELD_WRITE_PHY(D,TPFILE,TZFIELD,PRT(:,:,1))
   END IF
END IF


IF(PRESENT(PDRUS_TURB)) THEN
!$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  PDRUS_TURB(IIJB:IIJE,1:IKT)   = PRUS(IIJB:IIJE,1:IKT) - PDRUS_TURB(IIJB:IIJE,1:IKT)
  PDRVS_TURB(IIJB:IIJE,1:IKT)   = PRVS(IIJB:IIJE,1:IKT) - PDRVS_TURB(IIJB:IIJE,1:IKT)
  PDRTHLS_TURB(IIJB:IIJE,1:IKT) = PRTHLS(IIJB:IIJE,1:IKT) - PDRTHLS_TURB(IIJB:IIJE,1:IKT)
  PDRRTS_TURB(IIJB:IIJE,1:IKT)  = PRRS(IIJB:IIJE,1:IKT,1) - PDRRTS_TURB(IIJB:IIJE,1:IKT)
  !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT,JSV=1:KSV)  
  PDRSVS_TURB(IIJB:IIJE,1:IKT,:)  = PRSVS(IIJB:IIJE,1:IKT,:) - PDRSVS_TURB(IIJB:IIJE,1:IKT,:)
  !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT,JSV=1:KSV)
END IF





IF ( KRRL >= 1 ) THEN
  IF ( KRRI >= 1 ) THEN
    !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
    PRT(IIJB:IIJE,1:IKT,1)  = PRT(IIJB:IIJE,1:IKT,1)  - PRT(IIJB:IIJE,1:IKT,2)  &
                                    - PRT(IIJB:IIJE,1:IKT,4)
    PRRS(IIJB:IIJE,1:IKT,1) = PRRS(IIJB:IIJE,1:IKT,1) - PRRS(IIJB:IIJE,1:IKT,2) &
                                    - PRRS(IIJB:IIJE,1:IKT,4)
    PTHLT(IIJB:IIJE,1:IKT)  = PTHLT(IIJB:IIJE,1:IKT)  + ZLVOCPEXNM(IIJB:IIJE,1:IKT) &
                                    * PRT(IIJB:IIJE,1:IKT,2) &
                                    + ZLSOCPEXNM(IIJB:IIJE,1:IKT) * PRT(IIJB:IIJE,1:IKT,4)
    PRTHLS(IIJB:IIJE,1:IKT) = PRTHLS(IIJB:IIJE,1:IKT) + ZLVOCPEXNM(IIJB:IIJE,1:IKT) &
                                    * PRRS(IIJB:IIJE,1:IKT,2) &
                                    + ZLSOCPEXNM(IIJB:IIJE,1:IKT) * PRRS(IIJB:IIJE,1:IKT,4)
    !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)

  ELSE
    !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
    PRT(IIJB:IIJE,1:IKT,1)  = PRT(IIJB:IIJE,1:IKT,1)  - PRT(IIJB:IIJE,1:IKT,2)
    PRRS(IIJB:IIJE,1:IKT,1) = PRRS(IIJB:IIJE,1:IKT,1) - PRRS(IIJB:IIJE,1:IKT,2)
    PTHLT(IIJB:IIJE,1:IKT)  = PTHLT(IIJB:IIJE,1:IKT)  + ZLOCPEXNM(IIJB:IIJE,1:IKT) &
                                    * PRT(IIJB:IIJE,1:IKT,2)
    PRTHLS(IIJB:IIJE,1:IKT) = PRTHLS(IIJB:IIJE,1:IKT) + ZLOCPEXNM(IIJB:IIJE,1:IKT) &
                                    * PRRS(IIJB:IIJE,1:IKT,2)
    !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  END IF
END IF


CALL SOURCES_NEG_CORRECT_PHY(D,KSV,HCLOUD, 'NETUR',KRR,PTSTEP,PPABST,PTHLT,PRT,PRTHLS,PRRS,PRSVS)





IF (TLES%LLES_CALL) THEN
  CALL SECOND_MNH(ZTIME1)
  CALL LES_MEAN_SUBGRID_PHY(D,TLES,PSFTH,TLES%X_LES_Q0)
  CALL LES_MEAN_SUBGRID_PHY(D,TLES,PSFRV,TLES%X_LES_E0)
  DO JSV=1,KSV
    CALL LES_MEAN_SUBGRID_PHY(D,TLES,PSFSV(:,JSV),TLES%X_LES_SV0(:,JSV))
  END DO
  CALL LES_MEAN_SUBGRID_PHY(D,TLES,PSFU,TLES%X_LES_UW0)
  CALL LES_MEAN_SUBGRID_PHY(D,TLES,PSFV,TLES%X_LES_VW0)
  
  !$mnh_expand_array(JIJ=IIJB:IIJE)
  ZWORK2D(IIJB:IIJE) = (PSFU(IIJB:IIJE)*PSFU(IIJB:IIJE)+PSFV(IIJB:IIJE)*PSFV(IIJB:IIJE))**0.25
  !$mnh_end_expand_array(JIJ=IIJB:IIJE)
  CALL LES_MEAN_SUBGRID_PHY(D,TLES,ZWORK2D,TLES%X_LES_USTAR)





  CALL LES_MEAN_SUBGRID_PHY(D,TLES,ZMWTH,TLES%X_LES_SUBGRID_W2Thl)
  CALL LES_MEAN_SUBGRID_PHY(D,TLES,ZMTH2,TLES%X_LES_SUBGRID_WThl2)
  IF (KRR>0) THEN
    CALL LES_MEAN_SUBGRID_PHY(D,TLES,ZMWR,TLES%X_LES_SUBGRID_W2Rt)
    CALL LES_MEAN_SUBGRID_PHY(D,TLES,ZMTHR,TLES%X_LES_SUBGRID_WThlRt)
    CALL LES_MEAN_SUBGRID_PHY(D,TLES,ZMR2,TLES%X_LES_SUBGRID_WRt2)
  END IF






  IF (TURBN%CTURBDIM=="1DIM") THEN
    
    !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
    ZWORK1(IIJB:IIJE,1:IKT) = 2./3.*PTKET(IIJB:IIJE,1:IKT)
    !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
    CALL LES_MEAN_SUBGRID_PHY(D,TLES,ZWORK1,TLES%X_LES_SUBGRID_U2)
    TLES%X_LES_SUBGRID_V2(:,:,:) = TLES%X_LES_SUBGRID_U2(:,:,:)
    TLES%X_LES_SUBGRID_W2(:,:,:) = TLES%X_LES_SUBGRID_U2(:,:,:)
    
    CALL GZ_M_W_PHY(D,PTHLT,PDZZ,ZWORK1)
    CALL MZF_PHY(D,ZWORK1,ZWORK2)
    !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
    ZWORK2(IIJB:IIJE,1:IKT)  = 2./3.*PTKET(IIJB:IIJE,1:IKT) *ZWORK2(IIJB:IIJE,1:IKT)
    !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
    CALL LES_MEAN_SUBGRID_PHY(D,TLES,ZWORK2,TLES%X_LES_RES_ddz_Thl_SBG_W2)
    
    IF (KRR>=1) THEN
      CALL GZ_M_W_PHY(D,PRT(:,:,1),PDZZ,ZWORK1)
      CALL MZF_PHY(D,ZWORK1,ZWORK2)
      !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
      ZWORK2(IIJB:IIJE,1:IKT)  = 2./3.*PTKET(IIJB:IIJE,1:IKT) *ZWORK2(IIJB:IIJE,1:IKT)
      !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
      CALL LES_MEAN_SUBGRID_PHY(D,TLES,ZWORK2,TLES%X_LES_RES_ddz_Rt_SBG_W2)
    END IF
    DO JSV=1,KSV
      CALL GZ_M_W_PHY(D,PSVT(:,:,JSV),PDZZ,ZWORK1)
      CALL MZF_PHY(D,ZWORK1,ZWORK2)
      !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
      ZWORK2(IIJB:IIJE,1:IKT)  = 2./3.*PTKET(IIJB:IIJE,1:IKT) *ZWORK2(IIJB:IIJE,1:IKT)
      !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
      CALL LES_MEAN_SUBGRID_PHY(D,TLES,ZWORK2, TLES%X_LES_RES_ddz_Sv_SBG_W2(:,:,:,JSV))
    END DO
  END IF






  CALL LES_MEAN_SUBGRID_PHY(D,TLES,ZLM,TLES%X_LES_SUBGRID_LMix)
  CALL LES_MEAN_SUBGRID_PHY(D,TLES,ZLEPS,TLES%X_LES_SUBGRID_LDiss)



  ZLEPS(:,:) = 0. 
  CALL LES_MEAN_SUBGRID_PHY(D,TLES,ZLEPS,TLES%X_LES_SUBGRID_WP)

  CALL SECOND_MNH(ZTIME2)
  TLES%XTIME_LES = TLES%XTIME_LES + ZTIME2 - ZTIME1
END IF

IF(PRESENT(PLEM)) THEN
  PLEM(IIJB:IIJE,IKTB:IKTE) = ZLM(IIJB:IIJE,IKTB:IKTE)
END IF


IF (LHOOK) THEN
  CALL DR_HOOK('TURB',1,ZHOOK_HANDLE)
END IF
CONTAINS


      SUBROUTINE COMPUTE_FUNCTION_THERMO(PALP,PBETA,PGAM,PLTT,PC,PT,PEXN,PCP,&
                                         PLOCPEXN,PAMOIST,PATHETA            )


















IMPLICIT NONE



REAL,                   INTENT(IN)    :: PALP,PBETA,PGAM,PLTT,PC
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)    :: PT,PEXN,PCP

REAL, DIMENSION(D%NIJT,D%NKT), INTENT(OUT)   :: PLOCPEXN
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(OUT)   :: PAMOIST,PATHETA



  IF (LHOOK) THEN
    CALL DR_HOOK('TURB:COMPUTE_FUNCTION_THERMO',0,ZHOOK_HANDLE2)
  END IF
  ZEPS = CST%XMV / CST%XMD



  !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  PLOCPEXN(IIJB:IIJE,1:IKT) = ( PLTT + (CST%XCPV-PC) *  (PT(IIJB:IIJE,1:IKT)-CST%XTT) ) &
                                     / PCP(IIJB:IIJE,1:IKT)



  ZRVSAT(IIJB:IIJE,1:IKT) =  EXP( PALP - PBETA/PT(IIJB:IIJE,1:IKT) - PGAM*ALOG( PT(IIJB:IIJE,1:IKT) ) )



  ZRVSAT(IIJB:IIJE,1:IKT) =  ZRVSAT(IIJB:IIJE,1:IKT) &
                                    * ZEPS / ( PPABST(IIJB:IIJE,1:IKT) - ZRVSAT(IIJB:IIJE,1:IKT) )



  ZDRVSATDT(IIJB:IIJE,1:IKT) = ( PBETA / PT(IIJB:IIJE,1:IKT)  - PGAM ) / PT(IIJB:IIJE,1:IKT)   &
                 * ZRVSAT(IIJB:IIJE,1:IKT) * ( 1. + ZRVSAT(IIJB:IIJE,1:IKT) / ZEPS )



  PAMOIST(IIJB:IIJE,1:IKT)=  0.5 / ( 1.0 + ZDRVSATDT(IIJB:IIJE,1:IKT) * PLOCPEXN(IIJB:IIJE,1:IKT) )



  PATHETA(IIJB:IIJE,1:IKT)= PAMOIST(IIJB:IIJE,1:IKT) * PEXN(IIJB:IIJE,1:IKT) *               &
        ( ( ZRVSAT(IIJB:IIJE,1:IKT) - PRT(IIJB:IIJE,1:IKT,1) ) * PLOCPEXN(IIJB:IIJE,1:IKT) / &
          ( 1. + ZDRVSATDT(IIJB:IIJE,1:IKT) * PLOCPEXN(IIJB:IIJE,1:IKT) )        *               &
          (                                                                  &
           ZRVSAT(IIJB:IIJE,1:IKT) * (1. + ZRVSAT(IIJB:IIJE,1:IKT)/ZEPS)                         &
                        * ( -2.*PBETA/PT(IIJB:IIJE,1:IKT) + PGAM ) / PT(IIJB:IIJE,1:IKT)**2      &
          +ZDRVSATDT(IIJB:IIJE,1:IKT) * (1. + 2. * ZRVSAT(IIJB:IIJE,1:IKT)/ZEPS)                 &
                        * ( PBETA/PT(IIJB:IIJE,1:IKT) - PGAM ) / PT(IIJB:IIJE,1:IKT)             &
          )                                                                  &
         - ZDRVSATDT(IIJB:IIJE,1:IKT)                                                  &
        )



  PLOCPEXN(IIJB:IIJE,1:IKT) = PLOCPEXN(IIJB:IIJE,1:IKT) / PEXN(IIJB:IIJE,1:IKT)
  !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)

IF (LHOOK) THEN
  CALL DR_HOOK('TURB:COMPUTE_FUNCTION_THERMO',1,ZHOOK_HANDLE2)
END IF
END SUBROUTINE COMPUTE_FUNCTION_THERMO


      SUBROUTINE COMPUTE_FUNCTION_THERMO_NEW_STAT(PALP,PBETA,PGAM,PLTT,PC,PT,PEXN,PCP,&
                                         PLOCPEXN,PAMOIST,PATHETA            )


















USE MODD_CST

IMPLICIT NONE



REAL, INTENT(IN)                      :: PALP,PBETA,PGAM,PLTT,PC
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)    :: PT,PEXN,PCP

REAL, DIMENSION(D%NIJT,D%NKT), INTENT(OUT)   :: PLOCPEXN
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(OUT)   :: PAMOIST,PATHETA



  IF (LHOOK) THEN
    CALL DR_HOOK('TURB:COMPUTE_FUNCTION_THERMO_NEW_STAT',0,ZHOOK_HANDLE2)
  END IF
  ZEPS = CST%XMV / CST%XMD



  !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  PLOCPEXN(IIJB:IIJE,1:IKT) = ( PLTT + (CST%XCPV-PC) *  (PT(IIJB:IIJE,1:IKT)-CST%XTT) ) / PCP(IIJB:IIJE,1:IKT)



  ZRVSAT(IIJB:IIJE,1:IKT) =  EXP( PALP - PBETA/PT(IIJB:IIJE,1:IKT) - PGAM*ALOG( PT(IIJB:IIJE,1:IKT) ) )



  ZRVSAT(IIJB:IIJE,1:IKT) =  ZRVSAT(IIJB:IIJE,1:IKT) * ZEPS / ( PPABST(IIJB:IIJE,1:IKT) - ZRVSAT(IIJB:IIJE,1:IKT) )



  ZDRVSATDT(IIJB:IIJE,1:IKT) = ( PBETA / PT(IIJB:IIJE,1:IKT)  - PGAM ) / PT(IIJB:IIJE,1:IKT)   &
                 * ZRVSAT(IIJB:IIJE,1:IKT) * ( 1. + ZRVSAT(IIJB:IIJE,1:IKT) / ZEPS )



  PAMOIST(IIJB:IIJE,1:IKT)=  1.0 / ( 1.0 + ZDRVSATDT(IIJB:IIJE,1:IKT) * PLOCPEXN(IIJB:IIJE,1:IKT) )



  PATHETA(IIJB:IIJE,1:IKT)= PAMOIST(IIJB:IIJE,1:IKT) * PEXN(IIJB:IIJE,1:IKT) * ZDRVSATDT(IIJB:IIJE,1:IKT)



  PLOCPEXN(IIJB:IIJE,1:IKT) = PLOCPEXN(IIJB:IIJE,1:IKT) / PEXN(IIJB:IIJE,1:IKT)
  !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)

IF (LHOOK) THEN
  CALL DR_HOOK('TURB:COMPUTE_FUNCTION_THERMO_NEW_STAT',1,ZHOOK_HANDLE2)
END IF
END SUBROUTINE COMPUTE_FUNCTION_THERMO_NEW_STAT



      SUBROUTINE DELT(PLM,ODZ)




















REAL, DIMENSION(D%NIJT,D%NKT), INTENT(OUT)   :: PLM
LOGICAL,                INTENT(IN)    :: ODZ


IF (LHOOK) THEN
  CALL DR_HOOK('TURB:DELT',0,ZHOOK_HANDLE2)
END IF

CALL MXF_PHY(D,PDXX,ZWORK1)
IF (.NOT. O2D) THEN
  CALL MYF_PHY(D,PDYY,ZWORK2)
END IF

IF (ODZ) THEN
  
  DO JK = IKTB,IKTE 
    !$mnh_expand_array(JIJ=IIJB:IIJE)
    PLM(IIJB:IIJE,JK) = PZZ(IIJB:IIJE,JK+IKL) - PZZ(IIJB:IIJE,JK)
    !$mnh_end_expand_array(JIJ=IIJB:IIJE)
  END DO
  !$mnh_expand_array(JIJ=IIJB:IIJE)
  PLM(IIJB:IIJE,IKU) = PLM(IIJB:IIJE,IKE)
  PLM(IIJB:IIJE,IKA) = PZZ(IIJB:IIJE,IKB) - PZZ(IIJB:IIJE,IKA)
  !$mnh_end_expand_array(JIJ=IIJB:IIJE)
  IF ( TURBN%CTURBDIM /= '1DIM' ) THEN  
    IF ( O2D) THEN
      !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
      PLM(IIJB:IIJE,1:IKT) = SQRT( PLM(IIJB:IIJE,1:IKT)*ZWORK1(IIJB:IIJE,1:IKT) )
      !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
    ELSE
      !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
      PLM(IIJB:IIJE,1:IKT) = (PLM(IIJB:IIJE,1:IKT)*ZWORK1(IIJB:IIJE,1:IKT) &
                                   * ZWORK2(IIJB:IIJE,1:IKT) ) ** (1./3.)
      !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
    END IF
  END IF
ELSE
  
  !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  PLM(IIJB:IIJE,1:IKT)=1.E10
  !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  IF ( TURBN%CTURBDIM /= '1DIM' ) THEN  
    IF ( O2D) THEN
      PLM(:,:) = ZWORK1(:,:)
    ELSE
      !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
      PLM(IIJB:IIJE,1:IKT) = (ZWORK1(IIJB:IIJE,1:IKT)*ZWORK2(IIJB:IIJE,1:IKT) ) ** (1./2.)
      !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
    END IF
  END IF
END IF




IF (.NOT. TURBN%LRMC01) THEN
  ZALPHA=0.5**(-1.5)
  
  DO JIJ=IIJB,IIJE
    IF (OOCEAN) THEN
      DO JK=IKTE,IKTB,-1
        ZD=ZALPHA*(PZZ(JIJ,IKTE+1)-PZZ(JIJ,JK))
        IF ( PLM(JIJ,JK)>ZD) THEN
          PLM(JIJ,JK)=ZD
        ELSE
          EXIT
        ENDIF
      END DO
    ELSE
      DO JK=IKTB,IKTE
        ZD=ZALPHA*(0.5*(PZZ(JIJ,JK)+PZZ(JIJ,JK+IKL))&
        -PZZ(JIJ,IKB)) *PDIRCOSZW(JIJ)
        IF ( PLM(JIJ,JK)>ZD) THEN
          PLM(JIJ,JK)=ZD
        ELSE
          EXIT
        ENDIF
      END DO
    ENDIF
  END DO
END IF

!$mnh_expand_array(JIJ=IIJB:IIJE)
PLM(IIJB:IIJE,IKA) = PLM(IIJB:IIJE,IKB)
PLM(IIJB:IIJE,IKU) = PLM(IIJB:IIJE,IKE)
!$mnh_end_expand_array(JIJ=IIJB:IIJE)

IF (LHOOK) THEN
  CALL DR_HOOK('TURB:DELT',1,ZHOOK_HANDLE2)
END IF
END SUBROUTINE DELT


      SUBROUTINE DEAR(PLM)






















REAL, DIMENSION(D%NIJT,D%NKT), INTENT(OUT)   :: PLM




IF (LHOOK) THEN
  CALL DR_HOOK('TURB:DEAR',0,ZHOOK_HANDLE2)
END IF
IF ( TURBN%CTURBDIM /= '1DIM' ) THEN
  CALL MXF_PHY(D,PDXX,ZWORK1)
  IF (.NOT. O2D) THEN
    CALL MYF_PHY(D,PDYY,ZWORK2)
  END IF
END IF

!$mnh_expand_array(JIJ=IIJB:IIJE,JK=IKTB:IKTE)
PLM(IIJB:IIJE,IKTB:IKTE) = PZZ(IIJB:IIJE,IKTB+IKL:IKTE+IKL) - PZZ(IIJB:IIJE,IKTB:IKTE)
!$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=IKTB:IKTE)
!$mnh_expand_array(JIJ=IIJB:IIJE)
PLM(IIJB:IIJE,IKU) = PLM(IIJB:IIJE,IKE)
PLM(IIJB:IIJE,IKA) = PZZ(IIJB:IIJE,IKB) - PZZ(IIJB:IIJE,IKA)
!$mnh_end_expand_array(JIJ=IIJB:IIJE)

IF ( TURBN%CTURBDIM /= '1DIM' ) THEN  
  IF ( O2D) THEN
    !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
    PLM(IIJB:IIJE,1:IKT) = SQRT( PLM(IIJB:IIJE,1:IKT)*ZWORK1(IIJB:IIJE,1:IKT) )
    !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  ELSE
    !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
    PLM(IIJB:IIJE,1:IKT) = (PLM(IIJB:IIJE,1:IKT)*ZWORK1(IIJB:IIJE,1:IKT) &
                                 * ZWORK2(IIJB:IIJE,1:IKT) ) ** (1./3.)
    !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  END IF
END IF


CALL ETHETA(D,CST,KRR,KRRI,PTHLT,PRT,ZLOCPEXNM,ZATHETA,PSRCT,OOCEAN,OCOMPUTE_SRC,ZETHETA)
CALL EMOIST(D,CST,KRR,KRRI,PTHLT,PRT,ZLOCPEXNM,ZAMOIST,PSRCT,OOCEAN,ZEMOIST)

IF (KRR>0) THEN
  DO JK = IKTB+1,IKTE-1
    DO JIJ=IIJB,IIJE
      ZDTHLDZ(JIJ,JK)= 0.5*((PTHLT(JIJ,JK+IKL)-PTHLT(JIJ,JK    ))/PDZZ(JIJ,JK+IKL)+ &
                              (PTHLT(JIJ,JK    )-PTHLT(JIJ,JK-IKL))/PDZZ(JIJ,JK    ))
      ZDRTDZ(JIJ,JK) = 0.5*((PRT(JIJ,JK+IKL,1)-PRT(JIJ,JK    ,1))/PDZZ(JIJ,JK+IKL)+ &
                              (PRT(JIJ,JK    ,1)-PRT(JIJ,JK-IKL,1))/PDZZ(JIJ,JK    ))
      IF (OOCEAN) THEN
        ZVAR=CST%XG*(CST%XALPHAOC*ZDTHLDZ(JIJ,JK)-CST%XBETAOC*ZDRTDZ(JIJ,JK))
      ELSE
        ZVAR=CST%XG/PTHVREF(JIJ,JK)*                                                  &
           (ZETHETA(JIJ,JK)*ZDTHLDZ(JIJ,JK)+ZEMOIST(JIJ,JK)*ZDRTDZ(JIJ,JK))
      END IF
      
      IF (ZVAR>0.) THEN
        PLM(JIJ,JK)=MAX(CST%XMNH_EPSILON,MIN(PLM(JIJ,JK), &
                      0.76* SQRT(PTKET(JIJ,JK)/ZVAR)))
      END IF
    END DO
  END DO
ELSE
  DO JK = IKTB+1,IKTE-1
    DO JIJ=IIJB,IIJE
      ZDTHLDZ(JIJ,JK)= 0.5*((PTHLT(JIJ,JK+IKL)-PTHLT(JIJ,JK    ))/PDZZ(JIJ,JK+IKL)+ &
                              (PTHLT(JIJ,JK    )-PTHLT(JIJ,JK-IKL))/PDZZ(JIJ,JK    ))
      IF (OOCEAN) THEN
        ZVAR= CST%XG*CST%XALPHAOC*ZDTHLDZ(JIJ,JK)
      ELSE
        ZVAR= CST%XG/PTHVREF(JIJ,JK)*ZETHETA(JIJ,JK)*ZDTHLDZ(JIJ,JK)
      END IF

      IF (ZVAR>0.) THEN
        PLM(JIJ,JK)=MAX(CST%XMNH_EPSILON,MIN(PLM(JIJ,JK), &
                      0.76* SQRT(PTKET(JIJ,JK)/ZVAR)))
      END IF
    END DO
  END DO
END IF

!$mnh_expand_array(JIJ=IIJB:IIJE)
ZDTHLDZ(IIJB:IIJE,IKB)=(PTHLT(IIJB:IIJE,IKB+IKL)-PTHLT(IIJB:IIJE,IKB))/PDZZ(IIJB:IIJE,IKB+IKL)
!$mnh_end_expand_array(JIJ=IIJB:IIJE)

IF (KRR>0) THEN
  !$mnh_expand_array(JIJ=IIJB:IIJE)
  ZDRTDZ(IIJB:IIJE,IKB)=(PRT(IIJB:IIJE,IKB+IKL,1)-PRT(IIJB:IIJE,IKB,1))/PDZZ(IIJB:IIJE,IKB+IKL)
  !$mnh_end_expand_array(JIJ=IIJB:IIJE)
ELSE
  ZDRTDZ(:,IKB)=0
ENDIF

IF (OOCEAN) THEN
  !$mnh_expand_array(JIJ=IIJB:IIJE)
  ZWORK2D(IIJB:IIJE)=CST%XG*(CST%XALPHAOC*ZDTHLDZ(IIJB:IIJE,IKB)-CST%XBETAOC*ZDRTDZ(IIJB:IIJE,IKB))
  !$mnh_end_expand_array(JIJ=IIJB:IIJE)
ELSE
  !$mnh_expand_array(JIJ=IIJB:IIJE)
  ZWORK2D(IIJB:IIJE)=CST%XG/PTHVREF(IIJB:IIJE,IKB)*                                           &
              (ZETHETA(IIJB:IIJE,IKB)*ZDTHLDZ(IIJB:IIJE,IKB)+ZEMOIST(IIJB:IIJE,IKB)*ZDRTDZ(IIJB:IIJE,IKB))
  !$mnh_end_expand_array(JIJ=IIJB:IIJE)
END IF
!$mnh_expand_where(JIJ=IIJB:IIJE)
WHERE(ZWORK2D(IIJB:IIJE)>0.)
  PLM(IIJB:IIJE,IKB)=MAX(CST%XMNH_EPSILON,MIN( PLM(IIJB:IIJE,IKB),                 &
                    0.76* SQRT(PTKET(IIJB:IIJE,IKB)/ZWORK2D(IIJB:IIJE))))
END WHERE
!$mnh_end_expand_where(JIJ=IIJB:IIJE)



IF (.NOT. TURBN%LRMC01) THEN
  ZALPHA=0.5**(-1.5)
  
  DO JIJ=IIJB,IIJE
    IF (OOCEAN) THEN
      DO JK=IKTE,IKTB,-1
        ZD=ZALPHA*(PZZ(JIJ,IKTE+1)-PZZ(JIJ,JK))
        IF ( PLM(JIJ,JK)>ZD) THEN
          PLM(JIJ,JK)=ZD
        ELSE
          EXIT
        ENDIF
      END DO
    ELSE
      DO JK=IKTB,IKTE
        ZD=ZALPHA*(0.5*(PZZ(JIJ,JK)+PZZ(JIJ,JK+IKL))-PZZ(JIJ,IKB)) &
          *PDIRCOSZW(JIJ)
        IF ( PLM(JIJ,JK)>ZD) THEN
          PLM(JIJ,JK)=ZD
        ELSE
          EXIT
        ENDIF
      END DO
    ENDIF
  END DO
END IF

!$mnh_expand_array(JIJ=IIJB:IIJE)
PLM(IIJB:IIJE,IKA) = PLM(IIJB:IIJE,IKB)
PLM(IIJB:IIJE,IKE) = PLM(IIJB:IIJE,IKE-IKL)
PLM(IIJB:IIJE,IKU) = PLM(IIJB:IIJE,IKU-IKL)
!$mnh_end_expand_array(JIJ=IIJB:IIJE)

IF (LHOOK) THEN
  CALL DR_HOOK('TURB:DEAR',1,ZHOOK_HANDLE2)
END IF
END SUBROUTINE DEAR


      SUBROUTINE CLOUD_MODIF_LM















































IMPLICIT NONE






IF (LHOOK) THEN
  CALL DR_HOOK('TURB:CLOUD_MODIF_LM',0,ZHOOK_HANDLE2)
END IF
ZPENTE = ( PCOEF_AMPL_SAT - 1. ) / ( PCEI_MAX - PCEI_MIN )
ZCOEF_AMPL_CEI_NUL = 1. - ZPENTE * PCEI_MIN

!$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
ZCOEF_AMPL(IIJB:IIJE,1:IKT) = 1.
!$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)






!$mnh_expand_where(JIJ=IIJB:IIJE,JK=1:IKT)
WHERE ( PCEI(IIJB:IIJE,1:IKT)>=PCEI_MAX ) 
  ZCOEF_AMPL(IIJB:IIJE,1:IKT)=PCOEF_AMPL_SAT
END WHERE
!$mnh_end_expand_where(JIJ=IIJB:IIJE,JK=1:IKT)




!$mnh_expand_where(JIJ=IIJB:IIJE,JK=1:IKT)
WHERE ( PCEI(IIJB:IIJE,1:IKT) <  PCEI_MAX .AND. PCEI(IIJB:IIJE,1:IKT) >  PCEI_MIN)
  ZCOEF_AMPL(IIJB:IIJE,1:IKT) = ZPENTE * PCEI(IIJB:IIJE,1:IKT) + ZCOEF_AMPL_CEI_NUL
END WHERE
!$mnh_end_expand_where(JIJ=IIJB:IIJE,JK=1:IKT)





IF (HTURBLEN_CL == TURBN%CTURBLEN) THEN
  ZLM_CLOUD(:,:) = ZLM(:,:)
ELSE
  SELECT CASE (HTURBLEN_CL)



  CASE ('BL89','RM17','HM21')
    ZSHEAR(:,:)=0.
    CALL BL89(D,CST,CSTURB,TURBN,PZZ,PDZZ,PTHVREF,ZTHLM,KRR,ZRM,PTKET,ZSHEAR,ZLM_CLOUD,OOCEAN)



  CASE ('DELT')
    CALL DELT(ZLM_CLOUD,ODZ=.TRUE.)



  CASE ('DEAR')
    CALL DEAR(ZLM_CLOUD)

  END SELECT
ENDIF





IF ( TURBN%LTURB_DIAG .AND. TPFILE%LOPENED ) THEN
  TZFIELD = TFIELDMETADATA(            &
    CMNHNAME   = 'LM_CLEAR_SKY',       &
    CSTDNAME   = '',                   &
    CLONGNAME  = 'LM_CLEAR_SKY',       &
    CUNITS     = 'm',                  &
    CDIR       = 'XY',                 &
    CCOMMENT   = 'X_Y_Z_LM CLEAR SKY', &
    NGRID      = 1,                    &
    NTYPE      = TYPEREAL,             &
    NDIMS      = 3,                    &
    LTIMEDEP   = .TRUE.                )
  CALL IO_FIELD_WRITE_PHY(D,TPFILE,TZFIELD,ZLM)
ENDIF



!$mnh_expand_where(JIJ=IIJB:IIJE,JK=1:IKT)
WHERE (ZCOEF_AMPL(IIJB:IIJE,1:IKT) /= 1.) 
  ZLM(IIJB:IIJE,1:IKT) = ZCOEF_AMPL(IIJB:IIJE,1:IKT)*ZLM_CLOUD(IIJB:IIJE,1:IKT)
END WHERE
!$mnh_end_expand_where(JIJ=IIJB:IIJE,JK=1:IKT)



!$mnh_expand_where(JIJ=IIJB:IIJE,JK=1:IKT)
WHERE (PCEI(IIJB:IIJE,1:IKT) == -1.)
  ZLM(IIJB:IIJE,1:IKT) = ZLM_CLOUD(IIJB:IIJE,1:IKT)
END WHERE
!$mnh_end_expand_where(JIJ=IIJB:IIJE,JK=1:IKT)





IF ( TURBN%LTURB_DIAG .AND. TPFILE%LOPENED ) THEN
  TZFIELD = TFIELDMETADATA(         &
    CMNHNAME   = 'COEF_AMPL',       &
    CSTDNAME   = '',                &
    CLONGNAME  = 'COEF_AMPL',       &
    CUNITS     = '1',               &
    CDIR       = 'XY',              &
    CCOMMENT   = 'X_Y_Z_COEF AMPL', &
    NGRID      = 1,                 &
    NTYPE      = TYPEREAL,          &
    NDIMS      = 3,                 &
    LTIMEDEP   = .TRUE.             )
  CALL IO_FIELD_WRITE_PHY(D,TPFILE,TZFIELD,ZCOEF_AMPL)
  
  TZFIELD = TFIELDMETADATA(        &
    CMNHNAME   = 'LM_CLOUD',       &
    CSTDNAME   = '',               &
    CLONGNAME  = 'LM_CLOUD',       &
    CUNITS     = 'm',              &
    CDIR       = 'XY',             &
    CCOMMENT   = 'X_Y_Z_LM CLOUD', &
    NGRID      = 1,                &
    NTYPE      = TYPEREAL,         &
    NDIMS      = 3,                &
    LTIMEDEP   = .TRUE.            )
  CALL IO_FIELD_WRITE_PHY(D,TPFILE,TZFIELD,ZLM_CLOUD)
  
ENDIF

IF (LHOOK) THEN
  CALL DR_HOOK('TURB:CLOUD_MODIF_LM',1,ZHOOK_HANDLE2)
END IF
END SUBROUTINE CLOUD_MODIF_LM

END SUBROUTINE TURB4
