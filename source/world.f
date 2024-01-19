!include 'Incl_WEPSXML.f'
! $Header: M:/default/source/RCS/world.f,v 1.250 2020/08/13 18:35:47 AGE Exp $
      SUBROUTINE WORLD
      IMPLICIT NONE
      include 'parametr'
      include 'ncntrl'
      include 'ngtdmrep'
      include 'intllocl'

      !include 'intl.txt'
       include 'intout'


      LOGICAL NEW /.TRUE./
      CHARACTER*18 FNAME
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR

      !INTEGER IYEARS


      INTEGER*4      RTOVALUE
      EXTERNAL        RTOVALUE
      INTEGER*4      IPMM

integer aa
integer bb
real cc



      IPMM=RTOVALUE('IPMM    ',0)                ! IS IPMM ON (1) OR OFF (0)

      !Main Routine For International Energy Module

        IF ( CURIYR .EQ. FIRSYR .AND. CURITR .EQ. 1) THEN
            FNAME='INTLDBG'
            DBGUNIT=FILE_MGR('O',FNAME,NEW)
        ENDIF

        !IF (CURIYR .EQ. FIRSYR) CALL OMS_Dat_In 


        IF ( CURIYR .EQ. FIRSYR .AND. CURITR .EQ. 1) THEN
            !CALL World_Data_In
            CALL LFMM_World_Data_In
            CALL OMS_Dat_In
        ENDIF
        !******************************

        !CALL OMS_Sim 
        !CALL Crd_Sup_Crv
        !CALL Prd_Sup_Crv 
        !CALL Sup_Crv_Adj
 !IF (CURIYR .EQ. LASTYR) CALL World_Oil_Report UNCOMMENT IT WHEN IT'S DONE  

        IF (CURCALYR .GE. FFY .AND. IPMM .EQ. 1) THEN
             !CALL World_Compute_New
             CALL World_LFMM_Compute_New
        END IF

        IF (CURIYR .EQ. LASTYR .AND. FCRL .EQ. 1) THEN
             CALL World_Oil_Report_LFMM
        END IF

        !****************************


        IF (CURIYR .EQ. LASTYR .AND. FCRL .EQ. 1) THEN
            FNAME='INTLDBG'
            DBGUNIT=FILE_MGR('C',FNAME,NEW)
        ENDIF

      RETURN
      END

      SUBROUTINE LFMM_World_Data_In


      include 'parametr'
      include 'ncntrl'
      include 'pmmrpt'
      include 'intout'
      include 'mxpblk'
      include 'ngtdmrep'
      include 'macout'
      include 'pmmout'
      include 'intllocl'
      include 'ogsmout'
      include 'lfmmout'




      INTEGER aa            !testing variable
      CHARACTER ch          !testing variable
      REAL rr, rQ, rP, iP
      INTEGER iPr
      REAL E_Q, E_P, I_Q, I_P

      INTEGER iW ! loop variables
      INTEGER ii, jj, kk, ll
      INTEGER iYr, iY, iD    !loop variables
      REAL rVal, rQExp, rQImp
      INTEGER iReg
      INTEGER iRefReg, iCrude

      CHARACTER*6 Step
      INTEGER T_Step, T_Year
      REAL T_Price, T_UB
      REAL Init_Price, Init_Quantity, Supp_Elast, Dem_Elast
      REAL P_hs
      REAL tTest

      CHARACTER*7 Source
      INTEGER OGSM_REG, CRUDE_TYPE

      INTEGER Nrow, Ncol    !loop variables

      INTEGER*4      RTOVALUE,FDBK_SW,EXP_CRD,EXP_LEASE

      LOGICAL New
      CHARACTER*18 Fname
      INTEGER Iunit1
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR

      !Data from 'Total_Crude' worksheet
      REAL P_Total_Crude_Init, Q_Total_Crude_Init, P_Init, Q_Init, S_E, D_E, P_Heavy_Sour, P_hs_Ratio
      COMMON /INIT_VALUES/P_Total_Crude_Init(CRSTEP, 1990:1989+MNXYR) , Q_Total_Crude_Init(CRSTEP, 1990:1989+MNXYR), P_Init(1990:1989+MNXYR), Q_Init(1990:1989+MNXYR), S_E(1990:1989+MNXYR), D_E(1990:1989+MNXYR), P_Heavy_Sour(1990:1989+MNXYR), P_hs_Ratio(1990:1989+MNXYR)

      REAL BP
      COMMON /BREAK_POINTS/ BP(CRSTEP+1)
      !End Data from 'Total_Crude' worksheet

      !Data from 'Crude_Supply_Inc_Domestic' worksheet
      REAL Q_Domestic_Crude_Production, Q_Domestic_Crude_REF, Q_Net_Product_Imp, Q_Net_Product_Imp_REF,Q_Net_Crude_REF
      COMMON /DOMESTIC_PRODUCTION/ Q_Domestic_Crude_Production(MNCRUD, MNUMOR, 1990:1989+MNXYR), Q_Domestic_Crude_REF(1990:1989+MNXYR), Q_Net_Product_Imp(1990:1989+MNXYR), Q_Net_Product_Imp_REF(1990:1989+MNXYR), Q_Net_Crude_REF(1990:1989+MNXYR)
      !End Data from 'Crude_Supply_Inc_Domestic' worksheet       
      !Q_Domestic_Crude_REF(FFY+Ncol-2)

      !Data from 'Crude_Supply_Inc_Foreign' worksheet
      REAL Cr_Type_Coeff, Cr_Type_Share, BRENT_p, WTI_p
      COMMON /WORLD_PRODUCTION/ Cr_Type_Coeff(MNCRUD, 1990:1989+MNXYR), Cr_Type_Share(MNCRUD, 1990:1989+MNXYR), BRENT_p(1990:1989+MNXYR), WTI_p(1990:1989+MNXYR)
      !End Data from 'Crude_Supply_Inc_Foreign' worksheet

 
 
 
 

      !Data from 'Imports_Exports' worksheet
      REAL IMP_Q, IMP_P, EXP_Q, EXP_P
      COMMON /IMP_EXP/ IMP_Q(MNPROD, 1990:1989+MNXYR), IMP_P(MNPROD, 1990:1989+MNXYR), EXP_Q(MNPROD, 1990:1989+MNXYR), EXP_P(MNPROD, 1990:1989+MNXYR)
      !End Data from 'Imports_Exports' worksheet

      !Data from 'EU_MG_Imports' worksheet
      REAL Q_EU_Gas, P_EU_Gas
      COMMON /EU_GAS_IMP/ Q_EU_Gas(1990:1989+MNXYR), P_EU_Gas(1990:1989+MNXYR)
      !End Data from 'EU_MG_Imports' worksheet

      !Data from 'C_MC_Prod_Demand' worksheet
      REAL C_MC_Q, C_MC_P
      COMMON /C_MC_PRODUCT_DEMAND/ C_MC_Q(MNPROD, 1990:1989+MNXYR), C_MC_P(MNPROD, 1990:1989+MNXYR)
      !End Data from 'C_MC_Prod_Demand' worksheet

      !Data from 'Price_Cases_Data' worksheet
      REAL Q_Non_USDemand_Base, Non_USDemand_Shares
      COMMON /Q_NONUS_DEM_BASE/ Q_Non_USDemand_Base(1990:1989+MNXYR), Non_USDemand_Shares(MNCRUD, 1990:1989+MNXYR)
      !End Data from 'Price_Cases_Data' worksheet

      !Data from Report_Imports worksheet
      !***********************************************************************
      REAL ILPTotal_Q, IHPTotal_Q
      COMMON /T118a/ ILPTotal_Q(1990:1989+MNXYR), IHPTotal_Q(1990:1989+MNXYR)

      REAL IOCanadaPct, IOMexicoPct, IONorthSeaPct, IOOPECPct, IOOPLatinAmericaPct, &
           IOOPNorthAfricaPct, IOOPWestAfricaPct, IOIndonesiaPct, IOOPPersianGulfPct, &
           IOOtherMiddleEastPct, IOOtherLatinAmericaPct, IOOtherAfricaPct, IOOtherAsiaPct
      COMMON /T118b/ IOCanadaPct(1990:1989+MNXYR), IOMexicoPct(1990:1989+MNXYR), IONorthSeaPct(1990:1989+MNXYR), IOOPECPct(1990:1989+MNXYR), &
           IOOPLatinAmericaPct(1990:1989+MNXYR), IOOPNorthAfricaPct(1990:1989+MNXYR), IOOPWestAfricaPct(1990:1989+MNXYR), IOIndonesiaPct(1990:1989+MNXYR), &
           IOOPPersianGulfPct(1990:1989+MNXYR), IOOtherMiddleEastPct(1990:1989+MNXYR), IOOtherLatinAmericaPct(1990:1989+MNXYR), &
           IOOtherAfricaPct(1990:1989+MNXYR), IOOtherAsiaPct(1990:1989+MNXYR)

      REAL ILPCanadaPct, ILPNorthEuropePct, ILPSouthEuropePct, ILPOPECPct, ILPOPAmericasPct, &
           ILPOPNoAfricaPct, ILPOPWestAfricaPct, ILPIndonesiaPct, ILPOPPersianGulfPct, &
           ILPCaribbeanPct, ILPAsiaPct, ILPOtherPct
      COMMON /T118c/ ILPCanadaPct(1990:1989+MNXYR), ILPNorthEuropePct(1990:1989+MNXYR), ILPSouthEuropePct(1990:1989+MNXYR), ILPOPECPct(1990:1989+MNXYR), &
           ILPOPAmericasPct(1990:1989+MNXYR), ILPOPNoAfricaPct(1990:1989+MNXYR), ILPOPWestAfricaPct(1990:1989+MNXYR), ILPIndonesiaPct(1990:1989+MNXYR), &
           ILPOPPersianGulfPct(1990:1989+MNXYR), ILPCaribbeanPct(1990:1989+MNXYR), ILPAsiaPct(1990:1989+MNXYR), ILPOtherPct(1990:1989+MNXYR)

      REAL IHPCanadaPct, IHPNorthEuropePct, IHPSouthEuropePct, IHPOPECPct, IHPOPAmericasPct, &
           IHPOPNoAfricaPct, IHPOPWestAfricaPct, IHPIndonesiaPct, IHPOPPersianGulfPct, &
           IHPCaribbeanPct, IHPAsiaPct, IHPOtherPct
      COMMON /T118d/ IHPCanadaPct(1990:1989+MNXYR), IHPNorthEuropePct(1990:1989+MNXYR), IHPSouthEuropePct(1990:1989+MNXYR), IHPOPECPct(1990:1989+MNXYR), &
           IHPOPAmericasPct(1990:1989+MNXYR), IHPOPNoAfricaPct(1990:1989+MNXYR), IHPOPWestAfricaPct(1990:1989+MNXYR), IHPIndonesiaPct(1990:1989+MNXYR), &
           IHPOPPersianGulfPct(1990:1989+MNXYR), IHPCaribbeanPct(1990:1989+MNXYR), IHPAsiaPct(1990:1989+MNXYR), IHPOtherPct(1990:1989+MNXYR)
    
    INTEGER tT
    !LASTYR=61
    
    
     !***********************************************************************


!To be deleted
!*********************************
!*********************************
      !This will hold old and new foreign crude purchases in the US
      !REAL LFMM_fc_purchase, LFMM_fc_purchase_new
      !COMMON /LFMM_FC_P/ LFMM_fc_purchase(MNCRUD,1990:1989+MNXYR), LFMM_fc_purchase_new(MNCRUD,1990:1989+MNXYR)
      !Fname = 'LFMM_fc_p.csv'
!*********************************
      !Iunit2 



        !OPEN(Iunit2, ACTION='READ',FILE=TRIM('LFMM_fc_p.csv')) !*******************************To be modified!!!!!!!

        !DO 40 t=1, (1989+MNUMYR+Max_LFMM_FYR-FFY+1)*MNCRUD
            !READ(Iunit2,*) c , iYr, LFMM_fc_purchase(c,iYr)
        !40 CONTINUE

        !CLOSE (40)
!*********************************
!*********************************
 

      FDBK_SW = RTOVALUE('INTLFDBK',0)

      Fname = 'INTALLIN'
      New = .FALSE.
      Iunit1 = FILE_MGR('O', Fname, New)

      call xmlSheet(Iunit1,'Total_Crude')

      !Read data for Total world Supply Curve for each step (Max_Total_Crude_Steps=9 steps) and for each year
      !between FFY (=2008) and 1989+MNUMYR+Max_LFMM_FYR (=1989+61+21=2071)
      iW = (MNUMYR+Max_LFMM_FYR-FFY+1990)*CRSTEP + 1
      do Nrow=2,iW
         call xmlRow(Nrow)
         do Ncol=1,9
            if (Ncol .EQ. 1) then
                call xmlCCell(Ncol,Step)
                read (Step(5:LEN_TRIM(Step)), '(I2)') T_Step               !LEN_TRIM(Step)
            else if (Ncol .EQ. 2) then
                call xmlRCell(Ncol,T_Price)
            else if (Ncol .EQ. 3) then
                call xmlRCell(Ncol,T_UB)
            else if (Ncol .EQ. 4) then
                call xmlICell(Ncol,T_Year)
            else if (Ncol .EQ. 5) then
                call xmlRCell(Ncol,Init_Price)
            else if (Ncol .EQ. 6) then
                call xmlRCell(Ncol,Init_Quantity)
            else if (Ncol .EQ. 7) then
                call xmlRCell(Ncol,Supp_Elast)
            else if (Ncol .EQ. 8) then
                call xmlRCell(Ncol,Dem_Elast)
            else if (Ncol .EQ. 9) then
                call xmlRCell(Ncol,P_hs)
            end if
         end do
         P_Total_Crude_Init(T_Step, T_Year) = T_Price
         Q_Total_Crude_Init(T_Step, T_Year) = T_UB
         P_Init(T_Year) = Init_Price
         Q_Init(T_Year) = Init_Quantity
         S_E(T_Year) = Supp_Elast
         D_E(T_Year) = Dem_Elast
         P_Heavy_Sour(T_Year) = P_hs*P_Init(T_Year)
         P_hs_Ratio(T_Year)=P_hs
      end do

      call xmlRow(iW+2)
      do Ncol=2,16
         call xmlRCell(Ncol,BP(Ncol-1))
      end do

      !*****************************************************P_Heavy_Sour(1990:1989+MNXYR)
      call xmlSheet(Iunit1,'Crude_Supply_Inc_Domestic')
      
      !Read data for Domestic Crue Production in Reference case
      Nrow=2
      call xmlRow(Nrow)
      do Ncol=2,LASTYR-FFY+2+1989
        call xmlRCell(Ncol,Q_Domestic_Crude_REF(FFY+Ncol-2))      
      end do
      
      !Read data for Net Product Import in Reference case
      Nrow=3
      call xmlRow(Nrow)
      do Ncol=2,LASTYR-FFY+2+1989
        call xmlRCell(Ncol,Q_Net_Product_Imp_REF(FFY+Ncol-2))      
      end do
      
      !Read data for crude demand in Reference case
      Nrow=4
      call xmlRow(Nrow)
      do Ncol=2,LASTYR-FFY+2+1989
        call xmlRCell(Ncol,Q_Net_Crude_REF(FFY+Ncol-2))      
      end do
      
!Q_Net_Crude_REF(1990:1989+MNXYR)      
     
      !Read data for Domestic Supply Curve for each step (only one step), for crude types 1-7, for each of OGSM regions (12 of them) and for each year
      !between FFY (=2008) and 1989+MNUMYR+Max_LFMM_FYR (=1989+61+21=2071)
!      iW = 7*(1990-FFY+MNUMYR+Max_LFMM_FYR)*12 + 1
!
!      do Nrow=2,iW
!        call xmlRow(Nrow)
!        do Ncol=1,6
!           if (Ncol .EQ. 1) then
!               call xmlICell(Ncol,CRUDE_TYPE)
!           else if (Ncol .EQ. 2) then
!               call xmlCCell(Ncol,Source)
!               read (Source(6:LEN_TRIM(Source)), '(I2)') OGSM_REG                 !LEN_TRIM(Step)
!           else if (Ncol .EQ. 3) then
!               call xmlICell(Ncol,T_Year)
!           else if (Ncol .EQ. 4) then
!               call xmlCCell(Ncol,Step)
!               read (Step(5:LEN_TRIM(Step)), '(I2)') T_Step
!           else if (Ncol .EQ. 5) then
!               call xmlRCell(Ncol,T_Price)
!           else if (Ncol .EQ. 6) then
!               call xmlRCell(Ncol,T_UB)
!           end if
!        end do
!        Q_Domestic_Crude_Production(CRUDE_TYPE, OGSM_REG, T_Year) = T_UB
!      end do

      !*****************************************************
      call xmlSheet(Iunit1,'Crude_Supply_Inc_Foreign')
          !Read data info for International Supply Curve. Specifically, we read coefficients for each crude type, by year,
          !used to compute crude type price. We also read, by year, the share of each crude type production out of world total crude like liquids,
      do Nrow=3,MNCRUD+1
        call xmlRow(Nrow)
        do Ncol=1,MNUMYR+Max_LFMM_FYR-FFY+1991
           if (Ncol .EQ. 1) then
               call xmlICell(Ncol,CRUDE_TYPE)
           else
               call xmlRCell(Ncol,Cr_Type_Coeff(CRUDE_TYPE, FFY+Ncol-2))
           end if
        end do
      end do

      do Nrow=14,MNCRUD+13
        call xmlRow(Nrow)
        do Ncol=1,MNUMYR+Max_LFMM_FYR-FFY+1991
           if (Ncol .EQ. 1) then
               call xmlICell(Ncol,CRUDE_TYPE)
           else
               call xmlRCell(Ncol,Cr_Type_Share(CRUDE_TYPE, FFY+Ncol-2))
           end if
        end do
      enddo

      Nrow=26
      call xmlRow(Nrow)
      do Ncol=2,MNUMYR+Max_LFMM_FYR-FFY+1991
          call xmlRCell(Ncol,BRENT_p(FFY+Ncol-2))
		  if ((FFY+Ncol-2) .LE. (1989+MNUMYR)) then
                START_PRICE(FFY+Ncol-2-1989) =  BRENT_p(FFY+Ncol-2)
          end if 
          if (fdbk_sw.eq.0) then
            XSTART_PRICE(FFY+Ncol-2-1989) = BRENT_p(FFY+Ncol-2)
            if ((FFY+Ncol-2) .LE. (1989+MNUMYR)) then
                START_PRICE(FFY+Ncol-2-1989) = XSTART_PRICE(FFY+Ncol-2-1989)
            end if
          end if
      end do

aa=91
      !if (fdbk_sw.eq.0) then
              do iY=FFY-1989,aa
                  XSTART_PRICE(iY)=XSTART_PRICE(iY)/MC_JPGDP(32)
        end do
              do iY=FFY-1989,MNUMYR
                  START_PRICE(iY)=START_PRICE(iY)/MC_JPGDP(32)
        end do
      !endif

write(DBGUNIT, '(//,A)') "Start Pricesa this is a new test AEO2017"
write (DBGUNIT,1234) (START_PRICE(ii)*MC_JPGDP(32) , ii = 19, 61)

write(DBGUNIT, '(//,A)') "XStart Pricesa"
write (DBGUNIT,12345) (XSTART_PRICE(ii)*MC_JPGDP(32) , ii = 19, 91)

!write(DBGUNIT, '(//,A)') "Brent Pricesa"
!write (DBGUNIT,1234) (BRENT_PRICE(ii)*MC_JPGDP(32) , ii = 19, 61)
1234          format (2x, 43F10.2)

!write(DBGUNIT, '(//,A)') "XBrent Pricesa"
!write (DBGUNIT,12345) (XBRENT_PRICE(ii)*MC_JPGDP(32) , ii = 19, 91)
12345          format (2x, 73F10.2)



      Nrow=27
      call xmlRow(Nrow)
      do Ncol=2,MNUMYR+Max_LFMM_FYR-FFY+1991
          call xmlRCell(Ncol,WTI_p(FFY+Ncol-2))
          XWTI_PRICE(FFY+Ncol-2-1989) = WTI_p(FFY+Ncol-2)
          if ((FFY+Ncol-2) .LE. (1989+MNUMYR)) then
              WTI_PRICE(FFY+Ncol-2-1989) = XWTI_PRICE(FFY+Ncol-2-1989)
          end if
      end do

            do iY=FFY-1989,aa
                XWTI_PRICE(iY)=XWTI_PRICE(iY)/MC_JPGDP(32)
      end do
            do iY=FFY-1989,MNUMYR
                WTI_PRICE(iY)=WTI_PRICE(iY)/MC_JPGDP(32)
            end do
            !XWTI_PRICE(:) = XWTI_PRICE(:)/MC_JPGDP(32)      
            
            
!This section retrieve initial crude exoprts which are also final crude exports in "No Crude Exports" case            
!*******************************************************************************
!*******************************************************************************
iW = 29+REFREG*MNCRUD

!iCrude = 1

do Nrow=30,iW
   call xmlRow(Nrow)
   do Ncol=2,MNUMYR+Max_LFMM_FYR-FFY+1991

      iRefReg = INT((Nrow-30)/11)+1

      iCrude = MOD (Nrow-30,11)+1

      call xmlRCell(Ncol, Q_CRUDE_TO_CAN(iRefReg,iCrude,Ncol+2006))

!      iCrude=iCrude+1
!      if (iCrude .EQ. (MNCRUD+1)) then
!        iCrude = 1
!      end if        Q_CRUDE_TO_CAN

   end do !Ncol

end do !Nrow

!*******************************************************************************
!*******************************************************************************
write(DBGUNIT, '(//,A)') "Exogenous Crude Exports to CAN"
write(DBGUNIT, 4441) (ii, ii=2008,2071)
4441 format(8x, 64I8)

do iRefReg=1,REFREG

    do iCrude=1,MNCRUD

        write(DBGUNIT, 444) iRefReg, iCrude, (Q_CRUDE_TO_CAN(iRefReg,iCrude,ii), ii=2008,2071)

    444 format ("R", I1, "C", I2, 4x, 64F8.2)

    end do

end do
            
!tTest = Q_CRUDE_EXPORTS(5,1,2040)      
!tTest = Q_CRUDE_EXPORTS(5,2,2040)      
!tTest = Q_CRUDE_EXPORTS(5,3,2040)      

      !*****************************************************

 !This section creates the values that must be updated in 'Crude_Supply_Inc_Domestic' worksheet column F. Only for price cases.
 FDBK_SW = RTOVALUE('INTLFDBK',0)  

!      do ii=FFY-1989,  MNUMYR-10   !MNUMYR+Max_LFMM_FYR   OGCRUDEREF(MNUMPR,MNCRUD,MNUMYR)  
!        do jj=1, MNCRUD-2
!            do kk=1, MNUMOR-1
!                WRITE (DBGUNIT, 4444) OGCRDPRD(kk,jj,ii)
!        4444    FORMAT (2x, F12.6)
!            end do
!        end do
!     end do
     
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!***********************************************
!Compute US crude expected production  Q_Domestic_Crude_REF(1990:1989+MNXYR)
      rr=0.0
      do kk=FFY-1989,  MNUMYR !-10
          do ii = 1, MNCRUD
            do jj = 1, MNUMPR-2
                rr = rr + OGCRUDEREF(jj, ii, kk)*((1000.0/365.0))
            end do
          end do
          WRITE (DBGUNIT, 4444) rr
          4444    FORMAT (2x, F12.6)
      end do
      
!! 
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!***********************************************

      !This section read center points for imports and exports curves
      call xmlSheet(Iunit1,'Imports_Exports')      
write(DBGUNIT, '(//,A)') "line 467"
      do Nrow=2,(1990-FFY+MNUMYR+Max_LFMM_FYR)*MNPROD+1

        call xmlRow(Nrow)

        do Ncol=1,6
            if (Ncol .EQ. 1) then
                call xmlICell(Ncol,iYr)
            else if (Ncol .EQ. 2) then
                call xmlICell(Ncol,iPr)
            else if (Ncol .EQ. 3) then
                call xmlRCell(Ncol,I_Q)
            else if (Ncol .EQ. 4) then
                call xmlRCell(Ncol,I_P)
            else if (Ncol .EQ. 5) then
                call xmlRCell(Ncol,E_Q)
                else if (Ncol .EQ. 6) then
            call xmlRCell(Ncol,E_P)
            end if
        end do
        IMP_Q(iPr,iYr)=I_Q
        IMP_P(iPr,iYr)=I_P
        EXP_Q(iPr,iYr)=E_Q
        EXP_P(iPr,iYr)=E_P


      end do

write(DBGUNIT, '(//,A)') "line 495"
!         !This section read gasoline imports (P,Q) from EU into US by year
!      call xmlSheet(Iunit1,'EU_MG_Imports')      
!
!      do Nrow=2,1990-FFY+MNUMYR+Max_LFMM_FYR+1
!        call xmlRow(Nrow)
!
!        do Ncol=1,4
!            if (Ncol .EQ. 1) then
!                call xmlICell(Ncol,iYr)
!            else if (Ncol .EQ. 2) then
!                call xmlICell(Ncol,iPr)
!            else if (Ncol .EQ. 3) then
!                call xmlRCell(Ncol,rQ)
!            else if (Ncol .EQ. 4) then
!                call xmlRCell(Ncol,rP)
!            end if
!        end do
!        Q_EU_Gas(iYr) = rQ
!        P_EU_Gas(iYr) = rP
!      end do


      !This section read product demands (P,Q) from C_MC region
      call xmlSheet(Iunit1,'C_MC_Prod_Demand')
write(DBGUNIT, '(//,A)') "line 520"
      do Nrow=2,(1990-FFY+MNUMYR+Max_LFMM_FYR)*MNPROD+1

        call xmlRow(Nrow)

        do Ncol=1,4
            if (Ncol .EQ. 1) then
                call xmlICell(Ncol,iYr)
            else if (Ncol .EQ. 2) then
                call xmlICell(Ncol,iPr)
            else if (Ncol .EQ. 3) then
                call xmlRCell(Ncol,rP)
            else if (Ncol .EQ. 4) then
                call xmlRCell(Ncol,rQ)
            end if
        end do
        C_MC_Q(iPr,iYr)=rQ
        C_MC_P(iPr,iYr)=rP


      end do

write(DBGUNIT, '(//,A)') "line 542"
!      !Data from 'Exp_Imp_Prod' worksheet
!      call xmlSheet(Iunit1,'Exp_Imp_Prod')
!
!      do Nrow=2,(1990-FFY+MNUMYR+Max_LFMM_FYR)*MNPROD*REFREG+1
!        call xmlRow(Nrow)
!
!        do Ncol=1,5
!            if (Ncol .EQ. 1) then
!                call xmlICell(Ncol,iYr)
!            else if (Ncol .EQ. 2) then
!                call xmlICell(Ncol,iPr)
!            else if (Ncol .EQ. 3) then
!                call xmlICell(Ncol,iReg)
!            else if (Ncol .EQ. 4) then
!                call xmlRCell(Ncol,rQImp)
!            else if (Ncol .EQ. 5) then
!                call xmlRCell(Ncol,rQExp)
!            end if
!        end do
!        Q_Import_Product(iPr,iReg,iYr)=rQImp
!        Q_Export_Product(iPr,iReg,iYr)=rQExp
!
!      end do


      !Data from 'Report_Imports' worksheet
      call xmlSheet(Iunit1,'Report_Imports')

write(DBGUNIT, '(//,A)') "line 571"
      !Crude Imports Pct
      call xmlRow(3)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,IOCanadaPct(Ncol))
        end do

      call xmlRow(4)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,IOMexicoPct(Ncol))
        end do

      call xmlRow(5)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,IONorthSeaPct(Ncol))
        end do

      call xmlRow(6)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,IOOPECPct(Ncol))
        end do

      call xmlRow(7)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,IOOPLatinAmericaPct(Ncol))
        end do

      call xmlRow(8)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,IOOPNorthAfricaPct(Ncol))
        end do

      call xmlRow(9)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,IOOPWestAfricaPct(Ncol))
        end do

      call xmlRow(10)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,IOIndonesiaPct(Ncol))
        end do

      call xmlRow(11)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,IOOPPersianGulfPct(Ncol))
        end do

      call xmlRow(12)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,IOOtherMiddleEastPct(Ncol))
        end do

      call xmlRow(13)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,IOOtherLatinAmericaPct(Ncol))
        end do

      call xmlRow(14)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,IOOtherAfricaPct(Ncol))
        end do

      call xmlRow(15)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,IOOtherAsiaPct(Ncol))
        end do


      !LP Imports Pct
      call xmlRow(20)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,ILPCanadaPct(Ncol))
        end do

      call xmlRow(21)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,ILPNorthEuropePct(Ncol))
        end do

      call xmlRow(22)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,ILPSouthEuropePct(Ncol))
        end do

      call xmlRow(23)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,ILPOPECPct(Ncol))
        end do

      call xmlRow(24)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,ILPOPAmericasPct(Ncol))
        end do

      call xmlRow(25)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,ILPOPNoAfricaPct(Ncol))
        end do

      call xmlRow(26)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,ILPOPWestAfricaPct(Ncol))
        end do

      call xmlRow(27)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,ILPIndonesiaPct(Ncol))
        end do

      call xmlRow(28)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,ILPOPPersianGulfPct(Ncol))
        end do

      call xmlRow(29)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,ILPCaribbeanPct(Ncol))
        end do

      call xmlRow(30)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,ILPAsiaPct(Ncol))
        end do

      call xmlRow(31)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,ILPOtherPct(Ncol))
        end do


      !HP Import Pct
      call xmlRow(36)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,IHPCanadaPct(Ncol))
        end do

      call xmlRow(37)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,IHPNorthEuropePct(Ncol))
        end do

      call xmlRow(38)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,IHPSouthEuropePct(Ncol))
        end do

      call xmlRow(39)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,IHPOPECPct(Ncol))
        end do

      call xmlRow(40)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,IHPOPAmericasPct(Ncol))
        end do

      call xmlRow(41)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,IHPOPNoAfricaPct(Ncol))
        end do

      call xmlRow(42)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,IHPOPWestAfricaPct(Ncol))
        end do

      call xmlRow(43)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,IHPIndonesiaPct(Ncol))
        end do

      call xmlRow(44)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,IHPOPPersianGulfPct(Ncol))
        end do

      call xmlRow(45)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,IHPCaribbeanPct(Ncol))
        end do

      call xmlRow(46)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,IHPAsiaPct(Ncol))
        end do

      call xmlRow(47)
        do Ncol= FFY, 1989+MNUMYR
            call xmlRCell(Ncol-FFY+2,IHPOtherPct(Ncol))
        end do

write(DBGUNIT, '(//,A)') "line 762"
      !This section read Q_Non_USDemand available from price cases, aggregated over all crude types
      call xmlSheet(Iunit1,'Price_Cases_Data')
write(DBGUNIT, '(//,A)') "line 765"
      do Nrow=3,1990-FFY+MNUMYR+Max_LFMM_FYR+2
        call xmlRow(Nrow)

        do Ncol=1,2
           if (Ncol .EQ. 1) then
               call xmlICell(Ncol,iYr)
           else if (Ncol .EQ. 2) then
                call xmlRCell(Ncol,rQ)
           end if
        end do
        Q_Non_USDemand_Base(iYr) = rQ
      end do


      do Nrow=71,70+MNCRUD
        call xmlRow(Nrow)
        do Ncol=1,MNUMYR+Max_LFMM_FYR-FFY+1991
            if (Ncol .EQ. 1) then
                        call xmlICell(Ncol,CRUDE_TYPE)
                    else
                        call xmlRCell(Ncol,Non_USDemand_Shares(CRUDE_TYPE, FFY+Ncol-2))
            end if
        end do
      end do


write(DBGUNIT, '(//,A)') "line 792"
      Iunit1 = FILE_MGR('C', Fname, New)
write(DBGUNIT, '(//,A)') "line 794"
      RETURN
      END

!***********************************************************************************
!***********************************************************************************
      SUBROUTINE World_LFMM_Compute_New

      !IMPLICIT NONE
      include 'parametr'
      include 'ncntrl'
      include 'pmmrpt'
      include 'intout'
      include 'ngtdmrep'
      include 'macout'
      include 'pmmout'
      include 'intllocl'
      include 'ogsmout'
      include 'lfmmout'
      include 'mxpblk'

!*********************************

!*********************************
!! This will be deleted!!!!!!!!!!!!!!!!!!!!
!INTEGER INTLREG,INTLSTEP
!PARAMETER(INTLREG=4)      ! Number of international regions
!PARAMETER(INTLSTEP=14)      ! Product imports/exports curves steps
!
!REAL Product_Import_Q(MNPROD,INTLREG,INTLSTEP,MNXYR) ! (intout, product imports supply curves quantities)
!REAL Product_Import_P(MNPROD,INTLREG,INTLSTEP,MNXYR) ! (intout, product imports supply curves prices)
!
!REAL Product_Export_Q(MNPROD,INTLREG,INTLSTEP,MNXYR) ! (intout, product exports demand curves quantities)
!REAL Product_Export_P(MNPROD,INTLREG,INTLSTEP,MNXYR) ! (intout, product exports demand curves prices)
!
!REAL Q_CRUDE_EXPORTS(REFREG+1,MNCRUD,1990:1989+MNXYR) ! (lfmmout, exported crude oil quntities)
!REAL P_CRUDE_EXPORTS(REFREG+1,MNCRUD,1990:1989+MNXYR) ! (lfmmout, exported crude oil price)
!!END This will be deleted!!!!!!!!!!!!!!!!!!!!
!!*********************************


      INTEGER aa            !testing variable
      INTEGER iR, iY
      CHARACTER ch          !testing variable
      REAL rr,ss,tt
      REAL rActualCrudeProd
      INTEGER ii, jj, kk, ll, t, c
      INTEGER iPr

      REAL rUSCrudeImports(1990:1989+MNXYR)

      INTEGER*4      RTOVALUE,FDBK_SW,EXP_CRD,EXP_LEASE

      CHARACTER*100 line,label

      LOGICAL New
      CHARACTER*18 Fname
      INTEGER Iunit1, Iunit2
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR

      INTEGER iSt, iYr    !loop variables
      INTEGER iFlag, fNonUSResponse

      !Variables used for buiding the new supply curve when there is feedback (INTLFDBK=1)
      REAL(8) P_Start, P_End, Q_Start, Q_End
      REAL(8) P_Step


      !Data from 'Total_Crude' worksheet
      REAL P_Total_Crude_Init, Q_Total_Crude_Init, P_Init, Q_Init, S_E, D_E, P_Heavy_Sour, P_hs_Ratio
      COMMON /INIT_VALUES/P_Total_Crude_Init(CRSTEP, 1990:1989+MNXYR) , Q_Total_Crude_Init(CRSTEP, 1990:1989+MNXYR), P_Init(1990:1989+MNXYR), Q_Init(1990:1989+MNXYR), S_E(1990:1989+MNXYR), D_E(1990:1989+MNXYR), P_Heavy_Sour(1990:1989+MNXYR), P_hs_Ratio(1990:1989+MNXYR)


      REAL BP
      COMMON /BREAK_POINTS/ BP(CRSTEP+1)
      !End Data from 'Total_Crude' worksheet

      !Data from 'Crude_Supply_Inc_Domestic' worksheet
      !REAL Q_Domestic_Crude_Production, Q_Domestic_Crude_REF
      !COMMON /DOMESTIC_PRODUCTION/ Q_Domestic_Crude_Production(MNCRUD, MNUMOR, 1990:1989+MNXYR), Q_Domestic_Crude_REF(1990:1989+MNXYR)
      !End Data from 'Crude_Supply_Inc_Domestic' worksheet

      !Data from 'Crude_Supply_Inc_Domestic' worksheet
      REAL Q_Domestic_Crude_Production, Q_Domestic_Crude_REF, Q_Net_Product_Imp, Q_Net_Product_Imp_REF,Q_Net_Crude_REF
      COMMON /DOMESTIC_PRODUCTION/ Q_Domestic_Crude_Production(MNCRUD, MNUMOR, 1990:1989+MNXYR), Q_Domestic_Crude_REF(1990:1989+MNXYR), Q_Net_Product_Imp(1990:1989+MNXYR), Q_Net_Product_Imp_REF(1990:1989+MNXYR), Q_Net_Crude_REF(1990:1989+MNXYR)
      !End Data from 'Crude_Supply_Inc_Domestic' worksheet       
     
 
 
 
      
      !Data from 'Crude_Supply_Inc_Foreign' worksheet
      REAL Cr_Type_Coeff, Cr_Type_Share, BRENT_p, WTI_p
      COMMON /WORLD_PRODUCTION/ Cr_Type_Coeff(MNCRUD, 1990:1989+MNXYR), Cr_Type_Share(MNCRUD, 1990:1989+MNXYR), BRENT_p(1990:1989+MNXYR), WTI_p(1990:1989+MNXYR)
      !End Data from 'Crude_Supply_Inc_Foreign' worksheet




      !Data from 'C_MC_Crude_Demand' worksheet
 !     REAL C_MC_Crude_Demand
 !     COMMON /C_MC_CR_DEM/ C_MC_Crude_Demand(MNCRUD, 1990:1989+MNXYR)
      !End Data from 'C_MC_Crude_Demand' worksheet

      !Data from 'Imports_Exports' worksheet
      REAL IMP_Q, IMP_P, EXP_Q, EXP_P, EXP_Q0
      COMMON /IMP_EXP/ IMP_Q(MNPROD, 1990:1989+MNXYR), IMP_P(MNPROD, 1990:1989+MNXYR), EXP_Q(MNPROD, 1990:1989+MNXYR), EXP_P(MNPROD, 1990:1989+MNXYR), EXP_Q0(MNPROD, 1990:1989+MNXYR)
      !End Data from 'Imports_Exports' worksheet

      !Data from 'EU_MG_Imports' worksheet
      REAL Q_EU_Gas, P_EU_Gas
      COMMON /EU_GAS_IMP/ Q_EU_Gas(1990:1989+MNXYR), P_EU_Gas(1990:1989+MNXYR)
      !End Data from 'EU_MG_Imports' worksheet

      !Data from 'C_MC_Prod_Demand' worksheet
      REAL C_MC_Q, C_MC_P
      COMMON /C_MC_PRODUCT_DEMAND/ C_MC_Q(MNPROD, 1990:1989+MNXYR), C_MC_P(MNPROD, 1990:1989+MNXYR)
      !End Data from 'C_MC_Prod_Demand' worksheet

      !These will hold WOP prices and quantities for global crude like liquids demands at equilibrium
      REAL P_Eql, Q_Eql
      COMMON /P_Q_EQUILIBRIUM/ P_Eql(1990:1989+MNXYR), Q_Eql(1990:1989+MNXYR)

      REAL D_Diff, S_Diff
      COMMON /DIFF/ D_Diff(1990:1989+MNXYR), S_Diff(1990:1989+MNXYR)

      !These will hold P's and Q's for international crude supply (non US crude production) by crude type at equilibrium
      REAL P_Crude, Q_Crude
      COMMON /P_Q_EQUILIBRIUM_CRUDES/ P_Crude(MNCRUD,1990:1989+MNXYR),Q_Crude(MNCRUD,1990:1989+MNXYR)

      REAL LFMM_PurchaseForeign_Crude, LFMM_Export_Crude, LFMM_Export_Crude_Net !(MNCRUD,1990:1989+MNXYR)
      COMMON /LFMM_IMPEXP_CRUDE/ LFMM_PurchaseForeign_Crude(MNCRUD,1990:1989+MNXYR), LFMM_Export_Crude(MNCRUD,1990:1989+MNXYR), LFMM_Export_Crude_Net(MNCRUD,1990:1989+MNXYR)

      !Data from 'Price_Cases_Data' worksheet
      REAL Q_Non_USDemand_Base, Non_USDemand_Shares
      COMMON /Q_NONUS_DEM_BASE/ Q_Non_USDemand_Base(1990:1989+MNXYR), Non_USDemand_Shares(MNCRUD, 1990:1989+MNXYR)
      !End Data from 'Price_Cases_Data' worksheet
      
      !LFMM_TOTAL_EXP(1989+CURIYR)
      REAL LFMM_TOTAL_EXP
      COMMON /TOTAL_EXP_NOCAN/ LFMM_TOTAL_EXP(1990:1989+MNXYR)
      
      !These will hold P's and Q's for international crude supply (non US crude production) by crude type at equilibrium
      REAL P_Crude_Exp, Q_Crude_Exp
      COMMON /P_Q_EQUILIBRIUM_CRUDES_EXP/ P_Crude_Exp(MNCRUD,1990:1989+MNXYR),Q_Crude_Exp(MNCRUD,1990:1989+MNXYR)
      
      REAL qLight, qMedium, qHeavy, diffNetMGEXP, LFMM_Export_Crude_Light 
	  
	  REAL ILPCanadaPct, ILPNorthEuropePct, ILPSouthEuropePct, ILPOPECPct, ILPOPAmericasPct, &
           ILPOPNoAfricaPct, ILPOPWestAfricaPct, ILPIndonesiaPct, ILPOPPersianGulfPct, &
           ILPCaribbeanPct, ILPAsiaPct, ILPOtherPct
      COMMON /T118c/ ILPCanadaPct(1990:1989+MNXYR), ILPNorthEuropePct(1990:1989+MNXYR), ILPSouthEuropePct(1990:1989+MNXYR), ILPOPECPct(1990:1989+MNXYR), &
           ILPOPAmericasPct(1990:1989+MNXYR), ILPOPNoAfricaPct(1990:1989+MNXYR), ILPOPWestAfricaPct(1990:1989+MNXYR), ILPIndonesiaPct(1990:1989+MNXYR), &
           ILPOPPersianGulfPct(1990:1989+MNXYR), ILPCaribbeanPct(1990:1989+MNXYR), ILPAsiaPct(1990:1989+MNXYR), ILPOtherPct(1990:1989+MNXYR)
 
      REAL IHPCanadaPct, IHPNorthEuropePct, IHPSouthEuropePct, IHPOPECPct, IHPOPAmericasPct, &
           IHPOPNoAfricaPct, IHPOPWestAfricaPct, IHPIndonesiaPct, IHPOPPersianGulfPct, &
           IHPCaribbeanPct, IHPAsiaPct, IHPOtherPct
      COMMON /T118d/ IHPCanadaPct(1990:1989+MNXYR), IHPNorthEuropePct(1990:1989+MNXYR), IHPSouthEuropePct(1990:1989+MNXYR), IHPOPECPct(1990:1989+MNXYR), &
           IHPOPAmericasPct(1990:1989+MNXYR), IHPOPNoAfricaPct(1990:1989+MNXYR), IHPOPWestAfricaPct(1990:1989+MNXYR), IHPIndonesiaPct(1990:1989+MNXYR), &
           IHPOPPersianGulfPct(1990:1989+MNXYR), IHPCaribbeanPct(1990:1989+MNXYR), IHPAsiaPct(1990:1989+MNXYR), IHPOtherPct(1990:1989+MNXYR)
   
   !INTEGER*4      RTOVALUE,FDBK_SW,EXP_CRD,EXP_LEASE                                                                                                              
   INTEGER*4 iItr, iItr2, iNruns
    iItr  = CURITR 
    iItr = LASTYR
    
    iItr2 = MAXITR
    !iItr2 = RTOVALUE('MAXITR  ',8)  
    
    !iNruns = NRUNS 
    iNruns = RTOVALUE('NRUNS   ',8)     
 

      !****** LFMM_Export_Crude(c,1989+CURIYR) = LFMM_Export_Crude(c,1989+CURIYR)+Q_CRUDE_EXPORTS(ii,c,1989+CURIYR)
!This version is for no shares analysis
!do c=1,9
!    do iR=1,9
!        do iY=1990,2080
!            Q_CRUDE_IMPORTS(iR,c,iY) = Q_CRUDE_IMPORTA(iR,c,iY)
!        end do
!    end do
!end do
 !*************************************

      FDBK_SW = RTOVALUE('INTLFDBK',0)
      EXP_CRD = RTOVALUE('CRUDEXP ',0)
      EXP_LEASE = RTOVALUE('EXPLEASE',0)

      !Compute US crude expected production 
      rr=Q_Domestic_Crude_REF(1989+CURIYR)
      
      rActualCrudeProd = 0.0
      do ii = 1, MNCRUD
        do jj = 1, MNUMPR-2
            rActualCrudeProd = rActualCrudeProd + OGCRUDEREF(jj, ii,CURIYR)*(1000.0/365.0) 
        end do 
      end do      


      S_Diff(1989+CURIYR) = rActualCrudeProd - rr

      !Compute the world crude demand for side cases
      if(FDBK_SW .EQ.1) then
        GLBCRDDMD(CURIYR) = GLBCRDDMD(CURIYR) - sum(Q_Non_US_Demand(:,Max_Crude_Source,Max_NonUS_Demand_Steps,1989+CURIYR),1) + Q_Non_USDemand_Base(1989+CURIYR)
      end if
      D_Diff(1989+CURIYR) = GLBCRDDMD(CURIYR) - Q_Init(1989+CURIYR)
      
      !Add impact of change in net product imports
      D_Diff(1989+CURIYR) = 1000*(sum(OGOILPRD(:,5,CURIYR)) + RFIMCR(MNUMPR,CURIYR) + RFCRDOTH(MNUMPR,CURIYR)) - Q_Net_Crude_REF(1989+CURIYR) + 1000.0*RFIMTP(MNUMPR,CURIYR) - Q_Net_Product_Imp_REF(1989+CURIYR)


      !Compute the WOP price and world crude like liquids at equilibrium point
      fNonUSResponse = 0
      if(fNonUSResponse .EQ. 0) then      
        P_Eql(1989+CURIYR) = P_Init(1989+CURIYR)*EXP(LOG(( Q_Init(1989+CURIYR)+S_Diff(1989+CURIYR) )/( Q_Init(1989+CURIYR)+D_Diff(1989+CURIYR)))/(D_E(1989+CURIYR) - S_E(1989+CURIYR)))
        Q_Eql(1989+CURIYR) = (Q_Init(1989+CURIYR)+S_Diff(1989+CURIYR))*(P_Eql(1989+CURIYR)/P_Init(1989+CURIYR))**S_E(1989+CURIYR)
      else 
        Q_Eql(1989+CURIYR) = Q_Init(1989+CURIYR) + S_Diff(1989+CURIYR)
        P_Eql(1989+CURIYR) = P_Init(1989+CURIYR) * EXP( LOG((Q_Init(1989+CURIYR)+S_Diff(1989+CURIYR))/(Q_Init(1989+CURIYR)+D_Diff(1989+CURIYR)))/D_E(1989+CURIYR))
      end if
      !Recenter for price cases
      if(FDBK_SW .EQ.0) then
        P_Eql(1989+CURIYR) =  P_Init(1989+CURIYR)
        Q_Eql(1989+CURIYR) =  Q_Init(1989+CURIYR)
      end if

      P_Heavy_Sour(1989+CURIYR) = P_Eql(1989+CURIYR)*P_hs_Ratio(1989+CURIYR)

      !START_PRICE(CURIYR) = P_Eql(1989+CURIYR)/MC_JPGDP(32)
      !WTI_PRICE(CURIYR) = START_PRICE(CURIYR)*(WTI_p(1989+CURIYR)/BRENT_p(1989+CURIYR))

iFlag=2

if(iFlag .EQ.1) then

write(DBGUNIT, '(//,A)') "Brent Prices1"
write (DBGUNIT,1122) (BRENT_PRICE(ii)*MC_JPGDP(32) , ii = 19, 61)
1122          format (2x, 43F10.2)

write(DBGUNIT, '(//,A)') "XBrent Prices1"
write (DBGUNIT,3333) (XBRENT_PRICE(ii)*MC_JPGDP(32) , ii = 19, 91)
3333          format (2x, 73F10.2)

      START_PRICE(CURIYR) = P_Eql(1989+CURIYR)/MC_JPGDP(32)
      WTI_PRICE(CURIYR) = START_PRICE(CURIYR)*(WTI_p(1989+CURIYR)/BRENT_p(1989+CURIYR))

write(DBGUNIT, '(//,A)') "Start Prices1"
write (DBGUNIT,1122) (START_PRICE(ii)*MC_JPGDP(32) , ii = 19, 61)

write(DBGUNIT, '(//,A)') "XStart Prices1"
write (DBGUNIT,3333) (XSTART_PRICE(ii)*MC_JPGDP(32) , ii = 19, 91)

write(DBGUNIT, '(//,A)') "WTI Prices1"
write (DBGUNIT,11222) (WTI_PRICE(ii)*MC_JPGDP(32) , ii = 19, 61)
11222          format (2x, 43F10.2)

write(DBGUNIT, '(//,A)') "XWTI Prices1"
write (DBGUNIT,33333) (XWTI_PRICE(ii)*MC_JPGDP(32) , ii = 19, 91)
33333          format (2x, 73F10.2)

end if


      !BRENT_PRICE(CURIYR) = BRENT_PRICE(CURIYR)/MC_JPGDP(32)
      !START_PRICE(CURIYR) = START_PRICE(CURIYR)/MC_JPGDP(32)
      !WTI_PRICE(CURIYR) = WTI_PRICE(CURIYR)/MC_JPGDP(32)




!C_MC_Crude_Demand(c,1989+CURIYR)
!LFMM_PurchaseForeign_Crude(c,1989+CURIYR)
!Q_Non_US_Demand(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)
!write(20, '(a5, 8x, 50F10.2)') 'PETTR', (PETTR(region,year), year=2001, 2050)

!do c=1,MNCRUD
 !       Q_Crude(c,1989+CURIYR) =  Q_Eql(1989+CURIYR)*Cr_Type_Share(c,1989+CURIYR)- sum(OGCRDPRD(:,c,CURIYR), 1)
!end do



      !Let the WOP in 2050 be the same untill 2080 ****** 1989+MNUMYR+Max_LFMM_FYR (2071)
      if (CURIYR .EQ. LASTYR) then
        do t=LASTYR+1, MNXYR
            P_Eql(1989+t) = P_Eql(1989+LASTYR)
            Q_Eql(1989+t) = Q_Eql(1989+LASTYR)
        end do
      end if

      !Build total world supply curve
      !if no feedback, then set P_Total_Crude to P_Total_Crude_Init and Q_Total_Crude to Q_Total_Crude_Init
      !if it's a price case
      if (FDBK_SW .EQ. 0) then
            do iSt = 1, CRSTEP
                P_Total_Crude(iSt, 1989+CURIYR) = P_Total_Crude_Init(iSt, 1989+CURIYR)
                Q_Total_Crude(iSt, 1989+CURIYR) = Q_Total_Crude_Init(iSt, 1989+CURIYR)
            end do
      end if

      !if feedback is turned on, then rebuild, for each year, the incremental 14 steps supply curve
      !if it's not a price case
      if (FDBK_SW .EQ. 1) then
        do t = 1, CRSTEP
            P_Start = P_Eql(1989+CURIYR)*(1+BP(t))
            P_End   = P_Eql(1989+CURIYR)*(1+BP(t+1))

            Q_Start = Q_Eql(1989+CURIYR)*(P_Start/P_Eql(1989+CURIYR))**S_E(1989+CURIYR)
            Q_End   = Q_Eql(1989+CURIYR)*(P_End/P_Eql(1989+CURIYR))**S_E(1989+CURIYR)

            P_Total_Crude(t, 1989+CURIYR) = (P_Start+P_End)/2
            Q_Total_Crude(t, 1989+CURIYR) = (Q_End-Q_Start)
        end do
      end if

      !Change to 1987 dollars
      do iSt = 1, CRSTEP
        P_Total_Crude(iSt, 1989+CURIYR) = P_Total_Crude(iSt, 1989+CURIYR)/MC_JPGDP(32)
      end do

      !Fill up values from 2050 to 2080 with values from 2040
            if (CURIYR .EQ. LASTYR) then
                do t=LASTYR+1, MNXYR
                    do iSt = 1, CRSTEP
                        P_Total_Crude(iSt, 1989+t) = P_Total_Crude(iSt, 1989+LASTYR)
                        Q_Total_Crude(iSt, 1989+t) = Q_Total_Crude(iSt, 1989+LASTYR)
                    end do
                end do
            end if

      !Build the international crude supply curves
      !First, crude type prices at equilibrium
      do t=1990, 1989+MNXYR
        P_Crude(1,t)=P_Eql(t)
        P_Crude(6,t)=P_Heavy_Sour(t)
      end do

      do c=2,MNCRUD
        P_Crude(c,1989+CURIYR) = (P_Crude(6,1989+CURIYR)-Cr_Type_Coeff(c,1989+CURIYR)*P_Crude(1,1989+CURIYR))/(1-Cr_Type_Coeff(c,1989+CURIYR))
      end do

      !Set prices after 2040 to the 2040 level    
      if (CURIYR .EQ. LASTYR) then
        do t=LASTYR+1, MNXYR
            do c=2,MNCRUD
                P_Crude(c,1989+t) = P_Crude(c,1989+LASTYR)  !test here
            end do
        end do
      end if

!***********************************
!***********************************
!testing purposses
if (CURIYR .EQ. 23) then
 kk=23
end if
      !Second, foreign crude type quantities at equilibrium
      do c=1,MNCRUD
        LFMM_Export_Crude(c,1989+CURIYR)=0
 !       do ii=1,MNUMPR-1
        
        
        
! if ( (CURIYR .GE. 32) .OR.(CURIYR .LE. 38)  )  then 
!        LFMM_Export_Crude(c,1989+CURIYR) = LFMM_Export_Crude(c,1989+CURIYR)+Q_CRUDE_EXPORTS(ii,c,1989+CURIYR)*1.08
!elseif (CURIYR .EQ. 31) then
!        LFMM_Export_Crude(c,1989+CURIYR) = LFMM_Export_Crude(c,1989+CURIYR)+Q_CRUDE_EXPORTS(ii,c,1989+CURIYR)*1.140          
! elseif (CURIYR .GE. 44) then
!        LFMM_Export_Crude(c,1989+CURIYR) = LFMM_Export_Crude(c,1989+CURIYR)+Q_CRUDE_EXPORTS(ii,c,1989+CURIYR)*1.112
! elseif (CURIYR .GE. 45) then
!        LFMM_Export_Crude(c,1989+CURIYR) = LFMM_Export_Crude(c,1989+CURIYR)+Q_CRUDE_EXPORTS(ii,c,1989+CURIYR)*1.103
! elseif (CURIYR .GE. 46) then
!        LFMM_Export_Crude(c,1989+CURIYR) = LFMM_Export_Crude(c,1989+CURIYR)+Q_CRUDE_EXPORTS(ii,c,1989+CURIYR)*1.108
! elseif (CURIYR .GE. 47) then
!        LFMM_Export_Crude(c,1989+CURIYR) = LFMM_Export_Crude(c,1989+CURIYR)+Q_CRUDE_EXPORTS(ii,c,1989+CURIYR)*1.108
! else
!        LFMM_Export_Crude(c,1989+CURIYR) = LFMM_Export_Crude(c,1989+CURIYR)+Q_CRUDE_EXPORTS(ii,c,1989+CURIYR)*1.12
! endif
 
  !if ( (CURIYR .EQ. 30)  )  then 
  !LFMM_Export_Crude(10,1989+CURIYR) = 1330.00
  !endif
  !
  !
  !if ( (CURIYR .EQ. 31)  )  then 
  !LFMM_Export_Crude(10,1989+CURIYR) = 1460.00
  !endif
  !
  !      
  !      if ( (CURIYR .EQ. 38)  )  then 
  !      LFMM_Export_Crude(c,1989+CURIYR) = LFMM_Export_Crude(c,1989+CURIYR)*0.9983         
  !      endif
  !      
  !      if ((CURIYR .EQ. 32)  )  then 
  !      LFMM_Export_Crude(c,1989+CURIYR) = LFMM_Export_Crude(c,1989+CURIYR)*1.005
  !      endif 
        
  !      end do

        if(EXP_CRD .EQ. 1) then
            Q_Crude(c,1989+CURIYR) = Q_Eql(1989+CURIYR)*Cr_Type_Share(c,1989+CURIYR)- ( sum (OGCRUDEREF(:,c,CURIYR), 1)-OGCRUDEREF(10,c,CURIYR) )*(1000.0/365.0)
        else
            Q_Crude(c,1989+CURIYR) = Q_Eql(1989+CURIYR)*Cr_Type_Share(c,1989+CURIYR)- ( sum (OGCRUDEREF(:,c,CURIYR), 1)-OGCRUDEREF(10,c,CURIYR) )*(1000.0/365.0)!+LFMM_Export_Crude(c,1989+CURIYR)
        end if                                                                         
                                                                                       !( sum (OGCRUDEREF(:,c,CURIYR), 1)-OGCRUDEREF(10,c,CURIYR) )
        !if (c .EQ. 6) then
        !    Q_Crude(c,1989+CURIYR) = Q_Eql(1989+CURIYR)* ( Cr_Type_Share(c,1989+CURIYR)-( ( sum(OGCRDPRD(:,c,CURIYR), 1)-OGCRDPRD(13,c,CURIYR) )*(1000.0/365.0))/Q_Eql(1989+CURIYR)   ) - ( sum(OGCRDPRD(:,c,CURIYR), 1)-OGCRDPRD(13,c,CURIYR) )*(1000.0/365.0)
        !end if

        if (c .EQ. 7) then
            Q_Crude(c,1989+CURIYR) = Q_Eql(1989+CURIYR)*( ( sum(OGCRDPRD(:,c,CURIYR), 1)-OGCRDPRD(13,c,CURIYR) )*(1000.0/365.0))/Q_Eql(1989+CURIYR) - ( sum(OGCRDPRD(:,c,CURIYR), 1)-OGCRDPRD(13,c,CURIYR) )*(1000.0/365.0)
        end if

      end do

 rr= ( sum(OGCRDPRD(:,7,CURIYR), 1)-OGCRDPRD(13,7,CURIYR) )*(1000.0/365.0)
      Q_Crude(6,1989+CURIYR) = Q_Crude(6,1989+CURIYR) - ( sum(OGCRDPRD(:,7,CURIYR), 1)-OGCRDPRD(13,7,CURIYR) )*(1000.0/365.0)


      !Check if world share of a crude is bigger than that crude domestic production
      do c=1,MNCRUD
        if( Q_Crude(c,1989+CURIYR) .LT. 0) then
            !Write message to INTLDBG file!
                 write (DBGUNIT, 1000) CURCALYR, CURIRUN, CURITR, c, Q_Crude(c,1989+CURIYR)
            1000 format (2x, "Foreign Crude Supply is negative:" 2x, "Year: " I4, 2x, "Cycle: " I2, 2x, "Iteration: " I2, 2x, "Crude type: " I2, 2x, "Crude supply: " F10.2)
        end if
      end do


      !Set quantities after 2040 to the 2040 level
      if (CURIYR .EQ. LASTYR) then
        do t=LASTYR+1, MNXYR
            do c=1,MNCRUD
                Q_Crude(c,1989+t) = Q_Crude(c,1989+LASTYR)
            end do
        end do
      end if
  
  !Hybrid version
  !!!!!!!!!!!!!!!!!!!!!!!!!!
      !!This is to change crude prices in case it is exported. And this is Hybrid version!!!!!!
     
      qLight = Q_Crude(11,1989+CURIYR) + Q_Crude(10,1989+CURIYR) + Q_Crude(1,1989+CURIYR) + Q_Crude(2,1989+CURIYR) 
      qMedium = Q_Crude(3,1989+CURIYR) + Q_Crude(4,1989+CURIYR) 
      qHeavy = Q_Crude(5,1989+CURIYR) + Q_Crude(5,1989+CURIYR)
      
      do c=1,MNCRUD
          !only for crudes allowed to be exported
          if ((c .NE. 7) .AND. (c .NE. 8) .AND. (c .NE. 9)) then
 
            LFMM_Export_Crude(c,1989+CURIYR)=0
            do ii=1,MNUMPR-2 !REFREG   
            
             LFMM_Export_Crude(c,1989+CURIYR) = LFMM_Export_Crude(c,1989+CURIYR)+Q_CRUDE_EXPORTS(ii,c,1989+CURIYR)-Q_CRUDE_TO_CAN(ii,c,1989+CURIYR)
            
!if ( (CURIYR .GE. 32) .OR.(CURIYR .LE. 38)  )  then 
!        LFMM_Export_Crude(c,1989+CURIYR) = LFMM_Export_Crude(c,1989+CURIYR)+Q_CRUDE_EXPORTS(ii,c,1989+CURIYR)*1.08-Q_CRUDE_TO_CAN(ii,c,1989+CURIYR)
!elseif (CURIYR .EQ. 31) then
!        LFMM_Export_Crude(c,1989+CURIYR) = LFMM_Export_Crude(c,1989+CURIYR)+Q_CRUDE_EXPORTS(ii,c,1989+CURIYR)*1.140-Q_CRUDE_TO_CAN(ii,c,1989+CURIYR)        
! elseif (CURIYR .GE. 44) then
!        LFMM_Export_Crude(c,1989+CURIYR) = LFMM_Export_Crude(c,1989+CURIYR)+Q_CRUDE_EXPORTS(ii,c,1989+CURIYR)*1.112-Q_CRUDE_TO_CAN(ii,c,1989+CURIYR)
! elseif (CURIYR .GE. 45) then
!        LFMM_Export_Crude(c,1989+CURIYR) = LFMM_Export_Crude(c,1989+CURIYR)+Q_CRUDE_EXPORTS(ii,c,1989+CURIYR)*1.103-Q_CRUDE_TO_CAN(ii,c,1989+CURIYR)
! elseif (CURIYR .GE. 46) then
!        LFMM_Export_Crude(c,1989+CURIYR) = LFMM_Export_Crude(c,1989+CURIYR)+Q_CRUDE_EXPORTS(ii,c,1989+CURIYR)*1.108-Q_CRUDE_TO_CAN(ii,c,1989+CURIYR)
! elseif (CURIYR .GE. 47) then
!        LFMM_Export_Crude(c,1989+CURIYR) = LFMM_Export_Crude(c,1989+CURIYR)+Q_CRUDE_EXPORTS(ii,c,1989+CURIYR)*1.108-Q_CRUDE_TO_CAN(ii,c,1989+CURIYR)
! else
!        LFMM_Export_Crude(c,1989+CURIYR) = LFMM_Export_Crude(c,1989+CURIYR)+Q_CRUDE_EXPORTS(ii,c,1989+CURIYR)*1.12-Q_CRUDE_TO_CAN(ii,c,1989+CURIYR)
! endif
 
  !if ( (CURIYR .EQ. 30)  )  then 
  !LFMM_Export_Crude(10,1989+CURIYR) = 1330.00 
  !endif
  !
  !
  !if ( (CURIYR .EQ. 31)  )  then 
  !LFMM_Export_Crude(10,1989+CURIYR) = 1460.00
  !endif
  !
  !
  !if ( (CURIYR .EQ. 38)  )  then
  !LFMM_Export_Crude(c,1989+CURIYR) = LFMM_Export_Crude(c,1989+CURIYR)*0.9983
  !endif
  !
  ! if ((CURIYR .EQ. 32)  )  then 
  !      LFMM_Export_Crude(c,1989+CURIYR) = LFMM_Export_Crude(c,1989+CURIYR)*1.005
  ! endif 
            end do
            
           
            if ( (c .EQ. 11) .OR. (c .EQ. 10) .OR. (c .EQ. 1) .OR. (c .EQ. 2) ) then
P_Crude_Exp(c,1989+CURIYR) = P_Crude(c,1989+CURIYR)*EXP(LOG(( Q_Crude(c,1989+CURIYR) + LFMM_Export_Crude(c,1989+CURIYR) )/ Q_Crude(c,1989+CURIYR))/( D_E(1989+CURIYR) - S_E(1989+CURIYR) - 1.25 ))
                P_Crude(c,1989+CURIYR) = P_Crude_Exp(c,1989+CURIYR)
            else if ( (c .EQ. 3) .OR. (c .EQ. 4) ) then
P_Crude_Exp(c,1989+CURIYR) = P_Crude(c,1989+CURIYR)*EXP(LOG(( qMedium + LFMM_Export_Crude(c,1989+CURIYR) )/( qMedium ))/(D_E(1989+CURIYR) - S_E(1989+CURIYR)))
                P_Crude(c,1989+CURIYR) = P_Crude_Exp(c,1989+CURIYR)
            else if ( (c .EQ. 5) .OR. (c .EQ. 6) ) then
P_Crude_Exp(c,1989+CURIYR) = P_Crude(c,1989+CURIYR)*EXP(LOG(( qHeavy + LFMM_Export_Crude(c,1989+CURIYR) )/( qHeavy ))/(D_E(1989+CURIYR) - S_E(1989+CURIYR)))
                P_Crude(c,1989+CURIYR) = P_Crude_Exp(c,1989+CURIYR)
            end if    
          
          end if !only for crude allowed to be exported
      end do
      
     
      !!!!!!!!!!!!!!!!!!!!!!!!!!
      !!EnD This is to change crude prices in case it is exported
  
  !End Hybrid version    
      
      

      !Third, build Foreign crude supply curves
      do c=1,MNCRUD
        do t=1,CISTEP
! if (c .EQ. 6) then
            P_Start = P_Crude(c,1989+CURIYR)*(1+BP(t))
            P_End   = P_Crude(c,1989+CURIYR)*(1+BP(t+1))


            Q_Start = Q_Crude(c,1989+CURIYR)*(P_Start/P_Crude(c,1989+CURIYR))**S_E(1989+CURIYR)
            Q_End   = Q_Crude(c,1989+CURIYR)*(P_End/P_Crude(c,1989+CURIYR))**S_E(1989+CURIYR)
            !condensate should be a flatter curve
            if ((c .EQ. 11) .OR. (c .EQ. 5) .OR. (c .EQ. 6) .OR. (c .EQ. 9)) then
                Q_Start = Q_Crude(c,1989+CURIYR)*(P_Start/P_Crude(c,1989+CURIYR))**(S_E(1989+CURIYR)+1.25)
                Q_End   = Q_Crude(c,1989+CURIYR)*(P_End/P_Crude(c,1989+CURIYR))**(S_E(1989+CURIYR)+1.25)
            end if    
            !end condensate should be a flatter curve
            
!******************Syncrude and dilbit prices can go very low P_Step
P_Step = (P_Start+P_End)/2
if((c .EQ. 8) .AND. ( P_Step .LE. 24.0)) then
     !P_Step = 24.0 - 3.0/t
else if ((c .EQ. 9) .AND. ( P_Step .LE. 22.0)) then
     !P_Step = 22.0 - 3.0/t   
end if    
    
!******************Syncrude and dilbit prices can go very low

            !P_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)=(P_Start+P_End)/2 - P_Crude(1,1989+CURIYR) 
            P_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)= P_Step - P_Crude(1,1989+CURIYR)
            Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)=(Q_End-Q_Start)
            
             !if ((c .EQ. 5) .OR. (c .EQ. 6) .OR. (c .EQ. 9)) then
            
             if ((c .EQ. 1) .OR. (c .EQ. 2) .OR. (c .EQ. 3) .OR. (c .EQ. 4) .OR. (c .EQ. 10) .OR. (c .EQ. 11)) then   
                 !P_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)= P_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)*1.05
                 Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)= Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)*1.00
             end if
             !this is 255 version  
            
             if ( ((c .EQ. 5) .OR. (c .EQ. 6) .OR. (c .EQ. 9)) .AND. ( (CURIYR .LE. 30) .OR. (37 .LE. CURIYR) ) ) then
             !if ( ((c .EQ. 5) .OR. (c .EQ. 6) .OR. (c .EQ. 9)) .AND. ( (30 .LE. CURIYR) ) ) then
                 P_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)= P_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)*1.00
                 Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)= Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)*1.00
                 if ((c .EQ. 9) .AND. (26 .LE. CURIYR) .AND. (CURIYR .LE. 30) ) then
                    Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)= Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)*1.00
                 end if
             end if
             
             !a little heavier berween 2020-2025
             if ( ((c .EQ. 5) .OR. (c .EQ. 6) .OR. (c .EQ. 9)) .AND. ( (CURIYR .LE. 36) .AND. (31 .LE. CURIYR) ) ) then
             !if ( ((c .EQ. 5) .OR. (c .EQ. 6) .OR. (c .EQ. 9)) .AND. ( (30 .LE. CURIYR) ) ) then
                 P_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)= P_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)*1.00
                 Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)= Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)*1.00                 
             end if
             
              
             
             
             
                     !if ( ((c .EQ. 5) .OR. (c .EQ. 6) .OR. (c .EQ. 9)) .AND. ( (CURIYR .GE. 48)  ) ) then
                     !   Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)= Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)*0.96
                     !end if
                     !
                     ! if ( ((c .EQ. 5) .OR. (c .EQ. 6) .OR. (c .EQ. 9)) .AND. ( (CURIYR .GE. 37)  ) ) then
                     !   Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)= Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)*1.03
                     !end if
                     !
                     !if ( ((c .EQ. 5) .OR. (c .EQ. 6) .OR. (c .EQ. 9)) .AND. ((CURIYR .EQ. 37) .OR.(CURIYR .EQ. 38) .OR. (CURIYR .EQ. 42) .OR. (CURIYR .EQ. 44).OR. (CURIYR .EQ. 45).OR. (CURIYR .EQ. 46).OR. (CURIYR .EQ. 47) ) ) then
                     !   Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)= Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)*1.03
                     !end if
             
             
             !if ( ((c .EQ. 5) ) .AND. (CURIYR .EQ. 31) )  then        !if ( (CURIYR .GE. 32) .OR.(CURIYR .LE. 38)  )  then
             !   Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)= Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)*1.10
             !end if
             !if ( ((c .EQ. 5) ) .AND. (CURIYR .EQ. 32) )  then
             !   Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)= Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)*1.15
             !end if
             !if ( ((c .EQ. 5) ) .AND. (CURIYR .EQ. 33) )  then
             !   Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)= Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)*1.20
             !end if
             !if ( ((c .EQ. 5) ) .AND. (CURIYR .EQ. 34) )  then
             !   Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)= Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)*1.25
             !end if
             !if ( ((c .EQ. 5) ) .AND. (CURIYR .EQ. 35) )  then
             !   Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)= Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)*1.30
             !end if
             !if ( ((c .EQ. 5) ) .AND. (CURIYR .EQ. 36) )  then
             !   Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)= Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)*1.35
             !end if
             
             
             
!             if ( ((c .EQ. 5) ) .AND. ( (31 .LE. CURIYR) .AND. (CURIYR .LE. 36) ) ) then
!                Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)= Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)*1.25
!             end if
             
!               if ( (c .EQ. 5)  .AND. ( (CURIYR .GE. 31) .AND. (36 .GE. CURIYR) ) ) then
!                    Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)= Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)*2           
!               end if
!               
!               if ( (c .EQ. 6)  .AND. ( (CURIYR .GE. 31) .AND. (36 .GE. CURIYR) ) ) then
!                    Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)= Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)*6.2           
!               end if
!               
!               if ( (c .EQ. 9)  .AND. ( (CURIYR .GE. 31) .AND. (36 .GE. CURIYR) ) ) then
!                    Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)= Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)/2.2           
!               end if
!end if

        end do
      end do

 !******************************
!For canadian crudes
!do t=9,CISTEP
!   !P_Foreign_Crude(8,Max_Crude_Source,t,1989+CURIYR)=P_Foreign_Crude(8,Max_Crude_Source,t,1989+CURIYR)+100 
!   Q_Foreign_Crude(8,Max_Crude_Source,t,1989+CURIYR)=0
!end do 
!
!do t=9,CISTEP
!  !P_Foreign_Crude(9,Max_Crude_Source,t,1989+CURIYR)=P_Foreign_Crude(9,Max_Crude_Source,t,1989+CURIYR)+100
!  Q_Foreign_Crude(9,Max_Crude_Source,t,1989+CURIYR)=0
!end do
 !******************************


      !Change P_Foreign_Crude to 1987 dollars
      do c=1,MNCRUD
        do t = 1, CISTEP
            P_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR) = P_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+CURIYR)/MC_JPGDP(32)
        end do
      end do

      !Fill up values from 2040 to 2080 with values from 2040
      if (CURIYR .EQ. LASTYR) then
        do iYr=LASTYR+1, MNXYR
            do c=1,MNCRUD
                do t = 1, CISTEP
                    P_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+iYr) = P_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+LASTYR)
                    Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+iYr) = Q_FOREIGN_CRUDE(c,Max_Crude_Source,t,1989+LASTYR)
                end do
            end do
        end do
      end if

      !Buid NonUS crude demand curves
!this is to avoid bad restart file starting numbers for Q_CRUDE_IMPORTS
!*******************************************************************************
!*******************************************************************************
!do iR=1, REFREG+1
!    do iY=1,MNXYR
!        Q_CRUDE_IMPORTS(iR,8,1989+iY)=0.0
!        Q_CRUDE_IMPORTS(iR,9,1989+iY)=0.0
!    end do
!end do

        


      !Quantities and Prices
      do c=1,MNCRUD
        LFMM_PurchaseForeign_Crude(c,1989+CURIYR)=0
        LFMM_Export_Crude(c,1989+CURIYR)=0
        do ii=1,MNUMPR-1 !REFREG
          LFMM_PurchaseForeign_Crude(c,1989+CURIYR) = LFMM_PurchaseForeign_Crude(c,1989+CURIYR)+Q_CRUDE_IMPORTS(ii,c,1989+CURIYR)
          LFMM_Export_Crude(c,1989+CURIYR) = LFMM_Export_Crude(c,1989+CURIYR)+Q_CRUDE_EXPORTS(ii,c,1989+CURIYR)
          Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_Crude(c,1989+CURIYR)-LFMM_PurchaseForeign_Crude(c,1989+CURIYR)+LFMM_Export_Crude(c,1989+CURIYR)
        end do
        !
        !if( ( (CURIYR .GE. 41) .AND. (CURIYR .LE. 45) ) .AND. ( (c.EQ.1) .OR. (c.EQ.3) .OR. (c.EQ.4) .OR. (c.EQ.10) .OR. (c.EQ.11) ) ) then         
        !      LFMM_Export_Crude(c,1989+CURIYR) = LFMM_Export_Crude(c,1989+CURIYR)*1.4
        !      Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR) = Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0282
        !      
        !else if (  (CURIYR .GE. 46) .AND. ( (c.EQ.1) .OR. (c.EQ.2) .OR. (c.EQ.3) .OR. (c.EQ.4) .OR. (c.EQ.10) .OR. (c.EQ.11) ) ) then   
        !      LFMM_Export_Crude(c,1989+CURIYR) = LFMM_Export_Crude(c,1989+CURIYR)*1.4
        !      Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR) = Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0288
        !
        !else if(  (CURIYR .LE. 40) .AND. ( (c.EQ.1) .OR. (c.EQ.2) .OR. (c.EQ.3) .OR. (c.EQ.4) .OR. (c.EQ.10) .OR. (c.EQ.11) ) ) then   
        !      LFMM_Export_Crude(c,1989+CURIYR) = LFMM_Export_Crude(c,1989+CURIYR)*1.48
        !      Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR) = Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.034
        !end if
        !
        !
        !if(  (CURIYR .GE. 49) .AND. ( (c.EQ.1) .OR. (c.EQ.3) .OR. (c.EQ.4) .OR. (c.EQ.10) .OR. (c.EQ.11) ) ) then         
        !      Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR) = Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9957
        !end if
        !
        !
        !
        ! if ( ( CURIYR .EQ. 32 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then 
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0000
        ! else if ( ( CURIYR .EQ. 33 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9980
        ! 
        ! else if ( ( CURIYR .EQ. 34 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0001
        ! else if  ( ( CURIYR .EQ. 35 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9999
        ! 
        ! else if  ( ( CURIYR .EQ. 36 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9995
        ! else if  ( ( CURIYR .EQ. 37 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9995
        ! else if  ( ( CURIYR .EQ. 38 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0002
        ! else if  ( ( CURIYR .EQ. 39 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9997
        ! else if  ( ( CURIYR .EQ. 40 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9988
        ! else if  ( ( CURIYR .EQ. 41 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0006
        !  else if  ( ( CURIYR .EQ. 42 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0009
        !  else if  ( ( CURIYR .EQ. 43 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0006
        ! else if  ( ( CURIYR .EQ. 44 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0009  
        ! else if  ( ( CURIYR .EQ. 45 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9998
        ! 
        ! else if  ( ( CURIYR .EQ. 46 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9991
        ! else if  ( ( CURIYR .EQ. 47 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9985  
        ! 
        ! else if  ( ( CURIYR .EQ. 48 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9972
        ! 
        ! else if  ( ( CURIYR .EQ. 49 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9996
        ! else if  ( ( CURIYR .EQ. 50 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9997
        ! else if  ( ( CURIYR .EQ. 51 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0003
        ! else if  ( ( CURIYR .EQ. 52 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9994
        ! 
        ! 
        ! else if  ( ( CURIYR .EQ. 53 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9999
        ! else if  ( ( CURIYR .EQ. 54 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9997
        ! else if  ( ( CURIYR .EQ. 55 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9995
        ! else if  ( ( CURIYR .EQ. 56 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9996
        ! else if  ( ( CURIYR .EQ. 57 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9999
        ! else if  ( ( CURIYR .EQ. 58 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9996      
        !          
        !      
        ! else if  ( ( CURIYR .EQ. 59 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9986
        ! 
        ! else if  ( ( CURIYR .EQ. 60 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0008
        ! else if  ( ( CURIYR .EQ. 61 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
        ! Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0005
        ! endif
        ! 
        
        
        
        
        
         !   
         ! if  ((EXP_CRD .EQ. 1) .AND. (CURIYR .EQ. 48) )then         
         !  Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9987
         !end if
         !
         !if  ((EXP_CRD .EQ. 1) .AND. (CURIYR .EQ. 49) )then          
         !  Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9982
         !end if
         !
         !if  ((EXP_CRD .EQ. 1) .AND. (CURIYR .EQ. 58) )then          
         !  Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.003
         !end if
         !
         !
         !
         !if ( ( CURIYR .EQ. 32 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
         !Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0131
         !else if  ( ( CURIYR .EQ. 33 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
         !Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0109
         !else if  ( ( CURIYR .EQ. 34 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
         !Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0100
         !else if  ( ( CURIYR .EQ. 35 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
         !Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0103
         !else if  ( ( CURIYR .EQ. 36 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
         !Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0092 
         !else if  ( ( CURIYR .EQ. 37 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
         !Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0100
         !else if  ( ( CURIYR .EQ. 38 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
         !Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0104
         !else if  ( ( CURIYR .EQ. 39 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
         !Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0094 
         !else if  ( ( CURIYR .EQ. 40 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
         !Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0096 
         !
         !else if  ( ( CURIYR .EQ. 41 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
         !Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9996
         !else if  ( ( CURIYR .EQ. 42 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
         !Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9997 
         !else if  ( ( CURIYR .EQ. 43 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
         !Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0000   
         !
         !else if  ( ( CURIYR .EQ. 44 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
         !Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.9997 
         !else if  ( ( CURIYR .EQ. 45 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
         !Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.9997 
         !else if  ( ( CURIYR .EQ. 46 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
         !Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0002 
         !
         !else if  ( ( CURIYR .EQ. 48 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
         !Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0006
         !else if  ( ( CURIYR .EQ. 49 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
         !Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9999
         !else if  ( ( CURIYR .EQ. 50 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
         !Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9996
         !else if  ( ( CURIYR .EQ. 54 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
         !Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0002
         !else if  ( ( CURIYR .EQ. 55 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
         !Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9999
         !else if  ( ( CURIYR .EQ. 56 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
         !Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0001
         !else if  ( ( CURIYR .EQ. 57 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
         !Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0001
         !else if  ( ( CURIYR .EQ. 58 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
         !Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9998
         !else if  ( ( CURIYR .EQ. 59 ) .AND. ( (c .EQ. 10) .OR. (c .EQ. 11) ) )then
         !Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*1.0001 
         !
         !end if
         !
         !
         !
         !if (CURIYR .EQ. 60) then          
         !  Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9980
         !else if (CURIYR .EQ. 61) then          
         !  Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)*0.9974
         !end if  
         !
         !
         
      
        
         
       
     
       

!*******************************************************************************
!*******************************************************************************
        !this is to avoid bad restart file starting numbers for Q_CRUDE_IMPORTS
        if (Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR) .LT. 0.0) then
            do ii=1,REFREG
                Q_CRUDE_IMPORTS(ii,c,1989+CURIYR) = 0.0
            end do

            Q_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=Q_Crude(c,1989+CURIYR)
        end if

!*******************************************************************************
!*******************************************************************************
        P_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=P_Crude(c,1989+CURIYR)
      end do

      !Change P_Non_US_Demand to 1987 dollars 
      do c=1,MNCRUD
        P_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)=P_NON_US_DEMAND(c, Max_Crude_Source, Max_NonUS_Demand_Steps,1989+CURIYR)/MC_JPGDP(32)
      end do

      !Buid NonUS crude demand curves curves into US from 2040 to 2080 with values from 2040
      !Fill up values from 2040 to 2080 with values from 2040 q
      if (CURIYR .EQ. LASTYR) then
          do iYr=LASTYR+1, MNXYR
            do c=1,MNCRUD
                P_NON_US_DEMAND(c,Max_Crude_Source,Max_NonUS_Demand_Steps,1989+iYr) = P_NON_US_DEMAND(c,Max_Crude_Source,Max_NonUS_Demand_Steps,1989+LASTYR)
                Q_NON_US_DEMAND(c,Max_Crude_Source,Max_NonUS_Demand_Steps,1989+iYr) = Q_NON_US_DEMAND(c,Max_Crude_Source,Max_NonUS_Demand_Steps,1989+LASTYR)
            end do
          end do
      end if

      !**************************************************************************

      !Build export demand curves for Products
      
      !This is to shift exp demand curves proportional with the amount of crude exports only for HRX case!!!!!!
      
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$       
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$       
       
!if ( (FDBK_SW .EQ. 1) .AND. (EXP_CRD .EQ. 1) .AND. (EXP_LEASE .EQ. 1) ) then !only for hrx case
      
!RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
!RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
      
!      LFMM_TOTAL_EXP(1989+CURIYR) = 0.0
!      do c=1,MNCRUD
!          if ((c .NE. 7) .AND. (c .NE. 8) .AND. (c .NE. 9)) then !crude allowed for exports
!     
!                LFMM_Export_Crude(c,1989+CURIYR)=0
!                do ii=1,MNUMPR-2 !REFREG               
!                  LFMM_Export_Crude(c,1989+CURIYR) = LFMM_Export_Crude(c,1989+CURIYR)+Q_CRUDE_EXPORTS(ii,c,1989+CURIYR)-Q_CRUDE_TO_CAN(ii,c,1989+CURIYR)
!                end do
!                
!                !LFMM_TOTAL_EXP(1989+CURIYR) = LFMM_TOTAL_EXP(1989+CURIYR) + LFMM_Export_Crude(c,1989+CURIYR)
!                LFMM_TOTAL_EXP(1989+CURIYR) = -RFIMCR(MNUMPR,CURIYR)*1000 + ILPOtherPct(1989+CURIYR)*1000
!                
!          end if !end crude allowed for exports  LFMM_TOTAL_EXP(1990:1989+MNXYR)    
!      end do
      if(CURITR .EQ. 1) then !IHPOtherPct
         IHPOtherPct(1989+CURIYR) = 0.0 
      else
         IHPOtherPct(1989+CURIYR) = -RFIMCR(MNUMPR,CURIYR)*1000 + ILPOtherPct(1989+CURIYR)*1000
      end if   
      
      
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

LFMM_TOTAL_EXP(1989+CURIYR) = 0.0
do c=1,MNCRUD
      LFMM_Export_Crude_Net(c,1989+CURIYR)=0
      do ii=1,MNUMPR-2 !REFREG               
        LFMM_Export_Crude_Net(c,1989+CURIYR) = LFMM_Export_Crude_Net(c,1989+CURIYR)+Q_CRUDE_EXPORTS(ii,c,1989+CURIYR)*1.25-Q_CRUDE_TO_CAN(ii,c,1989+CURIYR)
      end do
      LFMM_TOTAL_EXP(1989+CURIYR) = LFMM_TOTAL_EXP(1989+CURIYR) + LFMM_Export_Crude_Net(c,1989+CURIYR)
end do  
       
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$      
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$        
      
 !end if !end only for hrx case
 
 !RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
 !RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
 if(CURITR .EQ. 1) then
 do iPr =1,MNPROD
    do ii =1, MNXYR
        EXP_Q0(iPr, 1990:1989+ii) = EXP_Q(iPr, 1990:1989+ii)
    end do
 end do
 end if
 !RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR    
       
      !End This is to shift exp demand curves proportional with the amount of crude exports only for HRX case!!!!!!
 
      do iPr =1,MNPROD
      
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$      
      if(iPr .EQ. 15) then ! ( (iPr .EQ. 2) .OR. (iPr .EQ. 3) .OR. (iPr .EQ. 5) .OR. (iPr .EQ. 14) .OR. (iPr .EQ. 15) .OR. (iPr .EQ. 16))
      
      
        EXP_Q(iPr,1989+CURIYR) = EXP_Q0(iPr,1989+CURIYR) - 0.25*LFMM_TOTAL_EXP(1989+CURIYR)
    
    
!                if( CURIYR .EQ. 35) then 
!                    EXP_Q(iPr,1989+CURIYR) = EXP_Q(iPr,1989+CURIYR) - 100.0
!                end if
            
        !EXP_Q(iPr,1989+CURIYR) = EXP_Q(iPr,1989+CURIYR) - 0.6*(-RFIMCR(MNUMPR,CURIYR)*1000 + ILPOtherPct(1989+CURIYR)*1000 )
            if (EXP_Q(iPr,1989+CURIYR) .LE. 0.0)then
                  EXP_Q(iPr,1989+CURIYR) = 0.0
            end if
      else if  (iPr .EQ. 7)   then ! ( (iPr .EQ. 2) .OR. (iPr .EQ. 3) .OR. (iPr .EQ. 5) .OR. (iPr .EQ. 14) .OR. (iPr .EQ. 15) .OR. (iPr .EQ. 16))
    
    
        EXP_Q(iPr,1989+CURIYR) = EXP_Q0(iPr,1989+CURIYR) - 0.15*LFMM_TOTAL_EXP(1989+CURIYR)
      
      
        !EXP_Q(iPr,1989+CURIYR) = EXP_Q(iPr,1989+CURIYR) - 0.3*(-RFIMCR(MNUMPR,CURIYR)*1000 + ILPOtherPct(1989+CURIYR)*1000 )
            if (EXP_Q(iPr,1989+CURIYR) .LE. 0.0)then
                  EXP_Q(iPr,1989+CURIYR) = 0.0
            end if      
      else if (iPr .EQ. 12) then
       EXP_Q(iPr,1989+CURIYR) = EXP_Q0(iPr,1989+CURIYR) - 0.0*LFMM_TOTAL_EXP(1989+CURIYR)
       !EXP_Q(iPr,1989+CURIYR) = EXP_Q(iPr,1989+CURIYR) - 0.1*(-RFIMCR(MNUMPR,CURIYR)*1000 + ILPOtherPct(1989+CURIYR)*1000 )
          if (EXP_Q(iPr,1989+CURIYR) .LE. 0.0)then
                  EXP_Q(iPr,1989+CURIYR) = 0.0
          end if     
      end if 
  
    
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$      $$$$$$$$$$$$   
      
!      
!      if (CURCALYR .EQ. 2040 .AND. FCRL .EQ. 1) then
!    
!        write(DBGUNIT, '(//,A)') "22xx rfimcr first and ILPOtherPct second and ILPOtherPct-rfimcr third IHPOtherPct fourth last two must be the same"         
!        
!        !write (DBGUNIT,8484) (RFIMCR(MNUMPR,ii-1989)*1000, ii = 2008, 2050)
!        !write (DBGUNIT,8484) (ILPOtherPct(ii)*1000, ii = 2008, 2050)
!        write (DBGUNIT,8484) (LFMM_TOTAL_EXP(ii), ii = 2008, 2050)
!        
!!        write (DBGUNIT,8484) (-RFIMCR(MNUMPR,ii-1989)*1000 + ILPOtherPct(ii)*1000, ii = 2008, 2050)
!!        write (DBGUNIT,8484) (IHPOtherPct(ii), ii = 2008, 2050)
!        
!        write (DBGUNIT,8484) (EXP_Q(15,ii), ii = 2008, 2050) 
!        write (DBGUNIT,8484) (EXP_Q(7,ii), ii = 2008, 2050) 
!        write (DBGUNIT,8484) (EXP_Q(12,ii), ii = 2008, 2050)   
!        
!8484          format (2x, 43F10.2)      
!     end if   



!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$    
      
      
  
      
        if(EXP_P(iPr,1989+CURIYR) .NE. 0 ) then

           do t=1,INTLSTEP

            P_Start = ( EXP_P(iPr,1989+CURIYR) + 0.96*MC_JPGDP(32)*(BRENT_PRICE(CURIYR) - START_PRICE(CURIYR)) )*(1-BP(t))
            P_End   = ( EXP_P(iPr,1989+CURIYR) + 0.96*MC_JPGDP(32)*(BRENT_PRICE(CURIYR) - START_PRICE(CURIYR)) )*(1-BP(t+1))

            if (t .EQ.1) then
                Q_Start = 0.0
            else
                Q_Start = EXP_Q(iPr,1989+CURIYR)*(P_Start/( EXP_P(iPr,1989+CURIYR) + 0.96*MC_JPGDP(32)*(BRENT_PRICE(CURIYR) - START_PRICE(CURIYR)) ))**(D_E(1989+CURIYR)-0.15)
            end if

            Q_End   = EXP_Q(iPr,1989+CURIYR)*(P_End/( EXP_P(iPr,1989+CURIYR) + 0.96*MC_JPGDP(32)*(BRENT_PRICE(CURIYR) - START_PRICE(CURIYR)) ))**(D_E(1989+CURIYR)-0.15)

            Product_Export_P(iPr,1,t,CURIYR)=(P_Start+P_End)/2
            Product_Export_Q(iPr,1,t,CURIYR)=(Q_End-Q_Start)
			
			 

           end do !do t=1,INTLSTEP

        end if
      end do



      !Change Product_Export_P to 1987 dollars
      do iPr=1,MNPROD !for each product
       do t=1,INTLSTEP !for each step
        Product_Export_P(iPr,1,t,CURIYR)=Product_Export_P(iPr,1,t,CURIYR)/MC_JPGDP(32)
       end do         !end for each step
      end do          !end for each product

      !Build product export demand curves into US from 2040 to 2080 with values from 2040
       if (CURIYR .EQ. LASTYR) then
            do iYr=LASTYR+1, MNXYR
            do iPr=1,MNPROD !for each product
            do t=1,INTLSTEP !for each step
                Product_Export_P(iPr,1,t,iYr)=Product_Export_P(iPr,1,t,LASTYR)
                Product_Export_Q(iPr,1,t,iYr)=Product_Export_Q(iPr,1,t,LASTYR)
            end do         !end for each step
            end do         !end for each product
            end do
       end if

!if (CURIYR .EQ. LASTYR .AND. CURITR .EQ. 1) then
! write (DBGUNIT,44) (Product_Export_Q(1,1,ii,CURIYR-1), ii = 1, 14)
! write (DBGUNIT,44) (Product_Export_P(1,1,ii,CURIYR-1), ii = 1, 14)
!
! write (DBGUNIT,44) (Product_Export_Q(1,1,ii,CURIYR), ii = 1, 14)
! write (DBGUNIT,44) (Product_Export_P(1,1,ii,CURIYR), ii = 1, 14)
! !44          format ("Check supply curve" 4x, 14F10.2)
!
! write (DBGUNIT,44) (Product_Export_Q(1,1,ii,CURIYR+1), ii = 1, 14)
! write (DBGUNIT,44) (Product_Export_P(1,1,ii,CURIYR+1), ii = 1, 14)
! !44          format ("Check supply curve" 4x, 14F10.2)
!
! write (DBGUNIT,44) (Product_Export_Q(1,1,ii,61), ii = 1, 14)
! write (DBGUNIT,44) (Product_Export_P(1,1,ii,61), ii = 1, 14)
! !44          format ("Check supply curve" 4x, 14F10.2)
!
! write (DBGUNIT,44) (Product_Export_Q(1,1,ii,91), ii = 1, 14)
! write (DBGUNIT,44) (Product_Export_P(1,1,ii,91), ii = 1, 14)
! 44          format ("Check demand curve" 4x, 14F10.3)
!end if



      !End Build export demand curves for Products

      !**************************************************************************

 if (CURCALYR .EQ. 2040 .AND. FCRL .EQ. 1) then
    
        write(DBGUNIT, '(//,A)') "22xx rfimcr first and ILPOtherPct second and ILPOtherPct-rfimcr third IHPOtherPct fourth last two must be the same"         
        
        !write (DBGUNIT,8484) (RFIMCR(MNUMPR,ii-1989)*1000, ii = 2008, 2050)
        !write (DBGUNIT,8484) (ILPOtherPct(ii)*1000, ii = 2008, 2050)
        write (DBGUNIT,8484) (LFMM_TOTAL_EXP(ii), ii = 2008, 2050)
        
!        write (DBGUNIT,8484) (-RFIMCR(MNUMPR,ii-1989)*1000 + ILPOtherPct(ii)*1000, ii = 2008, 2050)
!        write (DBGUNIT,8484) (IHPOtherPct(ii), ii = 2008, 2050)
        
        write (DBGUNIT,8484) (EXP_Q(15,ii), ii = 2008, 2050) 
        write (DBGUNIT,8484) (EXP_Q(7,ii), ii = 2008, 2050) 
        write (DBGUNIT,8484) (EXP_Q(12,ii), ii = 2008, 2050) 
        
8484          format (2x, 43F10.2)      
     end if   



      !Build import supply curves for Products
      do iPr=1,MNPROD !for each product
       do t=1,INTLSTEP !for each step

         P_Start = ( IMP_P(iPr,1989+CURIYR) + 0.96*MC_JPGDP(32)*(BRENT_PRICE(CURIYR) - START_PRICE(CURIYR)) )*(1+BP(t))
         P_End   = ( IMP_P(iPr,1989+CURIYR) + 0.96*MC_JPGDP(32)*(BRENT_PRICE(CURIYR) - START_PRICE(CURIYR)) )*(1+BP(t+1))

         Q_Start = IMP_Q(iPr,1989+CURIYR)*(P_Start/( IMP_P(iPr,1989+CURIYR) + 0.96*MC_JPGDP(32)*(BRENT_PRICE(CURIYR) - START_PRICE(CURIYR)) ))**(S_E(1989+CURIYR)+0.15)
         Q_End   = IMP_Q(iPr,1989+CURIYR)*(P_End/( IMP_P(iPr,1989+CURIYR) + 0.96*MC_JPGDP(32)*(BRENT_PRICE(CURIYR) - START_PRICE(CURIYR)) ))**(S_E(1989+CURIYR)+0.15)

         
         
         Product_Import_P(iPr,1,t,CURIYR)=(P_Start+P_End)/2
         Product_Import_Q(iPr,1,t,CURIYR)=(Q_End-Q_Start)
         
         !!!!!!!!!!!!!!!!!
  !       if ( (iPr .EQ. 4) .OR. (iPr .EQ. 6) .OR. (iPr .EQ. 7) .OR. (iPr .EQ. 8) .OR. (iPr .EQ. 10) ) then
  !          Product_Import_Q(iPr,1,t,CURIYR) = Product_Import_Q(iPr,1,t,CURIYR)*1.5
  !          Product_Import_P(iPr,1,t,CURIYR) = Product_Import_P(iPr,1,t,CURIYR)/1.5
  !       end if
         
  !       if ( (iPr .EQ. 3) .OR. (iPr .EQ. 5) .OR. (iPr .EQ. 14) .OR. (iPr .EQ. 15) .OR. (iPr .EQ. 16) ) then
  !          Product_Import_Q(iPr,1,t,CURIYR) = Product_Import_Q(iPr,1,t,CURIYR)/1.5
  !          Product_Import_P(iPr,1,t,CURIYR) = Product_Import_P(iPr,1,t,CURIYR)*1.5
  !       end if          
         !!!!!!!!!!!!!!!!!
		 
		


       end do         !end for each step
      end do          !end for each product

     !Change Product_Import_P to 1987 dollars
      do iPr=1,MNPROD !for each product
       do t=1,INTLSTEP !for each step
        Product_Import_P(iPr,1,t,CURIYR)=Product_Import_P(iPr,1,t,CURIYR)/MC_JPGDP(32)
       end do         !end for each step
      end do          !end for each product

      !Build product import supply curves into US from 2040 to 2080 with values from 2040
       if (CURIYR .EQ. LASTYR) then
        do iYr=LASTYR+1, MNXYR
            do iPr=1,MNPROD !for each product
            do t=1,INTLSTEP !for each step
                Product_Import_P(iPr,1,t,iYr)=Product_Import_P(iPr,1,t,LASTYR)
                Product_Import_Q(iPr,1,t,iYr)=Product_Import_Q(iPr,1,t,LASTYR)
            end do         !end for each step
            end do         !end for each product
        end do
       end if

   !End Build import supply curves for Products
  !**************************************************************************
   !Some curves must behave as constants!
!   do iPr=1,MNPROD
!   if( (iPr .NE. 5) .AND. (iPr .NE. 7) .AND. (iPr .NE. 8) .AND. (iPr .NE. 15) .AND. (iPr .NE. 16) .AND. (iPr .NE. 13) .AND. (iPr .NE. 19)) then
!    do iYr=1, MNXYR
!        do t=1,INTLSTEP
!
!                Product_Import_Q(iPr,1,t,iYr) = 0.0
!                Product_Export_Q(iPr,1,t,iYr) = 0.0
!                Product_Import_P(iPr,1,t,iYr) = 0.0
!                Product_Export_P(iPr,1,t,iYr) = 0.0
!
!        end do !do t=1,INTLSTEP
!        Product_Import_Q(iPr,1,1,iYr) = IMP_Q(iPr,1989+iYr)*0.9
!        Product_Export_Q(iPr,1,1,iYr) = EXP_Q(iPr,1989+iYr)*0.9
!        Product_Import_Q(iPr,1,2,iYr) = IMP_Q(iPr,1989+iYr)*0.1
!        Product_Export_Q(iPr,1,2,iYr) = EXP_Q(iPr,1989+iYr)*0.1
!
!        Product_Import_P(iPr,1,1,iYr) = ( IMP_P(iPr,1989+CURIYR) + 0.96*MC_JPGDP(32)*(BRENT_PRICE(CURIYR) - START_PRICE(CURIYR)) )*0.01
!        Product_Export_P(iPr,1,1,iYr) = ( EXP_P(iPr,1989+CURIYR) + 0.96*MC_JPGDP(32)*(BRENT_PRICE(CURIYR) - START_PRICE(CURIYR)) )*100.0
!        Product_Import_P(iPr,1,2,iYr) = ( IMP_P(iPr,1989+CURIYR) + 0.96*MC_JPGDP(32)*(BRENT_PRICE(CURIYR) - START_PRICE(CURIYR)) )*0.02
!        Product_Export_P(iPr,1,2,iYr) = ( EXP_P(iPr,1989+CURIYR) + 0.96*MC_JPGDP(32)*(BRENT_PRICE(CURIYR) - START_PRICE(CURIYR)) )*90.0
!
!    end do !end do iYr=1, MNXYR
!   end if
!   end do ! end do iPr=1,MNPROD
   
   !Imports of PCF and GO3 should behave like constants!
!   do iYr=1, MNXYR
!        do t=1,INTLSTEP
!
!                Product_Import_Q(13,1,t,iYr) = 0.0  
!                Product_Import_P(13,1,t,iYr) = 0.0  
!                Product_Import_Q(19,1,t,iYr) = 0.0              
!                Product_Import_P(19,1,t,iYr) = 0.0                
!
!        end do !do t=1,INTLSTEP
!        
!        Product_Import_Q(13,1,1,iYr) = IMP_Q(13,1989+iYr)*0.9     
!        Product_Import_Q(13,1,2,iYr) = IMP_Q(13,1989+iYr)*0.1
!        Product_Import_Q(19,1,1,iYr) = IMP_Q(19,1989+iYr)*0.9    
!        Product_Import_Q(19,1,2,iYr) = IMP_Q(19,1989+iYr)*0.1 
!         
!
!        Product_Import_P(13,1,1,iYr) = ( IMP_P(13,1989+CURIYR) + 0.96*MC_JPGDP(32)*(BRENT_PRICE(CURIYR) - START_PRICE(CURIYR)) )*0.01    
!        Product_Import_P(13,1,2,iYr) = ( IMP_P(13,1989+CURIYR) + 0.96*MC_JPGDP(32)*(BRENT_PRICE(CURIYR) - START_PRICE(CURIYR)) )*0.02  
!        Product_Import_P(19,1,1,iYr) = ( IMP_P(19,1989+CURIYR) + 0.96*MC_JPGDP(32)*(BRENT_PRICE(CURIYR) - START_PRICE(CURIYR)) )*0.01   
!        Product_Import_P(19,1,2,iYr) = ( IMP_P(19,1989+CURIYR) + 0.96*MC_JPGDP(32)*(BRENT_PRICE(CURIYR) - START_PRICE(CURIYR)) )*0.02
!         
!
!    end do !end do iYr=1, MNXYR
   
   
   !End Imports of PCF and GO3 should behave like constants!
   
   
   ! End Some curves must behave like "constants"!

   if (CURIYR .EQ. LASTYR .AND. CURITR .EQ. 1) then
   do iYr=1, MNXYR
       write(DBGUNIT, 46) 1989+iYr
       46   format (2x, "Year:", I4)

       do iPr=1,MNPROD
           write(DBGUNIT, 47) iPr
           47   format (2x, "Product:", I4)

                    write (DBGUNIT,44) (Product_Import_Q(iPr,1,ii,iYr), ii = 1, 14)
                    write (DBGUNIT,44) (Product_Import_P(iPr,1,ii,iYr), ii = 1, 14)
                44  format ("End Import" 4x, 14F10.4)
                    write (DBGUNIT,45) (Product_Export_Q(iPr,1,ii,iYr), ii = 1, 14)
                    write (DBGUNIT,45) (Product_Export_P(iPr,1,ii,iYr), ii = 1, 14)
                45  format ("End Export" 4x, 14F10.4)
       end do

   end do
   end if
   !**************************************************************************

!if (CURIYR .EQ. LASTYR .AND. CURITR .EQ. 1) then
! write (DBGUNIT,44) (Product_Import_Q(1,1,ii,CURIYR-1), ii = 1, 14)
! write (DBGUNIT,44) (Product_Import_P(1,1,ii,CURIYR-1), ii = 1, 14)
!
! write (DBGUNIT,44) (Product_Import_Q(1,1,ii,CURIYR), ii = 1, 14)
! write (DBGUNIT,44) (Product_Import_P(1,1,ii,CURIYR), ii = 1, 14)
! !44          format ("Check supply curve" 4x, 14F10.2)
!
! write (DBGUNIT,44) (Product_Import_Q(1,1,ii,61), ii = 1, 14)
! write (DBGUNIT,44) (Product_Import_P(1,1,ii,61), ii = 1, 14)
! !44          format ("Check supply curve" 4x, 14F10.2)
!
! write (DBGUNIT,44) (Product_Import_Q(1,1,ii,91), ii = 1, 14)
! write (DBGUNIT,44) (Product_Import_P(1,1,ii,91), ii = 1, 14)
! 44          format ("Check supply curve" 4x, 14F10.3)
!end if


!
!      !Build EU gasoline import supply curves into US
!       do t=1,EUSTEP
!        P_Start = P_EU_Gas(1989+CURIYR)*(1+BP(t))
!        P_End   = P_EU_Gas(1989+CURIYR)*(1+BP(t+1))
!
!        Q_Start = Q_EU_Gas(1989+CURIYR)*(P_Start/P_EU_Gas(1989+CURIYR))**S_E(1989+CURIYR)
!        Q_End   = Q_EU_Gas(1989+CURIYR)*(P_End/P_EU_Gas(1989+CURIYR))**S_E(1989+CURIYR)
!
!        P_Europe_Gas(t,1989+CURIYR)=(P_Start+P_End)/2
!        Q_Europe_Gas(t,1989+CURIYR)=(Q_End-Q_Start)
!       end do
!
!
!
!      !Change P_Europe_Gas to 1987 dollars
!      do t=1,EUSTEP
!        P_Europe_Gas(t,1989+CURIYR)=P_Europe_Gas(t,1989+CURIYR)/MC_JPGDP(32)
!      end do
!
!       !Buid EU gasoline import supply curves into US from 2040 to 2080 with values from 2040
!       !Fill up values from 2040 to 2080 with values from 2040
!       if (CURIYR .EQ. LASTYR) then
!            do iYr=LASTYR+1, MNXYR
!                    do t = 1, EUSTEP
!                        P_Europe_Gas(t,1989+iYr) = P_Europe_Gas(t,1989+LASTYR)
!                        Q_Europe_Gas(t,1989+iYr) = Q_Europe_Gas(t,1989+LASTYR)
!                    end do
!            end do
!       end if
!
      !Build C_MC product demands curves into US
      do iPr =1,MNPROD
        if(C_MC_P(iPr,1989+CURIYR) .NE. 0 ) then

           do t=1,MCSTEP



            P_Start = ( C_MC_P(iPr,1989+CURIYR) + 0.96*MC_JPGDP(32)*(BRENT_PRICE(CURIYR) - START_PRICE(CURIYR)) )*(1-BP(t))
            P_End   = ( C_MC_P(iPr,1989+CURIYR) + 0.96*MC_JPGDP(32)*(BRENT_PRICE(CURIYR) - START_PRICE(CURIYR)) )*(1-BP(t+1))


            if (t .EQ.1) then
                Q_Start = 0.0
            else
                Q_Start = C_MC_Q(iPr,1989+CURIYR)*(P_Start/( C_MC_P(iPr,1989+CURIYR) + 0.96*MC_JPGDP(32)*(BRENT_PRICE(CURIYR) - START_PRICE(CURIYR)) ))**D_E(1989+CURIYR)
            end if

            Q_End   = C_MC_Q(iPr,1989+CURIYR)*(P_End/( C_MC_P(iPr,1989+CURIYR) + 0.96*MC_JPGDP(32)*(BRENT_PRICE(CURIYR) - START_PRICE(CURIYR)) ))**D_E(1989+CURIYR)

            P_C_MC_Demand(t,1989+CURIYR,iPr)=(P_Start+P_End)/2
            Q_C_MC_Demand(t,1989+CURIYR,iPr)=(Q_End-Q_Start)
           end do

        end if
      end do  






!if (CURCALYR .EQ. 2011) then 
 !       write(DBGUNIT, '(//,A)') "2011 prod8 p q"
  !
   !         write (DBGUNIT,10001) (P_C_MC_Demand(t,CURCALYR,8), t = 1, 14)
    !        write (DBGUNIT,10001) (Q_C_MC_Demand(t,CURCALYR,8), t = 1, 14)
!10001       format (2x, 14F10.2)

 !end if



      !Change P_C_MC_Demand to 1987 dollars
      do iPr =1,MNPROD
           do t=1,MCSTEP
             P_C_MC_Demand(t,1989+CURIYR,iPr)= P_C_MC_Demand(t,1989+CURIYR,iPr)/MC_JPGDP(32)
        end do
      end do

      !Fill up values from 2040 to 2080 with values from 2040
      if (CURIYR .EQ. LASTYR) then
        do iYr=LASTYR+1, MNXYR
            do iPr =1,MNPROD
                do t = 1, MCSTEP
                    P_C_MC_Demand(t,1989+iYr,iPr) = P_C_MC_Demand(t,1989+LASTYR,iPr)
                    Q_C_MC_Demand(t,1989+iYr,iPr) = Q_C_MC_Demand(t,1989+LASTYR,iPr)
                end do
            end do
        end do
      end if


 rr=(sum(Q_CRUDE_IMPORTS(:,1,2011),1)-Q_CRUDE_IMPORTS(10,1,2011))


 rUSCrudeImports(CURCALYR) = 0
 do c=1,9
    do iR=1,8
        rUSCrudeImports(CURCALYR) = rUSCrudeImports(CURCALYR)+Q_CRUDE_IMPORTS(iR,c,CURCALYR)
    end do
 end do

 !******************************************************************************
!     write(DBGUNIT, '(/,A)') "Year,Eql_Price,Eql_Quantity,Supp_Diff,Dem_Diff"
!     write(DBGUNIT, 814) "LFMM", CURCALYR, ',', P_Eql(CURCALYR), ',', Q_Eql(CURCALYR), ',', S_Diff(CURCALYR), ',', D_Diff(CURCALYR)
!814   format (2x, A, I8, A3, F12.2, A3, F12.2, A3, F12.2, A3, F12.2)


!     write(6, '(/,A)') "Year,Eql_Price,Eql_Quantity,Supp_Diff,Dem_Diff"
!    write(6, 812) "LFMM", CURCALYR, ',', P_Eql(CURCALYR), ',', Q_Eql(CURCALYR), ',', S_Diff(CURCALYR), ',', D_Diff(CURCALYR)
!812   format (2x, A, I8, A3, F12.2, A3, F12.2, A3, F12.2, A3, F12.2)
 !******************************************************************************

 !******************************************************************************
 !******************************************************************************

 !    write(DBGUNIT, 824) CURCALYR, CURIRUN, CURITR, sum(Q_Non_US_Demand(:,Max_Crude_Source,Max_NonUS_Demand_Steps,CURCALYR),1), rUSCrudeImports(CURCALYR), D_Diff(CURCALYR)
!824   format (2x, "LFMM_WORLD_DEBUG", 2x, "Year:", I4, 2x, "Cycle:" I2, 2x, "Iteration:", I2, 2x, "Non_USDemands:", F10.2, 2x, "US_Imports:" F10.2, 2x, "Demand diff:" F10.2)


 !     write(6, 812) CURCALYR, CURIRUN, CURITR, sum(Q_Non_US_Demand(:,Max_Crude_Source,Max_NonUS_Demand_Steps,CURCALYR),1), rUSCrudeImports(CURCALYR), D_Diff(CURCALYR)
!812   format (2x, "LFMM_WORLD_DEBUG", 2x, "Year:", I4, 2x, "Cycle:" I2, 2x, "Iteration:", I2, 2x, "Non_USDemands:", F10.2, 2x, "US_Imports:" F10.2, 2x, "Demand diff:" F10.2)

!******************************************************************************
!******************************************************************************

iFlag = 2 ! 1 for debug run and 2 for regular run

if (iFlag .EQ. 2) then  !REGULAR RUN


!if (CURCALYR .EQ. 2040 .AND. FCRL .EQ. 1) then
!     do ii=FFY-1989,  MNUMYR-10   !MNUMYR+Max_LFMM_FYR
!        do jj=1, MNCRUD-2
!            do kk=1, MNUMOR-1
!                WRITE (DBGUNIT, 4488) OGCRDPRD(kk,jj,ii)
!        4488    FORMAT (2x, F12.6)
!            end do
!        end do
!     end do
! end if


     if (CURCALYR .EQ. 2050 .AND. FCRL .EQ. 1) then
     !if (CURCALYR .EQ. 2015) then
     write(DBGUNIT, '(//,A)') "Year,Eql_Price,Eql_Quantity,Supp_Diff,Dem_Diff"
     do 81, ii = FFY,1989+LASTYR
     write (DBGUNIT, 82) ii, ',', P_Eql(ii), ',', Q_Eql(ii), ',', S_Diff(ii), ',', D_Diff(ii)
82   format (2x, I8, A3, F12.2, A3, F12.2, A3, F12.2, A3, F12.2)
81   continue
     end if

     if (CURCALYR .EQ. 2050 .AND. FCRL .EQ. 1) then
     !if (CURCALYR .EQ. 2015) then
        write(DBGUNIT, '(//,A)') "Foreign Crude Production"
        do 83, c=1,MNCRUD
            write (DBGUNIT,84) (Q_Crude(c,ii), ii = 2008, 2071)
84          format (2x, 64F10.2)
83      continue
     end if

     if (CURCALYR .EQ. 2050 .AND. FCRL .EQ. 1) then
     !if (CURCALYR .EQ. 2015) then
        write(DBGUNIT, '(//,A)') "Total domestic crude production at equilibrium"
        do 85, c=1,MNCRUD
            write (DBGUNIT,86) ( (   (sum(OGCRUDEREF(:,c,ii), 1)-OGCRUDEREF(10,c,ii))*(1000.0/365.0)   ), ii = 19, 61     )
86          format (2x, 64F10.2)
85      continue
     end if

     if (CURCALYR .EQ. 2050 .AND. FCRL .EQ. 1) then
     !if (CURCALYR .EQ. 2015) then
            write(DBGUNIT, '(//,A)') "Total Non_US_Crude_Demand"

            write (DBGUNIT,87) (sum(Q_NON_US_DEMAND(:,Max_Crude_Source,Max_NonUS_Demand_Steps,ii),1), ii = 2008, 2071)
87          format (2x, 64F10.2)
     end if



     if (CURCALYR .EQ. 2050 .AND. FCRL .EQ. 1) then
     !if (CURCALYR .EQ. 2015) then
            write(DBGUNIT, '(//,A)') "Foreign Crude supply by crude type"

            do 89, c=1,MNCRUD
            write (DBGUNIT,90) (Q_NON_US_DEMAND(c,Max_Crude_Source,Max_NonUS_Demand_Steps,ii) - Q_Crude(c,ii), ii = 2008, 2071)
90          format (2x, 64F10.2)
89          continue
     end if


     if (CURCALYR .EQ. 2050 .AND. FCRL .EQ. 1) then
     !if (CURCALYR .EQ. 2015) then
            write(DBGUNIT, '(//,A)') "Crude prices at equilibrium by crude type"

            do 91, c=1,MNCRUD
                write (DBGUNIT,92) (P_Crude(c,ii) , ii = 2008, 2071)
                !write (DBGUNIT,92) (P_Foreign_Crude(c,1,8,ii)*MC_JPGDP(32)+P_Crude(1,ii) , ii = 2008, 2071)     !P_Foreign_Crude(c,Max_Crude_Source,t,1989+CURIYR)
                !write (DBGUNIT,92) (P_Total_Crude(8,ii)*MC_JPGDP(32) , ii = 2008, 2071)
                write (DBGUNIT,92) (P_NON_US_DEMAND(c,1,1,ii)*MC_JPGDP(32) , ii = 2008, 2071)
92          format (2x, 64F10.2)
91          continue

write(DBGUNIT, '(//,A)') "Brent Prices"
write (DBGUNIT,99) (BRENT_PRICE(ii)*MC_JPGDP(32) , ii = 19, 61)
99          format (2x, 43F10.2)

write(DBGUNIT, '(//,A)') "XBrent Prices"
write (DBGUNIT,100) (XBRENT_PRICE(ii)*MC_JPGDP(32) , ii = 19, 91)
100          format (2x, 73F10.2)

write(DBGUNIT, '(//,A)') "Start Prices"
write (DBGUNIT,99) (START_PRICE(ii)*MC_JPGDP(32) , ii = 19, 61)

write(DBGUNIT, '(//,A)') "XStart Prices"
write (DBGUNIT,100) (XSTART_PRICE(ii)*MC_JPGDP(32) , ii = 19, 91)

write(DBGUNIT, '(//,A)') "WTI Prices1"
write (DBGUNIT,101) (WTI_PRICE(ii)*MC_JPGDP(32) , ii = 19, 61)
101          format (2x, 43F10.2)

write(DBGUNIT, '(//,A)') "XWTI Prices1"
write (DBGUNIT,102) (XWTI_PRICE(ii)*MC_JPGDP(32) , ii = 19, 91)
102          format (2x, 73F10.2)

     end if


      if (CURCALYR .EQ. 2050 .AND. FCRL .EQ. 1) then
      !if (CURCALYR .EQ. 2015) then
            write(DBGUNIT, '(//,A)') "Imports of crudes in all 9 RefReg"
            do 93, c=1,MNCRUD
            !write (DBGUNIT,94)  (Q_CRUDE_IMPORTS(1,c,ii)+Q_CRUDE_IMPORTS(2,c,ii)+Q_CRUDE_IMPORTS(3,c,ii)+Q_CRUDE_IMPORTS(4,c,ii)+Q_CRUDE_IMPORTS(5,c,ii)+Q_CRUDE_IMPORTS(6,c,ii)+Q_CRUDE_IMPORTS(7,c,ii)+Q_CRUDE_IMPORTS(8,c,ii)+Q_CRUDE_IMPORTS(9,c,ii), ii = 2008, 2071)
            write (DBGUNIT,94) ( (sum(Q_CRUDE_IMPORTS(:,c,ii),1)-Q_CRUDE_IMPORTS(10,c,ii)), ii = 2008, 2071)

94          format (2x, 64F10.2)
93            continue
      end if


      !**********************
      if (CURCALYR .EQ. 2050 .AND. FCRL .EQ. 1) then
      !if (CURCALYR .EQ. 2015) then
            write(DBGUNIT, '(//,A)') "Imports of crudes in RegRef9"
            do 97, c=1,MNCRUD
            write (DBGUNIT,98)  (Q_CRUDE_IMPORTS(9,c,ii), ii = 2008, 2071)            !(sum(Q_CRUDE_IMPORTS(9,:,ii),2), ii = 2008, 2071)
98          format (2x, 64F10.2)
97            continue
      end if
      !**********************




!      if (CURCALYR .EQ. 2040 .AND. FCRL .EQ. 1) then
!
!            write(DBGUNIT, '(//,A)') "C_MC_Crude Demand"
!            do 95, c=1,MNCRUD
!                write (DBGUNIT,96) (C_MC_Crude_Demand(c,ii), ii = 2008, 2071)
!96          format (2x, 64F10.2)
!95            continue
!      end if


 if (CURCALYR .EQ. 2050 .AND. FCRL .EQ. 1) then
  write(DBGUNIT, '(//,A)') "Deflators"
    do 888, ii = -2, 61
    write (DBGUNIT,889) (ii, mc_jpgdp(ii))

889 format (2x, I2, 4x, 64F12.8)
888 continue

 end if



end if  !END REGULAR RUN


      RETURN
      END



      SUBROUTINE OMS_Dat_In
!  Read OMSINPUT.wk1 (world oil production and demand) and OMSECON.txt (econometric info)
      IMPLICIT NONE
      include 'parametr'
      include 'ncntrl'
      include 'intout'
      !LOGICAL New
      LOGICAL*1     NEW/.FALSE./
      CHARACTER*18 Fname
      INTEGER Iunit1
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
      INTEGER I, J, IY, Year(MNUMYR+1)
      REAL Demand, Supply, NC_Oil, Net_CPE, &
          OPEC_Prod, Price_Adj, Discrep, &
          GDP, P_Elas_Dem, FB_Elas_Dem, P_Elas_Sup, I_Elas, &
          Dem_Lag, Sup_Lag
      COMMON /OMSDATA/ Demand(17, MNUMYR+1), Supply(14, MNUMYR+1), NC_Oil(9, MNUMYR+1), &
          Net_CPE(MNUMYR+1), OPEC_Prod(5, MNUMYR+1), Price_Adj(18, MNUMYR+1), Discrep(MNUMYR+1), &
          GDP(16, MNUMYR+1), P_Elas_Dem(16), &
          FB_Elas_Dem(16), P_Elas_Sup(14), I_Elas(16), Dem_Lag(16), Sup_Lag(14)
      REAL Hold16(16,MNUMYR+1),Hold18(18,MNUMYR+1),Hold8(8,MNUMYR+1),Hold5(5,MNUMYR+1),Hold9(9,MNUMYR+1)

!   Inputs International Reference Supply And Demand Values

      IY = IJUMPYR + 1
      !Fname='OMSECON'
      Fname='INTBALANCE'

      New=.FALSE.
      Iunit1=FILE_MGR('O',Fname,New)
!  CALL SUBROUTINE TO READ ALL DEFINED RANGES FROM WORKSHEET
!  This stores the ranges in a temporary data area that can get overwritten by the next model
!  if they use it.  So all ranges have to be extracted from the temporary area immediately.
         CALL ReadRngXML(Iunit1, 'data')
         !CALL readrng(Iunit1)
         !CALL mreadrng(Iunit1)

!  Close worksheet file

         Iunit1 = FILE_MGR('C',Fname,New)

!  Copy each range from worksheet data area to variables
!  GETRNGI : Copies an Integer*2 variable from the worksheet data area into the variable.  The variable
!            dimensions are passed as the 3rd,4th,&5th arguments, (eg, ... 1,1,1).
!            A variable with dimesions of 1,1,1 is a scalar.
!            A variable with dimensions of 26,1,1 is a one-dimensional array with 26 elements.
!  GETRNGR:  Copies a REAL variable from the worksheet data area into the variable.

         CALL GETRNGI('YEAR            ',Year           ,MNUMYR+1,1,1)
         CALL GETRNGR('PRICEADJUST     ',Hold18         ,18,MNUMYR+1,1)
         Price_Adj(:,:)=Hold18(:,:)
         CALL GETRNGR('REFDEMANDS      ',Hold16         ,16,MNUMYR+1,1)
         Demand(2:17,:)=Hold16(1:16,:)
         CALL GETRNGR('REFSUPPLIES1    ',Hold8          ,8,MNUMYR+1,1)
         Supply(2:9,:)=Hold8(1:8,:)
         CALL GETRNGR('OPECSUPPLY      ',OPEC_Prod      ,5,MNUMYR+1,1)
         CALL GETRNGR('REFSUPPLIES2    ',Hold5          ,5,MNUMYR+1,1)
         Supply(10:14,:)=Hold5(1:5,:)
         CALL GETRNGR('REFNCOIL        ',Hold9         ,9,MNUMYR+1,1)
         NC_Oil(:,:)=Hold9(:,:)
         CALL GETRNGR('DISCREPANCY     ',Discrep        ,MNUMYR+1,1,1)


      RETURN
      END


      SUBROUTINE Skip_Comments(File_Num)
!   Skips Commented Lines In All Input Files
      CHARACTER*1 Star, A
      INTEGER File_Num
      DATA Star/'*'/
      READ (File_Num, 100) A
  100 FORMAT (A1)
      DO WHILE (A.EQ.Star)
      READ (File_Num, 100) A
      END DO
      BACKSPACE File_Num
      RETURN
      END


 SUBROUTINE World_Oil_Report_LFMM
      IMPLICIT NONE

      include 'parametr'
      include 'ncntrl'
      include 'intout'
      include 'pmmrpt'
      include 'intllocl'






      !include 'ngtdmrep'
      include 'macout'
      include 'pmmout'

      include 'ogsmout'
      include 'lfmmout'


      !!!!!!!!!!!!!!!!!!!!!!
      !For use in Table 118
      REAL IOCanadaPct, IOMexicoPct, IONorthSeaPct, IOOPECPct, IOOPLatinAmericaPct, &
           IOOPNorthAfricaPct, IOOPWestAfricaPct, IOOPIndonesiaPct, IOOPPersianGulfPct, &
           IOOtherMiddleEastPct, IOOtherLatinAmericaPct, IOOtherAfricaPct, IOOtherAsiaPct
      COMMON /T118b/ IOCanadaPct(1990:1989+MNXYR), IOMexicoPct(1990:1989+MNXYR), IONorthSeaPct(1990:1989+MNXYR), IOOPECPct(1990:1989+MNXYR), &
           IOOPLatinAmericaPct(1990:1989+MNXYR), IOOPNorthAfricaPct(1990:1989+MNXYR), IOOPWestAfricaPct(1990:1989+MNXYR), IOOPIndonesiaPct(1990:1989+MNXYR), &
           IOOPPersianGulfPct(1990:1989+MNXYR), IOOtherMiddleEastPct(1990:1989+MNXYR), IOOtherLatinAmericaPct(1990:1989+MNXYR), &
           IOOtherAfricaPct(1990:1989+MNXYR), IOOtherAsiaPct(1990:1989+MNXYR)

      REAL ILPCanadaPct, ILPNorthEuropePct, ILPSouthEuropePct, ILPOPECPct, ILPOPAmericasPct, &
           ILPOPNoAfricaPct, ILPOPWestAfricaPct, ILPOPIndonesiaPct, ILPOPPersianGulfPct, &
           ILPCaribbeanPct, ILPAsiaPct, ILPOtherPct
      COMMON /T118c/ ILPCanadaPct(1990:1989+MNXYR), ILPNorthEuropePct(1990:1989+MNXYR), ILPSouthEuropePct(1990:1989+MNXYR), ILPOPECPct(1990:1989+MNXYR), &
           ILPOPAmericasPct(1990:1989+MNXYR), ILPOPNoAfricaPct(1990:1989+MNXYR), ILPOPWestAfricaPct(1990:1989+MNXYR), ILPOPIndonesiaPct(1990:1989+MNXYR), &
           ILPOPPersianGulfPct(1990:1989+MNXYR), ILPCaribbeanPct(1990:1989+MNXYR), ILPAsiaPct(1990:1989+MNXYR), ILPOtherPct(1990:1989+MNXYR)

      REAL IHPCanadaPct, IHPNorthEuropePct, IHPSouthEuropePct, IHPOPECPct, IHPOPAmericasPct, &
           IHPOPNoAfricaPct, IHPOPWestAfricaPct, IHPOPIndonesiaPct, IHPOPPersianGulfPct, &
           IHPCaribbeanPct, IHPAsiaPct, IHPOtherPct
      COMMON /T118d/ IHPCanadaPct(1990:1989+MNXYR), IHPNorthEuropePct(1990:1989+MNXYR), IHPSouthEuropePct(1990:1989+MNXYR), IHPOPECPct(1990:1989+MNXYR), &
           IHPOPAmericasPct(1990:1989+MNXYR), IHPOPNoAfricaPct(1990:1989+MNXYR), IHPOPWestAfricaPct(1990:1989+MNXYR), IHPOPIndonesiaPct(1990:1989+MNXYR), &
           IHPOPPersianGulfPct(1990:1989+MNXYR), IHPCaribbeanPct(1990:1989+MNXYR), IHPAsiaPct(1990:1989+MNXYR), IHPOtherPct(1990:1989+MNXYR)
     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      INTEGER iReg, iYrs, iC
      REAL    LFMM_ImportByReg_Crude
      COMMON /IMPBYREG/ LFMM_ImportByReg_Crude(MNUMPR,1990:1989+MNXYR)


      INTEGER I, J
      REAL Demand, Supply, NC_Oil, Net_CPE, &
          OPEC_Prod, Price_Adj, Discrep, &
          GDP, P_Elas_Dem, FB_Elas_Dem, P_Elas_Sup, I_Elas, &
          Dem_Lag, Sup_Lag
      COMMON /OMSDATA/ Demand(17, MNUMYR+1), Supply(14, MNUMYR+1), NC_Oil(9, MNUMYR+1), &
          Net_CPE(MNUMYR+1), OPEC_Prod(5, MNUMYR+1), Price_Adj(18, MNUMYR+1), Discrep(MNUMYR+1), &
          GDP(16, MNUMYR+1), P_Elas_Dem(16), &
          FB_Elas_Dem(16), P_Elas_Sup(14), I_Elas(16), Dem_Lag(16), Sup_Lag(14)


      REPORT=0.0


      DO I=1,LASTYR
         REPORT(I,1:14)=Supply(:,I+1)
         REPORT(I,15:19)=OPEC_Prod(:,I+1)
         REPORT(I,20:28)=NC_Oil(:,I+1)
         REPORT(I,29:45)=Demand(:,I+1)
         REPORT(I,52)=Demand(10, I+1)+Demand(11,I+1)+Demand(12,I+1) &
             +Demand(13,I+1)+Demand(14,I+1)+Demand(15,I+1)+Demand(16,I+1)
         REPORT(I,54)=OPEC_Prod(1,I+1)+OPEC_Prod(2,I+1)+OPEC_Prod(3,I+1)+ &
              OPEC_Prod(4,I+1)+OPEC_Prod(5,I+1)+NC_Oil(9,I+1)
         REPORT2(I,1:18)=Price_Adj(:,I+1)

      ENDDO

      REPORT(1:LASTYR,56)=Net_CPE(2:LASTYR+1)
      REPORT(:,53)=REPORT(:,41)+REPORT(:,44)+REPORT(:,52)
      REPORT(:,55)=REPORT(:,33)-REPORT(:,54)
      REPORT(:,57)=REPORT(:,54)/REPORT(:,33)

!     DO 2, J=1, 57
!     WRITE (*, 610) (REPORT(I, J), I=1, 41)
! 610 FORMAT (8x, 6F8.3/ 7F8.3/ 7F8.3/ 7F8.3/ 7F8.3/ 7F8.3//)
!   2 CONTINUE

!********************************************
!********************************************
!do c=1,MNCRUD
!        LFMM_PurchaseForeign_Crude(c,1989+CURIYR)=0
!        LFMM_Export_Crude(c,1989+CURIYR)=0
!        do ii=1,REFREG
!          LFMM_PurchaseForeign_Crude(c,1989+CURIYR) = LFMM_PurchaseForeign_Crude(c,1989+CURIYR)+Q_CRUDE_IMPORTS(ii,c,1989+CURIYR)
!          LFMM_Export_Crude(c,1989+CURIYR) = LFMM_Export_Crude(c,1989+CURIYR)+Q_CRUDE_EXPORTS(ii,c,1989+CURIYR)
!        end do
!end do

!    REAL LFMM_PurchaseForeign_Crude, LFMM_Export_Crude !(MNCRUD,1990:1989+MNXYR)
!    COMMON /LFMM_IMPEXP_CRUDE/ LFMM_PurchaseForeign_Crude(MNCRUD,1990:1989+MNXYR), LFMM_Export_Crude(MNCRUD,1990:1989+MNXYR)

!LFMM_ImportByReg_Crude(MNUMPR,1990:1989+MNXYR)
!********************************************
!********************************************

!Just to be sure that initial values are zero
DO 401 iReg = 1,MNUMPR
    DO 402 iYrs = 1990,1989+MNXYR
        LFMM_ImportByReg_Crude(iReg,iYrs) = 0.0
    402 CONTINUE
401 CONTINUE

!Compute crude imports in each refinery region, by year
DO 403 iReg = 1,9
    DO 404 iYrs = 1990,1989+MNXYR
        DO 405 iC = 1,MNCRUD
            LFMM_ImportByReg_Crude(iReg,iYrs) = LFMM_ImportByReg_Crude(iReg,iYrs) + Q_CRUDE_IMPORTS(iReg,iC,iYrs)
        405 CONTINUE
    404 CONTINUE
403 CONTINUE

     !Start computing Import Crude Oil quantities for each Region starting year 2004
     DO 241 J = FFY, 1989+MNUMYR
          !ICOCanada(J-1989) = RFQICRD(10,J-1989)*IOCanadaPct(J)
          ICOCanada(J-1989) = RFQICRD(2,J-1989) + RFQICRD(3,J-1989) + RFQICRD(6,J-1989) + 0.23*RFQICRD(1,J-1989) + 0.17*( RFQICRD(7,J-1989) + RFQICRD(8,J-1989) )

 241 CONTINUE

     DO 242 J = FFY, 1989+MNUMYR
          !ICOMexico(J-1989) =  RFQICRD(10,J-1989)*IOMexicoPct(J)
          ICOMexico(J-1989) = 0.23*( RFQICRD(4,J-1989) + RFQICRD(5,J-1989) )
 242 CONTINUE

     DO 243 J = FFY, 1989+MNUMYR
          ICONorthSea(J-1989) =  RFQICRD(10,J-1989)*IONorthSeaPct(J)
 243 CONTINUE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     DO 244 J = FFY, 1989+MNUMYR
!          !ICOOPEC(J-1989) =  RFQICRD(10,J-1989)*IOOPECPct(J)
!          ICOOPEC(J-1989) =  RFQICRD(10,J-1989) - ICOCanada(J-1989) - ICOMexico(J-1989) - ICONorthSea(J-1989) - ICOOtherMidEast(J-1989) - ICOOtherAmericas(J-1989) - ICOOtherAfrica(J-1989) - ICOOtherAsia(J-1989)
! 244 CONTINUE
!
!     DO 245 J = FFY, 1989+MNUMYR
!          ICOOPAmericas(J-1989) =  RFQICRD(10,J-1989)*IOOPLatinAmericaPct(J)
! 245 CONTINUE
!
!     DO 246 J = FFY, 1989+MNUMYR
!          ICOOPNoAfrica(J-1989) =  RFQICRD(10,J-1989)*IOOPNorthAfricaPct(J)
! 246 CONTINUE
!
!     DO 247 J = FFY, 1989+MNUMYR
!          ICOOPWestAfrica(J-1989) =  RFQICRD(10,J-1989)*IOOPWestAfricaPct(J)
! 247 CONTINUE
!
!     DO 248 J = FFY, 1989+MNUMYR
!          ICOOPIndonesia(J-1989) =  RFQICRD(10,J-1989)*IOOPIndonesiaPct(J)
! 248 CONTINUE
!
!     DO 249 J = FFY, 1989+MNUMYR
!          ICOOPPersianGulf(J-1989) =  RFQICRD(10,J-1989)*IOOPPersianGulfPct(J)
! 249 CONTINUE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     DO 250 J = FFY, 1989+MNUMYR
          ICOOtherMidEast(J-1989) =  RFQICRD(10,J-1989)*IOOtherMiddleEastPct(J)
 250 CONTINUE

     DO 251 J = FFY, 1989+MNUMYR
          ICOOtherAmericas(J-1989) =  RFQICRD(10,J-1989)*IOOtherLatinAmericaPct(J)
 251 CONTINUE

     DO 252 J = FFY, 1989+MNUMYR
          ICOOtherAfrica(J-1989) =  RFQICRD(10,J-1989)*IOOtherAfricaPct(J)
 252 CONTINUE

     DO 253 J = FFY, 1989+MNUMYR
          ICOOtherAsia(J-1989) =  RFQICRD(10,J-1989)*IOOtherAsiaPct(J)
 253 CONTINUE

     DO 254 J = FFY, 1989+MNUMYR
          ICOTotal(J-1989) =  RFQICRD(10,J-1989)
 254 CONTINUE

   DO 244 J = FFY, 1989+MNUMYR
          !ICOOPEC(J-1989) =  RFQICRD(10,J-1989)*IOOPECPct(J)
          ICOOPEC(J-1989) =  RFQICRD(10,J-1989) - ICOCanada(J-1989) - ICOMexico(J-1989) - ICONorthSea(J-1989) - ICOOtherMidEast(J-1989) - ICOOtherAmericas(J-1989) - ICOOtherAfrica(J-1989) - ICOOtherAsia(J-1989)
 244 CONTINUE

     DO 245 J = FFY, 1989+MNUMYR
          ICOOPAmericas(J-1989) =  ICOOPEC(J-1989)*IOOPLatinAmericaPct(J)
 245 CONTINUE

     DO 246 J = FFY, 1989+MNUMYR
          ICOOPNoAfrica(J-1989) =  ICOOPEC(J-1989)*IOOPNorthAfricaPct(J)
 246 CONTINUE

     DO 247 J = FFY, 1989+MNUMYR
          ICOOPWestAfrica(J-1989) =  ICOOPEC(J-1989)*IOOPWestAfricaPct(J)
 247 CONTINUE

     DO 248 J = FFY, 1989+MNUMYR
          ICOOPIndonesia(J-1989) =  ICOOPEC(J-1989)*IOOPIndonesiaPct(J)
 248 CONTINUE

     DO 249 J = FFY, 1989+MNUMYR
          ICOOPPersianGulf(J-1989) =  ICOOPEC(J-1989)*IOOPPersianGulfPct(J)
 249 CONTINUE

     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     !Compute the Total Light Refined Products Imports
      DO 255 J = FFY, 1989+MNUMYR
           ILPTotal(J-1989) =  (ETHIMP(11,J-1989) + BIODIMP(11,J-1989))/1000.0 + &
                               (RFIPQPR(10,J-1989,2)+RFIPQPY(10,J-1989,2)+RFIPQET(10,J-1989,2)+RFIPQBU(10,J-1989,2)+RFIPQIS(10,J-1989,2)+RFIPQPP(10,J-1989,2))/1000.0 + &
                               (RFIPQCG(10,J-1989,2) + RFIPQMG(10,J-1989,2) + RFIPQRG(10,J-1989,2) + RFIPQCBOB(10,J-1989,2) + RFIPQRBOB(10,J-1989,2) + RFMETI(10,J-1989))/1000.0 + &
                               (RFIPQDL(10,J-1989,2) + RFIPQDU(10,J-1989,2) + RFIPQDS(10,J-1989,2) + RFIPQJF(10,J-1989,2) + RFIPQAG(10,J-1989,2))/1000.0 + &
                               (RFIPQPR(10,J-1989,2) + RFIPQPY(10,J-1989,2) + RFIPQET(10,J-1989,2) + RFIPQBU(10,J-1989,2) + RFIPQIS(10,J-1989,2) + RFIPQPP(10,J-1989,2))/1000.0
 255 CONTINUE

     !Start computing Light Refined Product quantities for each Region starting year 2004
      DO 256 J = FFY, 1989+MNUMYR
          ILPCanada(J-1989) =  ILPTotal(J-1989)*ILPCanadaPct(J)
 256 CONTINUE

      DO 257 J = FFY, 1989+MNUMYR
          ILPNorthEurope(J-1989) =  ILPTotal(J-1989)*ILPNorthEuropePct(J)
 257 CONTINUE

      DO 258 J = FFY, 1989+MNUMYR
          ILPSouthEurope(J-1989) =  ILPTotal(J-1989)*ILPSouthEuropePct(J)
 258 CONTINUE

      DO 259 J = FFY, 1989+MNUMYR
          ILPOPEC(J-1989) =  ILPTotal(J-1989)*ILPOPECPct(J)
 259 CONTINUE

      DO 260 J = FFY, 1989+MNUMYR
          ILPOPAmericas(J-1989) =  ILPOPEC(J-1989)*ILPOPAmericasPct(J)
 260 CONTINUE

      DO 261 J = FFY, 1989+MNUMYR
          ILPOPNoAfrica(J-1989) =  ILPOPEC(J-1989)*ILPOPNoAfricaPct(J)
 261 CONTINUE

      DO 262 J = FFY, 1989+MNUMYR
          ILPOPWestAfrica(J-1989) =  ILPOPEC(J-1989)*ILPOPWestAfricaPct(J)
 262 CONTINUE

      DO 263 J = FFY, 1989+MNUMYR
          ILPOPIndonesia(J-1989) =  ILPOPEC(J-1989)*ILPOPIndonesiaPct(J)
 263 CONTINUE

      DO 264 J = FFY, 1989+MNUMYR
          ILPOPPersianGulf(J-1989) =  ILPOPEC(J-1989)*ILPOPPersianGulfPct(J)
 264 CONTINUE

      DO 265 J = FFY, 1989+MNUMYR
          ILPCaribbean(J-1989) =  ILPTotal(J-1989)*ILPCaribbeanPct(J)
 265 CONTINUE

      DO 266 J = FFY, 1989+MNUMYR
          ILPAsia(J-1989) =  ILPTotal(J-1989)*ILPAsiaPct(J)
 266 CONTINUE

      DO 267 J = FFY, 1989+MNUMYR
          ILPOther(J-1989) =  ILPTotal(J-1989)*ILPOtherPct(J)
 267 CONTINUE

     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     !Compute the Total Heavy Refined Products Imports
      DO 268 J = FFY, 1989+MNUMYR
          IHPTotal(J-1989) = (RFIPQAS(10,J-1989,2) + RFIPQLU(10,J-1989,2) + RFIPQRH(10,J-1989,2) + RFIPQRL(10,J-1989,2) + RFIPQPF(10,J-1989,2) )/1000 + RFPQUFC(10,J-1989,2)
 268 CONTINUE

     !Start computing Heavy Refined Product quantities for each Region starting year 2004
      DO 269 J = FFY, 1989+MNUMYR
          IHPCanada(J-1989) =  IHPTotal(J-1989)*IHPCanadaPct(J)
 269 CONTINUE

      DO 270 J = FFY, 1989+MNUMYR
          IHPNorthEurope(J-1989) =  IHPTotal(J-1989)*IHPNorthEuropePct(J)
 270 CONTINUE

      DO 271 J = FFY, 1989+MNUMYR
          IHPSouthEurope(J-1989) =  IHPTotal(J-1989)*IHPSouthEuropePct(J)
 271 CONTINUE

      DO 272 J = FFY, 1989+MNUMYR
          IHPOPEC(J-1989) =  IHPTotal(J-1989)*IHPOPECPct(J)
 272 CONTINUE

      DO 273 J = FFY, 1989+MNUMYR
          IHPOPAmericas(J-1989) =  IHPOPEC(J-1989)*IHPOPAmericasPct(J)
 273 CONTINUE

      DO 274 J = FFY, 1989+MNUMYR
          IHPOPNoAfrica(J-1989) =  IHPOPEC(J-1989)*IHPOPNoAfricaPct(J)
 274 CONTINUE

      DO 275 J = FFY, 1989+MNUMYR
          IHPOPWestAfrica(J-1989) =  IHPOPEC(J-1989)*IHPOPWestAfricaPct(J)
 275 CONTINUE

      DO 276 J = FFY, 1989+MNUMYR
          IHPOPIndonesia(J-1989) =  IHPOPEC(J-1989)*IHPOPIndonesiaPct(J)
 276 CONTINUE

      DO 277 J = FFY, 1989+MNUMYR
          IHPOPPersianGulf(J-1989) =  IHPOPEC(J-1989)*IHPOPPersianGulfPct(J)
 277 CONTINUE

      DO 278 J = FFY, 1989+MNUMYR
          IHPCaribbean(J-1989) =  IHPTotal(J-1989)*IHPCaribbeanPct(J)
 278 CONTINUE

      DO 279 J = FFY, 1989+MNUMYR
          IHPAsia(J-1989) =  IHPTotal(J-1989)*IHPAsiaPct(J)
 279 CONTINUE

      DO 280 J = FFY, 1989+MNUMYR
          IHPOther(J-1989) =  IHPTotal(J-1989)*IHPOtherPct(J)
 280 CONTINUE


      RETURN
      END










!The first subroutine used is "xmlSheet(abc,xyz)". It is used to find the position of the worksheet named "xyz"
!in a file that has been opened with the file unit number "abc". The file unit "abc" must be an integer variable,
!or can be the actual number. The argument, "xyz", can be any character variable name the user likes, or can be
!the actual worksheet name in single or double quotes. The argument name is not case sensitive.
!
!The second subroutine used is "xmlRow(xyz)". It is used to find the position of the row numbered by "xyz". The
!argument, "xyz", must be an integer variable, or can be the actual row number.
!
!There are three variations of the third subroutine, one each for real, integer, and character returned variables.
!
!One variation is "xmlRCell(xyz,data)". It is used to find the position of the column numbered by "xyz" and then
!to return the value of the real number, "data", in that cell. The argument, "xyz", must be an integer variable, or
!can be the the actual column number. The argument, "data", must be a real variable and returns the cell value.
!
!Another variation is "xmlICell(xyz,data)". It is used to find the position of the column numbered by "xyz" and then
!to return the value of the integer number, "data", in that cell. The argument, "xyz", must be an integer variable,
!or can be the actual column number. The argument, "data", must be an integer variable and returns the cell value.
!
!Another variation is "xmlCCell(xyz,data)". It is used to find the position of the column numbered by "xyz" and then
!to return the value of the character string, "data", in that cell. The argument, "xyz", must be an integer variable,
!or can be the actual column number. The argument, "data", must be a character variable, typed as character*200 and
!returns the cell value.

!******************************************
SUBROUTINE xmlSheet(FUnit, XSheet)
implicit none

integer CurUnit,CurRow,CurCol
character*3000 CurBuf
common/XMLExcel/CurUnit,CurRow,CurCol,CurBuf

integer FUnit,xDone,IsWKS
character*3000 TextLine,TextComp
character*100 TmpStr
character*(*) XSheet

CurUnit=FUnit
TmpStr='Name="'//trim(XSheet)//'"'
call xmlLC(TmpStr,TmpStr)  !Convert to lower space
xDone=0
do while (xDone.eq.0)
 read(CurUnit,'(a)') TextLine
 call xmlLC(TextLine,TextComp)
 IsWKS=index(TextComp,'<worksheet')
 if(IsWKS.ne.0) then
  xDone=index(TextComp,trim(TmpStr))
 endif
end do
CurRow=0
CurBuf=TextLine

return
end

!******************************************
subroutine xmlLC(ZText,XText)
!Converts upper case letters to lower case in a character variable.
implicit none
character*(*) XText,ZText
integer x,XLen

XText=ZText
XLen=len_trim(XText)
do x=1,XLen
 if(ichar(XText(x:x)).ge.65.and.ichar(XText(x:x)).le.90) then
  XText(x:x)=char(ichar(XText(x:x))+32)
 endif
end do

return
end

!******************************************
subroutine xmlRow(XRow)
implicit none

integer CurUnit,CurRow,CurCol
character*3000 CurBuf
common/XMLExcel/CurUnit,CurRow,CurCol,CurBuf

integer XRow,xDone,IsRow,pi,pa,pb
character*3000 TextLine,TextComp

xDone=0
do while (xDone.eq.0)
 if(len_trim(CurBuf).gt.0) then !Check if there is anything left over
  TextLine=CurBuf
  CurBuf=''
 else
  read(CurUnit,'(a)') TextLine
 endif
 call xmlLC(TextLine,TextComp)
 IsRow=index(TextComp,'<row')
 if(IsRow.ne.0) then
  pi=index(TextComp,'index=')
  if(pi.eq.0) then
   CurRow=CurRow+1
  else
   pa=index(TextLine(pi+1:),'"')+pi
   pb=index(TextLine(pa+1:),'"')+pa
   read(TextLine(pa+1:pb-1),*) CurRow
  endif
 endif
 if(CurRow.eq.XRow) xDone=1
end do
CurCol=0

!Check to see if there is anything left over on this line.
pa=index(TextLine(IsRow+1:),'>')
if(pa.gt.0) then
 pb=len_trim(TextLine)
 if(pb.gt.(IsRow+pa)) CurBuf=TextLine(IsRow+pa+1:)
else
 CurBuf=''
endif

return
end

!******************************************
subroutine xmlRCell(XCol,AData)
implicit none

integer XCol,pi
real AData
character*200 XData

call xmlData(XCol,XData)
pi=len_trim(XData)
read(XData(1:pi),*) AData

return
end

!******************************************
subroutine xmlICell(XCol,AData)
implicit none

integer XCol,pi
integer AData
character*200 XData

call xmlData(XCol,XData)
pi=len_trim(XData)
read(XData(1:pi),*) AData

return
end

!******************************************
subroutine xmlCCell(XCol,AData)
implicit none

integer XCol,pi
character*(*) AData
character*200 XData

call xmlData(XCol,XData)
pi=len_trim(XData)
AData=XData(1:pi)

return
end

!******************************************
subroutine xmlData(XCol,XData)
implicit none

integer CurUnit,CurRow,CurCol
character*3000 CurBuf
common/XMLExcel/CurUnit,CurRow,CurCol,CurBuf

integer XCol,xDone,IsCol,IsAll,pi,pa,pb,pd,pe
character*200 XData
character*3000 TextLine,TextComp,WorkLine

20 continue

xDone=0
do while (xDone.eq.0)
 if(len_trim(CurBuf).gt.0) then !Check if there is anything left over
  TextLine=CurBuf
  CurBuf=''
 else
  read(CurUnit,'(a)') TextLine
 endif
 call xmlLC(TextLine,TextComp)
 XDone=index(TextComp,'<cell')
end do
WorkLine=TextLine(XDone:)

call xmlLC(WorkLine,TextComp)
pe=index(TextComp,'</cell')
if(pe.eq.0) pe=index(TextComp,'/>')
if(pe.eq.0) then
 do while (pe.eq.0)
  read(CurUnit,'(a)') TextLine
  WorkLine=trim(WorkLine)//trim(TextLine)
  call xmlLC(WorkLine,TextComp)
  pe=index(TextComp,'</cell')
  if(pe.eq.0) pe=index(TextComp,'/>')
 end do
endif
pe=pe+index(WorkLine(pe+1:),'>')
if(len(WorkLine).gt.pe) CurBuf=WorkLine(pe+1:)
WorkLine=WorkLine(1:pe)

call xmlLC(WorkLine,TextComp)
pi=index(TextComp,'index=')
if(pi.eq.0) then
 CurCol=CurCol+1
else
 pa=index(WorkLine(pi+1:),'"')+pi
 pb=index(WorkLine(pa+1:),'"')+pa
 read(WorkLine(pa+1:pb-1),*) CurCol
endif

if(CurCol.ne.XCol) goto 20

pd=index(TextComp,'<data')
pa=index(TextComp(pd+1:),'>')+pd
pb=index(TextComp(pa+1:),'</data')+pa
XData=WorkLine(pa+1:pb-1)

return
end

!******************************************
subroutine xmlRow2(XRow, error)
! Only used in WEM as of 2010.23.08
implicit none

integer CurUnit,CurRow,CurCol
character*3000 CurBuf
common/XMLExcel/CurUnit,CurRow,CurCol,CurBuf

integer XRow,xDone,IsRow,pi,pa,pb,error
character*3000 TextLine,TextComp

xDone=0
do while (xDone.eq.0)
 if(len_trim(CurBuf).gt.0) then !Check if there is anything left over
  TextLine=CurBuf
  CurBuf=''
 else
  read(CurUnit,'(a)', IOSTAT=error) TextLine
  if(error < 0) then !Exit program if end of file reached
   return
  endif
 endif

 call xmlLC(TextLine,TextComp)
 IsRow=index(TextComp,'<row')
 if(IsRow.ne.0) then
  pi=index(TextComp,'index=')
  if(pi.eq.0) then
   CurRow=CurRow+1
  else
   pa=index(TextLine(pi+1:),'"')+pi
   pb=index(TextLine(pa+1:),'"')+pa
   read(TextLine(pa+1:pb-1),*) CurRow
  endif
 endif
 if(CurRow.eq.XRow) xDone=1
end do
CurCol=0

!Check to see if there is anything left over on this line.
pa=index(TextLine(IsRow+1:),'>')
if(pa.gt.0) then
 pb=len_trim(TextLine)
 if(pb.gt.(IsRow+pa)) CurBuf=TextLine(IsRow+pa+1:)
else
 CurBuf=''
endif

return
end

!******************************************
function xmlRCell2(XCol)
! Only used in WEM as of 2010.23.08
implicit none

integer XCol,pi
real*8 xmlRCell2
character*200 XData

call xmlData(XCol,XData)
pi=len_trim(XData)

read(XData(1:pi),*) xmlRCell2
return
end

!******************************************
function xmlICell2(XCol)
! Only used in WEM as of 2010.23.08
implicit none

integer XCol,pi
integer xmlICell2
character*200 XData

call xmlData(XCol,XData)
pi=len_trim(XData)

read(XData(1:pi),*) xmlICell2
return
end
