! $Header: m:/default/source/RCS/gdxf9def.f,v 1.2 2011/02/18 21:04:29 dsa Exp $
! ?header?
! Interfaces for fortran-callable C library of "GDX" routines.  GDX routines implement data structures
! used in connection with the GAMS.  For NEMS, this routines here are referenced in filer to allow read/writes of gdx files
! for GAMS interface.
!DSA! lines are changes to convert from USE module to linkable object code since
!DSA! this is  compatible with NEMS configuration management scripts.
!
!DSA! MODULE gdxf9def
!DSA! IMPLICIT NONE


!DSA! CONTAINS


  LOGICAL FUNCTION gdxCreate(pgdx, errMsg)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxCreate
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxCreate
    INTEGER(KIND=8), INTENT(OUT) :: pgdx
    CHARACTER(LEN=*), INTENT(OUT) :: errMsg
    LOGICAL(KIND=4), EXTERNAL :: c_gdxCreate

    gdxCreate = c_gdxCreate(pgdx, loc(errMsg), %val(len(errMsg)))
  END FUNCTION gdxCreate

  LOGICAL FUNCTION gdxCreateD(pgdx, dirName, errMsg)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxCreateD
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxCreateD
    INTEGER(KIND=8), INTENT(OUT) :: pgdx
    CHARACTER(LEN=*), INTENT(IN) :: dirName
    CHARACTER(LEN=*), INTENT(OUT) :: errMsg
    LOGICAL(KIND=4), EXTERNAL :: c_gdxCreateD

    gdxCreateD = c_gdxCreateD(pgdx, loc(dirName), %val(len(dirName)), &
         loc(errMsg), %val(len(errMsg)))
  END FUNCTION gdxCreateD

  LOGICAL FUNCTION gdxCreateL(pgdx, libName, errMsg)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxCreateL
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxCreateL
    INTEGER(KIND=8), INTENT(OUT) :: pgdx
    CHARACTER(LEN=*), INTENT(IN) :: libName
    CHARACTER(LEN=*), INTENT(OUT) :: errMsg
    LOGICAL(KIND=4), EXTERNAL :: c_gdxCreateL

    gdxCreateL = c_gdxCreateL(pgdx, loc(libName), %val(len(libName)), &
         loc(errMsg), %val(len(errMsg)))
  END FUNCTION gdxCreateL

  LOGICAL FUNCTION gdxGetReady(errMsg)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxGetReady
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxGetReady
    CHARACTER(LEN=*), INTENT(OUT) :: errMsg
    LOGICAL(KIND=4), EXTERNAL :: c_gdxGetReady

    gdxGetReady = c_gdxGetReady(loc(errMsg), %val(len(errMsg)))
  END FUNCTION gdxGetReady

  LOGICAL FUNCTION gdxGetReadyD(dirName, errMsg)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxGetReadyD
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxGetReadyD
    CHARACTER(LEN=*), INTENT(IN) :: dirName
    CHARACTER(LEN=*), INTENT(OUT) :: errMsg
    LOGICAL(KIND=4), EXTERNAL :: c_gdxGetReadyD

    gdxGetReadyD = c_gdxGetReadyD(loc(dirName), %val(len(dirName)), &
         loc(errMsg), %val(len(errMsg)))
  END FUNCTION gdxGetReadyD

  LOGICAL FUNCTION gdxGetReadyL(libName, errMsg)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxGetReadyL
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxGetReadyL
    CHARACTER(LEN=*), INTENT(IN) :: libName
    CHARACTER(LEN=*), INTENT(OUT) :: errMsg
    LOGICAL(KIND=4), EXTERNAL :: c_gdxGetReadyL

    gdxGetReadyL = c_gdxGetReadyL(loc(libName), %val(len(libName)), &
         loc(errMsg), %val(len(errMsg)))
  END FUNCTION gdxGetReadyL

  SUBROUTINE gdxExit(i)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxExit
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxExit
    INTEGER(KIND=4), INTENT(IN) :: i

    CALL c_gdxExit(%val(i))
  END SUBROUTINE gdxExit

  LOGICAL FUNCTION gdxFree(pgdx)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxFree
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxFree
    INTEGER(KIND=8), INTENT(OUT) :: pgdx
    LOGICAL(KIND=4), EXTERNAL :: c_gdxFree

    gdxFree = c_gdxFree(pgdx)
  END FUNCTION gdxFree

  LOGICAL FUNCTION gdxGetScreenIndicator()
    !DEC$ ATTRIBUTES STDCALL :: c_gdxGetScreenIndicator
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxGetScreenIndicator
    LOGICAL(KIND=4), EXTERNAL :: c_gdxGetScreenIndicator

    gdxGetScreenIndicator = c_gdxGetScreenIndicator()
  END FUNCTION gdxGetScreenIndicator

  SUBROUTINE gdxSetScreenIndicator(scrInd)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxSetScreenIndicator
    INTEGER(KIND=4), INTENT(IN) :: scrInd
    EXTERNAL c_gdxSetScreenIndicator

    CALL c_gdxSetScreenIndicator(%val(scrInd))
  END SUBROUTINE gdxSetScreenIndicator

  LOGICAL FUNCTION gdxGetExitIndicator()
    !DEC$ ATTRIBUTES STDCALL :: c_gdxGetExitIndicator
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxGetExitIndicator
    LOGICAL(KIND=4), EXTERNAL :: c_gdxGetExitIndicator

    gdxGetExitIndicator = c_gdxGetExitIndicator()
  END FUNCTION gdxGetExitIndicator

  SUBROUTINE gdxSetExitIndicator(extInd)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxSetExitIndicator
    INTEGER(KIND=4), INTENT(IN) :: extInd
    EXTERNAL c_gdxSetExitIndicator

    CALL c_gdxSetExitIndicator(%val(extInd))
  END SUBROUTINE gdxSetExitIndicator

  SUBROUTINE gdxSetErrorCallback(cbFunc)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxSetErrorCallback
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxSetErrorCallback
    INTEGER(KIND=4), EXTERNAL :: cbFunc

    CALL c_gdxSetErrorCallback(cbFunc)
  END SUBROUTINE gdxSetErrorCallback

  INTEGER FUNCTION gdxGetAPIErrorCount()
    !DEC$ ATTRIBUTES STDCALL :: c_gdxGetAPIErrorCount
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxGetAPIErrorCount
   INTEGER(KIND=4), EXTERNAL :: c_gdxGetAPIErrorCount

    gdxGetAPIErrorCount = c_gdxGetAPIErrorCount()
  END FUNCTION gdxGetAPIErrorCount


  INTEGER(KIND=4) FUNCTION gdxAcronymAdd(pgdx,AName,AText,Indx)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxAcronymAdd
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxAcronymAdd
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    CHARACTER(LEN=*), INTENT(IN) :: AName
    CHARACTER(LEN=*), INTENT(IN) :: AText
    INTEGER(KIND=4), INTENT(IN) :: Indx
    INTEGER(KIND=4), EXTERNAL :: c_gdxAcronymAdd

    gdxAcronymAdd=c_gdxAcronymAdd(%val(pgdx),loc(AName),%val(len(AName)), &
         loc(AText),%val(len(AText)),%val(Indx))
  END FUNCTION gdxAcronymAdd

  INTEGER(KIND=4) FUNCTION gdxAcronymCount(pgdx)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxAcronymCount
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxAcronymCount
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), EXTERNAL :: c_gdxAcronymCount

    gdxAcronymCount=c_gdxAcronymCount(%val(pgdx))
  END FUNCTION gdxAcronymCount

  INTEGER(KIND=4) FUNCTION gdxAcronymGetInfo(pgdx,N,AName,AText,Indx)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxAcronymGetInfo
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxAcronymGetInfo
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: N
    CHARACTER(LEN=*), INTENT(OUT) :: AName
    CHARACTER(LEN=*), INTENT(OUT) :: AText
    INTEGER(KIND=4), INTENT(IN OUT) :: Indx
    INTEGER(KIND=4), EXTERNAL :: c_gdxAcronymGetInfo

    gdxAcronymGetInfo=c_gdxAcronymGetInfo(%val(pgdx),%val(N),loc(AName),%val(len(AName)), &
         loc(AText),%val(len(AText)),Indx)
  END FUNCTION gdxAcronymGetInfo

  INTEGER(KIND=4) FUNCTION gdxAcronymGetMapping(pgdx,N,orgIndx,newIndx, &
         autoIndex)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxAcronymGetMapping
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxAcronymGetMapping
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: N
    INTEGER(KIND=4), INTENT(IN OUT) :: orgIndx
    INTEGER(KIND=4), INTENT(IN OUT) :: newIndx
    INTEGER(KIND=4), INTENT(IN OUT) :: autoIndex
    INTEGER(KIND=4), EXTERNAL :: c_gdxAcronymGetMapping

    gdxAcronymGetMapping=c_gdxAcronymGetMapping(%val(pgdx),%val(N), &
         orgIndx,newIndx,autoIndex)
  END FUNCTION gdxAcronymGetMapping

  INTEGER(KIND=4) FUNCTION gdxAcronymIndex(pgdx,V)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxAcronymIndex
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxAcronymIndex
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    REAL(KIND=8), INTENT(IN) :: V
    INTEGER(KIND=4), EXTERNAL :: c_gdxAcronymIndex

    gdxAcronymIndex=c_gdxAcronymIndex(%val(pgdx),%val(V))
  END FUNCTION gdxAcronymIndex

  INTEGER(KIND=4) FUNCTION gdxAcronymName(pgdx,V,AName)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxAcronymName
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxAcronymName
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    REAL(KIND=8), INTENT(IN) :: V
    CHARACTER(LEN=*), INTENT(OUT) :: AName
    INTEGER(KIND=4), EXTERNAL :: c_gdxAcronymName

    gdxAcronymName=c_gdxAcronymName(%val(pgdx),%val(V),loc(AName),%val(len(AName)))
  END FUNCTION gdxAcronymName

  INTEGER(KIND=4) FUNCTION gdxAcronymNextNr(pgdx,NV)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxAcronymNextNr
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxAcronymNextNr
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: NV
    INTEGER(KIND=4), EXTERNAL :: c_gdxAcronymNextNr

    gdxAcronymNextNr=c_gdxAcronymNextNr(%val(pgdx),%val(NV))
  END FUNCTION gdxAcronymNextNr

  INTEGER(KIND=4) FUNCTION gdxAcronymSetInfo(pgdx,N,AName,AText,Indx)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxAcronymSetInfo
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxAcronymSetInfo
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: N
    CHARACTER(LEN=*), INTENT(IN) :: AName
    CHARACTER(LEN=*), INTENT(IN) :: AText
    INTEGER(KIND=4), INTENT(IN) :: Indx
    INTEGER(KIND=4), EXTERNAL :: c_gdxAcronymSetInfo

    gdxAcronymSetInfo=c_gdxAcronymSetInfo(%val(pgdx),%val(N),loc(AName),%val(len(AName)), &
         loc(AText),%val(len(AText)),%val(Indx))
  END FUNCTION gdxAcronymSetInfo

  REAL(KIND=8) FUNCTION gdxAcronymValue(pgdx,Indx)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxAcronymValue
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxAcronymValue
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: Indx
    REAL(KIND=8), EXTERNAL :: c_gdxAcronymValue

    gdxAcronymValue=c_gdxAcronymValue(%val(pgdx),%val(Indx))
  END FUNCTION gdxAcronymValue

  INTEGER(KIND=4) FUNCTION gdxAddAlias(pgdx,AName1,AName2)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxAddAlias
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxAddAlias
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    CHARACTER(LEN=*), INTENT(IN) :: AName1
    CHARACTER(LEN=*), INTENT(IN) :: AName2
    INTEGER(KIND=4), EXTERNAL :: c_gdxAddAlias

    gdxAddAlias=c_gdxAddAlias(%val(pgdx),loc(AName1),%val(len(AName1)), &
         loc(AName2),%val(len(AName2)))
  END FUNCTION gdxAddAlias

  INTEGER(KIND=4) FUNCTION gdxAddSetText(pgdx,s,N)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxAddSetText
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxAddSetText
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    CHARACTER(LEN=*), INTENT(IN) :: s
    INTEGER(KIND=4), INTENT(IN OUT) :: N
    INTEGER(KIND=4), EXTERNAL :: c_gdxAddSetText

    gdxAddSetText=c_gdxAddSetText(%val(pgdx),loc(s),%val(len(s)), &
         N)
  END FUNCTION gdxAddSetText

  INTEGER(KIND=4) FUNCTION gdxAutoConvert(pgdx,NV)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxAutoConvert
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxAutoConvert
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: NV
    INTEGER(KIND=4), EXTERNAL :: c_gdxAutoConvert

    gdxAutoConvert=c_gdxAutoConvert(%val(pgdx),%val(NV))
  END FUNCTION gdxAutoConvert

  INTEGER(KIND=4) FUNCTION gdxClose(pgdx)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxClose
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxClose
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), EXTERNAL :: c_gdxClose

    gdxClose=c_gdxClose(%val(pgdx))
  END FUNCTION gdxClose

  INTEGER(KIND=4) FUNCTION gdxDataErrorCount(pgdx)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxDataErrorCount
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxDataErrorCount
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), EXTERNAL :: c_gdxDataErrorCount

    gdxDataErrorCount=c_gdxDataErrorCount(%val(pgdx))
  END FUNCTION gdxDataErrorCount

  INTEGER(KIND=4) FUNCTION gdxDataErrorRecord(pgdx,Rn,AElements,AVals)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxDataErrorRecord
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxDataErrorRecord
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: Rn
    INTEGER(KIND=4), DIMENSION(:), INTENT(IN OUT) :: AElements
    REAL(KIND=8), DIMENSION(:), INTENT(IN OUT) :: AVals
    INTEGER(KIND=4), EXTERNAL :: c_gdxDataErrorRecord

    gdxDataErrorRecord=c_gdxDataErrorRecord(%val(pgdx),%val(Rn),AElements, &
         AVals)
  END FUNCTION gdxDataErrorRecord

  INTEGER(KIND=4) FUNCTION gdxDataReadDone(pgdx)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxDataReadDone
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxDataReadDone
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), EXTERNAL :: c_gdxDataReadDone

    gdxDataReadDone=c_gdxDataReadDone(%val(pgdx))
  END FUNCTION gdxDataReadDone

  INTEGER(KIND=4) FUNCTION gdxDataReadFilteredStart(pgdx,SyNr,ADomainNrs, &
         NrRecs)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxDataReadFilteredStart
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxDataReadFilteredStart
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: SyNr
    INTEGER(KIND=4), DIMENSION(:), INTENT(IN) :: ADomainNrs
    INTEGER(KIND=4), INTENT(IN OUT) :: NrRecs
    INTEGER(KIND=4), EXTERNAL :: c_gdxDataReadFilteredStart

    gdxDataReadFilteredStart=c_gdxDataReadFilteredStart(%val(pgdx), &
         %val(SyNr),ADomainNrs,NrRecs)
  END FUNCTION gdxDataReadFilteredStart

  INTEGER(KIND=4) FUNCTION gdxDataReadMap(pgdx,Rn,AElements,AVals, &
         AFDim)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxDataReadMap
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxDataReadMap
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: Rn
    INTEGER(KIND=4), DIMENSION(:), INTENT(IN OUT) :: AElements
    REAL(KIND=8), DIMENSION(:), INTENT(IN OUT) :: AVals
    INTEGER(KIND=4), INTENT(IN OUT) :: AFDim
    INTEGER(KIND=4), EXTERNAL :: c_gdxDataReadMap

    gdxDataReadMap=c_gdxDataReadMap(%val(pgdx),%val(Rn),AElements, &
         AVals,AFDim)
  END FUNCTION gdxDataReadMap

  INTEGER(KIND=4) FUNCTION gdxDataReadMapStart(pgdx,SyNr,NrRecs)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxDataReadMapStart
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxDataReadMapStart
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: SyNr
    INTEGER(KIND=4), INTENT(IN OUT) :: NrRecs
    INTEGER(KIND=4), EXTERNAL :: c_gdxDataReadMapStart

    gdxDataReadMapStart=c_gdxDataReadMapStart(%val(pgdx),%val(SyNr), &
         NrRecs)
  END FUNCTION gdxDataReadMapStart

  INTEGER(KIND=4) FUNCTION gdxDataReadRaw(pgdx,AElements,AVals,AFDim)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxDataReadRaw
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxDataReadRaw
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), DIMENSION(:), INTENT(IN OUT) :: AElements
    REAL(KIND=8), DIMENSION(:), INTENT(IN OUT) :: AVals
    INTEGER(KIND=4), INTENT(IN OUT) :: AFDim
    INTEGER(KIND=4), EXTERNAL :: c_gdxDataReadRaw

    gdxDataReadRaw=c_gdxDataReadRaw(%val(pgdx),AElements,AVals,AFDim)
  END FUNCTION gdxDataReadRaw

  INTEGER(KIND=4) FUNCTION gdxDataReadRawStart(pgdx,SyNr,NrRecs)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxDataReadRawStart
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxDataReadRawStart
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: SyNr
    INTEGER(KIND=4), INTENT(IN OUT) :: NrRecs
    INTEGER(KIND=4), EXTERNAL :: c_gdxDataReadRawStart

    gdxDataReadRawStart=c_gdxDataReadRawStart(%val(pgdx),%val(SyNr), &
         NrRecs)
  END FUNCTION gdxDataReadRawStart

  INTEGER(KIND=4) FUNCTION gdxDataReadSlice(pgdx,AFiltElements,ADim, &
         DP)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxDataReadSlice
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxDataReadSlice
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: AFiltElements
    INTEGER(KIND=4), INTENT(IN OUT) :: ADim
    EXTERNAL DP
    INTEGER(KIND=4), EXTERNAL :: c_gdxDataReadSlice

    gdxDataReadSlice=c_gdxDataReadSlice(%val(pgdx),loc(AFiltElements),%val(len(AFiltElements)), &
         ADim,DP)
  END FUNCTION gdxDataReadSlice

  INTEGER(KIND=4) FUNCTION gdxDataReadSliceStart(pgdx,ASyNr,ANrElems)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxDataReadSliceStart
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxDataReadSliceStart
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: ASyNr
    INTEGER(KIND=4), DIMENSION(:), INTENT(IN OUT) :: ANrElems
    INTEGER(KIND=4), EXTERNAL :: c_gdxDataReadSliceStart

    gdxDataReadSliceStart=c_gdxDataReadSliceStart(%val(pgdx),%val(ASyNr), &
         ANrElems)
  END FUNCTION gdxDataReadSliceStart

  INTEGER(KIND=4) FUNCTION gdxDataReadStr(pgdx,AStrElements,AVals, &
         AFDim)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxDataReadStr
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxDataReadStr
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    CHARACTER(LEN=*), DIMENSION(:), INTENT(OUT) :: AStrElements
    REAL(KIND=8), DIMENSION(:), INTENT(IN OUT) :: AVals
    INTEGER(KIND=4), INTENT(IN OUT) :: AFDim
    INTEGER(KIND=4), EXTERNAL :: c_gdxDataReadStr

    gdxDataReadStr=c_gdxDataReadStr(%val(pgdx),loc(AStrElements),%val(len(AStrElements)), &
         AVals,AFDim)
  END FUNCTION gdxDataReadStr

  INTEGER(KIND=4) FUNCTION gdxDataReadStrStart(pgdx,SyNr,NrRecs)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxDataReadStrStart
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxDataReadStrStart
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: SyNr
    INTEGER(KIND=4), INTENT(IN OUT) :: NrRecs
    INTEGER(KIND=4), EXTERNAL :: c_gdxDataReadStrStart

    gdxDataReadStrStart=c_gdxDataReadStrStart(%val(pgdx),%val(SyNr), &
         NrRecs)
  END FUNCTION gdxDataReadStrStart

  INTEGER(KIND=4) FUNCTION gdxDataSliceUELS(pgdx,AIndx,AUELs)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxDataSliceUELS
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxDataSliceUELS
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), DIMENSION(:), INTENT(IN) :: AIndx
    CHARACTER(LEN=*), DIMENSION(:), INTENT(OUT) :: AUELs
    INTEGER(KIND=4), EXTERNAL :: c_gdxDataSliceUELS

    gdxDataSliceUELS=c_gdxDataSliceUELS(%val(pgdx),AIndx,loc(AUELs),%val(len(AUELs)))
  END FUNCTION gdxDataSliceUELS

  INTEGER(KIND=4) FUNCTION gdxDataWriteDone(pgdx)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxDataWriteDone
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxDataWriteDone
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), EXTERNAL :: c_gdxDataWriteDone

    gdxDataWriteDone=c_gdxDataWriteDone(%val(pgdx))
  END FUNCTION gdxDataWriteDone

  INTEGER(KIND=4) FUNCTION gdxDataWriteMap(pgdx,AElements,AVals)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxDataWriteMap
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxDataWriteMap
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), DIMENSION(:), INTENT(IN) :: AElements
    REAL(KIND=8), DIMENSION(:), INTENT(IN) :: AVals
    INTEGER(KIND=4), EXTERNAL :: c_gdxDataWriteMap

    gdxDataWriteMap=c_gdxDataWriteMap(%val(pgdx),AElements,AVals)
  END FUNCTION gdxDataWriteMap

  INTEGER(KIND=4) FUNCTION gdxDataWriteMapStart(pgdx,AName,AText, &
         ADim,AType,AUserInfo)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxDataWriteMapStart
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxDataWriteMapStart
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    CHARACTER(LEN=*), INTENT(IN) :: AName
    CHARACTER(LEN=*), INTENT(IN) :: AText
    INTEGER(KIND=4), INTENT(IN) :: ADim
    INTEGER(KIND=4), INTENT(IN) :: AType
    INTEGER(KIND=4), INTENT(IN) :: AUserInfo
    INTEGER(KIND=4), EXTERNAL :: c_gdxDataWriteMapStart

    gdxDataWriteMapStart=c_gdxDataWriteMapStart(%val(pgdx),loc(AName),%val(len(AName)), &
         loc(AText),%val(len(AText)),%val(ADim),%val(AType),%val(AUserInfo))
  END FUNCTION gdxDataWriteMapStart

  INTEGER(KIND=4) FUNCTION gdxDataWriteRaw(pgdx,AElements,AVals)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxDataWriteRaw
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxDataWriteRaw
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), DIMENSION(:), INTENT(IN) :: AElements
    REAL(KIND=8), DIMENSION(:), INTENT(IN) :: AVals
    INTEGER(KIND=4), EXTERNAL :: c_gdxDataWriteRaw

    gdxDataWriteRaw=c_gdxDataWriteRaw(%val(pgdx),AElements,AVals)
  END FUNCTION gdxDataWriteRaw

  INTEGER(KIND=4) FUNCTION gdxDataWriteRawStart(pgdx,AName,AText, &
         ADim,AType,AUserInfo)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxDataWriteRawStart
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxDataWriteRawStart
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    CHARACTER(LEN=*), INTENT(IN) :: AName
    CHARACTER(LEN=*), INTENT(IN) :: AText
    INTEGER(KIND=4), INTENT(IN) :: ADim
    INTEGER(KIND=4), INTENT(IN) :: AType
    INTEGER(KIND=4), INTENT(IN) :: AUserInfo
    INTEGER(KIND=4), EXTERNAL :: c_gdxDataWriteRawStart

    gdxDataWriteRawStart=c_gdxDataWriteRawStart(%val(pgdx),loc(AName),%val(len(AName)), &
         loc(AText),%val(len(AText)),%val(ADim),%val(AType),%val(AUserInfo))
  END FUNCTION gdxDataWriteRawStart

  INTEGER(KIND=4) FUNCTION gdxDataWriteStr(pgdx,AStrElements,AVals)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxDataWriteStr
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxDataWriteStr
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: AStrElements
    REAL(KIND=8), DIMENSION(:), INTENT(IN) :: AVals
    INTEGER(KIND=4), EXTERNAL :: c_gdxDataWriteStr
    gdxDataWriteStr=c_gdxDataWriteStr(%val(pgdx),loc(AStrElements),%val(len(AStrElements)), &
         AVals)
  END FUNCTION gdxDataWriteStr

  INTEGER(KIND=4) FUNCTION gdxDataWriteStrStart(pgdx,AName,AText, &
         ADim,AType,AUserInfo)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxDataWriteStrStart
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxDataWriteStrStart
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    CHARACTER(LEN=*), INTENT(IN) :: AName
    CHARACTER(LEN=*), INTENT(IN) :: AText
    INTEGER(KIND=4), INTENT(IN) :: ADim
    INTEGER(KIND=4), INTENT(IN) :: AType
    INTEGER(KIND=4), INTENT(IN) :: AUserInfo
    INTEGER(KIND=4), EXTERNAL :: c_gdxDataWriteStrStart

    gdxDataWriteStrStart=c_gdxDataWriteStrStart(%val(pgdx),loc(AName),%val(len(AName)), &
         loc(AText),%val(len(AText)),%val(ADim),%val(AType),%val(AUserInfo))
  END FUNCTION gdxDataWriteStrStart

  INTEGER(KIND=4) FUNCTION gdxGetDLLVersion(pgdx,V)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxGetDLLVersion
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxGetDLLVersion
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    CHARACTER(LEN=*), INTENT(OUT) :: V
    INTEGER(KIND=4), EXTERNAL :: c_gdxGetDLLVersion

    gdxGetDLLVersion=c_gdxGetDLLVersion(%val(pgdx),loc(V),%val(len(V)))
  END FUNCTION gdxGetDLLVersion

  INTEGER(KIND=4) FUNCTION gdxErrorCount(pgdx)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxErrorCount
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxErrorCount
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), EXTERNAL :: c_gdxErrorCount

    gdxErrorCount=c_gdxErrorCount(%val(pgdx))
  END FUNCTION gdxErrorCount

  INTEGER(KIND=4) FUNCTION gdxErrorStr(pgdx,N,s)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxErrorStr
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxErrorStr
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: N
    CHARACTER(LEN=*), INTENT(OUT) :: s
    INTEGER(KIND=4), EXTERNAL :: c_gdxErrorStr

    gdxErrorStr=c_gdxErrorStr(%val(pgdx),%val(N),loc(s),%val(len(s)))
  END FUNCTION gdxErrorStr

  INTEGER(KIND=4) FUNCTION gdxFileInfo(pgdx,FileVer,ComprLev)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxFileInfo
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxFileInfo
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN OUT) :: FileVer
    INTEGER(KIND=4), INTENT(IN OUT) :: ComprLev
    INTEGER(KIND=4), EXTERNAL :: c_gdxFileInfo

    gdxFileInfo=c_gdxFileInfo(%val(pgdx),FileVer,ComprLev)
  END FUNCTION gdxFileInfo

  INTEGER(KIND=4) FUNCTION gdxFileVersion(pgdx,FileStr,ProduceStr)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxFileVersion
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxFileVersion
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    CHARACTER(LEN=*), INTENT(OUT) :: FileStr
    CHARACTER(LEN=*), INTENT(OUT) :: ProduceStr
    INTEGER(KIND=4), EXTERNAL :: c_gdxFileVersion

    gdxFileVersion=c_gdxFileVersion(%val(pgdx),loc(FileStr),%val(len(FileStr)), &
         loc(ProduceStr),%val(len(ProduceStr)))
  END FUNCTION gdxFileVersion

  INTEGER(KIND=4) FUNCTION gdxFilterExists(pgdx,N)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxFilterExists
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxFilterExists
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: N
    INTEGER(KIND=4), EXTERNAL :: c_gdxFilterExists

    gdxFilterExists=c_gdxFilterExists(%val(pgdx),%val(N))
  END FUNCTION gdxFilterExists

  INTEGER(KIND=4) FUNCTION gdxFilterRegister(pgdx,V)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxFilterRegister
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxFilterRegister
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: V
    INTEGER(KIND=4), EXTERNAL :: c_gdxFilterRegister

    gdxFilterRegister=c_gdxFilterRegister(%val(pgdx),%val(V))
  END FUNCTION gdxFilterRegister

  INTEGER(KIND=4) FUNCTION gdxFilterRegisterDone(pgdx)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxFilterRegisterDone
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxFilterRegisterDone
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), EXTERNAL :: c_gdxFilterRegisterDone

    gdxFilterRegisterDone=c_gdxFilterRegisterDone(%val(pgdx))
  END FUNCTION gdxFilterRegisterDone

  INTEGER(KIND=4) FUNCTION gdxFilterRegisterStart(pgdx,Nr)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxFilterRegisterStart
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxFilterRegisterStart
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: Nr
    INTEGER(KIND=4), EXTERNAL :: c_gdxFilterRegisterStart

    gdxFilterRegisterStart=c_gdxFilterRegisterStart(%val(pgdx),%val(Nr))
  END FUNCTION gdxFilterRegisterStart

  INTEGER(KIND=4) FUNCTION gdxFindSymbol(pgdx,AName,AIx)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxFindSymbol
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxFindSymbol
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    CHARACTER(LEN=*), INTENT(IN) :: AName
    INTEGER(KIND=4), INTENT(IN OUT) :: AIx
    INTEGER(KIND=4), EXTERNAL :: c_gdxFindSymbol

    gdxFindSymbol=c_gdxFindSymbol(%val(pgdx),loc(AName),%val(len(AName)), &
         AIx)
  END FUNCTION gdxFindSymbol

  INTEGER(KIND=4) FUNCTION gdxGetElemText(pgdx,N,s,Node)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxGetElemText
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxGetElemText
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: N
    CHARACTER(LEN=*), INTENT(OUT) :: s
    INTEGER(KIND=4), INTENT(IN OUT) :: Node
    INTEGER(KIND=4), EXTERNAL :: c_gdxGetElemText

    gdxGetElemText=c_gdxGetElemText(%val(pgdx),%val(N),loc(s),%val(len(s)), &
         Node)
  END FUNCTION gdxGetElemText

  INTEGER(KIND=4) FUNCTION gdxGetLastError(pgdx)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxGetLastError
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxGetLastError
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), EXTERNAL :: c_gdxGetLastError

    gdxGetLastError=c_gdxGetLastError(%val(pgdx))
  END FUNCTION gdxGetLastError

  INTEGER(KIND=4) FUNCTION gdxGetSpecialValues(pgdx,AVals)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxGetSpecialValues
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxGetSpecialValues
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    REAL(KIND=8), DIMENSION(:), INTENT(IN OUT) :: AVals
    INTEGER(KIND=4), EXTERNAL :: c_gdxGetSpecialValues

    gdxGetSpecialValues=c_gdxGetSpecialValues(%val(pgdx),AVals)
  END FUNCTION gdxGetSpecialValues

  INTEGER(KIND=4) FUNCTION gdxGetUEL(pgdx,UelNr,s)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxGetUEL
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxGetUEL
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: UelNr
    CHARACTER(LEN=*), INTENT(OUT) :: s
    INTEGER(KIND=4), EXTERNAL :: c_gdxGetUEL

    gdxGetUEL=c_gdxGetUEL(%val(pgdx),%val(UelNr),loc(s),%val(len(s)))
  END FUNCTION gdxGetUEL

  INTEGER(KIND=4) FUNCTION gdxMapValue(pgdx,D,sv)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxMapValue
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxMapValue
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    REAL(KIND=8), INTENT(IN) :: D
    INTEGER(KIND=4), INTENT(IN OUT) :: sv
    INTEGER(KIND=4), EXTERNAL :: c_gdxMapValue

    gdxMapValue=c_gdxMapValue(%val(pgdx),%val(D),sv)
  END FUNCTION gdxMapValue

  INTEGER(KIND=4) FUNCTION gdxOpenRead(pgdx,Afn,ErrNr)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxOpenRead
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxOpenRead
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    CHARACTER(LEN=*), INTENT(IN) :: Afn
    INTEGER(KIND=4), INTENT(IN OUT) :: ErrNr
    INTEGER(KIND=4), EXTERNAL :: c_gdxOpenRead

    gdxOpenRead=c_gdxOpenRead(%val(pgdx),loc(Afn),%val(len(Afn)), &
         ErrNr)
  END FUNCTION gdxOpenRead

  INTEGER(KIND=4) FUNCTION gdxOpenWrite(pgdx,Afn,AProducer,ErrNr)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxOpenWrite
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxOpenWrite
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    CHARACTER(LEN=*), INTENT(IN) :: Afn
    CHARACTER(LEN=*), INTENT(IN) :: AProducer
    INTEGER(KIND=4), INTENT(IN OUT) :: ErrNr
    INTEGER(KIND=4), EXTERNAL :: c_gdxOpenWrite

    gdxOpenWrite=c_gdxOpenWrite(%val(pgdx),loc(Afn),%val(len(Afn)), &
         loc(AProducer),%val(len(AProducer)),ErrNr)
  END FUNCTION gdxOpenWrite

  INTEGER(KIND=4) FUNCTION gdxOpenWriteEx(pgdx,Afn,AProducer,Compr, &
         ErrNr)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxOpenWriteEx
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxOpenWriteEx
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    CHARACTER(LEN=*), INTENT(IN) :: Afn
    CHARACTER(LEN=*), INTENT(IN) :: AProducer
    INTEGER(KIND=4), INTENT(IN) :: Compr
    INTEGER(KIND=4), INTENT(IN OUT) :: ErrNr
    INTEGER(KIND=4), EXTERNAL :: c_gdxOpenWriteEx

    gdxOpenWriteEx=c_gdxOpenWriteEx(%val(pgdx),loc(Afn),%val(len(Afn)), &
         loc(AProducer),%val(len(AProducer)),%val(Compr),ErrNr)
  END FUNCTION gdxOpenWriteEx

  INTEGER(KIND=4) FUNCTION gdxResetSpecialValues(pgdx)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxResetSpecialValues
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxResetSpecialValues
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), EXTERNAL :: c_gdxResetSpecialValues

    gdxResetSpecialValues=c_gdxResetSpecialValues(%val(pgdx))
  END FUNCTION gdxResetSpecialValues

  INTEGER(KIND=4) FUNCTION gdxSetHasText(pgdx,N)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxSetHasText
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxSetHasText
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: N
    INTEGER(KIND=4), EXTERNAL :: c_gdxSetHasText

    gdxSetHasText=c_gdxSetHasText(%val(pgdx),%val(N))
  END FUNCTION gdxSetHasText

  INTEGER(KIND=4) FUNCTION gdxSetReadSpecialValues(pgdx,AVals)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxSetReadSpecialValues
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxSetReadSpecialValues
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    REAL(KIND=8), DIMENSION(:), INTENT(IN) :: AVals
    INTEGER(KIND=4), EXTERNAL :: c_gdxSetReadSpecialValues

    gdxSetReadSpecialValues=c_gdxSetReadSpecialValues(%val(pgdx), &
         AVals)
  END FUNCTION gdxSetReadSpecialValues

  INTEGER(KIND=4) FUNCTION gdxSetSpecialValues(pgdx,AVals)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxSetSpecialValues
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxSetSpecialValues
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    REAL(KIND=8), DIMENSION(:), INTENT(IN) :: AVals
    INTEGER(KIND=4), EXTERNAL :: c_gdxSetSpecialValues

    gdxSetSpecialValues=c_gdxSetSpecialValues(%val(pgdx),AVals)
  END FUNCTION gdxSetSpecialValues

  INTEGER(KIND=4) FUNCTION gdxSetTextNodeNr(pgdx,N,Node)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxSetTextNodeNr
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxSetTextNodeNr
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: N
    INTEGER(KIND=4), INTENT(IN) :: Node
    INTEGER(KIND=4), EXTERNAL :: c_gdxSetTextNodeNr

    gdxSetTextNodeNr=c_gdxSetTextNodeNr(%val(pgdx),%val(N),%val(Node))
  END FUNCTION gdxSetTextNodeNr

  INTEGER(KIND=4) FUNCTION gdxSetTraceLevel(pgdx,N,s)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxSetTraceLevel
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxSetTraceLevel
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: N
    CHARACTER(LEN=*), INTENT(IN) :: s
    INTEGER(KIND=4), EXTERNAL :: c_gdxSetTraceLevel

    gdxSetTraceLevel=c_gdxSetTraceLevel(%val(pgdx),%val(N),loc(s),%val(len(s)))
  END FUNCTION gdxSetTraceLevel

  INTEGER(KIND=4) FUNCTION gdxSymbIndxMaxLength(pgdx,SyNr,DimInfo)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxSymbIndxMaxLength
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxSymbIndxMaxLength
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: SyNr
    INTEGER(KIND=4), DIMENSION(:), INTENT(IN OUT) :: DimInfo
    INTEGER(KIND=4), EXTERNAL :: c_gdxSymbIndxMaxLength

    gdxSymbIndxMaxLength=c_gdxSymbIndxMaxLength(%val(pgdx),%val(SyNr), &
         DimInfo)
  END FUNCTION gdxSymbIndxMaxLength

  INTEGER(KIND=4) FUNCTION gdxSymbMaxLength(pgdx)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxSymbMaxLength
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxSymbMaxLength
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), EXTERNAL :: c_gdxSymbMaxLength

    gdxSymbMaxLength=c_gdxSymbMaxLength(%val(pgdx))
  END FUNCTION gdxSymbMaxLength

  INTEGER(KIND=4) FUNCTION gdxSymbolAddComment(pgdx,SyNr,s)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxSymbolAddComment
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxSymbolAddComment
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: SyNr
    CHARACTER(LEN=*), INTENT(IN) :: s
    INTEGER(KIND=4), EXTERNAL :: c_gdxSymbolAddComment

    gdxSymbolAddComment=c_gdxSymbolAddComment(%val(pgdx),%val(SyNr), &
         loc(s),%val(len(s)))
  END FUNCTION gdxSymbolAddComment

  INTEGER(KIND=4) FUNCTION gdxSymbolGetComment(pgdx,SyNr,N,s)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxSymbolGetComment
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxSymbolGetComment
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: SyNr
    INTEGER(KIND=4), INTENT(IN) :: N
    CHARACTER(LEN=*), INTENT(OUT) :: s
    INTEGER(KIND=4), EXTERNAL :: c_gdxSymbolGetComment

    gdxSymbolGetComment=c_gdxSymbolGetComment(%val(pgdx),%val(SyNr), &
         %val(N),loc(s),%val(len(s)))
  END FUNCTION gdxSymbolGetComment

  INTEGER(KIND=4) FUNCTION gdxSymbolGetDomain(pgdx,SyNr,DomainIDs)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxSymbolGetDomain
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxSymbolGetDomain
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: SyNr
    INTEGER(KIND=4), DIMENSION(:), INTENT(IN OUT) :: DomainIDs
    INTEGER(KIND=4), EXTERNAL :: c_gdxSymbolGetDomain

    gdxSymbolGetDomain=c_gdxSymbolGetDomain(%val(pgdx),%val(SyNr), &
         DomainIDs)
  END FUNCTION gdxSymbolGetDomain

  INTEGER(KIND=4) FUNCTION gdxSymbolInfo(pgdx,SyNr,AName,ADim,ATyp)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxSymbolInfo
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxSymbolInfo
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: SyNr
    CHARACTER(LEN=*), INTENT(OUT) :: AName
    INTEGER(KIND=4), INTENT(IN OUT) :: ADim
    INTEGER(KIND=4), INTENT(IN OUT) :: ATyp
    INTEGER(KIND=4), EXTERNAL :: c_gdxSymbolInfo

    gdxSymbolInfo=c_gdxSymbolInfo(%val(pgdx),%val(SyNr),loc(AName),%val(len(AName)), &
         ADim,ATyp)
  END FUNCTION gdxSymbolInfo

  INTEGER(KIND=4) FUNCTION gdxSymbolInfoX(pgdx,SyNr,ACount,AUserInfo, &
         AExplTxt)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxSymbolInfoX
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxSymbolInfoX
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: SyNr
    INTEGER(KIND=4), INTENT(IN OUT) :: ACount
    INTEGER(KIND=4), INTENT(IN OUT) :: AUserInfo
    CHARACTER(LEN=*), INTENT(OUT) :: AExplTxt
    INTEGER(KIND=4), EXTERNAL :: c_gdxSymbolInfoX

    gdxSymbolInfoX=c_gdxSymbolInfoX(%val(pgdx),%val(SyNr),ACount, &
         AUserInfo,loc(AExplTxt),%val(len(AExplTxt)))
  END FUNCTION gdxSymbolInfoX

  INTEGER(KIND=4) FUNCTION gdxSymbolSetDomain(pgdx,DomainIDs)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxSymbolSetDomain
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxSymbolSetDomain
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: DomainIDs
    INTEGER(KIND=4), EXTERNAL :: c_gdxSymbolSetDomain

    gdxSymbolSetDomain=c_gdxSymbolSetDomain(%val(pgdx),loc(DomainIDs),%val(len(DomainIDs)))
  END FUNCTION gdxSymbolSetDomain

  INTEGER(KIND=4) FUNCTION gdxSystemInfo(pgdx,NrSy,NrUel)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxSystemInfo
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxSystemInfo
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN OUT) :: NrSy
    INTEGER(KIND=4), INTENT(IN OUT) :: NrUel
    INTEGER(KIND=4), EXTERNAL :: c_gdxSystemInfo

    gdxSystemInfo=c_gdxSystemInfo(%val(pgdx),NrSy,NrUel)
  END FUNCTION gdxSystemInfo

  INTEGER(KIND=4) FUNCTION gdxUELMaxLength(pgdx)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxUELMaxLength
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxUELMaxLength
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), EXTERNAL :: c_gdxUELMaxLength

    gdxUELMaxLength=c_gdxUELMaxLength(%val(pgdx))
  END FUNCTION gdxUELMaxLength

  INTEGER(KIND=4) FUNCTION gdxUELRegisterDone(pgdx)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxUELRegisterDone
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxUELRegisterDone
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), EXTERNAL :: c_gdxUELRegisterDone

    gdxUELRegisterDone=c_gdxUELRegisterDone(%val(pgdx))
  END FUNCTION gdxUELRegisterDone

  INTEGER(KIND=4) FUNCTION gdxUELRegisterMap(pgdx,UelNr,Elem)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxUELRegisterMap
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxUELRegisterMap
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: UelNr
    CHARACTER(LEN=*), INTENT(IN) :: Elem
    INTEGER(KIND=4), EXTERNAL :: c_gdxUELRegisterMap

    gdxUELRegisterMap=c_gdxUELRegisterMap(%val(pgdx),%val(UelNr), &
         loc(Elem),%val(len(Elem)))
  END FUNCTION gdxUELRegisterMap

  INTEGER(KIND=4) FUNCTION gdxUELRegisterMapStart(pgdx)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxUELRegisterMapStart
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxUELRegisterMapStart
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), EXTERNAL :: c_gdxUELRegisterMapStart

    gdxUELRegisterMapStart=c_gdxUELRegisterMapStart(%val(pgdx))
  END FUNCTION gdxUELRegisterMapStart

  INTEGER(KIND=4) FUNCTION gdxUELRegisterRaw(pgdx,Elem)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxUELRegisterRaw
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxUELRegisterRaw
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    CHARACTER(LEN=*), INTENT(IN) :: Elem
    INTEGER(KIND=4), EXTERNAL :: c_gdxUELRegisterRaw

    gdxUELRegisterRaw=c_gdxUELRegisterRaw(%val(pgdx),loc(Elem),%val(len(Elem)))
  END FUNCTION gdxUELRegisterRaw

  INTEGER(KIND=4) FUNCTION gdxUELRegisterRawStart(pgdx)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxUELRegisterRawStart
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxUELRegisterRawStart
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), EXTERNAL :: c_gdxUELRegisterRawStart

    gdxUELRegisterRawStart=c_gdxUELRegisterRawStart(%val(pgdx))
  END FUNCTION gdxUELRegisterRawStart

  INTEGER(KIND=4) FUNCTION gdxUELRegisterStr(pgdx,Elem,UelNr)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxUELRegisterStr
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxUELRegisterStr
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    CHARACTER(LEN=*), INTENT(IN) :: Elem
    INTEGER(KIND=4), INTENT(IN OUT) :: UelNr
    INTEGER(KIND=4), EXTERNAL :: c_gdxUELRegisterStr

    gdxUELRegisterStr=c_gdxUELRegisterStr(%val(pgdx),loc(Elem),%val(len(Elem)), &
         UelNr)
  END FUNCTION gdxUELRegisterStr

  INTEGER(KIND=4) FUNCTION gdxUELRegisterStrStart(pgdx)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxUELRegisterStrStart
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxUELRegisterStrStart
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), EXTERNAL :: c_gdxUELRegisterStrStart

    gdxUELRegisterStrStart=c_gdxUELRegisterStrStart(%val(pgdx))
  END FUNCTION gdxUELRegisterStrStart

  INTEGER(KIND=4) FUNCTION gdxUMFindUEL(pgdx,s,EN,UMap)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxUMFindUEL
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxUMFindUEL
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    CHARACTER(LEN=*), INTENT(IN) :: s
    INTEGER(KIND=4), INTENT(IN OUT) :: EN
    INTEGER(KIND=4), INTENT(IN OUT) :: UMap
    INTEGER(KIND=4), EXTERNAL :: c_gdxUMFindUEL

    gdxUMFindUEL=c_gdxUMFindUEL(%val(pgdx),loc(s),%val(len(s)),EN, &
         UMap)
  END FUNCTION gdxUMFindUEL

  INTEGER(KIND=4) FUNCTION gdxUMUelGet(pgdx,N,s,UMap)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxUMUelGet
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxUMUelGet
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN) :: N
    CHARACTER(LEN=*), INTENT(OUT) :: s
    INTEGER(KIND=4), INTENT(IN OUT) :: UMap
    INTEGER(KIND=4), EXTERNAL :: c_gdxUMUelGet

    gdxUMUelGet=c_gdxUMUelGet(%val(pgdx),%val(N),loc(s),%val(len(s)), &
         UMap)
  END FUNCTION gdxUMUelGet

  INTEGER(KIND=4) FUNCTION gdxUMUelInfo(pgdx,NrElem,HighMap)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxUMUelInfo
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxUMUelInfo
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), INTENT(IN OUT) :: NrElem
    INTEGER(KIND=4), INTENT(IN OUT) :: HighMap
    INTEGER(KIND=4), EXTERNAL :: c_gdxUMUelInfo

    gdxUMUelInfo=c_gdxUMUelInfo(%val(pgdx),NrElem,HighMap)
  END FUNCTION gdxUMUelInfo

  INTEGER(KIND=4) FUNCTION gdxCurrentDim(pgdx)
    !DEC$ ATTRIBUTES STDCALL :: c_gdxCurrentDim
    !DEC$ ATTRIBUTES REFERENCE :: c_gdxCurrentDim
    INTEGER(KIND=8), INTENT(IN) :: pgdx
    INTEGER(KIND=4), EXTERNAL :: c_gdxCurrentDim

    gdxCurrentDim=c_gdxCurrentDim(%val(pgdx))
  END FUNCTION gdxCurrentDim

  ! properties


!DSA! END MODULE gdxf9def
