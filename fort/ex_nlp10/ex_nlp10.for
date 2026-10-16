c
c     Callbacks.  LINDO API is a C library and calls these with C argument
c     passing, as declared by the typedefs in lindo.h (Funcalc_type,
c     Gradcalc_type, printModelLOG_t, cbFunc_t): BIND(C), pointers and
c     scalars by VALUE, arrays by reference.  Variables, 0-based as LINDO
c     API numbers them: a b c x y z = pdX(0) ... pdX(5).
c
      MODULE EX_NLP10_CALLBACKS
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      CONTAINS
c
c     Value of row nRow at pdX, constant terms on the left (g(x) = 0):
c     nRow = -1, the objective (all of it: a, b and c also appear
c     nonlinearly, so the model gives them no fixed coefficient);
c     nRow = 0, x*a + y*b + z*c - 850.
c
      INTEGER(C_INT) FUNCTION FUNCAL(pModel, pUserData, nRow, pdX,
     +    nJDiff, dXJBase, pdFuncVal, pReserved) BIND(C)
      INTEGER(C_INTPTR_T), VALUE :: pModel, pUserData, pReserved
      INTEGER(C_INT), VALUE :: nRow, nJDiff
      REAL(C_DOUBLE), VALUE :: dXJBase
      REAL(C_DOUBLE), INTENT(IN) :: pdX(0:5)
      REAL(C_DOUBLE), INTENT(OUT) :: pdFuncVal
      REAL(C_DOUBLE) :: a, b, c, x, y, z
c
      a = pdX(0)
      b = pdX(1)
      c = pdX(2)
      x = pdX(3)
      y = pdX(4)
      z = pdX(5)
      IF (nRow .EQ. -1) THEN
        pdFuncVal = a*(561.0D0 + 7.92D0*x + 0.00156D0*x*x)
     +            + b*(310.0D0 + 7.85D0*y + 0.00194D0*y*y)
     +            + c*( 78.0D0 + 7.97D0*z + 0.00482D0*z*z)
      ELSE IF (nRow .EQ. 0) THEN
        pdFuncVal = x*a + y*b + z*c - 850.0D0
      ELSE
        pdFuncVal = 0.0D0
      END IF
      FUNCAL = 0
      END FUNCTION FUNCAL
c
c     Partial derivatives of row nRow, as FUNCAL computes it, with
c     respect to the nNPar variables listed in pnParList
c
      INTEGER(C_INT) FUNCTION GRADCAL(pModel, pUserData, nRow, pdX,
     +    pdLB, pdUB, nNewPnt, nNPar, pnParList, pdPartial) BIND(C)
      INTEGER(C_INTPTR_T), VALUE :: pModel, pUserData
      INTEGER(C_INT), VALUE :: nRow, nNewPnt, nNPar
      REAL(C_DOUBLE), INTENT(IN) :: pdX(0:5), pdLB(0:5), pdUB(0:5)
      INTEGER(C_INT), INTENT(IN) :: pnParList(0:*)
      REAL(C_DOUBLE), INTENT(INOUT) :: pdPartial(0:*)
      REAL(C_DOUBLE) :: a, b, c, x, y, z, d
      INTEGER :: i
c
      a = pdX(0)
      b = pdX(1)
      c = pdX(2)
      x = pdX(3)
      y = pdX(4)
      z = pdX(5)
      DO i = 0, nNPar - 1
        d = 0.0D0
        IF (nRow .EQ. -1) THEN
          SELECT CASE (pnParList(i))
          CASE (0)
            d = 561.0D0 + 7.92D0*x + 0.00156D0*x*x
          CASE (1)
            d = 310.0D0 + 7.85D0*y + 0.00194D0*y*y
          CASE (2)
            d =  78.0D0 + 7.97D0*z + 0.00482D0*z*z
          CASE (3)
            d = a*(7.92D0 + 0.00312D0*x)
          CASE (4)
            d = b*(7.85D0 + 0.00388D0*y)
          CASE (5)
            d = c*(7.97D0 + 0.00964D0*z)
          END SELECT
        ELSE IF (nRow .EQ. 0) THEN
          SELECT CASE (pnParList(i))
          CASE (0)
            d = x
          CASE (1)
            d = y
          CASE (2)
            d = z
          CASE (3)
            d = a
          CASE (4)
            d = b
          CASE (5)
            d = c
          END SELECT
        END IF
        pdPartial(i) = d
      END DO
      GRADCAL = 0
      END FUNCTION GRADCAL
c
c     Log messages of the model: pachStr is a C string that may hold
c     several lines; each non-blank one is printed after ' LOG: '
c
      SUBROUTINE LOGGER(pModel, pachStr, pvPrnData) BIND(C)
      INTEGER(C_INTPTR_T), VALUE :: pModel, pvPrnData
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: pachStr(*)
      INTEGER :: i, j, k
c
      i = 1
      j = 1
      DO
        IF (pachStr(j) .EQ. C_NULL_CHAR .OR.
     +      pachStr(j) .EQ. C_NEW_LINE) THEN
          k = j - 1
          IF (k .GE. i) THEN
            IF (pachStr(k) .EQ. C_CARRIAGE_RETURN) k = k - 1
          END IF
          IF (k .GE. i) THEN
            IF (ANY(pachStr(i:k) .NE. ' '))
     +        WRITE(6,'(A,*(A))') ' LOG: ', pachStr(i:k)
          END IF
          IF (pachStr(j) .EQ. C_NULL_CHAR) EXIT
          i = j + 1
        END IF
        j = j + 1
      END DO
      END SUBROUTINE LOGGER
c
c     Solver progress (cbFunc_t), as the manual's print_log example:
c     the nonlinear solver's iterations (LSLOC_CONOPT) and, under the
c     multistart solver, each local optimum (LSLOC_LOCAL_OPT) with its
c     pass and the best objective so far; cbData(1) counts the lines
c
      INTEGER(C_INT) FUNCTION LOCAL_SOL_LOG(NPTRMOD, ILOC, CBDATA)
     +    BIND(C)
      INTEGER(C_INTPTR_T), VALUE :: NPTRMOD
      INTEGER(C_INT), VALUE :: ILOC
      INTEGER(C_INT), INTENT(INOUT) :: CBDATA(*)
c
      include '../../../include/lindo.fi'
c
      INTEGER :: NERR, niter, biter, siter, npass
      REAL(C_DOUBLE) :: pfeas, dfeas, pobj, bestobj
c
      LOCAL_SOL_LOG = 0
      IF (ILOC .NE. LSLOC_LOCAL_OPT .AND. ILOC .NE. LSLOC_CONOPT)
     +    RETURN
      niter = 0
      biter = 0
      siter = 0
      npass = 0
      pfeas = 0.0D0
      dfeas = 0.0D0
      pobj = 0.0D0
      bestobj = 0.0D0
      IF (CBDATA(1) .EQ. 0) WRITE(6,'(2A6,4A17)')
     +    'PASS', 'ITER', 'POBJ', 'PINFEAS', 'DINFEAS', 'BESTOBJ'
      NERR = LSgetCallbackInfo(NPTRMOD, ILOC, LS_IINFO_NLP_ITER,
     +    LOC(niter))
      NERR = LSgetCallbackInfo(NPTRMOD, ILOC, LS_DINFO_POBJ, LOC(pobj))
      NERR = LSgetCallbackInfo(NPTRMOD, ILOC, LS_DINFO_PINFEAS,
     +    LOC(pfeas))
      NERR = LSgetCallbackInfo(NPTRMOD, ILOC, LS_DINFO_DINFEAS,
     +    LOC(dfeas))
      IF (ILOC .EQ. LSLOC_LOCAL_OPT) THEN
c       multistart: a local optimum, with the pass and the best so far
        NERR = LSgetCallbackInfo(NPTRMOD, ILOC, LS_IINFO_SIM_ITER,
     +      LOC(siter))
        NERR = LSgetCallbackInfo(NPTRMOD, ILOC, LS_IINFO_BAR_ITER,
     +      LOC(biter))
        NERR = LSgetCallbackInfo(NPTRMOD, ILOC, LS_IINFO_MSW_PASS,
     +      LOC(npass))
        NERR = LSgetCallbackInfo(NPTRMOD, ILOC, LS_DINFO_MSW_POBJ,
     +      LOC(bestobj))
        WRITE(6,'(2I6,4ES17.5)') npass, niter + siter + biter, pobj,
     +      pfeas, dfeas, bestobj
      ELSE
c       the nonlinear solver's progress
        WRITE(6,'(6X,I6,3ES17.5)') niter, pobj, pfeas, dfeas
      END IF
      CBDATA(1) = CBDATA(1) + 1
      END FUNCTION LOCAL_SOL_LOG
c
      END MODULE EX_NLP10_CALLBACKS
c
      PROGRAM SAMP1
c
c!***********************************************************************
c!  MINIMIZE
C!   0.00156*a*x^2 + 7.92*x*a + 561*a + 0.00194b*y^2 
C!   + 7.85*y*b + 310*b + 0.00482*c*z^2 + 7.97*z*c + 78*c
c! 
c!  SUBJECT TO:
C!    x*a + y*b + z*c = 850.0
C!
C!  BOUNDS
C!    0  <= a <= 10
C!       0  <= b <= 10
C!    0  <= c <= 10
C!    150<= x <= 600   
C!       100<= y <= 400
C!    50 <= z <= 200
C!
C!    a, b, c are integer
c!    x,y,z are continuous
C!    
c!  $ ifx ex_nlp10.for /extend-source:132 ..\..\..\lib\win64\lindo64_17_0.lib
c!***********************************************************************
c      
      USE EX_NLP10_CALLBACKS
      IMPLICIT NONE
      include '../../../include/lindo.fi'
c
c
      INTEGER NERR, NM, NN, NDIR, NNZ, I, k
      INTEGER(INT_PTR_KIND()) NPTRENV, NPTRMOD
      INTEGER NBEGCOL( 101 ), NROWX( 50), ALENCOL(100)
      INTEGER(INT_PTR_KIND()) NULL
      INTEGER NCBDATA(1)
      INTEGER NSOLSTAT, numCont, numVars
c
      REAL*8 DOBJCONST, DC(100),DB(50),DA(200),DOBJVAL,X(100)
c
      CHARACTER*1 KCH, VARTYPE(100)
      CHARACTER*1 KCONSTYPES ( 51 )
      CHARACTER*255 LICENSEKEY, LICENSEFILE
c
      INTEGER  aiCols (101), acCols (100), aiRows (200), nObj,aiObj(100)
c
      REAL * 8 adCoef (200), adObjCoef(100)
c
      REAL * 8 primal (100)
C
      REAL * 8 PADL (100), PADU(100)
c
      INTEGER POINTER
      

      DATA NULL /0/
      DATA NCBDATA /0/
c
      LICENSEFILE = '../../../license/lndapi170.lic' // char(0)
c
      NERR = LSLOADLICENSESTRING(LOC(LICENSEFILE), LOC(LICENSEKEY))
      IF ( NERR .NE. 0) GO TO 9001
c
c     ******************************************
c     Step 1 <<< Create a LINDO environment. 
c     ******************************************
c
      NPTRENV = LSCREATEENV( LOC( NERR), LOC(LICENSEKEY))
      IF ( NERR .NE. 0) GO TO 9001
c
c     **********************************************
c     Step 2 <<< Create a model in the environment. 
c     **********************************************
c
      NPTRMOD = LSCREATEMODEL( NPTRENV, LOC( NERR))
      IF ( NERR .NE. 0) GO TO 9001
c
c     ********************************************
c      Step 3 Specify the LP portion of the model
c     ********************************************
c
c     To specify our model, we make a call to LSloadLPData, 
c     passing it:
c 
c     - A pointer to the model which we are specifying(NPTRMOD)
c     - The number of constraints in the model
c     - The number of variables in the model
c     - The direction of the optimization (i.e. minimize or 
c     -  maximize)
c     - The value of the constant term in the objective (may 
c       be zero)
c     - The coefficients of the objective function
c     - The right-hand sides of the constraints
c     - The types of the constraints 
c     - The number of nonzeros in the constraint matrix
c     - The indices of the first nonzero in each column
c     - The length of each column
c     - The nonzero coefficients
c     - The row indices of the nonzero coefficients
c     - Simple upper and lower bounds on the variables
     


c/* Number of constraints */
c
      NM = 1
c
c/* Number of variables */
c
      NN = 6
c
c/* The direction of optimization (MAX:-1, MIN:1)*/
c
      NDIR = LS_MIN 
c
c/* The objective's constant term */
c
      DOBJCONST = 0.D0
c
c/* The coefficients of the objective function */
c
c    a, b, c also appear nonlinearly in the objective, so they have no
c    fixed coefficient here: FUNCAL returns the whole objective
      DC( 1) = 0.0D0
      DC( 2) = 0.0D0
      DC( 3) = 0.0D0
      DC(4) = 0.0D0
      DC(5) = 0.0D0
      DC(6) = 0.0D0
c
c/* The right-hand sides of the constraints *
c
c    the constant is on the left: FUNCAL returns x*a + y*b + z*c - 850
      DB( 1) = 0.0D0
c
c/* The constraint types */
c
      KCONSTYPES ( 1 ) = CHAR(LS_CONTYPE_EQ)
c
      KCONSTYPES ( 2 ) = CHAR( 0)
c
c
c/* The number of nonzeros in the constraint matrix */
      NNZ = 6
c
c
c/* The 0-based indices of the first nonzero in each column 'constraint'*/
c
      NBEGCOL( 1) = 0
      NBEGCOL( 2) = 1
      NBEGCOL( 3) = 2
      NBEGCOL( 4) = 3
      NBEGCOL( 5) = 4
      NBEGCOL( 6) = 5
      NBEGCOL( 7) = 6

c/*  The lenght of each column   */

      ALENCOL (1) = 1
      ALENCOL (2) = 1
      ALENCOL (3) = 1
      ALENCOL (4) = 1
      ALENCOL (5) = 1
      ALENCOL (6) = 1

c/* The NLP have 0 at each of their occurrence  */

      DA( 1) = 0.D0
      DA( 2) = 0.D0
      DA( 3) = 0.D0
      DA( 4) = 0.D0
      DA( 5) = 0.D0
      DA( 6) = 0.D0
c
c/ * The row indices of the nonzero coefficients  */

      NROWX( 1) = 0
      NROWX( 2) = 0
      NROWX( 3) = 0
      NROWX( 4) = 0
      NROWX( 5) = 0
      NROWX( 6) = 0


c     Variable limits
      PADL(1) = 0.0D0
      PADL(2) = 0.0D0
      PADL(3) = 0.0D0
      PADL(4) = 150.0D0
      PADL(5) = 100.0D0
      PADL(6) = 50.0D0

      PADU(1) =  10.0D0
      PADU(2) =  10.0D0
      PADU(3) =  10.0D0
      PADU(4) =  600.0D0
      PADU(5) =  400.0D0
      PADU(6) =  200.0D0
c
      nErr= LSsetModelLogfunc( NPTRMOD, Loc(Logger), NULL )
      IF ( nErr .NE. 0) GO TO 9001

      NERR = LSsetCallback(NPTRMOD,LOC(LOCAL_SOL_LOG),LOC(NCBDATA))
      IF ( NERR .NE. 0) GO TO 9001

c/* We have now assembled a full description of the model. 
c    We pass this information to LSloadLPData with the 
c    following call. */
      if (2.gt.1) then
          NERR = LSLOADLPDATA( NPTRMOD, NM, NN, NDIR, 
     +      DOBJCONST, LOC(DC), LOC( DB), LOC( KCONSTYPES), NNZ,
     +      LOC( NBEGCOL), LOC(ALENCOL), LOC( DA), LOC( NROWX), LOC (PADL),
     +      LOC( PADU) )
      else  
        NERR = LSreadMPSFile( NPTRMOD, 
     +      LOC('../../../samples/data/bm23.mps'//CHAR(NULL)), 0)
      endif
      IF ( NERR .NE. 0) GO TO 9001

C     Set variable types (or not)
      if (0>1) then
        vartype(1) = 'I'
        vartype(2) = 'I'
        vartype(3) = 'I'
        vartype(4) = 'C'
        vartype(5) = 'C'
        vartype(6) = 'C'

        NERR = LSLOADVARTYPE( NPTRMOD, LOC(vartype))
        IF ( NERR .NE. 0) GO TO 9001
      endif
c 
c     *********************************************
c      Step 3 Specify the NLP portion of the model
c     *********************************************
c
c    the index of the first nonlinear nonzero in each column.'constraint'
c    
      aiCols ( 1 ) = 0
      aiCols ( 2 ) = 1
      aiCols ( 3 ) = 2 
      aiCols ( 4 ) = 3
      aiCols ( 5 ) = 4
      aiCols ( 6 ) = 5 
      aiCols ( 7 ) = 6

c an integer vector containing the number of nonlinear elements in each column.
c
      acCols ( 1 ) = 1
      acCols ( 2 ) = 1
      acCols ( 3 ) = 1
      acCols ( 4 ) = 1
      acCols ( 5 ) = 1
      acCols ( 6 ) = 1


c
c A pointer to an integer vector containing the row indices of the nonlinear elements.
      aiRows ( 1 ) = 0
      aiRows ( 2 ) = 0
      aiRows ( 3 ) = 0
      aiRows ( 4 ) = 0
      aiRows ( 5 ) = 0
      aiRows ( 6 ) = 0

c
c the number of nonlinear variables in the objective.
c
      nObj = 6
c
c integer vector containing the column indices of nonlinear variables in the objective function.
c pointer to an integer vector containing the column indices of nonlinear variables in the objective function.
      aiObj ( 1 ) = 0
      aiObj ( 2 ) = 1
      aiObj ( 3 ) = 2
      aiObj ( 4 ) = 3
      aiObj ( 5 ) = 4
      aiObj ( 6 ) = 5
c

      NERR = LSLOADNLPDATA( NPTRMOD, LOC(aiCols), LOC(acCols), 
     +  NULL, loc(aiRows), nObj, LOC(aiObj), NULL )
      IF ( NERR .NE. 0) GO TO 9001


c     **********************************
c      Step 4 Set up callback functions 
c     **********************************
      NERR= LSsetFuncalc( NPTRMOD, LOC(Funcal), null )
c
      IF ( NERR .NE. 0) GO TO 9001
c
      NERR= LSsetGradcalc( NPTRMOD, LOC(Gradcal), null , 0, null )
c
      IF ( NERR .NE. 0) GO TO 9001
c
c     *************************************
c      Step 5 <<< Perform the optimization 
c     *************************************
c   /* Turn multi-start search on */
      NERR = LSsetModelIntParameter(NPTRMOD,LS_IPARAM_NLP_SOLVER,LS_NMETHOD_MSW_GRG)
      IF ( NERR .NE. 0) GO TO 9001     

c   /* Set maximum number of local optimizations */
      NERR = LSsetModelIntParameter(NPTRMOD,LS_IPARAM_NLP_MAXLOCALSEARCH,5)
      IF ( NERR .NE. 0) GO TO 9001  
c
      NERR = LSgetInfo(NPTRMOD, LS_IINFO_NUM_CONT, LOC(numCont))
      NERR = LSgetInfo(NPTRMOD, LS_IINFO_NUM_VARS, LOC(numVars))
      if (numVars>numCont) then
        NERR = LSsolveMIP ( NPTRMOD,LOC( NSOLSTAT))
      else
        NERR = LSoptimize ( NPTRMOD,0,LOC( NSOLSTAT))
      endif
      WRITE (*,*) 'SOLUTION STATUS=',NSOLSTAT
      IF ( NERR .NE. 0) GO TO 9001
    
!     ***************************************
!      Step 6 <<< Retrieve the solution 
!     ***************************************
      IF ( NSOLSTAT .EQ. 1.or.NSOLSTAT .EQ. 8.or.NSOLSTAT .EQ. 2) THEN                 
!c /* Get the value of the objective */
        if (numVars>numCont) then      
          NERR = LSGETINFO( NPTRMOD, LS_DINFO_MIPOBJ , LOC(dobjval))
        else
          NERR = LSGETINFO( NPTRMOD, LS_DINFO_POBJ , LOC(dobjval))
        endif      
        IF ( NERR .NE. 0) GO TO 9001         
        WRITE(6,*) 'Objective Value =', DOBJVAL

!c /* Get the variable values */
        if (numVars>numCont) then      
          NERR = LSGETMIPPRIMALSOLUTION( NPTRMOD,  LOC(X))
        else
          NERR = LSGETPRIMALSOLUTION( NPTRMOD,  LOC(X))
        endif
        IF ( NERR .NE. 0) GO TO 9001
        WRITE(6,*) 'Primal values = ',( X( I), I = 1, NN)
      ELSE
         WRITE(6,*) 'Solution not optimal. Status = ', NSOLSTAT
      ENDIF

c
c    *****************************************
c     Step 7 <<< Delete the LINDO environment 
c    *****************************************
      NERR = LSDELETEENV( LOC(NPTRENV) )
      IF ( NERR .NE. 0) GO TO 9001
c
      GO TO 9999
c
c COME HERE WHEN API RETURNS AN ERROR
 9001 CONTINUE
      WRITE(6,*) ' LINDO API error code:', NERR
      GO TO 9999
c
 9999 CONTINUE
c
      PAUSE ' Press Enter...'
c
      END
