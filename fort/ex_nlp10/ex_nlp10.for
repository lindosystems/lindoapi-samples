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
c!  $ ifort  ex_nlp7.for /4L132 ..\..\..\lib\win32\lindo13_0.lib /iface:cvf
c!***********************************************************************
c      
      IMPLICIT NONE
      include '../../../include/lindo.fi'
c
c
      INTEGER NPTRENV, NERR, NPTRMOD, NM, NN, NDIR, NNZ, I, k
      INTEGER NBEGCOL( 101 ), NROWX( 50), NULL, ALENCOL(100)
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
c
      EXTERNAL Funcal
c
      EXTERNAL Gradcal
c
      EXTERNAL Logger, LOCAL_SOL_LOG
      

      DATA NULL /0/
c
      LICENSEFILE = '../../../license/lndapi130.LIC' // char(0)
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
      DC( 1) = 561.0D0
      DC( 2) = 310.0D0
      DC( 3) = 78.0D0
      DC(4) = 0.0D0
      DC(5) = 0.0D0
      DC(6) = 0.0D0
c
c/* The right-hand sides of the constraints *
c
      DB( 1) = 850.0D0
c
c/* The constraint types */
c
      KCONSTYPES ( 1 ) = CHAR(LS_CONTYPE_EQ)
c
      KCONSTYPES ( 2 ) = CHAR( 0)
c
c
c/* The number of nonzeros in the constraint matrix */
      NNZ = 0
c
c
c/* The 0-based indices of the first nonzero in each column 'constraint'*/
c
      NBEGCOL( 1) = 0
      NBEGCOL( 2) = 0
      NBEGCOL( 3) = 0
      NBEGCOL( 4) = 0
      NBEGCOL( 5) = 0
      NBEGCOL( 6) = 0
      NBEGCOL( 7) = 0

c/*  The lenght of each column   */

      ALENCOL (1) = 0
      ALENCOL (2) = 0
      ALENCOL (3) = 0
      ALENCOL (4) = 0
      ALENCOL (5) = 0
      ALENCOL (6) = 0

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

      NERR = LSsetCallback(NPTRMOD,LOC(LOCAL_SOL_LOG),NULL)
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
!      NERR = LSsetModelIntParameter(NPTRMOD,LS_IPARAM_NLP_SOLVER,LS_NMETHOD_MSW_GRG)
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
c
      Integer Function Funcal ( pModel, pUserData, nRow, pdx,
     +                          nJDiff, dXJDiff, pdFunVal, pReserved )
c
      IMPLICIT NONE
c


      INTEGER pModel            [VALUE]
      INTEGER pUserData         [REFERENCE]
      INTEGER nRow              [VALUE]
      REAL * 8 pdx              [REFERENCE]
      INTEGER nJDiff            [VALUE]
      REAL * 8 dXJdiff          [VALUE]
      REAL * 8 pdFunVal         [REFERENCE]
      
      INTEGER pReserved         [REFERENCE]
c
      DIMENSION pdx( 0:100 )
c
      real*8 a, b, c, x, y, z
c
      a = pdx(0)
      b = pdx(1)
      c = pdx(2)
      x = pdx(3)
      y = pdx(4)
      z = pdx(5) 
c
      if ( nRow .eq. -1 ) then
        pdFunVal = 0.00156*x*x*a + 7.92*x*a + 0.00194*y*y*b 
     +  + 7.85*y*b + 0.00482*z*z*c + 7.97*z*c !+ 561*a + 310*b + 78*c
      else if ( nRow .eq. 0 ) then
         pdFunVal = x*a + y*b + z*c - 850.0
      end if

c
      Funcal = 0
c
      End Function Funcal
c
c
      Integer Function Gradcal ( pModel, pUserData, nRow, pdx, pdLB, 
     +                           pdUB, nNewPnt, nNPar, pnParList, 
     +                           pdPartial                     )
c
      IMPLICIT NONE
c
      INTEGER pModel           [VALUE]
      INTEGER pUserData        [REFERENCE]
      INTEGER nRow             [VALUE]
      INTEGER nNewPnt          [VALUE]
      INTEGER nNPar            [VALUE]
      INTEGER pnParList        [REFERENCE]
c
      REAL * 8 pdx             [REFERENCE] 
      REAL * 8 pdLB            [REFERENCE] 
      REAL * 8 pdUB            [REFERENCE]              
      REAL * 8 pdPartial       [REFERENCE]
c
      real*8 x, y, z, a, b, c
c
      integer i2
c
      DIMENSION pdx ( 0:100 )
c
      DIMENSION pdPartial (0:100 )
c
      DIMENSION pnParList ( 0:100 )
c
      DIMENSION pdLB ( 0:100 )
c
      DIMENSION pdUB ( 0:100 )
c
      a = pdx(0)
      b = pdx(1)
      c = pdx(2)
      x = pdx(3)
      y = pdx(4)
      z = pdx(5)  
c
c     jac obj row
      if ( nRow .eq. -1 ) then
c
        do i2 = 0, nNPar - 1
          if ( pdLB(pnParList(i2)) .ne. pdUB(pnParList(i2)) ) then
            if ( pnParList(i2) .eq. 0 ) then
              pdPartial(i2) = 0.00156*x*x + 7.92*x + 561
            else if ( pnParList(i2) .eq. 1 ) then
                pdPartial(i2) = 0.00194*y*y + 7.85*y +310
                else if ( pnParList(i2) .eq. 2 ) then 
                        pdPartial(i2) = 0.00482*z*z + 7.93*z + 78
                    else if ( pnParList(i2) .eq. 3 ) then
                            pdPartial(i2) =0.003124*a*x + 7.92*a
                        else if    ( pnParList(i2) .eq. 4 ) then
                                pdPartial(i2) =0.00388*b*y + 7.85*b
                            else if    ( pnParList(i2) .eq. 5 ) then
                                pdPartial(i2) = 0.00964*c*z + 7.93*c
                     end if
        end if
        end do
c
c    jac row 0
      else if ( nRow .eq. 0 ) then
c
        do i2 = 0, nNPar - 1
          if ( pdLB(pnParList(i2)) .ne. pdUB(pnParList(i2)) ) then
            if ( pnParlist(i2) .eq. 0 ) then
              pdPartial(i2) = x
            else if ( pnParList(i2) .eq. 1 ) then
                pdPartial(i2) = y
             else if ( pnParList(i2) .eq. 2 ) then
                    pdPartial(i2) = z
                else if ( pnParList(i2) .eq. 3 ) then
                        pdPartial(i2) = a
                     else if ( pnParList(i2) .eq. 4 ) then
                            pdPartial(i2) = b
                        else if ( pnParList(i2) .eq. 5 ) then
                                pdPartial(i2) = c


            end if
          endif
        end do
      end if
c

      Gradcal = 0
c
      End Function Gradcal

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine Logger ( pModel, szMessage, pUserData )
c
      IMPLICIT NONE
c
      INTEGER pModel            [VALUE]
      INTEGER szMessage         [REFERENCE]
      INTEGER pUserData         [REFERENCE]
c
      WRITE(6,*) ' LOG:', szMessage
      Return
      End
    
      INTEGER FUNCTION LOCAL_SOL_LOG(NPTRMOD,ILOC,CBDATA) RESULT(NERR)

      INTEGER(INT_PTR_KIND()), INTENT(INOUT) :: NPTRMOD
      INTEGER, VALUE         , INTENT(IN   ) :: ILOC
      INTEGER                , INTENT(INOUT) :: CBDATA(*)

      include '../../../include/lindo.fi'

      INTEGER :: niter,biter,siter
      INTEGER :: iter=0
      INTEGER :: npass, nbrn
      REAL(8) :: pfeas=0.0, pobj=0.0
      REAL(8) :: bestobj

      if (iLoc .ne. LSLOC_LOCAL_OPT) then
        nerr = 0 ! return a nonzero to stop the solver, e.g. niter>threshold
        return
      endif
      
      if (cbData(1) == 0) write(6,*) ' Iter Objective  Infeas     Best       Branches'
      NERR = LSgetCallbackInfo(NPTRMOD,ILOC,LS_IINFO_MIP_NLP_ITER   ,loc(niter  ))
      NERR = LSgetCallbackInfo(NPTRMOD,ILOC,LS_IINFO_MIP_SIM_ITER   ,loc(siter  ))
      NERR = LSgetCallbackInfo(NPTRMOD,ILOC,LS_IINFO_MIP_BAR_ITER   ,loc(biter  ))
      NERR = LSgetCallbackInfo(NPTRMOD,ILOC,LS_DINFO_POBJ           ,loc(pobj   ))
      NERR = LSgetCallbackInfo(NPTRMOD,ILOC,LS_DINFO_PINFEAS        ,loc(pfeas  ))
      NERR = LSgetCallbackInfo(NPTRMOD,ILOC,LS_DINFO_MSW_POBJ       ,loc(bestobj))
      NERR = LSgetCallbackInfo(NPTRMOD,ILOC,LS_IINFO_MIP_BRANCHCOUNT,loc(nbrn   ))
      iter = niter + siter + biter
      write(6,'(i5,3g16.8,i10)') iter,pobj,pfeas,bestobj,nbrn
      cbData(1) = cbData(1) + 1
      
      nerr = 0
      return

      end function
    