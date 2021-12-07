      PROGRAM SAMP1
      
      IMPLICIT NONE      
                        
      include '../../../include/lindo.fi'
      
      INTEGER NERR, NM, NN, NDIR, NNZ, I

      INTEGER(INT_PTR_KIND()) NPTRENV, NPTRMOD
      INTEGER(INT_PTR_KIND()) NVNAMES(2)      

      INTEGER NBEGCOL(3), NCOLCNT(2), NROWX( 4), NULL
      INTEGER NSOLSTAT
      REAL*8 DOBJCONST, DC(2), DB( 3), DA( 4), DOBJVAL
      REAL*8 X( 100), Y(100), S(100), DJ(100),UB(2),LB(2)
      CHARACTER*1 KCH
      CHARACTER*4 KCONSTYPES
      CHARACTER*1024 LICENSEKEY, LICENSEFILE, VERSIONSTR, BUILDSTR
      
      INTEGER ILEN, J
      CHARACTER*16 KVNAMES( 2)
      CHARACTER*16 KWORK      
      CHARACTER*1 KWORKB(16)
      EQUIVALENCE(KWORK,KWORKB)
      DATA KVNAMES /'MYVAR1', 'MYVAR2'/

      DATA NULL /0/

      licensefile = '../../../license/lndapi100.lic' // char(0)

      NERR = LSloadLicenseString(LOC(LICENSEFILE), LOC(LICENSEKEY))
      IF ( NERR .NE. 0) GO TO 9001

c/* >>> Step 1 <<< Create a LINDO environment.
      NPTRENV = LScreateEnv( LOC( NERR), LOC(LICENSEKEY))
      IF ( NERR .NE. 0) GO TO 9001
      
       CALL LSgetVersionInfo(LOC(VERSIONSTR),LOC(BUILDSTR))
       WRITE(6,*) VERSIONSTR
       WRITE(6,*) BUILDSTR

c/* >>> Step 2 <<< Create a model in the environment. */
      NPTRMOD = LScreateModel( NPTRENV, LOC( NERR))
      IF ( NERR .NE. 0) GO TO 9001

c/* >>> Step 3 <<< Specify the model.

c To specify our model, we make a call to LSloadLPData, 
c  passing it:
c 
c - A pointer to the model which we are specifying(NPTRMOD)
c - The number of constraints in the model
c - The number of variables in the model
c - The direction of the optimization (i.e. minimize or 
c -  maximize)
c - The value of the constant term in the objective (may 
c    be zero)
c - The coefficients of the objective function
c - The right-hand sides of the constraints
c - The types of the constraints 
c - The number of nonzeros in the constraint matrix
c - The indices of the first nonzero in each column
c - The length of each column
c - The nonzero coefficients
c - The row indices of the nonzero coefficients
c - Simple upper and lower bounds on the variables
      
c/* Number of constraints */
      NM = 3

c/* Number of variables */
      NN = 2

c/* The direction of optimization (MAX:-1, MIN:1)*/
      NDIR = -1

c/* The objective's constant term */
      DOBJCONST = 0.

c/* The coefficients of the objective function */
      DC( 1) = 20.D0
      DC( 2) = 30.D0

c/* The right-hand sides of the constraints *
      DB( 1) = 120.D0
      DB( 2) =  60.D0
      DB( 3) =  50.D0

c/* The constraint types */
      KCONSTYPES = 'LLL' // CHAR( 0)

c/* The number of nonzeros in the constraint matrix */
      NNZ = 4

c/* The 0-based indices of the first nonzero in each column */
      NBEGCOL( 1) = 0
      NBEGCOL( 2) = 2
      NBEGCOL( 3) = 4
      do 10 i = 1, NN
      NCOLCNT( i) = NBEGCOL(i+1) - NBEGCOL(i)
      UB(i) = LS_INFINITY
      LB(i) = 0.0
10    continue

c/* The nonzero coefficients */
      DA( 1) = 1.D0
      DA( 2) = 1.D0
      DA( 3) = 2.D0
      DA( 4) = 1.D0

c/* The 0-based row indices of the nonzero coefficients */
      NROWX( 1) = 0
      NROWX( 2) = 1
      NROWX( 3) = 0
      NROWX( 4) = 2

c/* We have now assembled a full description of the model. 
c    We pass this information to LSloadLPData with the 
c    following call. */
      NERR = LSloadLPData( NPTRMOD, NM, NN, NDIR, 
     + DOBJCONST, LOC( DC), LOC( DB), LOC( KCONSTYPES), NNZ,
     + LOC( NBEGCOL), LOC(NCOLCNT), LOC( DA), LOC( NROWX), LOC(LB), LOC(UB))
      IF ( NERR .NE. 0) GO TO 9001

C Load variable names
      DO 100 I = 1, NN
      
C Store this names address
        NVNAMES( I) = LOC( KVNAMES( I))      
      
C Terminate each name with a NULL
         KWORK = KVNAMES( I)
         DO J = 16-1, 0, -1
           IF ( KWORKB( J) .NE. ' ' .OR. J .EQ. 0) THEN
              KWORKB( J + 1) = CHAR( NULL)
              KVNAMES( I) = KWORK
              GO TO 100
           ENDIF
         ENDDO
         
  100 CONTINUE
  
C Pass names to Lindo
      NERR = LSloadNameData( NPTRMOD, NULL, NULL, NULL, NULL, 
     + NULL, NULL, LOC( NVNAMES), NULL)      
      IF ( NERR .NE. 0) GO TO 9001     
      
C Write an MPS file      
      NERR = LSwriteMPSFile( NPTRMOD, LOC('MODEL.MPS'//CHAR(NULL)), 0)
      IF ( NERR .NE. 0) GO TO 9001
      WRITE(6,*) 'MPS file written to: MODEL.MPS'       

c /* >>> Step 4 <<< Perform the optimization */
      NERR = LSoptimize( NPTRMOD, 1,LOC(NSOLSTAT))
      IF ( NERR .NE. 0) GO TO 9001

c /* >>> Step 5 <<< Retrieve the solution */

c    Get the state of the solution
      nerr = LSgetInfo(nptrmod, LS_IINFO_MODEL_STATUS,LOC( NSOLSTAT))      
      
C      NERR = LSgetSolutionInfo( NPTRMOD, NULL, NULL,
C     +  NULL, NULL, NULL, LOC( NSOLSTAT), NULL, NULL, 
C     +  NULL, NULL, NULL, NULL)
      IF ( NERR .NE. 0) GO TO 9001

      IF ( NSOLSTAT .EQ. 1.or.NSOLSTAT .EQ. 2) THEN
         
c /* Get the value of the objective */
         nerr = LSgetInfo(nptrmod, LS_DINFO_POBJ,LOC( DOBJVAL))
         IF ( NERR .NE. 0) GO TO 9001
         WRITE(6,*) 'Objective Value =', DOBJVAL

c /* Get the variable values */
         NERR = LSgetPrimalSolution( NPTRMOD, LOC( X))
         IF ( NERR .NE. 0) GO TO 9001
         WRITE(6,*) 'Primal values = ',( X( I), I = 1, NN)
         
         NERR = LSgetSlacks( NPTRMOD, LOC( S))
         IF ( NERR .NE. 0) GO TO 9001
         WRITE(6,*) 'Primal slacks = ',( S( I), I = 1, NM)                  
         
         NERR = LSgetDualSolution( NPTRMOD, LOC( Y))
         IF ( NERR .NE. 0) GO TO 9001
         WRITE(6,*) 'Dual values = ',( Y( I), I = 1, NM)         
         
         NERR = LSgetReducedCosts( NPTRMOD, LOC( DJ))
         IF ( NERR .NE. 0) GO TO 9001                
         WRITE(6,*) 'Dual slacks = ',( DJ( I), I = 1, NN)
         
      ELSE

         WRITE(6,*) 'Solution not optimal.'

      ENDIF

c /* >>> Step 6 <<< Delete the LINDO environment */
      NERR = LSdeleteEnv( LOC(NPTRENV) )
      IF ( NERR .NE. 0) GO TO 9001

      GO TO 9999

C COME HERE WHEN API RETURNS AN ERROR
 9001 CONTINUE
      WRITE(6,*) ' LINDO API error code:', NERR
      GO TO 9999

 9999 CONTINUE

      PAUSE ' Press Enter...'

      END
