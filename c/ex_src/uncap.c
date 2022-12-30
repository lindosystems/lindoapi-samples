/*
  MODEL:
  ! 
  The uncapacitated facility location problem.
  The total demand that each facility may satisfy is unlimited.

  Customer 	i	  1	    2	    3	    4	    5
  Demand        80    270   250   160   180
  Facility j	cij                           fj	
  1	            4	    5	    6	    8	    10	1500
  2	            6	    4	    3	    5	    8	  1500
  3	            9	    7	    4	    3	    4	  1500
  ;

  SETS:
  FACILITY: F, X;
  CUSTOMER: D;
  ROUTES( FACILITY, CUSTOMER) : C, Z;
  ENDSETS

  DATA:
  FACILITY,F =  F1,1500 F2,1500 F3,1500;   
  CUSTOMER,D =  C1,80,  C2,270  C3,250  C4,160,  C5,180;  

  C =    4		5	6	8	10
  6		4	3	5	8
  9		7	4	3	4;
  M = 100000;
  ENDDATA

  ! The objective;
  [R_OBJ] MIN = @SUM( ROUTES: C * D * Z ) + @SUM(FACILITY: F* X );
  ! The demand constraints;
  @FOR( CUSTOMER( J): @SUM( FACILITY( I): Z( I, J)) =  1);
  ! The supply constraints;
  ! @FOR( FACILITY( I): @SUM( CUSTOMER( J): Z( I, J)) <= X(I) * M);
  @FOR( FACILITY( I): @SUM( CUSTOMER( J): Z( I, J)) <= 0);

  @FOR( FACILITY( I): @BIN(X(I)));
  @FOR( FACILITY( I): @FOR( CUSTOMER( J): @BIN(Z( I, J))));
  END
*/ 

/*
MODEL: ! SCALAR MODEL;
[R_OBJ] MIN= 320 * Z_F1_C1 + 1350 * Z_F1_C2 + 1500 * Z_F1_C3 + 1280 * Z_F1_C4 +
1800 * Z_F1_C5 + 480 * Z_F2_C1 + 1080 * Z_F2_C2 + 750 * Z_F2_C3 + 800 * Z_F2_C4 +
1440 * Z_F2_C5 + 720 * Z_F3_C1 + 1890 * Z_F3_C2 + 1000 * Z_F3_C3 + 480 * Z_F3_C4 +
720 * Z_F3_C5 + 1500 * X_F1 + 1500 * X_F2 + 1500 * X_F3;                          
[_0] Z_F1_C1 + Z_F2_C1 + Z_F3_C1 = 1;
[_1] Z_F1_C2 + Z_F2_C2 + Z_F3_C2 = 1;
[_2] Z_F1_C3 + Z_F2_C3 + Z_F3_C3 = 1;
[_3] Z_F1_C4 + Z_F2_C4 + Z_F3_C4 = 1;
[_4] Z_F1_C5 + Z_F2_C5 + Z_F3_C5 = 1;
[_5] Z_F1_C1 + Z_F1_C2 + Z_F1_C3 + Z_F1_C4 + Z_F1_C5 <= 0;
[_6] Z_F2_C1 + Z_F2_C2 + Z_F2_C3 + Z_F2_C4 + Z_F2_C5 <= 0;
[_7] Z_F3_C1 + Z_F3_C2 + Z_F3_C3 + Z_F3_C4 + Z_F3_C5 <= 0;
@BIN( Z_F1_C1); @BIN( Z_F1_C2); @BIN( Z_F1_C3); @BIN( Z_F1_C4);
@BIN( Z_F1_C5); @BIN( Z_F2_C1); @BIN( Z_F2_C2); @BIN( Z_F2_C3);
@BIN( Z_F2_C4); @BIN( Z_F2_C5); @BIN( Z_F3_C1); @BIN( Z_F3_C2);
@BIN( Z_F3_C3); @BIN( Z_F3_C4); @BIN( Z_F3_C5); @BIN( X_F1); @BIN( X_F2);
@BIN( X_F3);
END
*/

{ // begin LP data

//////// Model sizes, objective sense and constant
  int     errorcode = 0;
  int     nCons = 8;
  int     nVars = 18;
  int     dObjSense = LS_MIN;
  double  dObjConst = 0;
//////// Objective coefficients //@SUM( ROUTES: C * D * Z ) + @SUM(FACILITY: F* X );
  double  padC[]= 
  {
    320,  1350,  1500,  1280,  1800,  480, 
    1080,  750,  800,  1440,  720,  1890, 
    1000,  480,  720,  1500,  1500,  1500, 
     -1.0
  };

//////// RHS values
  double  padB[]= 
  {
    1,  1,  1,  1,  1,  0, 
    0,  0,   -1.0
  };

//////// Constraint types
  char  pszConTypes[]= {'E',  'E',  'E',  'E',  'E',  'L',  'L',  'L',   0};

////////  Total nonzeros in A matrix
  int     nAnnz = 30;
////////  Column offset
  int  paiAcols[]= 
  {
               0,             2,             4,             6,             8,            10, 
              12,            14,            16,            18,            20,            22, 
              24,            26,            28,            30,            30,            30, 
              30,   -1
  };

////////  Column counts
  int  panAcols[]= 
  {
               2,             2,             2,             2,             2,             2, 
               2,             2,             2,             2,             2,             2, 
               2,             2,             2,             0,             0,             0, 
     -1
  };

////////  A coeff
  double  padAcoef[]= 
  {
    1,  1,  1,  1,  1,  1, 
    1,  1,  1,  1,  1,  1, 
    1,  1,  1,  1,  1,  1, 
    1,  1,  1,  1,  1,  1, 
    1,  1,  1,  1,  1,  1, 
     -1.0
  };

///////// Row indices
  int  paiArows[]= 
  {
               0,             5,             1,             5,             2,             5, 
               3,             5,             4,             5,             0,             6, 
               1,             6,             2,             6,             3,             6, 
               4,             6,             0,             7,             1,             7, 
               2,             7,             3,             7,             4,             7, 
     -1
  };

//////// Lower bounds
  double  padL[]= 
  {
    0,  0,  0,  0,  0,  0, 
    0,  0,  0,  0,  0,  0, 
    0,  0,  0,  0,  0,  0, 
     -1.0
  };

//////// Upper bounds
  double  padU[]= 
  {
    1,  1,  1,  1,  1,  1, 
    1,  1,  1,  1,  1,  1, 
    1,  1,  1,  1,  1,  1, 
     -1.0
  };

//////// Load LP data
   errorcode = LSloadLPData(pModel,
               nCons,nVars,dObjSense,dObjConst,padC,padB,
               pszConTypes,nAnnz,paiAcols,panAcols,padAcoef,paiArows,padL,padU);
   if (errorcode) return errorcode;
} // end LP/QP/CONE data




{ // begin INTEGER data
/*
@FOR( FACILITY( I): @BIN(X(I)));
@FOR( FACILITY( I): @FOR( CUSTOMER( J): @BIN(Z( I, J))));
*/
////////  Variable type

  int     errorcode = 0;
  char  pszVarTypes[]= 
  {
    'B',  'B',  'B',  'B',  'B',  'B', 
    'B',  'B',  'B',  'B',  'B',  'B', 
    'B',  'B',  'B',  'B',  'B',  'B', 
     0
  };

   errorcode = LSloadVarType(pModel,pszVarTypes);
   if (errorcode) return errorcode;
} // end INTEGER data

   /***************************************************************
    * Step 4.b: Load indicators
    ***************************************************************/
   {
     int nErrorCode = 0;
     int nFacilities, *paiIndicRows, *paiIndicCols, *paiIndicVals;
     int *ibuf=NULL, nbuf, i;
     nFacilities = 3;      // 3 facilities
     nbuf = nFacilities*3; // 3 arrays of size 'nIndicRows'
     ibuf = malloc(sizeof(int)*nbuf);
     memset(ibuf,0,sizeof(int)*nbuf);
     paiIndicRows = ibuf;
     paiIndicCols = paiIndicRows + nFacilities;
     paiIndicVals = paiIndicCols + nFacilities;
     for (i=0; i<nFacilities; i++) {
       // constraint_i: @sum(z[i,j]) <= 0;
       paiIndicRows[i] = 5+i;                       // indices of indicator constraints
       // min @sum(z[i,j]*c[i,j])+ @sum(f[i]*x[i])
       paiIndicCols[i] = 15 + i;                    // indices of x[i] variables, starting right after z[] variables
       paiIndicVals[i] = 0;                         // x[i]=0 => enable i'th indicator constraint
     }
     nErrorCode = LSloadIndData(pModel,nFacilities,paiIndicRows,paiIndicCols,paiIndicVals);
   }


{ // begin QCP data

} // end QCP data




{ // begin CONE data

} // end CONE data




{ // begin SETS data

} // end SETS data




{ // begin SC data

} // end SC data


