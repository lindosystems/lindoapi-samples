/* port.c
###################################################################
#                       LINDO-API
#                    Sample Programs
#                  Copyright (c) 2011
#
#         LINDO Systems, Inc.           312.988.7422
#         1415 North Dayton St.         info@lindo.com
#         Chicago, IL 60622             http://www.lindo.com
###################################################################
  File   : port.c
  Purpose: Solve a quadratic mixed integer programming problem.
  Model  : Portfolio Selection Problem with a Restriction on
           the Number of Assets

           MINIMIZE   0.5 w'Q w
           s.t.   sum_i  w(i)              =  1
                  sum_i  r(i)w(i)         >=  R
                  for_i  w(i) - u(i) x(i) <=  0   i=1...n
                  sum_i  x(i)             <=  K
                  for_i  x(i) are binary          i=1...n
           where
           r(i)  : return on asset i.
           u(i)  : an upper bound on the proportion of total budget
                   that could be invested on asset i.
           Q(i,j): covariance between the returns of i^th and j^th
                   assets.
           K     : max number of assets allowed in the portfolio
           w(i)  : proportion of total budget invested on asset i
           x(i)  : a 0-1 indicator if asset i is invested on.

  Data:
  Covariance Matrix:
               A1      A2      A3      A4      A5      A6      A7
       A1 [  1.00    0.11    0.04    0.02    0.08    0.03    0.10 ]
       A2 [  0.11    1.00    0.21    0.13    0.43    0.14    0.54 ]
       A3 [  0.04    0.21    1.00    0.05    0.16    0.05    0.20 ]
   Q = A4 [  0.02    0.13    0.05    1.00    0.10    0.03    0.12 ]
       A5 [  0.08    0.43    0.16    0.10    1.00    0.10    0.40 ]
       A6 [  0.03    0.14    0.05    0.03    0.10    1.00    0.12 ]
       A7 [  0.10    0.54    0.20    0.12    0.40    0.12    1.00 ]

  Returns Vector:
               A1      A2      A3      A4      A5      A6      A7
    r =   [  0.14    0.77    0.28    0.17    0.56    0.18    0.70 ]

  Maximum Proportion of Total Budget to be Invested on Assets
               A1      A2      A3      A4      A5      A6      A7
    u =   [  0.04    0.56    0.37    0.32    0.52    0.38    0.25 ]

  Target Return:
  R = 0.30

  Maximum Number of Assets:
  K = 3
*/
#include <stdlib.h>
#include <stdio.h>
/* LINDO API header file */
#include "lindo.h"
#include "../common/commonutils.c"
/* Define a macro to declare variables for
    error checking */
#define APIERRORSETUP  \
   int nErrorCode; \
   char cErrorMessage[LS_MAX_ERROR_MESSAGE_LENGTH] \
/* Define a macro to do our error checking */
#define APIERRORCHECK  \
   if (nErrorCode) \
   { \
      if ( pEnv) \
      { \
         LSgetErrorMessage( pEnv, nErrorCode, \
          cErrorMessage); \
         printf("Errorcode=%d:  %s\n", nErrorCode, \
          cErrorMessage); \
      } else {\
         printf( "Fatal Error\n"); \
      } \
      goto Terminate; \
   } \

#define APIVERSION \
{\
    char szVersion[255], szBuild[255];\
    LSgetVersionInfo(szVersion,szBuild);\
    printf("\nLINDO API Version %s built on %s\n",szVersion,szBuild);\
}\

/* main entry point */
int main()
{
   APIERRORSETUP;
/* Number of constraints */
   int nM = 10;
/* Number of assets (7) plus number of indicator variables (7) */
   int nN = 14;
/* declare an instance of the LINDO environment object */
   pLSenv pEnv = NULL;
/* declare an instance of the LINDO model object */
   pLSmodel pModel;

   char MY_LICENSE_KEY[1024];
  /****************************************************************
   * Step 1: Create a LINDO environment.
   ****************************************************************/
   nErrorCode = LSloadDefaultLicenseString(MY_LICENSE_KEY);
   if ( nErrorCode != LSERR_NO_ERROR)
   {
      printf( "Failed to load license key (error %d)\n",nErrorCode);
      exit( 1);
   }

   APIVERSION;
   pEnv = LScreateEnv ( &nErrorCode, MY_LICENSE_KEY);
   if ( nErrorCode == LSERR_NO_VALID_LICENSE)
   {
      printf( "Invalid License Key!\n");
      exit( 1);
   }
   APIERRORCHECK;
  /****************************************************************
   * Step 2: Create a model in the environment.
   ****************************************************************/
   pModel = LScreateModel ( pEnv, &nErrorCode);
   APIERRORCHECK;
   {
  /*****************************************************************
   * Step 3: Specify and load the LP portion of the model.
   *****************************************************************/
     /* The maximum number of assets allowed in a portfolio */
      int  K = 3;
     /* The target return */
      double R = 0.30;
     /* The direction of optimization */
      int objsense = LS_MIN;
      /* The objective's constant term */
      double objconst = 0.;
      /* There are no linear components in the objective function.*/
      double c[14] = { 0., 0., 0., 0., 0., 0.,0.,
                       0., 0., 0., 0., 0., 0.,0.};
      /* The right-hand sides of the constraints */
      double rhs[10] = { 1.0, R, 0., 0., 0., 0., 0., 0., 0., K};
      /* The constraint types */
      char contype[10] = {'E','G','L','L','L','L','L','L','L','L'};
      /* The number of nonzeros in the constraint matrix */
      int Anz = 35;
      /* The indices of the first nonzero in each column */
      int Abegcol[15] = { 0,  3,  6,  9, 12, 15, 18,
                         21, 23, 25, 27, 29, 31, 33,Anz};
      /* The length of each column. Since we aren't leaving
       * any blanks in our matrix, we can set this to NULL */
      int *Alencol = NULL;
      /* The nonzero coefficients */
      double A[35] = {  1.00, 0.14, 1.00,
                        1.00, 0.77, 1.00,
                        1.00, 0.28, 1.00,
                        1.00, 0.17, 1.00,
                        1.00, 0.56, 1.00,
                        1.00, 0.18, 1.00,
                        1.00, 0.70, 1.00,
                        -0.04, 1.00,
                        -0.56, 1.00,
                        -0.37, 1.00,
                        -0.32, 1.00,
                        -0.52, 1.00,
                        -0.38, 1.00,
                        -0.25, 1.00 };
      /* The row indices of the nonzero coefficients */
      int Arowndx[35] = { 0, 1, 2, 0, 1, 3, 0, 1, 4, 0, 1, 5,
                          0, 1, 6, 0, 1, 7, 0, 1, 8, 2, 9, 3,
                          9, 4, 9, 5, 9, 6, 9, 7, 9, 8, 9    };
      /* By default, all variables have a lower bound of zero
       * and an upper bound of infinity. Therefore pass NULL
       * pointers in order to use these default values. */
      double *lb = NULL, *ub = NULL;
  /*****************************************************************
   * Step 4: Specify and load the quadratic matrix
   *****************************************************************/
   /* The number of nonzeros in the quadratic matrix */
      int Qnz = 28;
   /* The nonzero coefficients in the Q-matrix */
   double Q[28] = { 1.00,  0.11,  0.04,  0.02,  0.08,  0.03,  0.10,
                    1.00,  0.21,  0.13,  0.43,  0.14,  0.54,
                    1.00,  0.05,  0.16,  0.05,  0.20,
                    1.00,  0.10,  0.03,  0.12,
                    1.00,  0.10,  0.40,
                    1.00,  0.12,
                    1.00 };
    /* The row indices of the nonzero coefficients in the Q-matrix*/
    int  Qrowndx[28] = { -1, -1, -1, -1, -1, -1, -1,
                         -1, -1, -1, -1, -1, -1,
                         -1, -1, -1, -1, -1,
                         -1, -1, -1, -1,
                         -1, -1, -1,
                         -1, -1,
                         -1 };
    /* The indices of the first nonzero in each column in the Q-matrix */
    int Qcolndx1[28] = {  0, 1, 2, 3, 4, 5, 6,
                          1, 2, 3, 4, 5, 6,
                          2, 3, 4, 5, 6,
                          3, 4, 5, 6,
                          4, 5, 6,
                          5, 6,
                          6};
    int Qcolndx2[28] = {  0, 0, 0, 0, 0, 0, 0,
                          1, 1, 1, 1, 1, 1,
                          2, 2, 2, 2, 2,
                          3, 3, 3, 3,
                          4, 4, 4,
                          5, 5,
                          6};
    /* Pass the linear portion of the data to problem structure
     * by a call to LSloadLPData() */
     nErrorCode = LSloadLPData( pModel, nM, nN, objsense, objconst,
                                c, rhs, contype,
                                Anz, Abegcol, Alencol, A, Arowndx,
                                lb, ub);
     APIERRORCHECK;
    /* Pass the quadratic portion of the data to problem structure
     * by a call to LSloadQCData()  */
     nErrorCode = LSloadQCData(pModel, Qnz, Qrowndx,
                               Qcolndx1, Qcolndx2, Q);
     APIERRORCHECK;
    /* Pass the integrality restriction to problem structure
     * by a call to LSloadVarData()  */
     {
       char vartype[14] ={ 'C','C','C','C','C','C','C',   /* w(j) */
                           'B','B','B','B','B','B','B' }; /* x(j) */
       nErrorCode = LSloadVarType(pModel, vartype);
       APIERRORCHECK;
     }
   }
  /*****************************************************************
   * Step 5: Perform the optimization using the MIP solver
   *****************************************************************/
   nErrorCode = LSsolveMIP( pModel, NULL);
   APIERRORCHECK;
   {
  /*****************************************************************
   * Step 6: Retrieve the solution
   *****************************************************************/
      int i;
      double x[14], MipObj;
      /* Get the value of the objective and solution */
      nErrorCode = LSgetInfo(pModel, LS_DINFO_MIP_OBJ, &MipObj);
      APIERRORCHECK;

      LSgetMIPPrimalSolution( pModel, x) ;
      APIERRORCHECK;
      printf ("*** Optimal Portfolio Objective = %f\n", MipObj);
      for (i = 0; i < nN/2; i++)
        printf( "Invest %5.2f percent of total budget in asset %d.\n",
                 100*x[i],i+1 );
      printf ("\n");
   }
Terminate:
  /*****************************************************************
   * Step 7: Delete the LINDO environment
   *****************************************************************/
   nErrorCode = LSdeleteEnv( &pEnv);
 /* Wait until user presses the Enter key */
   printf("Press <Enter> ...");
   getchar();
}
