 /*
#################################################################
#                       LINDO-API
#                    Sample Programs
#                  Copyright (c) 2011
#
#         LINDO Systems, Inc.           312.988.7422
#         1415 North Dayton St.         info@lindo.com
#         Chicago, IL 60622             http://www.lindo.com
#################################################################
  File   : markow.c
  Purpose: Solve a quadratic programming problem.
  Model  : The Markowitz Portfolio Selection Model

           MAXIMIZE  r(1)w(1) + ... +r(n)w(n)
           st.       sum_{ij} Q(i,j)w(i)w(j) <= K
                         w(1) + ..... + w(n)  = 1
                         w(1),         ,w(n) >= 0
           where
           r(i)  : return on asset i
           Q(i,j): covariance between the returns of i^th and
                   j^th assets.
           K     : a scalar denoting the level of risk of loss.
           w(i)  : proportion of total budget invested on asset i

  Covariance Matrix:
          w1    w2    w3    w4
     w1 [ 1.00  0.64  0.27  0.    ]
     w2 [ 0.64  1.00  0.13  0.    ]
     w3 [ 0.27  0.13  1.00  0.    ]
     w4 [ 0.    0.    0.    1.00  ]

  Returns Vector:
          w1    w2    w3    w4
  r =   [ 0.30  0.20 -0.40  0.20  ]

  Risk of Loss Factor:
  K = 0.4

*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "lindo.h"

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
int main(int argc, char **argv)
{
   APIERRORSETUP;
   int nM = 2;      /* Number of constraints */
   int nN = 4;      /* Number of assets */

   double K = 0.20; /* 1/2 of the risk level*/

  /* declare an instance of the LINDO environment object */
   pLSenv pEnv = NULL;
  /* declare an instance of the LINDO model object */
   pLSmodel pModel;

   char MY_LICENSE_KEY[1024];

  /*****************************************************************
   * Step 1: Create a model in the environment.
   *****************************************************************/
   nErrorCode = LSloadLicenseString("../../../license/lndapi130.lic",MY_LICENSE_KEY);
   if ( nErrorCode != LSERR_NO_ERROR)
   {
      printf( "Failed to load license key (error %d)\n",nErrorCode);
      exit( 1);
   }

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
   APIVERSION;
   pModel = LScreateModel ( pEnv, &nErrorCode);
   APIERRORCHECK;
  /*****************************************************************
   * Step 3: Specify and load the LP portion of the model.
   *****************************************************************/
   {
     /* The direction of optimization */
      int objsense = LS_MAX;
      /* The objective's constant term */
      double objconst = 0.;
      /* The coefficients of the objective function are the expected
      returns*/
      double reward[4] = { .3, .2, -.4, .2};
      /* The right-hand sides of the constraints */
      double rhs[2] = { K, 1.0 };
      /* The constraint types */
      char contype[2] = {'L','E'};
      /* The number of nonzeros in the constraint matrix */
      int Anz = 4;
      /* The indices of the first nonzero in each column */
      int Abegcol[5] = { 0, 1, 2, 3, Anz};
      /* The length of each column.  Since we aren't leaving
       * any blanks in our matrix, we can set this to NULL */
      int *Alencol = NULL;
      /* The nonzero coefficients */
      double A[4] = { 1., 1., 1., 1.};
      /* The row indices of the nonzero coefficients */
      int Arowndx[4] = { 1, 1, 1, 1};
      /* By default, all variables have a lower bound of zero
       * and an upper bound of infinity.  Therefore pass NULL
       * pointers in order to use these default values. */
      double *lb = NULL, *ub = NULL;
  /*****************************************************************
   * Step 4: Specify and load the quadratic matrix
   *****************************************************************/
     /* The number of nonzeros in the quadratic matrix */
      int Qnz = 7;
      /* The nonzero coefficients in the Q-matrix */
      double Q[7] = { 1.00, .64, .27,
                           1.00, .13,
                                1.00,
                                      1.00} ;
      /* Specify the row indices of the nonzero coefficients in the
         Q-matrix. */
      int Qrowndx[7] = { 0, 0, 0, 0, 0, 0, 0};
      /* The indices of variables in the Q-matrix */
      int Qcolndx1[7] = {  0, 1, 2, 1, 2, 2, 3};
      int Qcolndx2[7] = {  0, 0, 0, 1, 1, 2, 3};
      /* Pass the linear portion of the data to problem structure
       * by a call to LSloadLPData() */
      nErrorCode = LSloadLPData( pModel, nM, nN, objsense, objconst,
                                reward, rhs, contype,
                                Anz, Abegcol, Alencol, A, Arowndx,
                                lb, ub);
      APIERRORCHECK;
     /* Pass the quadratic portion of the data to problem structure
      * by a call to LSloadQCData()  */
      nErrorCode = LSloadQCData(pModel, Qnz, Qrowndx,
                                 Qcolndx1, Qcolndx2, Q );
      APIERRORCHECK;
   }
  /*****************************************************************
   * Step 5: Perform the optimization using the barrier solver
   *****************************************************************/
   nErrorCode = LSoptimize( pModel, LS_METHOD_BARRIER,NULL);
   APIERRORCHECK;
  /***************************************************************
   * Step 6: Retrieve the solution
   ***************************************************************/
   {
      int i;
      double W[4], dObj;
   /* Get the value of the objective */
      nErrorCode = LSgetInfo( pModel, LS_DINFO_POBJ, &dObj) ;
      APIERRORCHECK;
      printf( "* Objective Value = %10g\n\n", dObj);
    /* Get the portfolio */
      nErrorCode = LSgetPrimalSolution ( pModel, W);
      APIERRORCHECK;
      printf ("* Optimal Portfolio : \n");
      for (i = 0; i < nN; i++)
      printf( "Invest %5.2f percent of total budget in asset %d.\n",
               100*W[i],i+1 );
      printf ("\n");
   }
Terminate:
  /***************************************************************
   * Step 7: Delete the LINDO environment
   *****************************************************************/
   nErrorCode = LSdeleteEnv( &pEnv);
   /* Wait until user presses the Enter key */
   printf("Press <Enter> ...");
   getchar();
}
