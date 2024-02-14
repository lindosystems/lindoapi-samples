/*
###################################################################
#                       LINDO-API
#                    Sample Programs
#                  Copyright (c) 2011
#
#         LINDO Systems, Inc.           312.988.7422
#         1415 North Dayton St.         info@lindo.com
#         Chicago, IL 60622             http://www.lindo.com
###################################################################

  File   : ex_soc2.c
  Purpose: Solve a second-order rotated cone program.
          A rotated cone constraint is of the form:
            2*x0*x1 - x2*x2 - x3*x3 - ... >= 0;
              x0, x1 >= 0;

    The model in natural form:
           MINIMIZE      11*x0 +  7*x1  + 9*x2;
           subject to     5/x0 +  6/x1  + 8/x2 <= 1;
                          x0, x1, x2 >= 0;

       Reformulated as a rotated cone:
           MINIMIZE      11*x0 +  7*x1  + 9*x2;
           subject to     2*r0 +  2*r1  + 2*r2 <= 1;
                            k0                 = 5^0.5;
                                    k1         = 6^0.5;
                                            k2 = 8^0.5
                       2*r0*x0>= k0^2;
                       2*r1*x1>= k1^2;
                       2*r2*x2>= k2^2;
                          x0, x1, x2 >= 0;
                          r0, r1, r2 >= 0;

  The constraint matrix for the linear constraints:
            0   1   2   3   4   5   6   7   8
           x0  x1  x2  r0  r1  r2  k0  k1  k2
         [  0   0   0   2   2   2   0   0   0] <= 1
    A =  [  0   0   0   0   0   0   1   0   0] = 5^0.5
         [  0   0   0   0   0   0   0   1   0] = 6^0.5
         [  0   0   0   0   0   0   0   0   1] = 8^0.5

*/
#include <stdlib.h>
#include <stdio.h>
#include "lindo.h"

/* Define a macro to declare variables for error checking */
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
   int nSolStatus;

   APIERRORSETUP;

   int nM = 4; /* Number of linear constraints */

   int nN = 9; /* Number of variables */

   pLSenv pEnv;

   pLSmodel pModel;

   char MY_LICENSE_KEY[1024];

  /*****************************************************************
   * Step 1: Create a model in the environment.
   *****************************************************************/
  // Load the license into MY_LICENSE_KEY
   nErrorCode = LSloadLicenseString( "../../../license/lndapi150.lic",MY_LICENSE_KEY);
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

  /*****************************************************************
   * Step 2: Create a model in the environment.
   *****************************************************************/
   pModel = LScreateModel ( pEnv, &nErrorCode);
   APIERRORCHECK;

   {
  /*****************************************************************
   * Step 3: Specify the linear portion of the model.
   *****************************************************************/

     /* The direction of optimization */
      int objsense = LS_MIN;

      /* The objective's constant term */
      double objconst = 0.;

      /* The coefficients of the objective function*/
      double cost[9] = { 11.0, 7.0, 9.0 , 0., 0., 0., 0., 0., 0.};

      /* The right-hand sides of the constraints( square roots of 5, 6, 8)*/
      double rhs[4] = { 1.0, 2.2360679775, 2.44948974278, 2.82842712475};

      /* The constraint types */
      char contype[4] = {'L', 'E', 'E', 'E'};

      /* The number of nonzeros in the constraint matrix */
      int Anz = 6;

      /* The indices in A[] of the first nonzero in each column */
      int Abegcol[10] = { 0, 0, 0, 0, 1, 2, 3, 4, 5, Anz};

      /* The length of each column.  Since we aren't leaving
       * any blanks in our matrix, we can set this to NULL */
      int *Alencol = NULL;

      /* The nonzero constraint coefficients */
      double A[6] = { 2.0, 2.0, 2.0, 1.0, 1.0, 1.0};

      /* The row indices of the nonzero coefficients */
      int Arowndx[6] = { 0, 0, 0, 1, 2, 3};

      /* All variables are non-negative */
      double lb[9] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};

      double ub[9] = {LS_INFINITY,LS_INFINITY,LS_INFINITY,
                      LS_INFINITY,LS_INFINITY,LS_INFINITY,
                      LS_INFINITY,LS_INFINITY,LS_INFINITY};

  /*****************************************************************
   * Step 4: Set up data describing the CONE constraints
   *****************************************************************/

     /* The number of Cone constraints */
      int nCones = 3;

      /* The col indices of the variables in each Cone constraint */
      int paiConecols[9] = {0, 3, 6,  1, 4, 7,  2, 5, 8};

     /* The start in paiConecols[] of the indices for each Cone constraint */
      int paiConebeg[4] =  {0,        3,        6,      9};

     /* These are Rotated Cone constraints */
      char pszConeTypes[3] = { 'R', 'R', 'R'};

     /* Pass the linear portion of the data to problem structure
      * by a call to LSloadLPData() */

      nErrorCode = LSloadLPData( pModel, nM, nN, objsense, objconst,
                                 cost, rhs, contype,
                                 Anz, Abegcol, Alencol, A, Arowndx,
                                 lb, ub);
      APIERRORCHECK;
     /* Pass the Cone portion of the data to the problem structure
      * by a call to LSloadConeData()  */
      nErrorCode = LSloadConeData(pModel, nCones, pszConeTypes, NULL,
        paiConebeg, paiConecols);
      APIERRORCHECK;

/* Optionally, write an MPS file version of the model */
      LSwriteMPSFile(pModel,"cone.mps",0);

   }
  /*****************************************************************
   * Step 5: Perform the optimization using the QCONE solver
   *****************************************************************/
   nErrorCode = LSsetModelIntParameter(pModel, LS_IPARAM_BARRIER_SOLVER,
                                       LS_BAR_METHOD_FREE);

   nErrorCode = LSoptimize( pModel, LS_METHOD_FREE, &nSolStatus);
   APIERRORCHECK;
  /*****************************************************************
   * Step 6: Retrieve the solution
   *****************************************************************/
   if (nSolStatus == LS_STATUS_OPTIMAL || nSolStatus == LS_STATUS_BASIC_OPTIMAL)
   {
      int i;
      double x[9], dObj;
      /* Get the value of the objective */
      nErrorCode = LSgetInfo( pModel, LS_DINFO_POBJ, &dObj) ;
      APIERRORCHECK;

      nErrorCode = LSgetPrimalSolution ( pModel, x);
      APIERRORCHECK;

      printf("        Obj =  %11.5f\n",dObj);
      for (i = 0; i < nN; i++)
        printf("%7s x[%d] = %11.5f\n","",i,x[i] );
      printf ("\n");
   }
   else
   {
     printf("Not optimal, status = %d\n",nSolStatus);
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
