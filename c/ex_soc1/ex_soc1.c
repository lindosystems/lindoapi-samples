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

   File   : ex_soc1.c
   Purpose: Solve a second-order cone program.
   Model  : Simple norm minimization

            MINIMIZE      w
            subject to            A.x    >= b
                         -w^2  + ||x||^2 <= 0
            x  : an n-vector
            w  : the norm of vector x.
   Data:
   A-matrix for linear constraints:
             w     x1    x2    x3    x4   x5
          [  0     19     0   -17    21   0   ]
     A =  [  0     12    21    0     0    0   ]
          [  0     0     12    0     0   16   ]

   b-vector:
     b =  [  1     1     1 ];

*/
#include <stdlib.h>
#include <stdio.h>
#include "lindo.h"

/* Define a macro to declare variables for error checking */
#define APIERRORSETUP  \
    int nErrorCode; \
    char cErrorMessage[LS_MAX_ERROR_MESSAGE_LENGTH] \

/* Define a macro to do our error checking */ #define APIERRORCHECK  \
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

    int nM = 4; /* Number of constraints */

    int nN = 6; /* Number of variables */

    pLSenv pEnv;

    pLSmodel pModel;

    char MY_LICENSE_KEY[1024];

   /*****************************************************************
    * Step 1: Create a model in the environment.
    *****************************************************************/
    nErrorCode = LSloadLicenseString(
      "../../../license/lndapi140.lic",MY_LICENSE_KEY);
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
       double cost[6] = { 1., 0., 0., 0., 0., 0.};

       /* The right-hand sides of the constraints */
       double rhs[4] = { 1.0, 1.0, 1.0,  0.0 };

       /* The constraint types */
       char contype[4] = {'E','E','E','L'};

       /* The number of nonzeros in the constraint matrix */
       int Anz = 7;

       /* The indices of the first nonzero in each column */
       int Abegcol[7] = { 0, 0, 2, 4, 5, 6, Anz};

       /* The length of each column.  Since we aren't leaving
        * any blanks in our matrix, we can set this to NULL */
       int *Alencol = NULL;

       /* The nonzero coefficients */
       double A[7] = { 19, 12 , 21, 12, -17, 21, 16};

       /* The row indices of the nonzero coefficients */
       int Arowndx[7] = { 0, 1, 1, 2, 0, 0, 2};

       /* All variables, except w, are free */
       double lb[6] = { 0.000000000,-LS_INFINITY,-LS_INFINITY,
                       -LS_INFINITY,-LS_INFINITY,-LS_INFINITY};

       double ub[6] = {LS_INFINITY,LS_INFINITY,LS_INFINITY,
                       LS_INFINITY,LS_INFINITY,LS_INFINITY};

   /*****************************************************************
    **Step 4: Specify the QCONE data
    *****************************************************************/

    /** The number of CONE constraints*/
       int nCones = 1;

    /** Specify the column indices of variables in the CONE constraint,*/
       int paiConecols[6] = {  0, 1, 2, 3, 4, 5};

       int paiConebeg[2] = {0, 6};

    /** Specify cone type */
       char pszConeTypes[1] = { LS_CONETYPE_QUAD };

      /* Pass the linear portion of the data to problem structure
       * by a call to LSloadLPData() */

       nErrorCode = LSloadLPData( pModel, nM, nN, objsense, objconst,
                                  cost, rhs, contype,
                                  Anz, Abegcol, Alencol, A, Arowndx,
                                  lb, ub);
       APIERRORCHECK;

      /* Pass the cone portion of the data to problem structure
       * by a call to LSloadConeDataData()  */
       nErrorCode = LSloadConeData(pModel, nCones, pszConeTypes, NULL,
         paiConebeg, paiConecols);
       APIERRORCHECK;

    /** Export the conic model in case required */
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
    if (nSolStatus == LS_STATUS_OPTIMAL ||
        nSolStatus == LS_STATUS_BASIC_OPTIMAL)
    {
       int i;
       double x[6], dObj;
       /* Get the value of the objective */
       nErrorCode = LSgetInfo( pModel, LS_DINFO_POBJ, &dObj) ;
       APIERRORCHECK;

       nErrorCode = LSgetPrimalSolution ( pModel, x);
       APIERRORCHECK;

       printf("Minimum norm = %11.5f*\n",x[0]);
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
