/* modify.c

  A C programming example of interfacing with the
  LINDO API demonstrating

    1) Adding new variables and constraints.
    2) Modifying objective coefficients

  Original problem:

     MAX = 20 * A + 30 * C
     S.T.       A +  2 * C  <= 120
                A           <=  60
                         C  <=  50

  Modified problem after adding a variable:

     MAX = 20 * A + 30 * C  - 2 *D
     S.T.       A +  2 * C          <= 120
                A                   <=  60
                         C          <=  50

  Modified problem after adding a constraint:

     MAX = 20 * A + 30 * C  - 2 *D
     S.T.       A +  2 * C          <= 120
                A                   <=  60
                         C          <=  50
         [               C  + D     >=  50]  (*)


  Modified problem after changing objective
  coefficients.

     MAX= -10 * A + 30 * C  - 3 *D
     S.T.       A +  2 * C          <= 120
                A                   <=  60
                         C          <=  50
                         C  + D     >=  50


   Solving such a problem with the LINDO API involves
   the following steps:

      1.  Create a LINDO environment.
      2.  Create a model in the environment.
      3.  Specify the model.
      4a. Add a new variable
      4b. Add a new constraint
      4c. Modify obj coeff
      5.  Perform the optimization.
      6.  Retrieve the solution.
      7.  Delete the LINDO environement.
*/

#include <stdlib.h>
#include <stdio.h>

/* LINDO API header file */
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
int main()
{

   APIERRORSETUP;

/* Number of constraints */
   int nM = 3;

/* Number of variables */
   int nN = 2;

/* declare an instance of the LINDO environment object */
   pLSenv pEnv;

/* declare an instance of the LINDO model object */
   pLSmodel pModel;

   char MY_LICENSE_KEY[1024];

  /*****************************************************************
   * Step 1: Create a model in the environment.
   *****************************************************************/
   nErrorCode = LSloadLicenseString("../../../license/lndapi140.lic",MY_LICENSE_KEY);
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

/* >>> Step 2 <<< Create a model in the environment. */
   pModel = LScreateModel ( pEnv, &nErrorCode);
   APIERRORCHECK;

   {

/* >>> Step 3 <<< Specify the model.

 To specify our model, we make a call to LSloadLPData,
  passing it:

 - A pointer to the model which we are specifying(pModel)
 - The number of constraints in the model
 - The number of variables in the model
 - The direction of the optimization (i.e. minimize or
 -  maximize)
 - The value of the constant term in the objective (may
    be zero)
 - The coefficients of the objective function
 - The right-hand sides of the constraints
 - The types of the constraints
 - The number of nonzeros in the constraint matrix
 - The indices of the first nonzero in each column
 - The length of each column
 - The nonzero coefficients
 - The row indices of the nonzero coefficients
 - Simple upper and lower bounds on the variables
*/

/* The direction of optimization */
      int nDir = LS_MAX;

/* The objective's constant term */
      double dObjConst = 0.;

/* The coefficients of the objective function */
      double adC[2] = { 20., 30.};

/* The right-hand sides of the constraints */
      double adB[3] = { 120., 60., 50.};

/* The constraint types */
      char acConTypes[3] = {'L', 'L', 'L'};

/* The number of nonzeros in the constraint matrix */
      int nNZ = 4;

/* The indices of the first nonzero in each column */
      int anBegCol[3] = { 0, 2, nNZ};

/* The length of each column.  Since we aren't leaving
    any blanks in our matrix, we can set this to NULL */
      int *pnLenCol = NULL;

/* The nonzero coefficients */
      double adA[4] = { 1., 1., 2., 1.};

/* The row indices of the nonzero coefficients */
      int anRowX[4] = { 0, 1, 0, 2};

/* Simple upper and lower bounds on the variables.
    By default, all variables have a lower bound of zero
    and an upper bound of infinity.  Therefore pass NULL
    pointers in order to use these default values. */
      double *pdLower = NULL, *pdUpper = NULL;

/* We have now assembled a full description of the model.
    We pass this information to LSloadLPData with the
    following call. */
      nErrorCode = LSloadLPData( pModel, nM, nN, nDir,
       dObjConst, adC, adB, acConTypes, nNZ, anBegCol,
       pnLenCol, adA, anRowX, pdLower, pdUpper);
      APIERRORCHECK;

   }
   printf("Input original model.\n");

   LSwriteLINDOFile(pModel,"original.ltx");

/* >>> Step 4.a <<< Add a new variable */
   {
     int nA = 1;
     char achVtype[1] = {'C'};
     double *pdLower = NULL, *pdUpper = NULL;
     char *apszVname[1] = {"TAVSAN"};
     //char **apszVname = NULL;
     int *ia=NULL;
     int *ka=NULL;
     int *cnta=NULL;
     double *a=NULL;
     double c[1]={-2.0};

     /* It is assumed that the new variable has no nonzeros in the existing
     constraints, therefore, sparse matrix data are set to NULL.*/
     nErrorCode = LSaddVariables(pModel, nA,achVtype,apszVname,NULL,NULL,NULL,
                                 NULL,c,NULL, NULL);

     printf("Called  LSaddVariables to add new variables.\n");
     LSwriteLINDOFile(pModel,"modify-1.ltx");

   }


/* >>> Step 4.b <<< Add a new constraint */
   {
     int nCon = 1;
     char achCtype[1] = {'G'};
     char *apszCname[1] = {"TAZI"};
     //char **apszCname = NULL;
     int ia[2]={1,2};
     int ka[2]={0,2};
     double a[2]={1, 1};
     double rhs[1]={50.0};

     /* It is assumed that the new variable has no nonzeros in the existing
     constraints, therefore, sparse matrix data are set to NULL.*/
     nErrorCode = LSaddConstraints(pModel, nCon,achCtype,apszCname,ka,a,ia,rhs);

     printf("Called  LSaddConstraints to add new constraints.\n");
     LSwriteLINDOFile(pModel,"modify-2.ltx");

   }


/* >>> Step 4.c <<< Modify coeff of variables */
   {
     int i;
     double padC   [2];
     int    paiVars[2];

     /* indices of the variables to be modified */
     paiVars[0] = 0;
     paiVars[1] = 2;

     /* new obj coefficients of these variables */
     padC[0] = -10;
     padC[1] = -3;

     LSmodifyObjective(pModel,2, paiVars, padC);

     printf("Called  LSmodifyObjective to modify objective coef.\n");
     LSwriteLINDOFile(pModel,"modify-3.ltx");


   }


 /* >>> Step 5 <<< Perform the optimization */
   nErrorCode = LSoptimize( pModel, LS_METHOD_PSIMPLEX, NULL);
   APIERRORCHECK;
   printf("Optimized final model.\n");

   {

 /* >>> Step 6 <<< Retrieve the solution */
      int i;
      double adX[ 100], adInc[100], adDec[100], dObj;

 /* Get the value of the objective */
      nErrorCode = LSgetInfo(pModel,LS_DINFO_POBJ,&dObj);
      APIERRORCHECK;

      printf( "Objective Value = %g\n", dObj);

 /* Get the variable values */
      nErrorCode = LSgetPrimalSolution ( pModel, adX);
      APIERRORCHECK;

      nErrorCode = LSgetInfo ( pModel, LS_IINFO_NUM_VARS, &nN);
      printf ("Primal values = ");
      for (i = 0; i < nN; i++) printf( "%g ", adX[i]);
      printf ("\n");

   }

Terminate:
 /* >>> Step 7 <<< Delete the LINDO environment */
   nErrorCode = LSdeleteEnv( &pEnv);
   /* Wait until user presses the Enter key */
   printf("Press <Enter> ...");
   getchar();
}
