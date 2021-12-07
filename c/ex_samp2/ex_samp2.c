/* samplecb.c

  A C programming example of interfacing with the
  LINDO API that employs a callback function.

  The problem:

     MAX = 20 * A + 30 * C
     S.T.       A +  2 * C  <= 120
                A           <=  60
                         C  <=  50

   Solving such a problem with the LINDO API involves
   the following steps:

      1. Create a LINDO environment.
      2. Create a model in the environment.
      3. Specify the model.
      4. Perform the optimization.
      5. Retrieve the solution.
      6. Delete the LINDO environment.
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
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

/* A callback function that will be called by the LINDO
    solver */
int CALLBACKTYPE MyCallback( pLSmodel pMod, int nLocation,
 void* pMyData)
{

/* Display the string we passed to LSsetCallback() */
   printf( "In MyCallback: %s\n", (char*)pMyData);

/* Display current iteration count and objective value */
   {
      int nIter;
      double dObj;
      LSgetCallbackInfo( pMod, nLocation, LS_IINFO_SIM_ITER,
       &nIter);
      LSgetCallbackInfo( pMod, nLocation, LS_DINFO_POBJ,
       &dObj);
      printf( "In MyCallback, Iters, Obj: %d %g\n",
       nIter, dObj);
   }

   return( 0);
}

/* main entry point */
int main()
{
   APIERRORSETUP;
   int i, j;
   char strbuffer[255];
   char MY_LICENSE_KEY[1024];

/* Number of constraints */
   int nM = 3;

/* Number of variables */
   int nN = 2;

/* declare an instance of the LINDO environment object */
   pLSenv pEnv;

/* declare an instance of the LINDO model object */
   pLSmodel pModel;

/* >>> Step 1 <<< Create a LINDO environment. */
   nErrorCode = LSloadLicenseString("../../../license/lndapi130.lic",MY_LICENSE_KEY);
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

/* Variable and constraint names */
      char **paszVarnames, **paszConnames;
      char *pszTitle = NULL, *pszObjname = NULL, *pszRhsname = NULL,
        *pszRngname = NULL, *pszBndname = NULL;

      paszConnames = (char **) malloc(nM*sizeof(char *));
      for (i=0; i < nM; i++)
      {
        paszConnames[i] = (char *) malloc(255*sizeof(char));
        sprintf(strbuffer,"CON%02d",i);
        strcpy(paszConnames[i],strbuffer);
      }

      paszVarnames = (char **) malloc(nN*sizeof(char *));
      for (j=0; j < nN; j++)
      {
        paszVarnames[j] = (char *) malloc(255*sizeof(char));
        sprintf(strbuffer,"VAR%02d",j);
        strcpy(paszVarnames[j],strbuffer);
      }

/* We have now assembled a full description of the model.
    We pass this information to LSloadLPData with the
    following call. */
      nErrorCode = LSloadLPData( pModel, nM, nN, nDir,
       dObjConst, adC, adB, acConTypes, nNZ, anBegCol,
       pnLenCol, adA, anRowX, pdLower, pdUpper);
      APIERRORCHECK;

/* Load name data */
      nErrorCode = LSloadNameData(pModel, pszTitle,
        pszObjname, pszRhsname, pszRngname,pszBndname,
        paszConnames, paszVarnames,NULL);

   }

   {
 /* Establish the callback function */
      char* pMyData = "My string!";
      nErrorCode = LSsetCallback( pModel,
       (cbFunc_t) MyCallback, pMyData);
      APIERRORCHECK;

 /* >>> Step 4 <<< Perform the optimization */
      nErrorCode = LSoptimize( pModel,
       LS_METHOD_PSIMPLEX, NULL);
      APIERRORCHECK;
   }

   {

 /* >>> Step 5 <<< Retrieve the solution */
      double adX[ 2], adY[3],dObj;

 /* Get the value of the objective */
      nErrorCode = LSgetInfo( pModel, LS_DINFO_POBJ, &dObj) ;
      APIERRORCHECK;

      printf( "Objective Value = %g\n", dObj);

 /* Get the primal and dual values */
      nErrorCode = LSgetPrimalSolution ( pModel, adX);
      APIERRORCHECK;

      nErrorCode = LSgetDualSolution ( pModel, adY);
      APIERRORCHECK;

      printf ("Primal values:\n");
      for (j = 0; j < nN; j++)
      {
        LSgetVariableNamej(pModel,j,strbuffer);
        printf( "%s = %g\n", strbuffer, adX[j]);
      }
      printf ("\n");

      printf ("Dual values:\n");
      for (i = 0; i < nM; i++)
      {
        LSgetConstraintNamei(pModel,i,strbuffer);
        printf( "%s = %g\n", strbuffer, adY[i]);
      }

   }

 /* >>> Step 6 <<< Delete the LINDO environment */
   LSdeleteModel( &pModel);
   LSdeleteEnv( &pEnv);

  /* Wait until user presses the Enter key */
   printf("Press <Enter> ...");
   getchar();


}
