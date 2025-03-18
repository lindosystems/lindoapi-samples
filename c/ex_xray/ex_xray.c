/* ex_xray.c

  A C programming example for finding the extreme ray of an unbounded problem.

  The unbounded problem:

     MAX = 20 * A + 30 * C
     S.T.       A +  2 * C  -  1*D  <= 120
                A           -  2*D  <=  60
                         C          <=  50

   Solving this problem with the LINDO API involves
   the following steps:

      1. Create a LINDO environment.
      2. Create a model in the environment.
      3. Specify the model.
      4. Perform the optimization.
      5. Check the status and compute the extreme ray if unbounded.
      6. Delete the LINDO environment.
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* LINDO API header file */
#include "lindo.h"
#include "../common/commonutils.c"
/* define number of variables  */
#define nN      3

/* define number of constraints */
#define nM      3

/* The number of nonzeros in the constraint matrix */
#define nNZ     6



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

/*
 *  A simple implementation of finding the extreme direction
 *  in an unbounded polyhedron characterized with
 *  <anBegCol, anRowX, adA, acConTypes>.
 *
 */
int findXray (pLSenv pEnv, pLSmodel pModel, int nVars, int nCons, int nDir, int *anBegCol,
              int * anRowX, double *adA, char *acConTypes)
{
  APIERRORSETUP;
  int nColLen, iEnter, isSlack, i;
  double dEps=1.e-10;

  /* sparse xRay vectors */
  double *aDx = malloc(nCons*sizeof(double));
  int *iDx = malloc(nCons*sizeof(int)), nDx;

  int *nBasis = malloc(nCons*sizeof(int));

  /* solution vectors */
  double *padRedcosts = malloc(nVars*sizeof(double));
  double *padSlacks = malloc(nCons*sizeof(double));
  double *padPrimals= malloc(nVars*sizeof(double));
  double *padDuals = malloc(nCons*sizeof(double));

  int *panCstatus = malloc(nVars*sizeof(int));
  int *panRstatus = malloc(nCons*sizeof(int));


  /* To retrieve the primals */
  nErrorCode = LSgetPrimalSolution(pModel, padPrimals);
  APIERRORCHECK;

  /* To retrieve the reduced costs */
  nErrorCode = LSgetReducedCosts(pModel, padRedcosts);
  APIERRORCHECK;

  /* To retrieve the dual solution */
  nErrorCode = LSgetDualSolution(pModel, padDuals);
  APIERRORCHECK;

  /* To retrieve the slacks */
  nErrorCode = LSgetSlacks(pModel, padSlacks);
  APIERRORCHECK;

  /* To retrieve the basis */
  nErrorCode = LSgetBasis(pModel,panCstatus, panRstatus);
  APIERRORCHECK;


  printf("\n%8s %12s %12s %12s\n",
    "ColIndex","Primal","ReducedCost","ColStatus");
  for (i=0; i<nVars; i++)
  {
    if (panCstatus[i] >=0)
      nBasis[panCstatus[i]]=i;
    printf("%8d %12.3f %12.3f %12d\n",
      i, padPrimals[i], padRedcosts[i],panCstatus[i]);
  }

  printf("\n%8s %12s %12s %12s\n",
    "RowIndex","Dual","Slack","RowStatus");
  for (i=0; i<nCons; i++)
  {
    if (panRstatus[i] >=0)
      nBasis[panRstatus[i]]=i+nVars;
    printf("%8d %12.3f %12.3f %12d\n",
      i, padDuals[i],padSlacks[i],panRstatus[i]);
  }


  iEnter = -1;
  isSlack = 0;

  /* Scan structural columns for an entering variable */
  for ( i = 0; i < nVars; i++)
  {

    if (nDir == LS_MAX)
    {
      if ( padRedcosts[i] > dEps && panCstatus[ i] == LS_BASTYPE_ATLO )
      {
        iEnter = i;
        break;
      }
      else if ( padRedcosts[i] < -dEps && panCstatus[ i] == LS_BASTYPE_ATUP )
      {
        iEnter = i;
        break;
      }
    }
    else
    {
      if ( padRedcosts[i] < -dEps && panCstatus[ i] == LS_BASTYPE_ATLO )
      {
        iEnter = i;
        break;
      }
      else if ( padRedcosts[i] > dEps && panCstatus[ i] == LS_BASTYPE_ATUP )
      {
        iEnter = i;
        break;
      }
    }
  }

  /* Scan slack/surplus columns for an entering variable
    (only if not found above) */
  if (iEnter == -1) for ( i = 0; i < nCons; i++)
  {

    if (nDir == LS_MAX)
    {
      if ( padDuals[i] < -dEps && panRstatus[ i] == LS_BASTYPE_ATLO )
      {
        iEnter = i;
        isSlack = 1;
        break;
      }
    }
    else
    {
      if ( padDuals[i] > +dEps && panRstatus[ i] == LS_BASTYPE_ATLO )
      {
        iEnter = i;
        isSlack = 1;
        break;
      }
    }
  }

  if (iEnter >= 0 && !isSlack)
  {
    printf( "\nEntering nonbasic column's index: %d  (structural column).\n", iEnter);

    // FTRAN the column
    nColLen = anBegCol[ iEnter + 1] - anBegCol[ iEnter];


    // print the indices of the variables in the entering column
    printf("\nSparse representation of entering column\n");
    printf( "%8s %12s\n","VarIndex","ColEnter");
    for ( i = anBegCol[ iEnter]; i < anBegCol[ iEnter + 1]; i++)
    {
       printf( "%8d %12.3f\n", anRowX[i], adA[i]);
    }


    nErrorCode = LSdoFTRAN( pModel,
      &nColLen, &anRowX[ anBegCol[ iEnter]], &adA[ anBegCol[ iEnter]], /* A(:,iEnter) */
      &nDx, iDx, aDx); /* Extreme ray */

    APIERRORCHECK;
  }


  else if (iEnter >= 0 && isSlack)
  {
    int    aiArtif[1]; /* nonz indices */
    double adArtif[1]; /* nonz values */
    int    anArtif   ; /* length of aiArtif and adArtif */

    printf( "\nEntering nonbasic column is the artificial column of row %d.\n", iEnter);


    /* artificial column has a single nonzero (+-1) at position iEnter */
    anArtif = 1;
    aiArtif[0] = iEnter;

    if (acConTypes[iEnter] == LS_CONTYPE_GE)
      adArtif[0] = -1;
    else
      adArtif[0] = +1;

    // print the indices of the variables in the entering column
    printf("\nSparse representation of entering column\n");
    printf( "%8s %12s\n","VarIndex","Vaues");
    for ( i = 0; i < anArtif; i++)
    {
       printf( "%8d %12.3f\n", aiArtif[i], adArtif[i]);
    }


    // FTRAN the column
    nErrorCode = LSdoFTRAN( pModel, &anArtif, &aiArtif[0],
     &adArtif[0], &nDx, iDx, aDx);

    APIERRORCHECK;

  }


  else
  {
    nErrorCode = LSERR_INTERNAL_ERROR;
    APIERRORCHECK;
  }



  /*
   * Print the xray by listing the
   *   - indices of the nonzero entries
   *   - values of the nonzero entries
   *   - constraint senses associated with nonzero values
   */
  printf("\nSparse representation of x-ray\n");
  printf( "%8s %8s %12s %8s %8s\n","BasIndex","VarIndex","Values","ConType","Status");
  for ( i = 0; i < nDx; i++)
  {
     printf( "%8d %8d %12.3f %8c %8s\n", iDx[i], nBasis[iDx[i]], -aDx[i], acConTypes[iDx[i]],"Basic");
  }
  printf( "%8s %8d %12.3f %8s %8s\n", "N/A", iEnter, 1.0, "N/A","NonBasic");

Terminate:
  free(aDx);
  free(iDx);
  free(nBasis);
  free(padRedcosts);
  free(padSlacks);
  free(padPrimals);
  free(padDuals);
  free(panCstatus);
  free(panRstatus);

  return nErrorCode;
}





/*
 *  Main entry point
 *
 */
int main()
{
    APIERRORSETUP;
    char MY_LICENSE_KEY[1024];

    int solstatus;/*solution status (see lindo.h for possible values)*/
    int prep_level;


    /* Specify the model. */


    /* The direction of optimization */
    int nDir = LS_MAX;

    /* The objective's constant term */
    double dObjConst = 0.;

    /* The coefficients of the objective function */
    double adC[nN] = { 20., 30., 0.};

    /* The right-hand sides of the constraints */
    double adB[nM] = { 120., 60., 50.};

    /* The constraint types */
    char acConTypes[nM] = {'L', 'L', 'L'};

    /* The indices of the first nonzero in each column */
    int anBegCol[nN+1] = { 0, 2, 4, nNZ};

    /* The length of each column.  Since we aren't leaving
    any blanks in our matrix, we can set this to NULL */
    int *pnLenCol = NULL;

    /* The nonzero coefficients */
    double adA[nNZ] = { 1., 1., 2., 1., -1., -2};

    /* The row indices of the nonzero coefficients */
    int anRowX[nNZ] = { 0, 1, 0, 2, 0, 1};


    double *pdLower = NULL, *pdUpper = NULL;

/* declare an instance of the LINDO environment object */
   pLSenv pEnv;

/* declare an instance of the LINDO model object */
   pLSmodel pModel;

/* Create a LINDO environment. */
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

/* Create a model in the environment. */
   pModel = LScreateModel ( pEnv, &nErrorCode);
   APIERRORCHECK;



   /*
    * Turn off the LP preprocessor. This is required if the model
    * is infeasible or unbounded and the user wishes to debug it.
    */
   nErrorCode = LSgetModelIntParameter(pModel,LS_IPARAM_LP_PRELEVEL,&prep_level);
   APIERRORCHECK;
   if (prep_level > 0)
   {
     nErrorCode = LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRELEVEL,0);
     nErrorCode = LSsetModelIntParameter(pModel,LS_IPARAM_SOLVER_IUSOL,1);
     printf("The LP presolver has been turned off. Solving ...\n\n");
   }


   /*
    * We have now assembled a full description of the model.
    * We pass this information to LSloadLPData with the
    * following call.
    */
   nErrorCode = LSloadLPData( pModel, nM, nN, nDir,
     dObjConst, adC, adB, acConTypes, nNZ, anBegCol,
     pnLenCol, adA, anRowX, pdLower, pdUpper);
   APIERRORCHECK;


   /*
    * Export the model in LINDO Format
    */
   nErrorCode = LSwriteLINDOFile( pModel, "xray.ltx");

   /*
    * Perform the optimization
    */
   nErrorCode = LSoptimize( pModel,LS_METHOD_PSIMPLEX, &solstatus);
   APIERRORCHECK;

   if (solstatus == LS_STATUS_BASIC_OPTIMAL)
   {
     printf("\tThe model is solved to optimality.\n");
   }
   else if (solstatus == LS_STATUS_UNBOUNDED)
   {
     printf("\nThe model is unbounded.. Analyzing...\n\n");
     findXray(pEnv,pModel,nN,nM,nDir,anBegCol,anRowX,adA,acConTypes);
   }


Terminate:

 /* >>> Step 6 <<< Delete the LINDO environment */
   LSdeleteModel( &pModel);
   LSdeleteEnv( &pEnv);

  /* Wait until user presses the Enter key */
   printf("Press <Enter> ...");
   getchar();


}
