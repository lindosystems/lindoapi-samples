/* ex_trans.c

   Solving Transportation Problems using LINDO API.

   The problem:

    min z(x) = @sum_i @sum_j cij xij
    subject to
    @sum(j) xij = ai, i = 1 to m
    @sum(i) xij = bj, j = 1 to n
    xij = 0 for all i, j.

 */

#include <stdlib.h>
#include <stdio.h>

/* LINDO API header file is located under \lindoapi\include */
#include "lindo.h"


/* Define a macro to declare variables for
    error checking */
#define APIERRORSETUP                             \
  int nErrorCode;                                 \
  char cErrorMessage[LS_MAX_ERROR_MESSAGE_LENGTH] \

/* Define a macro to do our error checking */
#define APIERRORCHECK                           \
  if (nErrorCode)                               \
  {                                             \
    if (pEnv)                                   \
    {                                           \
      LSgetErrorMessage(pEnv, nErrorCode,       \
                        cErrorMessage);         \
      printf("Errorcode=%d:  %s\n", nErrorCode, \
             cErrorMessage);                    \
    } else {                                    \
      printf("Fatal Error\n");                  \
    }                                           \
    exit(1);                                    \
  }                                             \

#define APIVERSION                                                      \
  {                                                                     \
    char szVersion[255], szBuild[255];                                  \
    LSgetVersionInfo(szVersion, szBuild);                               \
    printf("\nLINDO API Version %s built on %s\n", szVersion, szBuild); \
  }                                                                     \

#define nSUPPLY    3
#define nDEMAND    4

/* Number of constraints */
#define nCons      nSUPPLY + nDEMAND

/* Number of variables */
#define nVars      nSUPPLY * nDEMAND

/* Nonzeroes */
#define nNonz      2 * nVars


/* main entry point */
int main()
{
  APIERRORSETUP;

  int i, j, k;

/* declare an instance of the LINDO environment object */
  pLSenv pEnv;

/* declare an instance of the LINDO model object */
  pLSmodel pModel;

  int      nSolStatus;

  char     MY_LICENSE_KEY[1024];

  /*****************************************************************
  * Step 1: Create a model in the environment.
  *****************************************************************/
  nErrorCode = LSloadLicenseString("../../../license/lndapi130.lic", MY_LICENSE_KEY);
  if (nErrorCode != LSERR_NO_ERROR)
  {
    printf("Failed to load license key (error %d)\n", nErrorCode);
    exit(1);
  }

  APIVERSION;
  pEnv = LScreateEnv(&nErrorCode, MY_LICENSE_KEY);
  if (nErrorCode == LSERR_NO_VALID_LICENSE)
  {
    printf("Invalid License Key!\n");
    exit(1);
  }
  APIERRORCHECK;

/* >>> Step 2 <<< Create a model in the environment. */
  pModel = LScreateModel(pEnv, &nErrorCode);
  APIERRORCHECK;

  {
/* >>> Step 3 <<< Specify the model.


   /* The direction of optimization */
    int nDir = LS_MIN;

/* The objective's constant term */
    double dObjConst = 0.;

/* The coefficients of the objective function */
    double CAPACITY[nSUPPLY] = { 30, 25, 21 };

// demand and supply
    double DEMAND[nDEMAND] = { 15, 17, 22, 12 };

    double COST[nVars] = {      6, 2, 6, 7,
                                4, 9, 5, 3,
                                8, 8, 1, 5  };

/* The constraint types */
    char pachSense[nCons];

/* The number of nonzeros in the constraint matrix */
    int nNZ;

/* The indices of the first nonzero in each column */
    int paiBegCol[nVars + 1];

/* The length of each column.  Since we aren't leaving
    any blanks in our matrix, we can set this to NULL */
    int *panColLen = NULL;

/* The nonzero coefficients */
    double padA[nNonz];

/* The row indices of the nonzero coefficients */
    int paiRows[nNonz];

/* Simple upper and lower bounds on the variables. */
    double padLB[nVars];

    double padUB[nVars];

/* RHS */
    double padB[nCons];

// set RHS
    k = 0;
    for (k = 0, j = 0; j < nDEMAND; j++, k++)
    {
      padB[k]      = DEMAND[j];
      pachSense[k] = 'G';
    }

    for (i = 0; i < nSUPPLY; i++, k++)
    {
      padB[k]      = CAPACITY[i];
      pachSense[k] = 'L';
    }

// set paiBegCol
    paiBegCol[0] = 0;
    for (k = 1; k < nVars + 1; k++)
    {
      paiBegCol[k] = paiBegCol[k - 1] + 2;
    }

// set LB/UB and matrix data padA, paiRows
    nNZ = 0;
    for (i = 0; i < nSUPPLY; i++)
    {
      for (j = 0; j < nDEMAND; j++)
      {
        k        = i * nDEMAND + j;
        padLB[k] = 0;
        padUB[k] = LS_INFINITY;

        padA[nNZ]    = 1.0;
        paiRows[nNZ] = j;
        nNZ++;

        padA[nNZ]    = 1.0;
        paiRows[nNZ] = nDEMAND + i;
        nNZ++;
      }
    }


/* We have now assembled a full description of the model.
    We pass this information to LSloadLPData with the
    following call. */
    nErrorCode = LSloadLPData(pModel, nCons, nVars, nDir,
                              dObjConst, COST, padB, pachSense, nNonz, paiBegCol,
                              panColLen, padA, paiRows, padLB, padUB);
    APIERRORCHECK;

    nErrorCode = LSwriteLINDOFile(pModel, "ex_trans.ltx");
    APIERRORCHECK;
    nErrorCode = LSwriteMPSFile(pModel, "ex_trans.mps", LS_UNFORMATTED_MPS);
    APIERRORCHECK;
    nErrorCode = LSwriteLINGOFile(pModel, "ex_trans.lng");
    APIERRORCHECK;

  }

  /* >>> Step 4 <<< Perform the optimization */
  nErrorCode = LSoptimize(pModel,
                          LS_METHOD_PSIMPLEX, &nSolStatus);
  APIERRORCHECK;

  if (nSolStatus == LS_STATUS_OPTIMAL ||
      nSolStatus == LS_STATUS_BASIC_OPTIMAL)
  {
    /* >>> Step 5 <<< Retrieve the solution */
    int    i;
    double padPrimals[ nVars], dObj;
    double padDuals[ nCons];
    double padRedcosts[ nVars];
    double padSlacks[ nCons];
    int    panCstatus[nVars], panRstatus[nCons];

    /* Get the value of the objective */
    nErrorCode = LSgetInfo(pModel, LS_DINFO_POBJ, &dObj);
    APIERRORCHECK;
    printf("Objective Value = %g\n", dObj);

    /* Get the variable values */
    nErrorCode = LSgetPrimalSolution(pModel, padPrimals);
    APIERRORCHECK;
    nErrorCode = LSgetDualSolution(pModel, padDuals);
    APIERRORCHECK;
    nErrorCode = LSgetSlacks(pModel, padSlacks);
    APIERRORCHECK;
    nErrorCode = LSgetReducedCosts(pModel, padRedcosts);
    APIERRORCHECK;

    /* To retrieve the basis */
    nErrorCode = LSgetBasis(pModel, panCstatus, panRstatus);
    APIERRORCHECK;

    if (1)
    {
      printf("\n%8s %12s %12s %12s\n",
             "ColIndex", "Primal", "ReducedCost", "ColStatus");
      for (i = 0; i < nVars; i++)
      {
        printf("%8d %12.3f %12.3f %12d\n",
               i, padPrimals[i], padRedcosts[i], panCstatus[i]);
      }

      printf("\n%8s %12s %12s %12s\n",
             "RowIndex", "Dual", "Slack", "RowStatus");
      for (i = 0; i < nCons; i++)
      {
        printf("%8d %12.3f %12.3f %12d\n",
               i, padDuals[i], padSlacks[i], panRstatus[i]);
      }
    }

  }
  else
  {
    /* see include\lindo.h for status definitions */
    printf("Optimal solution was not"
           " found -- status: %d\n", nSolStatus);
  }

  /* >>> Step 6 <<< Delete the LINDO environment */
  nErrorCode = LSdeleteModel(&pModel);
  nErrorCode = LSdeleteEnv(&pEnv);

  /* Wait until user presses the Enter key */
  printf("Press <Enter> ...");
  getchar();
}

