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

  File   : ex_iostream.c

  Purpose: Input a model from a char stream and optimize.
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

/* main entry point */

/****************************************************************
   Standard callback function to display local and intermediate
   solutions
 ****************************************************************/
int  LS_CALLTYPE print_log(pLSmodel model,int iLoc, void *cbData)
{
  static int iter=0;
  static double pfeas=0.0,pobj=0.0;
  static double bestbnd;
  static int nStatus;

  if (iLoc == LSLOC_MIP)
  {
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_STATUS,&nStatus);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_SIM_ITER,&iter);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_MIP_OBJ,&pobj);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_MIP_BESTBOUND,&bestbnd);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_PINFEAS,&pfeas);
    printf("%2d\tIt=%4d \tInfeas= %8.3e\tObj=%11.5e \tBestBound=%11.5e \tStat=%d\n",
      iLoc,iter,pfeas,pobj,bestbnd,nStatus);
  }

  else if ( iLoc == LSLOC_PRIMAL || iLoc ==LSLOC_DUAL ||
            iLoc ==LSLOC_CONOPT  || iLoc == LSLOC_BARRIER)
  {
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_STATUS,&nStatus);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_PINFEAS,&pfeas);
    printf("%2d\tIt=%4d \tInfeas= %8.3e\tObj=%11.5e \tBestBound=%11.5e \tStat=%d\n",
      iLoc,iter,pfeas,pobj,bestbnd,nStatus);
  }


  return 0;
} /*print_log*/


static void LS_CALLTYPE print_line_log(pLSmodel pModel, char *line, void *userdata)
{
  if (line)
  {
    printf("%s",line);
  } /*if*/
} /*print_line*/


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
  double *aDx = malloc(nVars*sizeof(double));
  int *iDx = malloc(nVars*sizeof(int)), nDx;

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
      if ( padDuals[i] > dEps && panRstatus[ i] == LS_BASTYPE_ATLO )
      {
        iEnter = i;
        break;
      }
    }
    else
    {
      if ( padDuals[i] < -dEps && panRstatus[ i] == LS_BASTYPE_ATLO )
      {
        iEnter = i;
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
      adArtif[0] = -1;

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


int main(int argc, char **argv)
{
   APIERRORSETUP;
   int nCons, nVars, nNonz;      /* number of constraints and vars and nonzs */
   int nCont=0, nBin=0, nGin=0;  /* number of cont, bin. int vars*/

   /* Remarks:
   1 - Avoid 'E' or 'e' as the first character in variable/constraint names. This
   char is reserved for the exponent operator as in 1e3.
   2 - Similarly, avoid to use  '.' as the first character in names. This is the
   decimal separator.
   3 - Terminate each line with the '\n' char. Max line length is 1022 chars.
   */
   char ltxStream[] = "Min 3x1 + 2x2         \n"
                     "s.t.                  \n"
                     "R1) 3x1 + x2 >= 3     \n"
                     "R2) 4x1 + 3x2 >= 6    \n"
                     "R3) x1 + x2 <= 3      \n"
                     "END                   \n"
                     "FREE x1               \n"
                     "SLB x1 -2             \n"
                     "SLB x2 0.5            \n"
                     "GIN x1                \n",
        *prifile = NULL;

   char mpxStream[] = "min " \
     " 3.000000*V_11   +9.000000*V_12   +7.000000*V_13   + " \
     "16.000000*V_21  +10.000000*V_22  +6.000000*V_23  + " \
     "2.000000*V_31   +7.000000*V_32   +11.000000*V_33;  " \
     "s.t.  " \
     "1.000000*V_11   +1.000000*V_21   +1.000000*V_31    = 1.000000; " \
     "1.000000*V_12   +1.000000*V_22   +1.000000*V_32    = 1.000000; " \
     "1.000000*V_13   +1.000000*V_23   +1.000000*V_33    = 1.000000; " \
     "1.000000*V_11   +1.000000*V_12   +1.000000*V_13    = 1.000000; " \
     "1.000000*V_21   +1.000000*V_22   +1.000000*V_23    = 1.000000; " \
     "1.000000*V_31   +1.000000*V_32   +1.000000*V_33    = 1.000000; " \
     "BINARY  " \
     " V_11;  V_12;  V_13;  V_21;  V_22;  V_23;  V_31;  V_32;  V_33; " \
     "End ";

   double dObj;
   int counter = 0, nStatus, optErrorCode;

/* declare an instance of the LINDO environment object */
   pLSenv pEnv = NULL;
/* declare an instance of the LINDO model object */
   pLSmodel pModel;

   char MY_LICENSE_KEY[1024];


  /****************************************************************
   * Step 1: Create a LINDO environment.
   ****************************************************************/
   nErrorCode = LSloadLicenseString("../../../license/lndapi140.lic",MY_LICENSE_KEY);
   if ( nErrorCode != LSERR_NO_ERROR)
   {
      printf( "Failed to load license key (error %d)\n",nErrorCode);
      exit( 1);
   }

   APIVERSION;
   pEnv = LScreateEnv ( &nErrorCode, MY_LICENSE_KEY);
   if ( nErrorCode == LSERR_NO_VALID_LICENSE) {
      printf( "Invalid License Key!\n");
      exit( 1);
   }
   APIERRORCHECK;

  /****************************************************************
   * Step 2: Create a model in the environment.
   ****************************************************************/
   pModel = LScreateModel ( pEnv, &nErrorCode);
   APIERRORCHECK;

  /****************************************************************
   * Step 3: Read the model from an MPS file and get the model size
   ****************************************************************/
   if (2>1) {
     // solve the ltxStream model
     nErrorCode = LSreadLINDOStream(pModel,ltxStream,strlen(ltxStream));
   } else {
     // solve the mpxStream model
     nErrorCode = LSreadMPXStream(pModel,mpxStream,strlen(mpxStream));
   }
   APIERRORCHECK;

   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_VARS,&nVars);
   APIERRORCHECK;
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_CONS,&nCons);
   APIERRORCHECK;
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_NONZ,&nNonz);
   APIERRORCHECK;
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_CONT,&nCont);
   APIERRORCHECK;
   /***************************************************************
    * Step 4: Read the priority file, if any exists.
    ***************************************************************/
   if (prifile) {
     nErrorCode = LSreadVarPriorities(pModel, prifile);
     APIERRORCHECK;
   }

   /***************************************************************
    * Step 5: Optimize the model
    ***************************************************************/
   nStatus = LS_STATUS_UNKNOWN;
   LSsetModelDouParameter(pModel,LS_DPARAM_CALLBACKFREQ,2);
   //LSsetModelIntParameter(pModel,LS_IPARAM_MIP_TOPOPT,1);
   //LSsetModelIntParameter(pModel,LS_IPARAM_MIP_PRELEVEL,0);
   //LSsetModelIntParameter(pModel,LS_IPARAM_MIP_ITRLIM,100);
   //LSsetModelIntParameter(pModel,LS_IPARAM_NLP_ITRLMT,2000);
   //LSsetModelIntParameter(pModel,LS_IPARAM_LP_ITRLMT,2000);
   LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRINTLEVEL,2);

   /* Install a log function to display solver's progress
   as reported by the internal solver */
   nErrorCode = LSsetModelLogfunc(pModel, (printModelLOG_t) print_line_log, NULL);

   /* Install a callback function to display solver's progress
   as specified by the user */
   //nErrorCode = LSsetCallback(pModel,(cbFunc_t) print_log, NULL);

   if (nVars - nCont > 0)
   {
     LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRINTLEVEL,0);
     optErrorCode = LSsolveMIP( pModel, &nStatus);
   }
   else
   {
     optErrorCode = LSoptimize( pModel, LS_METHOD_FREE, &nStatus);
   }
   nErrorCode = optErrorCode;
   APIERRORCHECK;

   /***************************************************************
    * Step 6: Access the final solution if optimal or feasible
    ***************************************************************/
   if (nStatus == LS_STATUS_OPTIMAL ||
       nStatus == LS_STATUS_BASIC_OPTIMAL ||
       nStatus == LS_STATUS_LOCAL_OPTIMAL ||
       nStatus == LS_STATUS_FEASIBLE)
   {
     char   varname[255], *nameptr;
     double *primal = NULL, *dual = NULL;

     char **paszVarnames, **paszConnames, *pachConNameData,*pachVarNameData;
     char pszTitle[255], *pszObjname = NULL, *pszRhsname = NULL;
     char *pszRngname = NULL, *pszBndname = NULL;

     int nTotalVarNameLen;
     int nTotalConNameLen;
     int    j;

     primal = (double *) malloc(nVars*sizeof(double));
     dual   = (double *) malloc(nCons*sizeof(double));

     if (nVars - nCont > 0) {
       nErrorCode = LSgetInfo(pModel,LS_DINFO_MIP_OBJ,&dObj);
       APIERRORCHECK;
       nErrorCode = LSgetMIPDualSolution( pModel,dual);
       APIERRORCHECK;
       nErrorCode = LSgetMIPPrimalSolution( pModel,primal);
       APIERRORCHECK;
     } else {
       nErrorCode = LSgetPrimalSolution( pModel, primal) ;
       APIERRORCHECK;
       nErrorCode = LSgetDualSolution( pModel, dual) ;
       APIERRORCHECK;
       nErrorCode = LSgetInfo(pModel,LS_DINFO_POBJ,&dObj);
       APIERRORCHECK;
     }

     /* Retrieve variable and constraint names */
     {
       /* Allocate pointers to name data */
       paszConnames = (char **) malloc(nCons*sizeof(char *));
       paszVarnames = (char **) malloc(nVars*sizeof(char *));

       /* Get the total number of characters in variable name data. */
       LSgetInfo(pModel, LS_IINFO_LEN_VARNAMES, &nTotalVarNameLen);
       pachVarNameData = (char *) malloc(sizeof(char)*nTotalVarNameLen);

       /* Get the total number of characters in constraint name data */
       LSgetInfo(pModel, LS_IINFO_LEN_CONNAMES, &nTotalConNameLen);
       pachConNameData = (char *) malloc(sizeof(char)*nTotalConNameLen);

       /* Get the name data */
       LSgetNameData( pModel, pszTitle, pszObjname, pszRhsname, pszRngname,
         pszBndname, paszConnames,  pachConNameData , paszVarnames, pachVarNameData);
     }

     printf ("\n Model Title: %s\n Objective at solution = %f (nStatus=%d, nErr=%d)\n",
       pszTitle, dObj,nStatus, optErrorCode);


     /* Print solution */
     if (1)
     {
        double *adDecRhs=NULL, *adIncRhs=NULL;
        double *adDecObj=NULL, *adIncObj=NULL;

        adDecRhs = (double *) calloc(nCons,sizeof(double));
        adDecObj = (double *) calloc(nVars,sizeof(double));
        adIncRhs = (double *) calloc(nCons,sizeof(double));
        adIncObj = (double *) calloc(nVars,sizeof(double));


        /* If an LP, do sensitivity analysis. */
        if (nVars - nCont == 0)
        {
          nErrorCode = LSgetConstraintRanges(pModel,adDecRhs,adIncRhs);
          APIERRORCHECK;

          nErrorCode = LSgetObjectiveRanges(pModel,adDecObj,adIncObj);
          APIERRORCHECK;
        }

        printf ("\n Primal Solution\n");
        printf("\t%8s %18s %18s %18s\n","VARS", "Primal","Obj Dec","Obj Inc");
        for (j = 0; j<nVars; j++)
        {
          nErrorCode = LSgetVariableNamej(pModel,j,varname);
          nameptr = paszVarnames[j];
          printf("\t%8s %18.10e %18.10e %18.10e\n",nameptr, primal[j],adDecObj[j],adIncObj[j]);
        }

        printf ("\n Dual Solution\n");
        printf("\t%8s %18s %18s %18s\n","CONS", "Dual","RHS Dec","RHS Inc");
        for (j = 0; j<nCons; j++)
        {
          nErrorCode = LSgetConstraintNamei(pModel,j,varname);
          nameptr = paszConnames[j];
          printf("\t%8s %18.10e %18.10e %18.10e\n",nameptr, dual[j],adDecRhs[j],adIncRhs[j]);
        }

        free(adDecRhs); free(adDecObj);
        free(adIncRhs); free(adIncObj);
     }



     free(primal);
     free(dual);
     free(pachVarNameData);
     free(pachConNameData);
     free(paszConnames);
     free(paszVarnames);
   }
   else if (nStatus == LS_STATUS_UNBOUNDED || nStatus == LS_STATUS_INFORUNB)
   {
       int     pdObjSense;
       double  pdObjConst;
       double  *padC = malloc(sizeof(double)*nVars);
       double  *padB = malloc(sizeof(double)*nCons);
       char    *pachConTypes = malloc(sizeof(char)*nCons);
       int     *paiAcols = malloc(sizeof(int)*nVars+1);
       int     *pacAcols = NULL;
       double  *padAcoef = malloc(sizeof(double)*nNonz);
       int     *paiArows = malloc(sizeof(int)*nNonz);
       double  *padL = NULL;
       double  *padU = NULL;

       LSgetLPData(   pModel,
                             &pdObjSense,
                             &pdObjConst,
                             padC,
                             padB,
                             pachConTypes,
                             paiAcols,
                             pacAcols,
                             padAcoef,
                             paiArows,
                             padL,
                             padU);
       LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRELEVEL,0);
       LSsetModelIntParameter(pModel,LS_IPARAM_SOLVER_IUSOL,1);

       nErrorCode = LSoptimize( pModel, LS_METHOD_PSIMPLEX, &nStatus);
       if (nStatus == LS_STATUS_UNBOUNDED)
         findXray(pEnv,pModel,nVars,nCons,pdObjSense,paiAcols,paiArows,padAcoef,pachConTypes);

   }
   else
   {
     char strbuf[255];
     LSgetErrorMessage(pEnv,nErrorCode,strbuf);
     printf ("\n Optimization failed. Status = %d ",nStatus);
     printf ("\n Error %d: %s\n",nErrorCode,strbuf);
   }
Terminate:
   /***************************************************************
    * Step 7: Terminate
    ***************************************************************/
   nErrorCode = LSdeleteModel( &pModel);
   nErrorCode = LSdeleteEnv( &pEnv);


  /* Wait until user presses the Enter key */
   printf("Press <Enter> ...");
   getchar();

}
