/*
###################################################################
#                       LINDO-API
#                    Sample Programs
#
#                  Copyright (c) 2011
#
#         LINDO Systems, Inc.           312.988.7422
#         1415 North Dayton St.         info@lindo.com
#         Chicago, IL 60622             http://www.lindo.com
###################################################################

  File   : ex_iis.c

  Purpose: Analyze an infeasible (unbounded) LP to isolate the
  constraints (variables) causing the infeasibility (unboundedness)
  of the model.
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

int CALLBACKTYPE MyCallback( pLSmodel pMod, int nLocation,
 void* pMyData)
{
   int *status = (int *) pMyData;

/* Display current iteration count and objective value */
   {
      int nIter,nNec=0,nSuf=0,
         nErr,nErr1,nErr2;
      double dObj, dInf;
      nErr=LSgetCallbackInfo(pMod,nLocation,LS_IINFO_SIM_ITER,&nIter);
      nErr=LSgetCallbackInfo(pMod,nLocation,LS_DINFO_POBJ,&dObj);
      nErr=LSgetCallbackInfo(pMod,nLocation,LS_DINFO_PINFEAS,&dInf);
      if (status && *status == LS_STATUS_INFEASIBLE)
      {
        nErr1=LSgetCallbackInfo(pMod,nLocation,LS_IINFO_NUM_IIS_ROWS,&nNec);
        nErr2=LSgetCallbackInfo(pMod,nLocation,LS_IINFO_NUM_SUF_ROWS,&nSuf);
      }
      else if (status && *status == LS_STATUS_UNBOUNDED)
      {
        nErr1=LSgetCallbackInfo(pMod,nLocation,LS_IINFO_NUM_IUS_COLS,&nNec);
        nErr2=LSgetCallbackInfo(pMod,nLocation,LS_IINFO_NUM_SUF_COLS,&nSuf);
      }

      printf( "@MyCallback %8d, %8d, %16g, %16g, %8d (%d)\n",
        nLocation,nIter,dObj,dInf,nNec,nSuf);

   }

   return( 0);
}

static void LS_CALLTYPE print_line_log(pLSmodel pModel, char *line, void *userdata)
{
  if (line)
  {
    printf("\n%s",line);
  } /*if*/
} /*print_line*/

 /* main entry point */
int main(int argc, char **argv)
{
   APIERRORSETUP;
   /* model data objects */
   int n; /* number of variables */
   int m; /* number of constraints */
   int solstatus;/*solution status (see lindo.h for possible values)*/
   int prep_level;
   char *mpsfile = NULL;
   char MY_LICENSE_KEY[1024];

   /* IIS related data objects */
   int nLevel,   /* level of analysis */
       nSuf_r,   /* number of sufficient rows     */
       nSuf_c,   /* number of sufficient columns  */
       nIIS_r,   /* number of rows in the IIS     */
       nIIS_c;   /* number of columns in the IIS  */
   int *aiRows = NULL, /* index set of rows in the IIS     */
       *aiCols = NULL, /* index set of columns in the IIS  */
       *anBnds = NULL; /* bound type of columns in the IIS */
   int j;
   char bndtype[255], oufname[255], varname[255];

   /* declare an instance of the LINDO environment object */
   pLSenv pEnv=NULL;
   /* declare an instance of the LINDO model object */
   pLSmodel pModel;
   /***************************************************************
    * Init: Command prompt calling sequence
    ***************************************************************/
   {
     char szVer[255], szBld[255];
     LSgetVersionInfo(szVer,szBld);
     printf("\nAN APPLICATION FOR ANALYZING & DEBUGGING LPs\n");
     printf("\nusing LINDO API Version %s (Built %s)\n\n",szVer,szBld);

   }
   if (argc == 1)
   {
     printf("\nUsage: ex_iis filename\n\n");
     goto Terminate;
   }
   else if (argc == 2)
   {
     mpsfile = argv[1];
   }
  /*****************************************************************
   * Step 1: Create a LINDO environment.
   *****************************************************************/
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

   /***************************************************************
    * Step 2: Create a model in the environment.
    ***************************************************************/
   pModel = LScreateModel ( pEnv, &nErrorCode);
   APIERRORCHECK;

   /***************************************************************
    * Step 3: Read the model from an MPS file and
    ***************************************************************/
   nErrorCode = LSreadMPSFile(pModel,mpsfile,LS_UNFORMATTED_MPS);
   if (nErrorCode != LSERR_NO_ERROR)
   {
     printf("\nBad MPS format... Trying LINDO format.\n");
     nErrorCode =LSreadLINDOFile(pModel,mpsfile);
     APIERRORCHECK;
     printf("\nLINDO format OK!\n\n");
   }
   else
   {
     printf("\nMPS format OK!\n\n");
   }
   nErrorCode = LSgetInfo(pModel, LS_IINFO_NUM_VARS, &n);
   APIERRORCHECK;

   nErrorCode = LSgetInfo(pModel, LS_IINFO_NUM_CONS, &m);
   APIERRORCHECK;

   /***************************************************************
    * Step 4: Set Model parameters
    ***************************************************************/
   /* Turn off the LP preprocessor. This is required if the model
   is infeasible and the user wishes to debug it. */
   nErrorCode = LSgetModelIntParameter(pModel,LS_IPARAM_LP_PRELEVEL,
     &prep_level);
   APIERRORCHECK;
   if (prep_level > 0)
     printf("The LP presolver has been turned off. Solving ...\n\n");

   nErrorCode = LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRELEVEL,0);

   /* set LP solver type for optimizations (cold start) */
   nErrorCode = LSsetModelIntParameter(pModel,LS_IPARAM_IIS_TOPOPT,
     LS_METHOD_FREE);

   /* set LP solver type for reoptimizations (warm start)*/
   nErrorCode = LSsetModelIntParameter(pModel,LS_IPARAM_IIS_REOPT,
     LS_METHOD_FREE);

#if 0
   nErrorCode = LSsetCallback( pModel,(cbFunc_t) MyCallback, NULL);
   APIERRORCHECK;
   printf( "            %8s, %8s, %16s, %16s, %8s (%s)\n",
        "LOCATION","ITERS","OBJECTIVE","INFEASIBILITY","NNEC","NSUF");
#endif

   /* Install a log function to display solver's progress
   as reported by the internal solver */
   nErrorCode = LSsetModelLogfunc(pModel, (printModelLOG_t) print_line_log, NULL);

   nErrorCode = LSsetModelDouParameter(pModel,LS_DPARAM_CALLBACKFREQ,0.5);
   APIERRORCHECK;

   /***************************************************************
    * Step 5: Optimize the model
    ***************************************************************/
   nErrorCode = LSoptimize( pModel,LS_METHOD_FREE, &solstatus);
   APIERRORCHECK;

#if 0
   /* set callback and solution status */
   nErrorCode = LSsetCallback( pModel,(cbFunc_t) MyCallback, &solstatus);
#endif

    if (solstatus == LS_STATUS_BASIC_OPTIMAL)
    {
      printf("\tThe model is solved to optimality.\n");
    }
   /***************************************************************
    * Step 6: Debug the model if unbounded or infeasible
    ***************************************************************/
    else if (solstatus == LS_STATUS_UNBOUNDED)
    {
      APIERRORCHECK;
      printf("\nThe model is unbounded.. Analyzing...\n\n");
      nLevel = LS_NECESSARY_COLS + LS_SUFFICIENT_COLS;

      /*** Step 6.1: Find IIS ***/
      nErrorCode = LSfindIUS(pModel,nLevel);
      APIERRORCHECK;

      strcpy(oufname,"findius.ltx");
      nErrorCode = LSwriteIUS(pModel,oufname);
      printf("\n\n IUS is written to %s !!\n",oufname);
    }
    else if (solstatus == LS_STATUS_INFEASIBLE)
    {
      printf("\nThe model is infeasible.. Analyzing...\n\n");
      aiRows = (int *) malloc(m*sizeof(int));
      aiCols = (int *) malloc(n*sizeof(int));
      anBnds = (int *) malloc(n*sizeof(int));

      /*** Step 6.1: Find IIS ***/
      nLevel = LS_NECESSARY_ROWS + LS_SUFFICIENT_ROWS;

      nErrorCode = LSfindIIS(pModel,nLevel);
      APIERRORCHECK;

      nErrorCode = LSgetIIS(pModel,&nSuf_r,&nIIS_r,aiRows,
                                   &nSuf_c,&nIIS_c,aiCols,anBnds);
      APIERRORCHECK;
      printf("\n\t ***  LSfindIIS Summary ***\n\n");
      printf("\t Number of Sufficient Rows = %u\n",nSuf_r);
      printf("\t Number of Sufficient Cols = %u\n",nSuf_c);
      printf("\t Number of Necessary  Rows = %u\n",nIIS_r - nSuf_r);
      printf("\t Number of Necessary  Cols = %u\n",nIIS_c - nSuf_c);
      printf("\n");

      /*** Step 6.2: Display row index sets ***/
      printf("\n IIS Rows\n");
      for (j=0; j<nIIS_r; j++)
      {
        nErrorCode = LSgetConstraintNamei(pModel,aiRows[j],varname);
        APIERRORCHECK;

        if (j<nSuf_r)
          printf("%2d] (%-8s) is"
          " in the sufficient set.\n",j,varname);
        else
          printf("%2d] (%-8s) is"
          " in the necessary set.\n",j,varname);
      }

      /*** Step 6.3: Display column index sets ***/
      printf("\n IIS Column Bounds\n");
      for (j=0; j<nIIS_c; j++)
      {
        if (anBnds[j] < 0)
          strcpy(bndtype,"Lower");
        else
          strcpy(bndtype,"Upper");

        nErrorCode = LSgetVariableNamej(pModel,aiCols[j],varname);
        APIERRORCHECK;
        if (j<nSuf_c)
          printf("%2d] %s bound of (%-8s) is"
          " in the sufficient set.\n",j,bndtype,varname);
        else
          printf("%2d] %s bound of (%-8s) is"
          " in the necessary set.\n",j,bndtype,varname);
      }

      strcpy(oufname,"findiis.ltx");
      LSwriteIIS(pModel,oufname);
      printf("\n\n IIS is written to %s !!\n",oufname);

      free(aiRows);
      free(aiCols);
      free(anBnds);
    }

Terminate:
 /*****************************************************************
  * Step 7: Terminate
  *****************************************************************/
   nErrorCode = LSdeleteModel( &pModel);

   nErrorCode = LSdeleteEnv( &pEnv);

  /* Wait until user presses the Enter key */
   printf("Press <Enter> ...");
   getchar();


}
