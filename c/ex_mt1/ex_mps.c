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

  File   : ex_mps.c

  Purpose: Read a model from an MPS file and optimize.
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>


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
         printf("\nErrorcode=%d:  %s\n", nErrorCode, \
          cErrorMessage); \
      } else {\
         printf( "\nFatal Error\n"); \
      } \
      return(nErrorCode); \
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
  int iter=0, i=0,modelType;
  static double pfeas=0.0,pobj=0.0;
  static double bestbnd;
  static int status;
  int threadID = (((ARGS *) cbData)->val);

  if (iLoc == LSLOC_MIP)
  {
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_STATUS,&status);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_SIM_ITER,&i); iter+=i;
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_BAR_ITER,&i); iter+=i;
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_NLP_ITER,&i); iter+=i;
    LSgetCallbackInfo(model,iLoc,LS_DINFO_MIP_OBJ,&pobj);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_MIP_BESTBOUND,&bestbnd);
    printf("\nThread %2d, iter=%4d best=%11.5e obj=%11.5e stat=%3d", threadID,iter,bestbnd,pobj,status);
  }

  else if ( iLoc == LSLOC_PRIMAL || iLoc ==LSLOC_DUAL ||
            iLoc ==LSLOC_CONOPT  || iLoc == LSLOC_BARRIER || iLoc == LSLOC_GOP)
  {
    LSgetCallbackInfo(model,iLoc,LS_IINFO_STATUS,&status);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_PINFEAS,&pfeas);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_POBJ,&pobj);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_SIM_ITER,&i); iter+=i;
    LSgetCallbackInfo(model,iLoc,LS_IINFO_BAR_ITER,&i); iter+=i;
    LSgetCallbackInfo(model,iLoc,LS_IINFO_NLP_ITER,&i); iter+=i;
    printf("\nThread %2d, iter=%4d feas=%11.3e obj=%11.5e stat=%3d", threadID,iter,pfeas,pobj,status);
  }
  fflush(stdout);


  return 0;
} /*print_log*/


static void LS_CALLTYPE print_line_log(pLSmodel pModel, char *line, void *userdata)
{
  if (line)
  {
    printf("%s",line);
  } /*if*/
} /*print_line*/

int solve(pLSenv pEnv, char *mpsfile, int threadID, void *userData)
{
   APIERRORSETUP;
   int m, n; /* number of constraints and vars */
   int nC=0, nB=0, nI=0; /* number of cont, bin. int vars*/

   double dObj;
   int counter = 0, status;


/* declare an instance of the LINDO model object */
   pLSmodel pModel;


  /****************************************************************
   * Step 2: Create a model in the environment.
   ****************************************************************/
   pModel = LScreateModel ( pEnv, &nErrorCode);
   APIERRORCHECK;

  /****************************************************************
   * Step 3: Read the model from an MPS file and get the model size
   ****************************************************************/
   nErrorCode = LSreadMPSFile(pModel,mpsfile,LS_UNFORMATTED_MPS);
   if (nErrorCode != LSERR_NO_ERROR) {
     printf("\n Bad  MPS  format... Trying MPI format.\n");
     nErrorCode =LSreadMPIFile(pModel,mpsfile);
     if (nErrorCode != LSERR_NO_ERROR){
       printf(" Bad MPI format... Trying  LINDO  format.\n");
       nErrorCode = LSreadLINDOFile(pModel,mpsfile);
       if (nErrorCode != LSERR_NO_ERROR){
         printf(" Bad  LINDO format... Terminating...\n");
         APIERRORCHECK;
       }else{
         printf(" LINDO format OK!\n\n");
       }
     }
   } else {

   }

   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_VARS,&n);
   APIERRORCHECK;
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_CONS,&m);
   APIERRORCHECK;
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_CONT,&nC);
   APIERRORCHECK;

   /***************************************************************
    * Step 5: Optimize the model
    ***************************************************************/
   status = LS_STATUS_UNKNOWN;
   //LSsetModelDouParameter(pModel,LS_DPARAM_CALLBACKFREQ,2);
   //LSsetModelIntParameter(pModel,LS_IPARAM_MIP_TOPOPT,1);
   //LSsetModelIntParameter(pModel,LS_IPARAM_MIP_PRELEVEL,0);
   //LSsetModelIntParameter(pModel,LS_IPARAM_MIP_ITRLIM,100);
   //LSsetModelIntParameter(pModel,LS_IPARAM_NLP_ITRLMT,2000);
   //LSsetModelIntParameter(pModel,LS_IPARAM_LP_ITRLMT,1);
   LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRINTLEVEL,0);
   LSsetModelIntParameter(pModel,LS_IPARAM_NLP_PRINTLEVEL,0);
   //LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRELEVEL,0);

   if (1)
   {
     /* Install a callback function to display solver's progress
     as specified by the user */
     nErrorCode = LSsetCallback(pModel,(cbFunc_t) print_log, userData);
   }
   else
   {
     /* Install a log function to display solver's progress
     as reported by the internal solver */
     nErrorCode = LSsetModelLogfunc(pModel, (printModelLOG_t) print_line_log, userData);
   }

   if (n - nC > 0)
   {
     nErrorCode = LSsolveMIP( pModel, &status);
   }
   else
   {
     nErrorCode = LSoptimize( pModel, LS_METHOD_FREE, &status);
   }
   APIERRORCHECK;

   /***************************************************************
    * Step 6: Access the final solution if optimal or feasible
    ***************************************************************/
   if (status == LS_STATUS_OPTIMAL ||
       status == LS_STATUS_BASIC_OPTIMAL ||
       status == LS_STATUS_LOCAL_OPTIMAL ||
       status == LS_STATUS_FEASIBLE)
   {
     char   varname[255], *nameptr;
     double *primal = NULL, *dual = NULL;

     char **paszVarnames, **paszConnames, *pachConNameData,*pachVarNameData;
     char pszTitle[255], *pszObjname = NULL, *pszRhsname = NULL;
     char *pszRngname = NULL, *pszBndname = NULL;

     int nTotalVarNameLen;
     int nTotalConNameLen;
     int    j;

     primal = (double *) malloc(n*sizeof(double));
     dual   = (double *) malloc(m*sizeof(double));

     if (n - nC > 0) {
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
     if (1)
     {
       /* Allocate pointers to name data */
       paszConnames = (char **) malloc(m*sizeof(char *));
       paszVarnames = (char **) malloc(n*sizeof(char *));

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

     printf ("\n\n Done thread %d", threadID);
     printf ("\n Model Title: %s\n Objective at solution = %f \n", pszTitle, dObj);

     // uncomment the block below if you would like the primal and dual solutions
     // to be printed on the screen.
     if (0)
     {
        double *dwork=NULL;
        double *adDecRhs, *adIncRhs;
        double *adDecObj, *adIncObj;
        dwork = (double*) malloc((n+m)*2*sizeof(double));
        adDecRhs = dwork;
        adIncRhs = adDecRhs + m;
        adDecObj = adIncRhs + m;
        adIncObj = adDecObj + n;


        nErrorCode = LSgetConstraintRanges(pModel,adDecRhs,adIncRhs);
        APIERRORCHECK;

        nErrorCode = LSgetObjectiveRanges(pModel,adDecObj,adIncObj);
        APIERRORCHECK;

        printf ("\n Primal Solution\n");
        printf("\t%8s %18s %18s %18s\n","VARS", "Primal","Obj Dec","Obj Inc");
        for (j = 0; j<n; j++)
        {
          nErrorCode = LSgetVariableNamej(pModel,j,varname);
          nameptr = paszVarnames[j];
          printf("\t%8s %18.10e %18.10e %18.10e\n",nameptr, primal[j],adDecObj[j],adIncObj[j]);
        }

        printf ("\n Dual Solution\n");
        printf("\t%8s %18s %18s %18s\n","CONS", "Dual","RHS Dec","RHS Inc");
        for (j = 0; j<m; j++)
        {
          nErrorCode = LSgetConstraintNamei(pModel,j,varname);
          nameptr = paszConnames[j];
          printf("\t%8s %18.10e %18.10e %18.10e\n",nameptr, dual[j],adDecRhs[j],adIncRhs[j]);
        }

        free(dwork);
     }



     free(primal);
     free(dual);
     free(pachVarNameData);
     free(pachConNameData);
     free(paszConnames);
     free(paszVarnames);
   }
   else
   {
     char strbuf[255];
     LSgetErrorMessage(pEnv,nErrorCode,strbuf);
     printf ("\n Optimization failed. Status = %d ",status);
     printf ("\n Error %d: %s\n",nErrorCode,strbuf);
   }

   /***************************************************************
    * Step 7: Delete model
    ***************************************************************/
   nErrorCode = LSdeleteModel( &pModel);

   return nErrorCode;

}
