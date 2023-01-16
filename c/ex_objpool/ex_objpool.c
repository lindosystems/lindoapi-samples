/*
###################################################################
#                       LINDO-API
#                    Sample Programs
#                  Copyright (c) 2001-2018
#
#         LINDO Systems, Inc.           312.988.7422
#         1415 North Dayton St.         info@lindo.com
#         Chicago, IL 60622             http://www.lindo.com
###################################################################

  File   : ex_bilinear.c

  Purpose: Read a base model from an MPS file and optimize with an objective pool
   where multiple objectives are created randomly.
*/
#ifdef WIN32
#ifndef _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_WARNINGS
#endif
#endif

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
  static int status, iobj;

#if 1
  if (iLoc == LSLOC_MIP)
  {
    LSgetCallbackInfo(model,iLoc,LS_IINFO_OBJIDX,&iobj);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_STATUS,&status);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_SIM_ITER,&iter);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_MIP_OBJ,&pobj);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_MIP_BESTBOUND,&bestbnd);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_PINFEAS,&pfeas);
    printf("%2d(%d)\tIt=%4d \tInfeas= %8.3e\tObj=%11.5e \tBestBound=%11.5e \tStat=%d\n",
      iLoc,iobj,iter,pfeas,pobj,bestbnd,status);
  }

  else if ( iLoc == LSLOC_PRIMAL || iLoc ==LSLOC_DUAL ||
            iLoc ==LSLOC_CONOPT  || iLoc == LSLOC_BARRIER)
  {
    LSgetCallbackInfo(model,iLoc,LS_IINFO_OBJIDX,&iobj);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_STATUS,&status);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_PINFEAS,&pfeas);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_SIM_ITER,&iter);
    printf("%2d(%d)\tIt=%4d \tInfeas= %8.3e\tObj=%11.5e \tStat=%d\n",
      iLoc,iobj,iter,pfeas,pobj,status);
  } else 
#endif
  if (iLoc == LSLOC_OBJPOOL) {
    LSgetCallbackInfo(model,iLoc,LS_IINFO_OBJIDX,&iobj);
    printf("\nOptimizing objective #%d",iobj);
#if 0 
    if (iobj==2) { 
      //break at this obj (testing only)
      return 1;
    }
#endif
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

/**
 * @brief Display resident solution
 * @remark None
 */
int displaySolution(pLSenv pEnv, pLSmodel pModel, char *szTag, int nVars, int nCont, char *pszTitle,
                     char **paszVarnames, double *primal, double *dual, double dObj, int status)
{
  APIERRORSETUP;
  int j;
  double siter,biter,niter;
  if (nVars - nCont > 0)
  {
    nErrorCode = LSgetMIPPrimalSolution( pModel, primal) ;
    APIERRORCHECK;
    //nErrorCode = LSgetMIPDualSolution( pModel, dual) ;
    APIERRORCHECK;
    nErrorCode = LSgetInfo(pModel,LS_DINFO_MIPOBJ,&dObj);
    APIERRORCHECK;
    nErrorCode = LSgetInfo(pModel,LS_DINFO_MIP_SIM_ITER,&siter);
    APIERRORCHECK;
    nErrorCode = LSgetInfo(pModel,LS_DINFO_MIP_BAR_ITER,&biter);
    APIERRORCHECK;
    nErrorCode = LSgetInfo(pModel,LS_DINFO_MIP_NLP_ITER,&niter);
    APIERRORCHECK;
  } else  {
    nErrorCode = LSgetPrimalSolution( pModel, primal) ;
    APIERRORCHECK;
    nErrorCode = LSgetDualSolution( pModel, dual) ;
    APIERRORCHECK;
    nErrorCode = LSgetInfo(pModel,LS_DINFO_POBJ,&dObj);
    APIERRORCHECK;
    nErrorCode = LSgetInfo(pModel,LS_DINFO_SIM_ITER,&siter);
    APIERRORCHECK;
    nErrorCode = LSgetInfo(pModel,LS_DINFO_BAR_ITER,&biter);
    APIERRORCHECK;
    nErrorCode = LSgetInfo(pModel,LS_DINFO_NLP_ITER,&niter);
    APIERRORCHECK;

  }

  printf ("\n Primal Solution\n");
  printf("\t%8s %18s\n","VARS", "Primal");
  for (j = 0; j<nVars; j++)
  {
    printf("\t%8s %18.10e\n",paszVarnames[j], primal[j]);
  }
  printf ("\n Model Title: %s\n Objective at solution = %f (iter:%g, status:%d)\n", pszTitle, dObj, siter+biter+niter,status);
Terminate:
  return nErrorCode;
}

int main(int argc, char **argv)
{
   APIERRORSETUP;
   int nCons, nVars; /* number of constraints and vars */
   int nCont=0, nB=0, nI=0; /* number of cont, bin. int vars*/
   int nTotalVarNameLen;
   int nTotalConNameLen;
   char *mpsfile, *basfile = NULL;
   double dObj;
   int counter = 0, status, nObj=0;
   char strbuf[255];

/* declare an instance of the LINDO environment object */
   pLSenv pEnv = NULL;
/* declare an instance of the LINDO model object */
   pLSmodel pModel=NULL;
   pLSrandGen pRG=NULL;
   char MY_LICENSE_KEY[1024];
   double *padC=NULL;

   double *primal = NULL, *dual = NULL;

   char **paszVarnames=NULL, **paszConnames=NULL, *pachConNameData=NULL,*pachVarNameData=NULL;
   char pszTitle[255], *pszObjname = NULL, *pszRhsname = NULL;
   char *pszRngname = NULL, *pszBndname = NULL;

  /****************************************************************
   * Init: Command prompt calling sequence
   ****************************************************************/
   if (argc == 1) {
     printf("\nUsage: ex_mps filename [basfilename]\n\n");
     goto Terminate;
   } else if (argc == 2) {
     mpsfile = argv[1];
   } else if(argc == 3) {
     mpsfile = argv[1];
     basfile = argv[2];
   }

  /****************************************************************
   * Step 1: Create a LINDO environment.
   ****************************************************************/
   nErrorCode = LSloadLicenseString("../../../license/lndapi140.lic",MY_LICENSE_KEY);
   APIERRORCHECK;
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

   LSsetModelIntParameter(pModel,LS_IPARAM_INSTRUCT_SUBOUT,1);

  /****************************************************************
   * Step 3: Read the model from an MPS file and get the model size
   ****************************************************************/
   nErrorCode = LSreadMPSFile(pModel,mpsfile,LS_UNFORMATTED_MPS);
   if (nErrorCode != LSERR_NO_ERROR) {
     printf("\n Bad  MPS  format... Trying LINDO format.\n");
     nErrorCode =LSreadLINDOFile(pModel,mpsfile);
     if (nErrorCode != LSERR_NO_ERROR){
       printf(" Bad LINDO format... Trying  MPI  format.\n");
       nErrorCode = LSreadMPIFile(pModel,mpsfile);
       if (nErrorCode != LSERR_NO_ERROR){
         printf(" Bad  MPI  format... Terminating...\n");
         APIERRORCHECK;
       }else{
         printf(" MPI format OK!\n\n");
       }
     }else
     {
       printf(" LINDO format OK!. Exporting in MPS format...\n\n");
       {
         char strbuf[255], *ptr;
         strcpy(strbuf,mpsfile);
         ptr = strbuf + strlen(strbuf);
         while (*ptr != '.') ptr--;
         strcpy(ptr,".mps");
         nErrorCode = LSwriteMPSFile(pModel,strbuf,LS_FORMATTED_MPS);
       }
     }
   } else if (0) {
     printf(" MPS format OK!. Exporting in LINDO format...\n\n");
     {
       char strbuf[255], *ptr;
       strcpy(strbuf,mpsfile);
       ptr = strbuf + strlen(strbuf);
       while (*ptr != '.') ptr--;
       strcpy(ptr,".ltx");
       nErrorCode = LSwriteLINDOFile(pModel,strbuf);
     }
   }

   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_VARS,&nVars);
   APIERRORCHECK;
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_NLP_CONS,&nCons);
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_CONS,&nCons);
   APIERRORCHECK;
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_CONT,&nCont);
   APIERRORCHECK;
   /***************************************************************
    * Step 4: Read the priority file, if any exists.
    ***************************************************************/
   if (basfile) {
     LSreadBasis(pModel,basfile,LS_BASFILE_BIN);
     LSwriteBasis(pModel,"init.bas",LS_BASFILE_TXT);
     LSreadModelParameter(pModel,"lindo.par");
     APIERRORCHECK;
   }   

   /***************************************************************
    * Step 5: Setup obj pool
    ***************************************************************/
   // generate random objective functions
   {
     int i=0, jRank=0;
     char szObjName[33];
     double u;
     pRG = LScreateRG(pEnv, LS_RANDGEN_FREE);
     padC = (double *) malloc(nVars*sizeof(double));

     LSsetRGSeed(pRG,10001);

     jRank=1;
     while (jRank<4) {
       for (i=0; i<nVars; i++) {
         u = LSgetDoubleRV(pRG);
         if (u<0.5) {
           padC[i] = 0;
         } else {
           padC[i] = (double) LSgetInt32RV(pRG,1,100);
         }
       }//for
       sprintf(szObjName,"OBJ%d",jRank);
       nErrorCode = LSaddObjPool(pModel,padC,LS_MIN,jRank,1e-6);
       fprintf(stdout,"\nAdded '%s' objective function, rank:%d, reltol:%g",szObjName,jRank,0.);
       APIERRORCHECK;
       jRank++;
     }//while
     LSdisposeRG(&pRG);
     free(padC);
     padC=NULL;
   }
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_OBJPOOL,&nObj); // get number of objs
   APIERRORCHECK;

   LSwriteMPSFile(pModel,"lex.mps",0);

   /* Retrieve variable and constraint names */
   if (1) {
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

   // solution access vectors
   primal = (double *) malloc(2*nVars*sizeof(double));
   dual   = (double *) malloc(nCons*sizeof(double));


   /***************************************************************
    * Step 5b: Optimize the model
    ***************************************************************/
   status = LS_STATUS_UNKNOWN;
   //LSsetModelDouParameter(pModel,LS_DPARAM_CALLBACKFREQ,0);
   //LSsetModelIntParameter(pModel,LS_IPARAM_MIP_TOPOPT,1);
   //LSsetModelIntParameter(pModel,LS_IPARAM_MIP_PRELEVEL,0);
   //LSsetModelIntParameter(pModel,LS_IPARAM_MIP_ITRLIM,100);
   //LSsetModelIntParameter(pModel,LS_IPARAM_LP_SCALE,0);
   //LSsetModelIntParameter(pModel,LS_IPARAM_SPLEX_ITRLMT,2000);
   //LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRINTLEVEL,2);
   //LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRTFG,2);
   //LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRELEVEL,0);
   //LSsetModelIntParameter(pModel,LS_IPARAM_MIP_PRINTLEVEL,0);

   /* Install a log function to display solver's progress
   as reported by the internal solver */
   nErrorCode = LSsetModelLogfunc(pModel, (printModelLOG_t) print_line_log, NULL);

   /* Install a callback function to display solver's progress
   as specified by the user */
   //nErrorCode = LSsetCallback(pModel,(cbFunc_t) print_log, NULL);


   { // allow atmost 'maxSolsPerObj' many solutions per objective in the solution pool
     int maxSolsPerObj=1;
     LSsetModelIntParameter(pModel,LS_IPARAM_SOLPOOL_LIM,maxSolsPerObj);
   }
 
   printf("\nOptimizing ..");
   LSsetModelIntParameter(pModel,LS_IPARAM_MIP_PRINTLEVEL,2);
   if (nVars - nCont > 0) {
     LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRINTLEVEL,0);
     //LSsetModelDouParameter(pModel,LS_DPARAM_MIP_ITRLIM,8800);
     //LSsetModelDouParameter(pModel,LS_DPARAM_MIP_TIMLIM,3);
     nErrorCode = LSsolveMIP( pModel, &status);
   } else {
     LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRINTLEVEL,2);
     nErrorCode = LSoptimize( pModel, 0, &status);
   }
   //APIERRORCHECK;

   // Walk through the solution pool and write available solutions
   {
     int k;
     int numSols=0, iObj=0;
     printf("\n\nWalking through the solution pool and writing available solutions");
     for (iObj=0; iObj<nObj; iObj++) {
       nErrorCode = LSgetObjPoolNumSol(pModel,iObj,&numSols);
       for (k=0; k<numSols; k++) {
         nErrorCode = LSloadSolutionAt(pModel,iObj,k);
         if (nErrorCode) {
           printf("\nError %d: failed to load solution at (%d,%d)", nErrorCode,iObj,k);
         } else {
           if (nVars - nCont > 0) {
             nErrorCode = LSgetInfo(pModel,LS_DINFO_MIPOBJ,&dObj);
             nErrorCode = LSgetInfo(pModel,LS_IINFO_MIP_STATUS,&status);
           } else {
             nErrorCode = LSgetInfo(pModel,LS_DINFO_POBJ,&dObj);
             nErrorCode = LSgetInfo(pModel,LS_IINFO_STATUS,&status);
           }
           printf ("\n Objective (%d,%d) = %f, status:%d \n", iObj, k, dObj,status);
           sprintf(strbuf,"model_obj%d_sol%d.sol",iObj,k);
           LSwriteSolution(pModel,strbuf);
         }
       }//for
     }//for
   }//

   // Determine the availability of solutions (1st method)
   {     
     int iObj; //index of last objective with a solution
     int iSol=0; //index of first solution
     iObj = nObj-1;
     sprintf(strbuf,"(%d,%d)",iObj,iSol);     

     // count down
     while (iObj>=0) {
      nErrorCode = LSloadSolutionAt(pModel,iObj,iSol);
      if (nErrorCode==LSERR_NO_ERROR) { //success;
        break;
      }
      iObj--;
     }
     if (iObj==nObj-1) {
       printf("\nLoaded Lexmin solution");
     } else if (iObj>=0) {
       printf("\nWarning: solutions for obj #%d and forth are not available",iObj+1);
     }     

     if (iObj>=0) { 
       if (nVars - nCont > 0) {
         nErrorCode = LSgetInfo(pModel,LS_IINFO_MIP_STATUS,&status);
       } else {
         nErrorCode = LSgetInfo(pModel,LS_IINFO_STATUS,&status);
       }
     } else {
       printf("\nNo solutions were found, status:%d",status);
     }

   }

   /***************************************************************
    * Step 6: Access the loaded solution (if optimal or feasible)
    ***************************************************************/
   if (status == LS_STATUS_OPTIMAL ||
       status == LS_STATUS_BASIC_OPTIMAL ||
       status == LS_STATUS_LOCAL_OPTIMAL ||
       status == LS_STATUS_FEASIBLE)
   {
     displaySolution(pEnv,pModel,strbuf,nVars,nCont,pszTitle,paszVarnames,primal,dual,dObj,status);
   }
   else
   {
     char strbuf[255];
     LSgetErrorMessage(pEnv,nErrorCode,strbuf);
     printf ("\n Optimization failed. Status = %d ",status);
     //printf ("\n Error %d: %s\n",nErrorCode,strbuf);
   }

   // Determine the availability of solutions (2nd method)
   {
     int iobj_last_optsol,iobj_last_anysol;
     LSgetInfo(pModel,LS_IINFO_LAST_OBJIDX_OPT,&iobj_last_optsol); //index of the last objective with an optimal solution
     LSgetInfo(pModel,LS_IINFO_LAST_OBJIDX_SOL,&iobj_last_anysol); //index of the last objective with a feasible solution
     printf("\nLS_IINFO_LAST_OBJIDX_OPT:%d, LS_IINFO_LAST_OBJIDX_SOL:%d",iobj_last_optsol,iobj_last_anysol);

     // load the last solution with optimal status
     nErrorCode = LSloadSolutionAt(pModel,iobj_last_optsol,0);
     displaySolution(pEnv,pModel,strbuf,nVars,nCont,pszTitle,paszVarnames,primal,dual,dObj,status);

     // load the last solution with any acceptable status (feasible/local_optimal)
     nErrorCode = LSloadSolutionAt(pModel,iobj_last_anysol,0);
     displaySolution(pEnv,pModel,strbuf,nVars,nCont,pszTitle,paszVarnames,primal,dual,dObj,status);
   }      

Terminate:
   /***************************************************************
    * Step 7: Terminate
    ***************************************************************/
   if (primal) free(primal);
   if (dual) free(dual);
   if (pachVarNameData) free(pachVarNameData);
   if (pachConNameData) free(pachConNameData);
   if (paszConnames) free(paszConnames);
   if (paszVarnames) free(paszVarnames);

   nErrorCode = LSdeleteModel( &pModel);
   nErrorCode = LSdeleteEnv( &pEnv);


  /* Wait until user presses the Enter key */
   printf("\nPress <Enter> ...");
   //getchar();

}
