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

  File   : ex_objlex.c

  Considers the formulation of the bi-objective linear assignment problem

  MODEL:
  SETS:
    WR / 1..5/: ;
    CS / 1..5/: ;
    ROUTES( WR, CS) : C1, C2, V;
  ENDSETS
    ! The objective;
    [OBJ] MIN = @SUM( ROUTES: C1 * V);
    ! The Demand constraints;
    @FOR( CS( J): [DEM] @SUM( WR( I): V( I, J)) =  1);
    ! The supply constraints;
    @FOR( WR( I): [SUP] @SUM( CS( J): V( I, J)) =  1);
    !@SUM( ROUTES: C1 * V) <=6.0+1e-7;
  DATA:
    C1 = 3 9 0 0 6 16 0 6 12 19 2 7 11 15 8 4 11 7 16 3 2 5 1 9 0;
    !C2 = 16 5 6 19 12 15 7 13 7 7 1 2 13 2 3 14 7 8 1 7 10 10 1 0 0;
  END

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
  static int status;

  if (iLoc == LSLOC_MIP)
  {
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_STATUS,&status);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_SIM_ITER,&iter);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_MIP_OBJ,&pobj);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_MIP_BESTBOUND,&bestbnd);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_PINFEAS,&pfeas);
    printf("%2d\tIt=%4d \tInfeas= %8.3e\tObj=%11.5e \tBestBound=%11.5e \tStat=%d\n",
      iLoc,iter,pfeas,pobj,bestbnd,status);
  }

  else if ( iLoc == LSLOC_PRIMAL || iLoc ==LSLOC_DUAL ||
            iLoc ==LSLOC_CONOPT  || iLoc == LSLOC_BARRIER)
  {
    LSgetCallbackInfo(model,iLoc,LS_IINFO_STATUS,&status);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_PINFEAS,&pfeas);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_SIM_ITER,&iter);
    printf("%2d\tIt=%4d \tInfeas= %8.3e\tObj=%11.5e \tStat=%d\n",
      iLoc,iter,pfeas,pobj,status);
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
                     char **paszVarnames, double *primal, double *dual, double dObj)
{
  APIERRORSETUP;
  int j;
  if (nVars - nCont > 0)
  {
    nErrorCode = LSgetMIPPrimalSolution( pModel, primal) ;
    APIERRORCHECK;
    //nErrorCode = LSgetMIPDualSolution( pModel, dual) ;
    APIERRORCHECK;
    nErrorCode = LSgetInfo(pModel,LS_DINFO_MIPOBJ,&dObj);
    APIERRORCHECK;
  } else  {
    nErrorCode = LSgetPrimalSolution( pModel, primal) ;
    APIERRORCHECK;
    nErrorCode = LSgetDualSolution( pModel, dual) ;
    APIERRORCHECK;
    nErrorCode = LSgetInfo(pModel,LS_DINFO_POBJ,&dObj);
    APIERRORCHECK;
  }

  printf ("\n Primal Solution\n");
  printf("\t%8s %18s\n","VARS", "Primal");
  for (j = 0; j<nVars; j++)
  {
    printf("\t%8s %18.10e\n",paszVarnames[j], primal[j]);
  }
  printf ("\n Model Title: %s\n Objective at solution = %f \n", pszTitle, dObj);
Terminate:
  return nErrorCode;
}

/**
 * @brief Display resident solution
 * @remark None
 */
int displaySolutionShort(pLSenv pEnv, pLSmodel pModel, char *szTag, int nVars, int nCont, char *pszTitle,
                     char **paszVarnames, double *primal, double *dual, double dObj)
{
  APIERRORSETUP;
  int j;
  if (nVars - nCont > 0)
  {
    nErrorCode = LSgetMIPPrimalSolution( pModel, primal) ;
    APIERRORCHECK;
    //nErrorCode = LSgetMIPDualSolution( pModel, dual) ;
    APIERRORCHECK;
    nErrorCode = LSgetInfo(pModel,LS_DINFO_MIPOBJ,&dObj);
    APIERRORCHECK;
  } else  {
    nErrorCode = LSgetPrimalSolution( pModel, primal) ;
    APIERRORCHECK;
    nErrorCode = LSgetDualSolution( pModel, dual) ;
    APIERRORCHECK;
    nErrorCode = LSgetInfo(pModel,LS_DINFO_POBJ,&dObj);
    APIERRORCHECK;
  }

  printf("\n%-16s",szTag?szTag:"-");
  for (j = 0; j<nVars; j++)
  {
    printf("%d",(int)primal[j]);
  }
  printf("\t%13.6f",dObj);
Terminate:
  return nErrorCode;
}

int main(int argc, char **argv)
{
   APIERRORSETUP;
   int nCons, nVars; /* number of constraints and vars */
   int nCont=0, nB=0, nI=0; /* number of cont, bin. int vars*/
   char *mpxStream=NULL;
   double dObj;
   int counter = 0, status, j=0;
   int nTotalVarNameLen;
   int nTotalConNameLen;
   char strbuf[255];

/* declare an instance of the LINDO environment object */
   pLSenv pEnv = NULL;
/* declare an instance of the LINDO model object */
   pLSmodel pModel=NULL;
   char MY_LICENSE_KEY[1024];

   double *primal = NULL, *dual = NULL;

   char **paszVarnames=NULL, **paszConnames=NULL, *pachConNameData=NULL,*pachVarNameData=NULL;
   char pszTitle[255], *pszObjname = NULL, *pszRhsname = NULL;
   char *pszRngname = NULL, *pszBndname = NULL;

  /****************************************************************
   * Init: Command prompt calling sequence
   ****************************************************************/

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

   /***************************************************************
    * Step 3: Read model and setup objpool
    ***************************************************************/
   //LSsetModelIntParameter(pModel,LS_IPARAM_INSTRUCT_SUBOUT,1);

   nErrorCode = LSreadMPIFile(pModel,"assign5.mpi");
   APIERRORCHECK;

   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_VARS,&nVars);
   APIERRORCHECK;

#if 0
   // convert to an integer model to test as MIP
   for (j=0; j<nVars; j++)
     nErrorCode = LSmodifyVariableType(pModel,1,&j,"B");
   APIERRORCHECK;
#endif

   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_NLP_CONS,&nCons);
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_CONS,&nCons);
   APIERRORCHECK;
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_CONT,&nCont);
   APIERRORCHECK;

   // add 2nd objective making it a bi-objective model
   {
     double padC2[25] = {16,5,6,19,12,15,7,13,7,7,1,2,13,2,3,14,7,8,1,7,10,10,1,0,0};
     nErrorCode = LSaddObjPool(pModel,padC2,LS_MIN,1,1e-6);
     APIERRORCHECK;
   }

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
    * Step 4: Optimize the model
    ***************************************************************/
   status = LS_STATUS_UNKNOWN;
   //LSsetModelDouParameter(pModel,LS_DPARAM_CALLBACKFREQ,0);
   //LSsetModelIntParameter(pModel,LS_IPARAM_MIP_TOPOPT,1);
   //LSsetModelIntParameter(pModel,LS_IPARAM_MIP_PRELEVEL,0);
   //LSsetModelIntParameter(pModel,LS_IPARAM_MIP_ITRLIM,100);
   //LSsetModelIntParameter(pModel,LS_IPARAM_LP_SCALE,0);
   //LSsetModelIntParameter(pModel,LS_IPARAM_SPLEX_ITRLMT,2000);
   LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRINTLEVEL,2);
   //LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRTFG,2);
   //LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRELEVEL,0);
   //LSsetModelIntParameter(pModel,LS_IPARAM_MIP_PRELEVEL,0);

   /* Install a log function to display solver's progress
   as reported by the internal solver */
   nErrorCode = LSsetModelLogfunc(pModel, (printModelLOG_t) print_line_log, NULL);

   /* Install a callback function to display solver's progress
   as specified by the user */
   //nErrorCode = LSsetCallback(pModel,(cbFunc_t) print_log, NULL);


   { // allow atmost 5 solutions per objective in the solution pool
     int maxSols=5;
     LSsetModelIntParameter(pModel,LS_IPARAM_SOLPOOL_LIM,maxSols);
   }

   LSsetModelIntParameter(pModel,LS_IPARAM_MIP_PRINTLEVEL,0);
   if (nVars - nCont > 0) {
     LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRINTLEVEL,0);
     nErrorCode = LSsolveMIP( pModel, &status);
   } else {
     LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRINTLEVEL,2);
     nErrorCode = LSoptimize( pModel, 0, &status);
   }
   APIERRORCHECK;

   // Walk through the solution pool and write available solutions
   {
     int k, nObj=2;
     int numSols=0, iObj=0;
     printf("\nWalking through the solution pool and writing available solutions");
     for (iObj=0; iObj<nObj; iObj++) {
       nErrorCode = LSgetObjPoolNumSol(pModel,iObj,&numSols);
       for (k=0; k<numSols; k++) {
         nErrorCode = LSloadSolutionAt(pModel,iObj,k);
         if (nErrorCode) {
           printf("\nError %d: failed to load solution at (%d,%d)", nErrorCode,iObj,k);
         } else {
           if (nVars - nCont > 0) {
             nErrorCode = LSgetInfo(pModel,LS_DINFO_MIPOBJ,&dObj);
           } else {
             nErrorCode = LSgetInfo(pModel,LS_DINFO_POBJ,&dObj);
           }
           printf ("\n Objective (%d,%d) = %f \n", iObj, k, dObj);
           sprintf(strbuf,"model_obj%d_sol%d.sol",iObj,k);
           LSwriteSolution(pModel,strbuf);
         }
       }//for
     }//for
   }//

   // Load the first lexmin solution
   {
     int iObj=1; //index of last objective
     int iSol=0; //index of first solution
     sprintf(strbuf,"(%d,%d)",iObj,iSol);
     printf("\nLoading Lexmin solution");
     nErrorCode = LSloadSolutionAt(pModel,iObj,iSol);
   }

   /***************************************************************
    * Step 6: Access the final solution if optimal or feasible
    ***************************************************************/
   if (status == LS_STATUS_OPTIMAL ||
       status == LS_STATUS_BASIC_OPTIMAL ||
       status == LS_STATUS_LOCAL_OPTIMAL ||
       status == LS_STATUS_FEASIBLE)
   {
     displaySolution(pEnv,pModel,strbuf,nVars,nCont,pszTitle,paszVarnames,primal,dual,dObj);
   }
   else
   {
     char strbuf[255];
     LSgetErrorMessage(pEnv,nErrorCode,strbuf);
     printf ("\n Optimization failed. Status = %d ",status);
     //printf ("\n Error %d: %s\n",nErrorCode,strbuf);
   }

   // Walk through the solution pool and display all solutions in a short summary
   {
     int k, nObj=2;
     int numSols=0, iObj=0;
     printf ("\n Solution pool in short format\n");
     printf("\n%-16s%25s\t%13s","Obj/Sol","Primal","ObjVal");
     for (iObj=0; iObj<nObj; iObj++) {
       nErrorCode = LSgetObjPoolNumSol(pModel,iObj,&numSols);
       for (k=0; k<numSols; k++) {
         sprintf(strbuf,"(%d,%d)%s",iObj,k,((iObj==nObj-1)&&k==0)?"*":"");
         nErrorCode = LSloadSolutionAt(pModel,iObj,k);
         if (!nErrorCode) {
           displaySolutionShort(pEnv,pModel,strbuf,nVars,nCont,pszTitle,paszVarnames,primal,dual,dObj);
         }
       }//for
     }//for
     printf ("\n\n(*) LexMin Solution\n");
   }//
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
