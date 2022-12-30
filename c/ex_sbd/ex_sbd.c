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

  File   : ex_sbd.c

  Purpose: Read a model from an MPS file and optimize with Benders method.
*/
#define _CRT_SECURE_NO_WARNINGS
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

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


int LS_CALLTYPE LSreadBlockStructure(pLSmodel pModel, char *pszFname);
int LS_CALLTYPE LSwriteBlockStructure(pLSmodel pModel, char *pszFname);
int LS_CALLTYPE LSsolveSBD(pLSmodel pModel, int nStages, int *panRowStage, int *panColStage, int *pnStatus);
pLSmodel LS_CALLTYPE LSgetSBDModel(pLSmodel pModel, int *pnerrorcode);

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


static void LS_CALLTYPE print_line(pLSmodel pModel, char *line, void *userdata)
{
  if (line)
  {
    printf("%s",line);
    fflush(stdout);
  } /*if*/
} /*print_line*/





/* main entry point */
/*
 *
 *
 *
 */
int main(int argc, char **argv)
{
   APIERRORSETUP;
   int numCons, numVars, numNonz; /* number of constraints and vars */
   int numCont=0, numBin=0, numInt=0; /* number of cont, bin. int vars*/
   int numCones=0, numConeNonz=0;

   char *mpsfile, *basfile = NULL;
   char strbuf[255], timfile[255];
   double dObj;
   int counter = 0, status=LS_STATUS_UNKNOWN;
   int isNBD=0;

/* declare an instance of the LINDO environment object */
   pLSenv pEnv = NULL;
/* declare an instance of the LINDO model object */
   pLSmodel pModel=NULL, deModel=NULL;
   int nStageTarget=2;

   char MY_LICENSE_KEY[1024];

   int i,j,k;
   int *rStage=NULL, *cStage=NULL;
   int nStages=0, nBlockType=0;
   int varStage[2]={0,0};
   int rowStage[2]={0,0};
   int *kA=NULL, *iA=NULL;
   int *rowContVars=NULL, *cmask=NULL;
   char *vtype=NULL;


  /****************************************************************
   * Init: Command prompt calling sequence
   ****************************************************************/
   if (argc == 1)
   {
     printf("\nUsage: ex_sbd filename\n\n");
     goto Terminate;
   }
   else if (argc == 2)
   {
     mpsfile = argv[1];
   }
   else if(argc >= 3)
   {
     mpsfile = argv[1];
     printf("\nWarning: Command line args beyond filename are ignored.\n");
   }

  /****************************************************************
   * Step 1: Create a LINDO environment.
   ****************************************************************/
   nErrorCode = LSloadLicenseString("../../../license/lndapi130.lic",MY_LICENSE_KEY);
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

   /* Install a log function to display solver's progress
   as reported by the internal solver */
   if (1)
   {
     nErrorCode = LSsetModelLogfunc(pModel, (printModelLOG_t) print_line, NULL);
   }
   else
   {
     /* Install a callback function to display solver's progress
     as specified by the user */
     nErrorCode = LSsetCallback(pModel,(cbFunc_t) print_log, NULL);
   }

   //LSsetModelDouParameter(pModel,LS_DPARAM_CALLBACKFREQ,0);
   //LSsetModelIntParameter(pModel,LS_IPARAM_MIP_TOPOPT,1);
   //LSsetModelIntParameter(pModel,LS_IPARAM_MIP_PRELEVEL,0);
   //LSsetModelIntParameter(pModel,LS_IPARAM_MIP_ITRLIM,100);
   //LSsetModelIntParameter(pModel,LS_IPARAM_NLP_ITRLMT,2000);
   //LSsetModelIntParameter(pModel,LS_IPARAM_SPLEX_ITRLMT,2000);
   //LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRELEVEL,0);

  /****************************************************************
   * Step 3: Read the model from an MPS file and get the model size
   ****************************************************************/
   timfile[0]='\0';
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
         char *ptr;
         strcpy(strbuf,mpsfile);
         ptr = strbuf + strlen(strbuf);
         while (*ptr != '.') ptr--;
         strcpy(ptr,".mps");
         nErrorCode = LSwriteMPSFile(pModel,strbuf,LS_FORMATTED_MPS);
         strcpy(timfile,mpsfile);
         ptr = timfile + strlen(timfile);
         while (*ptr != '.') ptr--;
         strcpy(ptr,".tim");
       }
     }
   } else if (0) {
     printf(" MPS format OK!. Exporting in LINDO format...\n\n");
     {
       char *ptr;
       strcpy(strbuf,mpsfile);
       ptr = strbuf + strlen(strbuf);
       while (*ptr != '.') ptr--;
       strcpy(ptr,".ltx");
       nErrorCode = LSwriteLINDOFile(pModel,strbuf);
       strcpy(timfile,mpsfile);
       ptr = timfile + strlen(timfile);
       while (*ptr != '.') ptr--;
       strcpy(ptr,".tim");
     }
   }


   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_VARS,&numVars);
   APIERRORCHECK;
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_NLP_CONS,&numCons);
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_CONS,&numCons);
   APIERRORCHECK;
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_CONT,&numCont);
   APIERRORCHECK;
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_NONZ,&numNonz);
   APIERRORCHECK;

   /***************************************************************
    * Step 4: Read parameter file, if any exists.
    ***************************************************************/
   //LSreadModelParameter(pModel,"lindo.par");


   /***************************************************************
    * Step 5: Read or Find/Load block structure
    ***************************************************************/
    nErrorCode = LSreadBlockStructure(pModel,timfile);
    if (nErrorCode==LSERR_NO_ERROR) {
      int itmp;
      fprintf(stdout,"\nA time file containing block structure has been read\n");
      nErrorCode = LSgetBlockStructure(pModel,&itmp,NULL,NULL,NULL);
      if (nErrorCode==LSERR_NO_ERROR)
      {
        LSsetModelIntParameter(pModel,LS_IPARAM_DECOMPOSITION_TYPE,LS_LINK_BLOCKS_MATRIX);
      }
      APIERRORCHECK;
      nErrorCode = LSsolveSBD(pModel,itmp,NULL,NULL,&status);
      APIERRORCHECK;
    } else {
     fprintf(stdout,"\nNo time file containing block structure was found\nTrying to detect the decomposition structure\n");
     kA = malloc((numVars+1)*sizeof(int));
     iA = malloc((numNonz)*sizeof(int));
     vtype = calloc(numVars,sizeof(char));
     nErrorCode = LSgetLPData(pModel,NULL,NULL,NULL,NULL,NULL,kA,NULL,NULL,iA,NULL,NULL);
     APIERRORCHECK;
     nErrorCode = LSgetVarType(pModel,vtype);
     APIERRORCHECK;
     if (numCont < numVars)
     {
       // MILP model, move integers to stage-0 as linking
       rowContVars = calloc(numCons,sizeof(int));
       cStage = calloc(numVars,sizeof(int));
       rStage = calloc(numCons,sizeof(int));
       for (j=0; j<numVars; j++)
       {
         if (vtype[j]=='C')
         {
           cStage[j]=1;
           for (k=kA[j]; k<kA[j+1]; k++)
           {
             i = iA[k];
             rowContVars[i] += 1;
           }
         }
         else
         {
           cStage[j]=0;
         }
         varStage[cStage[j]]++;
       }
       for (i=0; i<numCons; i++)
       {
         if (rowContVars[i]==0)
         {
           rStage[i]=0; // pure integer row
         }
         else
         {
           rStage[i]=1;
         }
         rowStage[rStage[i]]++;
         if (0)fprintf(stdout,"\ni=%d, rowcontvar:%d, rstage=%d",i,rowContVars[i],rStage[i]);
       }
       fprintf(stdout,"\nstage=%d, rows:%d, vars=%d",0,rowStage[0],varStage[0]);
       fprintf(stdout,"\nstage=%d, rows:%d, vars=%d",1,rowStage[1],varStage[1]);
       fprintf(stdout,"\n");

       if (varStage[0]==0)
       {
         fprintf(stdout,"\nWarning: Model does not have any integer variables in stage-0");
         nErrorCode = LSsolveMIP(pModel,&status);
       }
       else if (varStage[1]>0)
       {
         //LSsetModelDouParameter(pModel,LS_DPARAM_STOC_INFBND,1e30);
         nErrorCode = LSsolveSBD(pModel,2,rStage,cStage,&status);
       }
       else
       {
         fprintf(stdout,"\nWarning: Model does not have any continuous variables in stage-1");
         nErrorCode = LSsolveMIP(pModel,&status);
       }
       fflush(stdout);
     }
     else
     {
	   LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRINTLEVEL,2);
       // continuous model, let the solver do the decomposition
       LSsetModelIntParameter(pModel, LS_IPARAM_DECOMPOSITION_TYPE, LS_LINK_BLOCKS_MATRIX);
       //LSsetModelDouParameter(pModel,LS_DPARAM_STOC_INFBND,1e30);
       nErrorCode = LSsolveSBD(pModel,2,rStage,cStage,&status);
     }
   }





   /***************************************************************
    * Step 6: Access the final solution if optimal or feasible
    ***************************************************************/
   if (status == LS_STATUS_OPTIMAL ||
       status == LS_STATUS_BASIC_OPTIMAL ||
       status == LS_STATUS_LOCAL_OPTIMAL ||
       status == LS_STATUS_FEASIBLE)
   {
     double *primal = NULL, *dual = NULL;

     char **paszVarnames=NULL,
          **paszConnames=NULL,
          *pachConNameData=NULL,
          *pachVarNameData=NULL;

     char pszTitle[255], *pszObjname = NULL, *pszRhsname = NULL;
     char *pszRngname = NULL, *pszBndname = NULL;

     int nTotalVarNameLen;
     int nTotalConNameLen;

     primal = (double *) malloc(numVars*sizeof(double));


     if (numCont < numVars)
     {
       nErrorCode = LSgetMIPPrimalSolution( pModel, primal);
       APIERRORCHECK;
       nErrorCode = LSgetInfo(pModel, LS_DINFO_MIP_OBJ, &dObj);
       APIERRORCHECK;
     }
     else
     {
       nErrorCode = LSgetPrimalSolution( pModel, primal);
       APIERRORCHECK;
       nErrorCode = LSgetInfo(pModel, LS_DINFO_POBJ, &dObj);
       APIERRORCHECK;
     }

     /* Retrieve variable and constraint names */
     if (0)
     {
       /* Allocate pointers to name data */
       paszConnames = (char **) malloc(numCons*sizeof(char *));
       paszVarnames = (char **) malloc(numVars*sizeof(char *));

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

     printf ("\n Objective at solution = %f \n", dObj);

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

   if (0)
   {
     pLSmodel sbdModel = NULL;
     sbdModel = LSgetSBDModel(pModel, &nErrorCode);
     APIERRORCHECK;
     LSwriteMPSFile(sbdModel,"coresbd.mps",0);
     LSsetModelIntParameter(sbdModel,LS_IPARAM_LP_PRINTLEVEL,0);
     LSsetModelDouParameter(sbdModel,LS_DPARAM_SOLVER_FEASTOL,1e-6);
     LSsetModelDouParameter(sbdModel,LS_DPARAM_SOLVER_OPTTOL,1e-5);
     printf ("\n Optimizing the SBD model\n");
     nErrorCode = LSsolveMIP(sbdModel,NULL);
     APIERRORCHECK;
     nErrorCode = LSgetInfo(sbdModel, LS_DINFO_MIP_OBJ, &dObj);
     APIERRORCHECK;
     printf ("\n SBD Objective at solution = %f \n", dObj);
     LSdeleteModel(&sbdModel);
   }


Terminate:
   /***************************************************************
    * Step 7: Terminate
    ***************************************************************/
   nErrorCode = LSdeleteModel( &pModel);
   nErrorCode = LSdeleteEnv( &pEnv);

   free(rStage);
   free(cStage);
   free(vtype);
   free(kA);
   free(iA);
   free(rowContVars);

  /* Wait until user presses the Enter key */
   printf("\n");
   //getchar();

}
