/*
###################################################################
#                       LINDO-API
#                    Sample Programs
#                  Copyright (c) 2008
#
#         LINDO Systems, Inc.           312.988.7422
#         1415 North Dayton St.         info@lindo.com
#         Chicago, IL 60622             http://www.lindo.com
###################################################################

  File   : ex_nlp6.c
  Purpose: Solve a nonlinear least squares problem to find a
           positive semidefinite matrix S which approximates
           an indefinite symmetric R.

            Min ||R - S||
            s.t.
            S(i,j) - @SUM(k: L(j,k)*L(k,i)) = 0; for all j<=i;
            S(i,i) = 1;                          for all i;

  where R is an indefinite matrix and S is a matrix decision variable
  of the same dimension as R. L is a lower triangular, namely L = chol(S).

  Remark: The solution is performed wrt the lower triangular R.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
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
         printf("nErrorCode=%d:  %s\n", nErrorCode, \
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

static void LS_CALLTYPE print_line_log(pLSmodel pModel, char *line, void *userdata)
{
  if (line)
  {
    printf("%s",line);
  } /*if*/
} /*print_line*/

#define myMalloc(nameVector,lenVector,dataType) {	\
        if ( ( (nameVector) = (dataType *) malloc( (lenVector) * sizeof(dataType) ) ) == NULL && (lenVector)>0) { \
             printf("out of memory\n. exiting...\n"); exit (1); \
        } \
}


#define DIM 3
#define pos(i,j,DIM) ((2*DIM+1-j)*(j)/2+(i-j))

int loadModel(pLSmodel pModel)
 {
    int i,j,k,errorcode;
    int nObjs=1,
        nCons=DIM*(DIM+1)/2,
        nVars=DIM*(DIM+1),
        nVals=DIM*(DIM+1)/2,
        lenCode;

    int ikod, iobj, icon;
    int   oSense[1];
    char  *cSense=NULL, *vType=NULL;
    int   *paiCode=NULL;
    int   objBeg[1], objLen[1], *consBeg=NULL, *consLen=NULL;
    double *padLB=NULL, *padUB=NULL;
    double *padVals=NULL,*pX0=NULL;

    double dtmp[] = {
        1.0000,    0.9000,   0.5000,
                   1.0000,   0.9000,
                             1.0000 };

    myMalloc(cSense , nCons, char);
    myMalloc(vType  , nVars  , char);
    myMalloc(paiCode, 10*DIM*DIM*DIM+10*DIM*DIM, int);
    myMalloc(consBeg, nCons, int);
    myMalloc(consLen, nCons, int);
    myMalloc(padLB  , nVars, double);
    myMalloc(padUB  , nVars, double);
    myMalloc(padVals, nVals+3, double);
    myMalloc(pX0    , nVars, double);

    /* bounds of variables */
    for (i=0; i<nVars; i++)
    {
      padLB[i]=-LS_INFINITY;
      padUB[i]=+LS_INFINITY;
      if (i<nVars/2)
        pX0[i] = dtmp[i];
      else
        pX0[i] = sqrt(fabs(dtmp[i-nVars/2]-0.002));
      vType[i] = 'C';
    }
    for (i=0; i<DIM; i++)
    {
      k = pos(i,i,DIM);
      padLB[k]=1;
      padUB[k]=1;
    }

    for (i=0; i<nCons; i++)
    {
      cSense[i]='E';
    }

    /* Count for instruction paiCode */
    ikod = 0;
    /* Count for objective row */
    iobj = 0;
    /* Count for constraint row */
    icon = 0;


    memcpy(padVals,dtmp,sizeof(double)*nVals);

    padVals[nVals+0]=0.0;
    padVals[nVals+1]=1.0;
    padVals[nVals+2]=2.0;

    /* Direction of optimization */
    oSense[iobj]= LS_MIN;
    /* Beginning position of objective */
    objBeg[iobj]=ikod;
    paiCode[ikod++]=  EP_PUSH_NUM;
    paiCode[ikod++]=    nVals+0; //0
    for (j=0; j<DIM; j++)
    {
      for (i=j; i<DIM; i++)
      {
        /* Instruction list paiCode */
        paiCode[ikod++]=  EP_PUSH_NUM;
        paiCode[ikod++]=     pos(i,j,DIM);
        paiCode[ikod++]=  EP_PUSH_VAR;
        paiCode[ikod++]=     pos(i,j,DIM);
        paiCode[ikod++]=  EP_MINUS;
        paiCode[ikod++]=  EP_PUSH_NUM;
        paiCode[ikod++]=      nVals+2;
        paiCode[ikod++]=  EP_POWER;
        paiCode[ikod++]=  EP_PLUS;
      }
    }

    /* Length of objective */
    objLen[iobj] = ikod - objBeg[iobj];
    /* Increment the objective count */
    iobj++;

    for (i=0; i<DIM; i++)
    {
      for (j=0; j<=i; j++)
      {
        consBeg[icon]= ikod;
        paiCode[ikod++]=  EP_PUSH_VAR;
        paiCode[ikod++]=    pos(i,j,DIM);
        for (k=0; k<=j; k++)
        {
          /* Instruction list paiCode */
          paiCode[ikod++]=  EP_PUSH_VAR;
          paiCode[ikod++]=    pos(i,k,DIM) + nVars/2;
          paiCode[ikod++]=  EP_PUSH_VAR;
          paiCode[ikod++]=    pos(j,k,DIM) + nVars/2;
          paiCode[ikod++]=  EP_MULTIPLY;
          paiCode[ikod++]=  EP_MINUS;
        }
        consLen[icon] = ikod - consBeg[icon];
        /* Increment the constraint count */
        icon++;
      }
    }


    /* Total number of items in the instruction list */
    lenCode = ikod;

    /* Set up automatic differentiation */
    errorcode = LSsetModelIntParameter (pModel,LS_IPARAM_NLP_AUTODERIV, 1);
    //errorcode = LSsetModelIntParameter (pModel,LS_IPARAM_NLP_AUTODERIV, 0);
    if (errorcode != LSERR_NO_ERROR) goto ErrorReturn;

    /* Pass the instruction list to problem structure
     * by a call to LSloadNLPCode() */
    errorcode = LSloadInstruct (pModel, nCons, nObjs, nVars, nVals+3,
                  oSense, cSense,  vType, paiCode, lenCode, NULL,
                  padVals, pX0, objBeg, objLen, consBeg,
                  consLen, padLB, padUB);
    if (errorcode != LSERR_NO_ERROR) goto ErrorReturn;

ErrorReturn:

    return errorcode;
 }


/* main entry point */
int main()

{
   APIERRORSETUP;
   pLSenv pEnv;
   pLSmodel pModel;

  char MY_LICENSE_KEY[1024];

  /*****************************************************************
   * Step 1: Create a model in the environment.
   *****************************************************************/
   nErrorCode = LSloadLicenseString("../../../license/lndapi150.lic",MY_LICENSE_KEY);
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
   pModel = LScreateModel(pEnv,&nErrorCode);
   APIERRORCHECK;
   nErrorCode = LSsetModelLogfunc(pModel, (printModelLOG_t) print_line_log, NULL);
   APIERRORCHECK;

   nErrorCode = loadModel(pModel);
   APIERRORCHECK;

   /*
    *  Perform the optimization using the MIP solver
    */
   nErrorCode = LSsetModelIntParameter(pModel,LS_IPARAM_NLP_PRINTLEVEL,1);
   APIERRORCHECK;
   //nErrorCode = LSsetModelIntParameter(pModel,LS_IPARAM_GOP_PRINTLEVEL,1);
   APIERRORCHECK;

   nErrorCode = LSoptimize(pModel, LS_METHOD_FREE, NULL);
   APIERRORCHECK;

   {
      int stat, i,j,nvars;
      double objval=0.0, primal[2*DIM*DIM];

      nErrorCode = LSgetInfo(pModel,LS_IINFO_MODEL_STATUS,&stat);
      APIERRORCHECK;
      printf("\nStatus     : %d \n",stat);

      /* Get the optimization result */
      nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_VARS,&nvars);
      APIERRORCHECK;
      nErrorCode = LSgetInfo(pModel,LS_DINFO_POBJ,&objval);
      APIERRORCHECK;
      nErrorCode = LSgetPrimalSolution(pModel,primal);
      APIERRORCHECK;

    if (stat==1 || stat==2 || stat==5 || stat==8)
    {
      printf("Primal obj : %f \n",objval);
      printf("S[] = \n");
      for (j=0; j<DIM; j++)
      {
        for (i=0; i<DIM; i++)
        {
          printf("%8.5f",primal[i*DIM+j]);
        }
        printf("\n");
      }
    }
    else
    {
      printf("\nNo solution was found.\n");
    }

    APIERRORCHECK;

   }
Terminate:
 /* >>> Step 7 <<< Delete the LINDO environment */
   LSdeleteEnv(&pEnv);

  /* Wait until user presses the Enter key */
   printf("\n\nPress <Enter> ...");
   getchar();

}

