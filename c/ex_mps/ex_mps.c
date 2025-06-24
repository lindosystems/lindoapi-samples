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
#include <math.h>
/* LINDO API header file */
#include "lindo.h"
#include "../common/commonutils.c"
#define _PRINT_BANNER_COUNT_ 25
/* userdata for illustration purposes */
typedef struct
{
  int      counter[2];
  pLSmodel pModel;
  double   elapsed;
  double   cbfreq;
  int      major_solver;
  double   _DINFO_SUB_OBJ                         ;
  double   _DINFO_SUB_PINF                        ;
  double   _DINFO_CUR_OBJ                         ;
  int   _IINFO_CUR_ITER                        ;
  double   _DINFO_CUR_BEST_BOUND                  ;
  int   _IINFO_CUR_STATUS                      ;
  int   _IINFO_CUR_LP_COUNT                    ;
  int   _IINFO_CUR_BRANCH_COUNT                ;
  int   _IINFO_CUR_ACTIVE_COUNT                ;
  int   _IINFO_CUR_NLP_COUNT                   ;
  int   _IINFO_CUR_MIP_COUNT                   ;
  int   _IINFO_CUR_CUT_COUNT                   ;
  double   _DINFO_CUR_ITER                        ;
} UserData, *pUserData;

#define mCB_INTERRUPT 0
/*
 - 0.  Disable callback-interrupts

 - 1.  Enable callback-interrupts using an iteration limit for QP/NLP/LPs
    You can test this setting while solving lindoapi/samples/data/testlp.ltx (.mps)

 - 2.  Enable callback-interrupts using the MIP gap.
    You can test this setting while solving lindoapi/samples/data/bm23.ltx (.mps)

*/


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
      goto ErrReturn; \
   } \

#define APIVERSION \
{\
    char szVersion[255], szBuild[255];\
    LSgetVersionInfo(szVersion,szBuild);\
    printf("\nLINDO API Version %s built on %s\n",szVersion,szBuild);\
}\
// undocumented public func
double LS_CALLTYPE LSgetTic(void);
int LS_CALLTYPE LSgetThreadNum(void);

/* globals */

int m, n, nz; /* number of constraints and vars and nonzs */
int nC=0, nB=0, nI=0; /* number of cont, bin. int vars*/
int qnz, nlpnz, nlpobjnz, conenz;
int is_lp, is_mip, is_nlp, is_socp, is_qp;
int use_gop = 0;
int supress_err_lines=1;
/******************************************************************************
 *
 *  Extended callback function to display local and intermediate
 *  solutions
 *
 */
int  LS_CALLTYPE print_progress_log(pLSmodel model,int iLoc, void *cbData)
{
  int iter=0, _tid=0;
  static int paiErr[20], KF=LS_DINFO_SUB_OBJ;
  pUserData pProgressData = (pUserData) cbData;
  int *countdata = pProgressData->counter;
  pLSmodel prob = pProgressData->pModel;
  char szerr[255];

  double pfeas=0.0,pobj=0.0, sobj=0.0;
  double bestbnd;
  int status,lpcnt,bncnt,accnt=0,nlpcnt,mipcnt,cutcnt;
  int nErr=0, newmipsol=0, mip_type=0;
  static int m=-1,n,nC;
  static int lit=-1;
  double curtime = LSgetTic();

  _tid = LSgetThreadNum();

  if (iLoc == LSLOC_PRESOLVE  || iLoc == LSLOC_FUNC_CALC ||
      iLoc == LSLOC_GEN_START  || iLoc == LSLOC_GEN_PROCESSING  ||
      iLoc == LSLOC_GEN_END || iLoc <0)
  {
    return 0;
  }

  if (iLoc == LSLOC_SP_WS_START)
  {
    printf("\n%-4d: LSLOC_SP_WS_START",iLoc);
  } else if (iLoc == LSLOC_SP_WS_END)
  {
    printf("\n%-4d: LSLOC_SP_WS_END",iLoc);
  }

  if (curtime - pProgressData->elapsed < pProgressData->cbfreq)
  {
    if (iLoc!=LSLOC_EXIT_SOLVER && iLoc!=LSLOC_SP) return 0;
  }
  else
  {
    pProgressData->elapsed = curtime;
  }


#if DEBUG_CHECK_ITER>0
  nErr = LSgetProgressInfo(model,iLoc,LS_IINFO_CUR_ITER,&iter);
  if (iter<lit) /*dropped below last iteration */
  {
    FILE *fp=NULL;
    printf("\n *********************************** \n");
    printf("\n ******** iteration error ********** \n");
    printf("\n *********************************** \n");
    printf("\n last_iter=%d, cur_iter=%d\n",lit,iter);
    printf("\n ** press enter to continue ** \n");
    nErr = LSgetProgressInfo(model,iLoc,LS_IINFO_CUR_ITER,&iter);
    getchar();
    /*return 1;*/
  }
  if (iLoc == 14 &&  iter== 31)
  {
    nErr = 0;
  }
  else if (iLoc == 5)
  {
    nErr = 0;
  }
#endif



  if (m<0)
  {
    nErr = LSgetInfo(prob,LS_IINFO_NUM_VARS,&n);
    nErr = LSgetInfo(prob,LS_IINFO_NUM_CONS,&m);
    nErr = LSgetInfo(prob,LS_IINFO_NUM_CONT,&nC);
  }

  if (pProgressData->major_solver == LSLOC_GOP ||
      pProgressData->major_solver == LSLOC_MIP ||
      pProgressData->major_solver == LSLOC_IISIUS ||
      pProgressData->major_solver == LSLOC_MSW || n>nC)
  {
    if (!(countdata[0]%_PRINT_BANNER_COUNT_) ||
        !countdata[0])
    {
      printf("\n\n%6s %8s %8s %8s %14s %14s %14s %8s %6s %8s\n",
        "LOC+TH","ITER","BRANCH","LPs","INFEAS","BEST BND","BEST SOL","ACTIVE","STATUS","CPUTIME");
    }
  }
  else
  {
    if (!(countdata[0]%_PRINT_BANNER_COUNT_) ||
        !countdata[0])
    {
      printf("\n\n%6s %8s %14s %14s %14s %8s %8s\n",
        "LOC+TH","ITER","INFEAS","BEST BND","POBJ","STATUS","CPUTIME");
    }
  }

  // Dummy call to intercept
  nErr = LSgetProgressInfo(model,iLoc,LS_IINFO_CUR_ITER,&iter);
  if (iLoc==LSLOC_EXIT_SOLVER)
  {
    nErr = nErr;
  }

  // Global iteration count
  nErr = LSgetProgressInfo(model,iLoc,LS_IINFO_CUR_ITER,&iter);
  paiErr[LS_IINFO_CUR_ITER-KF]=nErr;
  if (nErr!=LSERR_NO_ERROR) {
    iter=-nErr;
  } else {
    pProgressData->_IINFO_CUR_ITER=iter;
  }

  // Global status
  nErr = LSgetProgressInfo(model,iLoc,LS_IINFO_CUR_STATUS,&status);
  paiErr[LS_IINFO_CUR_STATUS-KF]=nErr;
  if (nErr!=LSERR_NO_ERROR) {
    status=-nErr;
  } else {
    pProgressData->_IINFO_CUR_STATUS=status;
  }
  szerr[0]='\0';

  // Global current obj
  nErr = LSgetProgressInfo(model,iLoc,LS_DINFO_CUR_OBJ,&pobj);
  paiErr[LS_DINFO_CUR_OBJ-KF]=nErr;
  if (nErr!=LSERR_NO_ERROR) {
    pobj=-DBL_MAX;
  } else {
    pProgressData->_DINFO_CUR_OBJ=pobj;
  }

  //
  nErr = LSgetProgressInfo(model,iLoc,LS_DINFO_SUB_OBJ,&sobj);
  paiErr[LS_DINFO_SUB_OBJ-KF]=nErr;
  if (nErr!=LSERR_NO_ERROR) {
    sobj=-DBL_MAX;
  } else {
    pProgressData->_DINFO_SUB_OBJ=sobj;
  }

  nErr = LSgetProgressInfo(model,iLoc,LS_DINFO_CUR_BEST_BOUND,&bestbnd);
  paiErr[LS_DINFO_CUR_BEST_BOUND-KF]=nErr;
  if (nErr!=LSERR_NO_ERROR) {
    bestbnd=-DBL_MAX;
  } else {
    pProgressData->_DINFO_CUR_BEST_BOUND=bestbnd;
  }

  nErr = LSgetProgressInfo(model,iLoc,LS_DINFO_SUB_PINF,&pfeas);
  paiErr[LS_DINFO_SUB_PINF-KF]=nErr;
  if (nErr!=LSERR_NO_ERROR) {
    pfeas=-DBL_MAX;
  } else {
    pProgressData->_DINFO_SUB_PINF=pfeas;
  }

  nErr = LSgetProgressInfo(model,iLoc,LS_IINFO_CUR_LP_COUNT,&lpcnt);
  paiErr[LS_IINFO_CUR_LP_COUNT-KF]=nErr;
  if (nErr!=LSERR_NO_ERROR) {
    lpcnt=-nErr;
  } else {
    pProgressData->_IINFO_CUR_LP_COUNT=lpcnt;
  }

  nErr = LSgetProgressInfo(model,iLoc,LS_IINFO_CUR_BRANCH_COUNT,&bncnt);
  paiErr[LS_IINFO_CUR_BRANCH_COUNT-KF]=nErr;
  if (nErr!=LSERR_NO_ERROR) {
    bncnt=-nErr;
  } else {
    pProgressData->_IINFO_CUR_BRANCH_COUNT=bncnt;
  }

  nErr = LSgetProgressInfo(model,iLoc,LS_IINFO_CUR_ACTIVE_COUNT,&accnt);
  paiErr[LS_IINFO_CUR_ACTIVE_COUNT-KF]=nErr;
  if (nErr!=LSERR_NO_ERROR) {
    accnt=-nErr;
  } else {
    pProgressData->_IINFO_CUR_ACTIVE_COUNT=accnt;
  }

  nErr = LSgetProgressInfo(model,iLoc,LS_IINFO_CUR_NLP_COUNT,&nlpcnt);
  paiErr[LS_IINFO_CUR_NLP_COUNT-KF]=nErr;
  if (nErr!=LSERR_NO_ERROR) {
    nlpcnt=-nErr;
  } else {
    pProgressData->_IINFO_CUR_NLP_COUNT=nlpcnt;
  }

  nErr = LSgetProgressInfo(model,iLoc,LS_IINFO_CUR_MIP_COUNT,&mipcnt);
  paiErr[LS_IINFO_CUR_MIP_COUNT-KF]=nErr;
  if (nErr!=LSERR_NO_ERROR) {
    mipcnt=-nErr;
  } else {
    pProgressData->_IINFO_CUR_MIP_COUNT=mipcnt;
  }

  nErr = LSgetProgressInfo(model,iLoc,LS_IINFO_CUR_CUT_COUNT,&cutcnt);
  paiErr[LS_IINFO_CUR_CUT_COUNT-KF]=nErr;
  if (nErr!=LSERR_NO_ERROR) {
    cutcnt=-nErr;
  } else {
    pProgressData->_IINFO_CUR_CUT_COUNT=cutcnt;
  }

  if (supress_err_lines)
  {
    iter = pProgressData->_IINFO_CUR_ITER;
    bncnt = pProgressData->_IINFO_CUR_BRANCH_COUNT;
    lpcnt = pProgressData->_IINFO_CUR_LP_COUNT;
    mipcnt = pProgressData->_IINFO_CUR_MIP_COUNT;
    pfeas = pProgressData->_DINFO_SUB_PINF;
    bestbnd = pProgressData->_DINFO_CUR_BEST_BOUND;
    pobj = pProgressData->_DINFO_CUR_OBJ;
    accnt = pProgressData->_IINFO_CUR_ACTIVE_COUNT;
    status = pProgressData->_IINFO_CUR_STATUS;
  }

  if (pProgressData->major_solver == LSLOC_GOP ||
      pProgressData->major_solver == LSLOC_MIP ||
      pProgressData->major_solver == LSLOC_IISIUS ||
      pProgressData->major_solver == LSLOC_MSW || n>nC)
  {
    printf("\n%3d+%2d:%8d %8d %8d %14e %14e %14e %8d %6d %8.2f %s",
      iLoc,_tid,iter,bncnt,lpcnt+mipcnt,pfeas,bestbnd,pobj,accnt,status,curtime,szerr);
  }
  else
  {
    printf("\n%3d+%2d:%8d %14e %14e %14e %8d %8.2f %s",
      iLoc,_tid,iter,pfeas,bestbnd,pobj,status,curtime,szerr);
  }

  (countdata[0])++;

  lit = iter;
  fflush(stdout);

  return 0;
} /*print_log*/

/* main entry point */

/****************************************************************
   Standard callback function to display local and intermediate
   solutions
 ****************************************************************/
int  LS_CALLTYPE print_log(pLSmodel model,int iLoc, void *cbData)
{
  static int iter=0;
   double pfeas=0.0,pobj=-DBL_MAX;
   double bestbnd=DBL_MAX;
  static int status;

  if (iLoc == LSLOC_MIP)
  {
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_STATUS,&status);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_SIM_ITER,&iter);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_MIP_OBJ,&pobj);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_MIP_BESTBOUND,&bestbnd);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_PINFEAS,&pfeas);
    printf("%2d\tIt=%4d \tInfeas= %8.3e\tObj=%11.5e \tBestBound=%11.5e \tStat=%d \tGap:%g\n",
      iLoc,iter,pfeas,pobj,bestbnd,status,fabs(pobj-bestbnd));
    if (iter >= 1000 && mCB_INTERRUPT==1) {
		printf("Interrupt flag 1\n");
		return 1;
	}
	if (fabs(pobj-bestbnd)<10 && mCB_INTERRUPT==2) {
		printf("Interrupt flag 2 (%g)\n",(pobj-bestbnd));
		return 1;
	}
  }

  else if ( iLoc == LSLOC_PRIMAL || iLoc ==LSLOC_DUAL ||
            iLoc ==LSLOC_CONOPT  || iLoc == LSLOC_BARRIER)
  {
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_STATUS,&status);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_PINFEAS,&pfeas);
    printf("%2d\tIt=%4d \tInfeas= %8.3e\tObj=%11.5e \tBestBound=%11.5e \tStat=%d\n",
      iLoc,iter,pfeas,pobj,bestbnd,status);
    if (iter >= 1000 && mCB_INTERRUPT==1) {
		printf("Interrupt flag 3\n");
		return 1;
	}
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

ErrReturn:
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


/*    Wrapper function to solve   */

int ls_solve_model(pLSmodel pModel, int *pStatus)
{
   int nErrorCode = LSERR_NO_ERROR;
   int optErrorCode = LSERR_NO_ERROR;

   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_VARS,&n);
   if (nErrorCode != 0) return nErrorCode;
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_CONS,&m);
   if (nErrorCode != 0) return nErrorCode;
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_NONZ,&nz);
   if (nErrorCode != 0) return nErrorCode;
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_CONT,&nC);
   if (nErrorCode != 0) return nErrorCode;
   nErrorCode =  LSgetInfo(pModel,LS_IINFO_NUM_QC_NONZ,&qnz);
   if (nErrorCode != 0) return nErrorCode;
   nErrorCode =  LSgetInfo(pModel,LS_IINFO_NUM_NLP_NONZ,&nlpnz);
   if (nErrorCode != 0) return nErrorCode;
   nErrorCode =  LSgetInfo(pModel,LS_IINFO_NUM_NLPOBJ_NONZ,&nlpobjnz);
   if (nErrorCode != 0) return nErrorCode;
   nErrorCode =  LSgetInfo(pModel,LS_IINFO_NUM_CONE_NONZ,&conenz);
   if (nErrorCode != 0) return nErrorCode;

   is_mip = (nC != n);
   is_nlp = (nlpnz + nlpobjnz> 0);
   is_qp = (qnz > 0);
   is_socp = (conenz > 0);
   is_lp = (!is_nlp && !is_qp && !is_mip && !is_socp);

   if (is_mip)
   {
     LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRINTLEVEL,0);

     if (is_lp || is_socp)
     {
       optErrorCode = LSsolveMIP( pModel, pStatus);
     }
     else
     {
       if (use_gop)
       {
         optErrorCode = LSsolveGOP( pModel, pStatus);
       }
       else
       {
         optErrorCode = LSsolveMIP( pModel, pStatus);
       }
     }
   }
   else
   {
     if (use_gop)
       optErrorCode = LSsolveGOP( pModel, pStatus);
     else
       optErrorCode = LSoptimize( pModel, LS_METHOD_FREE, pStatus);
   }

   return optErrorCode;
}


/*    Wrapper function to get solution   */

int ls_get_solution(pLSmodel pModel, double *primal, double *dual, double *dObj)
{
  int nErrorCode = LSERR_NO_ERROR;

  if (is_mip> 0) {
    nErrorCode = LSgetInfo(pModel,LS_DINFO_MIP_OBJ,dObj);
    if (nErrorCode != 0) return nErrorCode;
    nErrorCode = LSgetMIPDualSolution( pModel,dual);
    if (nErrorCode != 0) return nErrorCode;
    nErrorCode = LSgetMIPPrimalSolution( pModel,primal);
    if (nErrorCode != 0) return nErrorCode;
  } else {
    nErrorCode = LSgetPrimalSolution( pModel, primal) ;
    if (nErrorCode != 0) return nErrorCode;
    nErrorCode = LSgetDualSolution( pModel, dual) ;
    if (nErrorCode != 0) return nErrorCode;
    nErrorCode = LSgetInfo(pModel,LS_DINFO_POBJ,dObj);
    if (nErrorCode != 0) return nErrorCode;
  }

  return nErrorCode;
}




int main(int argc, char **argv)
{
   APIERRORSETUP;

   char *mpsfile, *prifile = NULL;
   double dObj;
   int status, optErrorCode;
   UserData progressData;

/* declare an instance of the LINDO environment object */
   pLSenv pEnv = NULL;
/* declare an instance of the LINDO model object */
   pLSmodel pModel = NULL;

   char MY_LICENSE_KEY[1024];

  /****************************************************************
   * Init: Command prompt calling sequence
   ****************************************************************/
   if (argc == 1) {
     printf("\nUsage: ex_mps filename [prifilename]\n\n");
     goto ErrReturn;
   } else if (argc == 2) {
     mpsfile = argv[1];
   } else if(argc == 3) {
     mpsfile = argv[1];
     prifile = argv[2];
   }

  /****************************************************************
   * Step 1: Create a LINDO environment.
   ****************************************************************/
   nErrorCode = LSloadDefaultLicenseString(MY_LICENSE_KEY);
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
   //nErrorCode = LSreadMPIFile(pModel,mpsfile);
   nErrorCode = LSreadMPSFile(pModel,mpsfile,LS_FORMATTED_MPS);
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
     }else {
       printf(" LINDO format OK!. Exporting in MPS format...\n\n");
       {
         char strbuf[255], *ptr;
         strcpy(strbuf,mpsfile);
         ptr = strbuf + strlen(strbuf);
         while (*ptr != '.') ptr--;
         strcpy(ptr,"_ltx.mps");
         nErrorCode = LSwriteMPSFile(pModel,strbuf,LS_UNFORMATTED_MPS);
       }
     }
   } else {
     printf(" MPS format OK!. Exporting in LINDO format...\n\n");
     {
       char strbuf[255], *ptr;
       strcpy(strbuf,mpsfile);
       ptr = strbuf + strlen(strbuf);
       while (*ptr != '.') ptr--;
       strcpy(ptr,"_mps.ltx");
       nErrorCode = LSwriteLINDOFile(pModel,strbuf);
     }
   }



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
   status = LS_STATUS_UNKNOWN;
   LSsetModelDouParameter(pModel,LS_DPARAM_CALLBACKFREQ,0.5);
   //LSsetModelIntParameter(pModel,LS_IPARAM_MIP_TOPOPT,1);
   //LSsetModelIntParameter(pModel,LS_IPARAM_MIP_PRELEVEL,0);
   //LSsetModelIntParameter(pModel,LS_IPARAM_MIP_ITRLIM,100);
   //LSsetModelIntParameter(pModel,LS_IPARAM_NLP_ITRLMT,2000);
   //LSsetModelIntParameter(pModel,LS_IPARAM_SPLEX_ITRLMT,2000);
   //LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRINTLEVEL,2);

   // Enable this block to set number of parallel threads > 1
   if (0>1) {
     // Threads
     nErrorCode = LSsetModelIntParameter(pModel,LS_IPARAM_NUM_THREADS,2);
     APIERRORCHECK;

      // Multithreading mode
     LSsetModelIntParameter(pModel,LS_IPARAM_MULTITHREAD_MODE,LS_MTMODE_FREE);
	// LS_MTMODE_FREE = -1, solver decides
	// LS_MTMODE_PPCC = 1, try parallel mode (PP), but if it is not available try concurrent mode (CC)
	// LS_MTMODE_PP   = 2, try parallel mode (PP) only,
	// LS_MTMODE_CCPP = 3, try concurrent mode (CC), but if it is not available try parallel mode (PP)
	// LS_MTMODE_CC   = 4, try concurrent mode (CC) only
   }

   memset(&progressData,0,sizeof(UserData));
   progressData.pModel = pModel;
   progressData.elapsed = LSgetTic();
   /* Install a log function to display solver's progress
   as reported by the internal solver */
   if (0>1) {
     nErrorCode = LSsetModelLogfunc(pModel, (printModelLOG_t) print_line_log, &progressData);
   } else {
   /* Install a callback function to display solver's progress
   as specified by the user */
     nErrorCode = LSsetCallback(pModel,(cbFunc_t) print_progress_log, &progressData);
   }

   /*  Use GOP solver by uncommenting the following line */
   use_gop = 1;
   progressData.major_solver = LSLOC_GOP;

   // Engage the solver
   optErrorCode = ls_solve_model(pModel,&status);

   nErrorCode = optErrorCode;
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

     nErrorCode = ls_get_solution(pModel, primal, dual, &dObj);
     APIERRORCHECK;

     /* Retrieve variable and constraint names */
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

     printf ("\n Model Title: %s\n Objective at solution = %f (status=%d, err=%d)\n",
       pszTitle, dObj,status, optErrorCode);

     // uncomment the block below if you would like the primal and dual solutions
     // to be printed on the screen.

     if (0>1)
     {
        double *adDecRhs=NULL, *adIncRhs=NULL; // RHS ranges
        double *adDecObj=NULL, *adIncObj=NULL; // Obj ranges
        double *adDecBnd=NULL, *adIncBnd=NULL; // Bound ranges

        adDecRhs = (double *) malloc(m*sizeof(double));
        adDecObj = (double *) malloc(n*sizeof(double));
        adIncRhs = (double *) malloc(m*sizeof(double));
        adIncObj = (double *) malloc(n*sizeof(double));
        adIncBnd = (double *) malloc(n*sizeof(double));
        adDecBnd = (double *) malloc(n*sizeof(double));


        nErrorCode = LSgetConstraintRanges(pModel,adDecRhs,adIncRhs);
        APIERRORCHECK;

        nErrorCode = LSgetObjectiveRanges(pModel,adDecObj,adIncObj);
        APIERRORCHECK;

        nErrorCode = LSgetBoundRanges(pModel,adDecBnd,adIncBnd);
        APIERRORCHECK;

        printf ("\n Primal Solution\n");
        printf("\t%8s %18s %18s %18s %18s %18s\n","VARS", "Primal","Obj Dec","Obj Inc","Bnd Dec","Bnd Inc");
        for (j = 0; j<n; j++)
        {
          nErrorCode = LSgetVariableNamej(pModel,j,varname);
          nameptr = paszVarnames[j];
          printf("\t%8s %18.10e %18.10e %18.10e %18.10e %18.10e\n",nameptr, primal[j],adDecObj[j],adIncObj[j],adDecBnd[j],adIncBnd[j]);
        }

        printf ("\n Dual Solution\n");
        printf("\t%8s %18s %18s %18s\n","CONS", "Dual","RHS Dec","RHS Inc");
        for (j = 0; j<m; j++)
        {
          nErrorCode = LSgetConstraintNamei(pModel,j,varname);
          nameptr = paszConnames[j];
          printf("\t%8s %18.10e %18.10e %18.10e\n",nameptr, dual[j],adDecRhs[j],adIncRhs[j]);
        }

        free(adDecRhs); free(adDecObj);
        free(adIncRhs); free(adIncObj);
        free(adDecBnd); free(adIncBnd);
     }



     free(primal);
     free(dual);
     free(pachVarNameData);
     free(pachConNameData);
     free(paszConnames);
     free(paszVarnames);
   }
   else if (status == LS_STATUS_UNBOUNDED || status == LS_STATUS_INFORUNB)
   {
       int     pdObjSense;
       double  pdObjConst;
       double  *padC = malloc(sizeof(double)*n);
       double  *padB = malloc(sizeof(double)*m);
       char    *pachConTypes = malloc(sizeof(char)*m);
       int     *paiAcols = malloc(sizeof(int)*n+1);
       int     *pacAcols = NULL;
       double  *padAcoef = malloc(sizeof(double)*nz);
       int     *paiArows = malloc(sizeof(int)*nz);
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

       nErrorCode = LSoptimize( pModel, LS_METHOD_PSIMPLEX, &status);
       if (status == LS_STATUS_UNBOUNDED)
         findXray(pEnv,pModel,n,m,pdObjSense,paiAcols,paiArows,padAcoef,pachConTypes);

   }
   else
   {
     char strbuf[255];
     LSgetErrorMessage(pEnv,nErrorCode,strbuf);
     printf ("\n Optimization failed. Status = %d ",status);
     printf ("\n Error %d: %s\n",nErrorCode,strbuf);
   }

ErrReturn:

   /***************************************************************
    * Step 7: Terminate
    ***************************************************************/
   nErrorCode = LSdeleteModel( &pModel);
   nErrorCode = LSdeleteEnv( &pEnv);

  /* Wait until user presses the Enter key */
   printf("Press <Enter> ...");
   getchar();

}
