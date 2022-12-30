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

  File   : ex_mt1.c

  Purpose: Create multiple threads over a single Lindo environment
    object (pLSenv) reading random models from a directory in MPS/MPI
    format. Each thread is then optimized with solve().
*/


#include <windows.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <conio.h>
#include <process.h>

/* LINDO API header file */
#include "lindo.h"

typedef struct
{
  int     val;
  char    fname[255];
  pLSenv  env;
} ARGS;

#include "ex_mps.c"

#if 1
int inputfiles_cnt = 3;
char *inputfiles[]=
{
  "bm23.ltx","testgop.mpi","testgop.mpi"
};
#else
int inputfiles_cnt = 96;
char *inputfiles[]=
{
"25fv47.mps.gz","boeing1.mps.gz","e226.mps.gz","greenbeb.mps.gz","pilot.mps.gz","scagr25.mps.gz","sctap3.mps.gz","stair.mps.gz",
"80bau3b.mps.gz","boeing2.mps.gz","etamacro.mps.gz","grow15.mps.gz","pilot4.mps.gz","scagr7.mps.gz","seba.mps.gz","standata.mps.gz",
"adlittle.mps.gz","bore3d.mps.gz","fffff800.mps.gz","grow22.mps.gz","pilot87.mps.gz","scfxm1.mps.gz","share1b.mps.gz","standgub.mps.gz",
"afiro.mps.gz","brandy.mps.gz","finnis.mps.gz","grow7.mps.gz","pilot_ja.mps.gz","scfxm2.mps.gz","share2b.mps.gz","standmps.mps.gz",
"agg.mps.gz","capri.mps.gz","fit1d.mps.gz","israel.mps.gz","pilot_we.mps.gz","scfxm3.mps.gz","shell.mps.gz","stocfor1.mps.gz",
"agg2.mps.gz","cycle.mps.gz","fit1p.mps.gz","kb2.mps.gz","pilotnov.mps.gz","scorpion.mps.gz","ship04l.mps.gz","stocfor2.mps.gz",
"agg3.mps.gz","czprob.mps.gz","fit2d.mps.gz","lotfi.mps.gz","qap08.mps.gz","scrs8.mps.gz","ship04s.mps.gz","stocfor3.mps.gz",
"bandm.mps.gz","d2q06c.mps.gz","fit2p.mps.gz","maros-r7.mps.gz","recipe.mps.gz","scsd1.mps.gz","ship08l.mps.gz","truss.mps.gz",
"beaconfd.mps.gz","d6cube.mps.gz","forplan.mps.gz","maros.mps.gz","sc105.mps.gz","scsd6.mps.gz","ship08s.mps.gz","tuff.mps.gz",
"blend.mps.gz","degen2.mps.gz","ganges.mps.gz","modszk1.mps.gz","sc205.mps.gz","scsd8.mps.gz","ship12l.mps.gz","vtp_base.mps.gz",
"bnl1.mps.gz","degen3.mps.gz","gfrd-pnc.mps.gz","nesm.mps.gz","sc50a.mps.gz","sctap1.mps.gz","ship12s.mps.gz","wood1p.mps.gz",
"bnl2.mps.gz","dfl001.mps.gz","greenbea.mps.gz","perold.mps.gz","sc50b.mps.gz","sctap2.mps.gz","sierra.mps.gz","woodw.mps.gz"
};
#endif
#define APIVERSION \
{\
    char szVersion[255], szBuild[255];\
    LSgetVersionInfo(szVersion,szBuild);\
    printf("\nLINDO API Version %s built on %s\n",szVersion,szBuild);\
}\



int solve(pLSenv pEnv, char *mpsfile, int threadID, void * user);


/****************************************************************
 *    Child thread
 */
void ThreadProc(void *param)
{
  ARGS *h        = ((ARGS*)param);
  int nErrorCode;

  printf("\nSolving %s on thread %d\n",h->fname,h->val);
  nErrorCode = solve(h->env,h->fname,h->val,h);

  _endthread();

}


/****************************************************************
 *      Main thread
 */
int main(int argc, char **argv)
{

  int nthreads, nErrorCode;
  int i,k;
  int val = 0;
  HANDLE *handle;
  ARGS *args;
  char MY_LICENSE_KEY[1024], *inputpath;
  pLSenv pEnv = NULL;

  if (argc != 3) {
    printf("\nUsage: ex_mt1 inputpath [nthreads]\n\n");
    return 0;
  }
  inputpath = argv[1];

  /*
   *      Create a LINDO Environment.
   */
  nErrorCode = LSloadLicenseString("../../../license/lndapi130.lic",MY_LICENSE_KEY);
   if ( nErrorCode != LSERR_NO_ERROR)
   {
      printf( "Failed to load license key (error %d)\n",nErrorCode);
      goto ErrorReturn;
   }

  APIVERSION;
  pEnv = LScreateEnv ( &nErrorCode, MY_LICENSE_KEY);
  if ( nErrorCode != LSERR_NO_ERROR)
  {
     printf( "Failed to create env (error %d)\n",nErrorCode);
     goto ErrorReturn;
  }

  nthreads = atoi(argv[2]);

  /*
   *    Create threads and solve
   */
  handle = (HANDLE *) malloc(sizeof(HANDLE)*nthreads);
  args = malloc(sizeof(ARGS)*nthreads);
  srand(1091);
  for(i=0;i<nthreads;i++)
  {
    val = i+1;
    args[i].val = val;

    /* pick a random model for this thread */
    k = (int) (inputfiles_cnt*(rand()/(RAND_MAX*1.0)));
    sprintf(args[i].fname,"%s/%s",inputpath,inputfiles[k]);
    args[i].env = pEnv;

    // create i^th child thread
    handle[i] = (HANDLE) _beginthread( ThreadProc,0,&args[i]);
  }

  WaitForMultipleObjects(nthreads,handle,1,INFINITE);
ErrorReturn:
  free(handle);
  free(args);

  LSdeleteEnv(&pEnv);

  printf("\n\nPress <Enter> ...");
  getchar();

  return 0;
}






