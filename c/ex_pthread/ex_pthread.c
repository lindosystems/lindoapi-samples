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

  File   : ex_pthread.c

  Purpose: Create multiple threads over a single Lindo environment
    object (pLSenv) reading random models from a directory in MPS/MPI
    format. Each thread is then optimized with solve().
*/


#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#ifndef _MSC_VER
#include <pthread.h>


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
// Simply extend this list by adding more files
int inputfiles_cnt = 3;
char *inputfiles[]=
{
  "testgop.mpi","testgop.mpi","testgop.mpi"
};

#else
int inputfiles_cnt = 96;
char *inputfiles[]=
{
"25fv47.mps","boeing1.mps","e226.mps","greenbeb.mps","pilot.mps","scagr25.mps","sctap3.mps","stair.mps",
"80bau3b.mps","boeing2.mps","etamacro.mps","grow15.mps","pilot4.mps","scagr7.mps","seba.mps","standata.mps",
"adlittle.mps","bore3d.mps","fffff800.mps","grow22.mps","pilot87.mps","scfxm1.mps","share1b.mps","standgub.mps",
"afiro.mps","brandy.mps","finnis.mps","grow7.mps","pilot_ja.mps","scfxm2.mps","share2b.mps","standmps.mps",
"agg.mps","capri.mps","fit1d.mps","israel.mps","pilot_we.mps","scfxm3.mps","shell.mps","stocfor1.mps",
"agg2.mps","cycle.mps","fit1p.mps","kb2.mps","pilotnov.mps","scorpion.mps","ship04l.mps","stocfor2.mps",
"agg3.mps","czprob.mps","fit2d.mps","lotfi.mps","qap08.mps","scrs8.mps","ship04s.mps","stocfor3.mps",
"bandm.mps","d2q06c.mps","fit2p.mps","maros-r7.mps","recipe.mps","scsd1.mps","ship08l.mps","truss.mps",
"beaconfd.mps","d6cube.mps","forplan.mps","maros.mps","sc105.mps","scsd6.mps","ship08s.mps","tuff.mps",
"blend.mps","degen2.mps","ganges.mps","modszk1.mps","sc205.mps","scsd8.mps","ship12l.mps","vtp_base.mps",
"bnl1.mps","degen3.mps","gfrd-pnc.mps","nesm.mps","sc50a.mps","sctap1.mps","ship12s.mps","wood1p.mps",
"bnl2.mps","dfl001.mps","greenbea.mps","perold.mps","sc50b.mps","sctap2.mps","sierra.mps","woodw.mps"
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
void *ThreadProc(void *param)
{
  ARGS *h        = ((ARGS*)param);
  int nErrorCode;

  printf("\nSolving %s on thread %d\n",h->fname,h->val);
  nErrorCode = solve(h->env,h->fname,h->val,h);

}


/****************************************************************
 *      Main thread
 */
int main(int argc, char **argv)
{

  int NTHREADS, nErrorCode;
  int i,k;
  int val = 0;
  pthread_t *threads;
  ARGS *args;
  char MY_LICENSE_KEY[1024], *inputpath;
  pLSenv pEnv = NULL;

  APIVERSION;
  if (argc != 3) {
    printf("\nUsage: ex_pthread inputpath [NTHREADS]\n\n");
    return 0;
  }
  inputpath = argv[1];


  /*
   *      Create a LINDO Environment.
   */
  nErrorCode = LSloadLicenseString("../../../license/lndapi150.lic",MY_LICENSE_KEY);
   if ( nErrorCode != LSERR_NO_ERROR)
   {
      nErrorCode = LSloadLicenseString("../../../../license/lndapi150.lic",MY_LICENSE_KEY);
      if ( nErrorCode != LSERR_NO_ERROR)
      {
        printf( "Failed to load license key (error %d)\n",nErrorCode);
        exit( 1);
      }
   }

  pEnv = LScreateEnv ( &nErrorCode, MY_LICENSE_KEY);
  if ( nErrorCode != LSERR_NO_ERROR)
  {
     printf( "Failed to create env (error %d)\n",nErrorCode);
     exit( 1);
  }

  /*
   *    Parse number of threads
   */
  NTHREADS = atoi(argv[2]);


  /*
   *    Create threads and solve
   */
  threads = (pthread_t *) malloc(sizeof(pthread_t)*NTHREADS);
  args = malloc(sizeof(ARGS)*NTHREADS);
  srand(1091);
  for(i=0;i<NTHREADS;i++)
  {
    val = i+1;
    args[i].val = val;

    /* pick a random model for this thread */
    k = (int) (inputfiles_cnt*(rand()/(RAND_MAX*1.0)));
    sprintf(args[i].fname,"%s/%s",inputpath,inputfiles[k]);
    args[i].env = pEnv;

    // create i^th child thread
    pthread_create(&threads[i], NULL, ThreadProc,&args[i]);
  }

  for(i=0;i<NTHREADS;i++)
  	pthread_join( threads[i], NULL);

  free(threads);
  free(args);

  LSdeleteEnv(&pEnv);

  printf("\n\nPress <Enter> ...");
  getchar();

  return 0;
}
#else
int main(int argc, char **argv)
{
	printf("\nERROR: Platform not supported\n");
  	printf("\n\nPress <Enter> ...");
  	getchar();
	return 0;
}
#endif