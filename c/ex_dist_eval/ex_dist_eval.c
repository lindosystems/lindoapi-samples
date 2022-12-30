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

  File   : ex_dist_eval.c (distribution evaluation)

  Purpose: Evaluate functions of various distributions at a given point.

*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <assert.h>

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
    printf("\nLINDO API Version %s built on %s\n\n",szVersion,szBuild);\
}\

#ifdef _MSC_VER
/* ignore MSVC++ warnings that are annoying and hard to remove:
 4702 unreachable code
 (there is an unreachable assert(0) in case somehow it is reached)
 */
#pragma warning( disable : 4702 )
#endif

#define OPTERRCOLON (1)
#define OPTERRNF (2)
#define OPTERRARG (3)

char *optarg;
int optreset = 0;
int optind = 1;
int opterr = 1;
int optopt;

static int
optiserr(int argc, char * const *argv, int oint, const char *optstr,
         int optchr, int err)
{
    (void) argc;  /* not used */
    (void) optstr; /* not used */
    if(opterr)
    {
        fprintf(stderr, "Error in argument %d, char %d: ", oint, optchr+1);
        switch(err)
        {
        case OPTERRCOLON:
            fprintf(stderr, ": in flags\n");
            break;
        case OPTERRNF:
            fprintf(stderr, "option not found %c\n", argv[oint][optchr]);
            break;
        case OPTERRARG:
            fprintf(stderr, "no argument for option %c\n", argv[oint][optchr]);
            break;
        default:
            fprintf(stderr, "unknown\n");
            break;
        }
    }
    optopt = argv[oint][optchr];
    return('?');
}



int
getopt(int argc, char* const *argv, const char *optstr)
{
    static int optchr = 0;
    static int dash = 0; /* have already seen the - */

    char *cp;

    if (optreset)
        optreset = optchr = dash = 0;
    if(optind >= argc)
        return(EOF);
    if(!dash && (argv[optind][0] !=  '-'))
        return(EOF);
    if(!dash && (argv[optind][0] ==  '-') && !argv[optind][1])
    {
        /*
         * use to specify stdin. Need to let pgm process this and
         * the following args
         */
        return(EOF);
    }
    if((argv[optind][0] == '-') && (argv[optind][1] == '-'))
    {
        /* -- indicates end of args */
        optind++;
        return(EOF);
    }
    if(!dash)
    {
        assert((argv[optind][0] == '-') && argv[optind][1]);
        dash = 1;
        optchr = 1;
    }

    /* Check if the guy tries to do a -: kind of flag */
    assert(dash);
    if(argv[optind][optchr] == ':')
    {
        dash = 0;
        optind++;
        return(optiserr(argc, argv, optind-1, optstr, optchr, OPTERRCOLON));
    }
    cp = strchr(optstr, argv[optind][optchr]);
    if(!cp)
    {
        int errind = optind;
        int errchr = optchr;

        if(!argv[optind][optchr+1])
        {
            dash = 0;
            optind++;
        }
        else
            optchr++;
        return(optiserr(argc, argv, errind, optstr, errchr, OPTERRNF));
    }
    if(cp[1] == ':')
    {
        dash = 0;
        optind++;
        if(optind == argc)
            return(optiserr(argc, argv, optind-1, optstr, optchr, OPTERRARG));
        optarg = argv[optind++];
        return(*cp);
    }
    else
    {
        if(!argv[optind][optchr+1])
        {
            dash = 0;
            optind++;
        }
        else
            optchr++;
        return(*cp);
    }
    assert(0);
    return(0);
}

#define  DIST_TYPE_BETA  "be"
#define  DIST_TYPE_BINOMIAL  "bi"
#define  DIST_TYPE_CAUCHY  "ca"
#define  DIST_TYPE_CHI_SQUARE  "ch"
#define  DIST_TYPE_EXPONENTIAL  "ex"
#define  DIST_TYPE_F_DISTRIBUTION  "fd"
#define  DIST_TYPE_GAMMA  "ga"
#define  DIST_TYPE_GEOMETRIC   "ge"
#define  DIST_TYPE_GUMBEL   "gu"
#define  DIST_TYPE_HYPER_GEOMETRIC  "hg"
#define  DIST_TYPE_LAPLACE   "la"
#define  DIST_TYPE_LOGNORMAL  "ln"
#define  DIST_TYPE_LOGARITHMIC  "lg"
#define  DIST_TYPE_LOGISTIC   "lo"
#define  DIST_TYPE_NEGATIVE_BINOMIAL  "nb"
#define  DIST_TYPE_NORMAL   "no"
#define  DIST_TYPE_PARETO   "pa"
#define  DIST_TYPE_POISSON   "po"
#define  DIST_TYPE_STABLE_PARETIAN  "sp"
#define  DIST_TYPE_STUDENTS_T   "st"
#define  DIST_TYPE_TRIANGULAR   "tr"
#define  DIST_TYPE_UNIFORM   "un"
#define  DIST_TYPE_WEIBULL  "wb"
#define  DIST_TYPE_WILCOXON   "wx"

typedef struct t_opts
{
  int nFuncType;
  int nDistType;
  int nSampType;
  char szDistName[16];
  double dargs[10];
  int nargs;
  int nPoints;
  double x;
  double inc;
} t_opts;

int parseopt (int argc, char **argv, t_opts *optList)
 {
      int tsum=0;
      extern char *optarg;
      extern int optind;
      int errflg = 0, nstg=0, errorcode=LSERR_NO_ERROR;
      char *ofile = NULL, *ptr=NULL;
      char seps[]   = ",", c, strbuf[255], *token;
      double *dargs=optList->dargs;
      int k=0;

      while ((c = getopt(argc, argv, "d:p:f:n:x:m:i:")) != EOF)
      {
           switch (c)
           {
           case 'd':
                printf("option distribution: %s\n",optarg);

                // default distribution type is normal(0,1)
                optList->nDistType = LSDIST_TYPE_NORMAL;
                dargs[0] = 0; // standard normal mean
                dargs[1] = 1; // standard normal stdev
                strcpy(optList->szDistName,optarg);

                if (!strcmp(optarg,DIST_TYPE_BETA))
                {
                  optList->nDistType = LSDIST_TYPE_BETA;
                }
                else if (!strcmp(optarg,DIST_TYPE_BINOMIAL))
                {
                  optList->nDistType = LSDIST_TYPE_BETA;
                }
                else if (!strcmp(optarg,DIST_TYPE_CAUCHY))
                {
                  optList->nDistType = LSDIST_TYPE_CAUCHY;
                }
                else if (!strcmp(optarg,DIST_TYPE_CHI_SQUARE))
                {
                  optList->nDistType = LSDIST_TYPE_CHI_SQUARE;
                }
                else if (!strcmp(optarg,DIST_TYPE_EXPONENTIAL))
                {
                  optList->nDistType = LSDIST_TYPE_EXPONENTIAL;
                }
                else if (!strcmp(optarg,DIST_TYPE_F_DISTRIBUTION))
                {
                  optList->nDistType = LSDIST_TYPE_F_DISTRIBUTION;
                }
                else if (!strcmp(optarg,DIST_TYPE_GAMMA))
                {
                  optList->nDistType = LSDIST_TYPE_GAMMA;
                }
                else if (!strcmp(optarg,DIST_TYPE_GEOMETRIC))
                {
                  optList->nDistType = LSDIST_TYPE_GEOMETRIC;
                }
                else if (!strcmp(optarg,DIST_TYPE_GUMBEL))
                {
                  optList->nDistType = LSDIST_TYPE_GUMBEL;
                }
                else if (!strcmp(optarg,DIST_TYPE_HYPER_GEOMETRIC))
                {
                  optList->nDistType = LSDIST_TYPE_HYPER_GEOMETRIC;
                }
                else if (!strcmp(optarg,DIST_TYPE_LAPLACE))
                {
                  optList->nDistType = LSDIST_TYPE_LAPLACE;
                }
                else if (!strcmp(optarg,DIST_TYPE_LOGARITHMIC))
                {
                  optList->nDistType = LSDIST_TYPE_LOGARITHMIC;
                }
                else if (!strcmp(optarg,DIST_TYPE_LOGISTIC))
                {
                  optList->nDistType = LSDIST_TYPE_LOGISTIC;
                }
                else if (!strcmp(optarg,DIST_TYPE_NEGATIVE_BINOMIAL))
                {
                  optList->nDistType = LSDIST_TYPE_NEGATIVE_BINOMIAL;
                }
                else if (!strcmp(optarg,DIST_TYPE_NORMAL))
                {
                  optList->nDistType = LSDIST_TYPE_NORMAL;
                }
                else if (!strcmp(optarg,DIST_TYPE_PARETO))
                {
                  optList->nDistType = LSDIST_TYPE_PARETO;
                }
                else if (!strcmp(optarg,DIST_TYPE_POISSON))
                {
                  optList->nDistType = LSDIST_TYPE_POISSON;
                }
                else if (!strcmp(optarg,DIST_TYPE_STABLE_PARETIAN))
                {
                  optList->nDistType = LSDIST_TYPE_STABLE_PARETIAN;
                }
                else if (!strcmp(optarg,DIST_TYPE_STUDENTS_T))
                {
                  optList->nDistType = LSDIST_TYPE_STUDENTS_T;
                }
                else if (!strcmp(optarg,DIST_TYPE_TRIANGULAR))
                {
                  optList->nDistType = LSDIST_TYPE_TRIANGULAR;
                }
                else if (!strcmp(optarg,DIST_TYPE_UNIFORM))
                {
                  optList->nDistType = LSDIST_TYPE_UNIFORM;
                }
                else if (!strcmp(optarg,DIST_TYPE_WEIBULL))
                {
                  optList->nDistType = LSDIST_TYPE_WEIBULL;
                }
                else if (!strcmp(optarg,DIST_TYPE_WILCOXON))
                {
                  optList->nDistType = LSDIST_TYPE_WILCOXON;
                }
                else
                {
                  printf("unknown distribution\n");
                  errflg++;
                }
                break;
           case 'p':

                strcpy(strbuf,optarg);
                ptr = &strbuf[0];
                // Establish string and get the first token:
                token = strtok( ptr, seps ); // C4996
                // Note: strtok is deprecated; consider using strtok_s instead
                k = 0;
                while( token != NULL )
                {
                  dargs[k++] = atof(token);
                  // While there are tokens in "string"
                  printf( "option distribution parameter (%d): %g\n", k, dargs[k-1]);

                  // Get next token:
                  token = strtok( NULL, seps ); // C4996
                }
                optList->nargs = k;
                break;
           case 'f':
                printf("option distribution function: %s.\n",optarg);
                if (!strcmp(optarg,"pdf"))
                {
                  optList->nFuncType = LS_PDF;
                }
                else if (!strcmp(optarg,"cdf"))
                {
                  optList->nFuncType = LS_CDF;
                }
                else if (!strcmp(optarg,"cdfinv"))
                {
                  optList->nFuncType = LS_CDFINV;
                }
                else if (!strcmp(optarg,"pdfdiff"))
                {
                  optList->nFuncType = LS_PDFDIFF;
                }

                break;
           case 'm':
                printf("option sampling method: %s.\n",optarg);
                if (!strcmp(optarg,"latin"))
                {
                  optList->nSampType = LS_LATINSQUARE;
                }
                else if (!strcmp(optarg,"anti"))
                {
                  optList->nSampType = LS_ANTITHETIC;
                }
                else if (!strcmp(optarg,"monte"))
                {
                  optList->nSampType = LS_MONTECARLO;
                }
                else
                {
                  optList->nSampType = LS_MONTECARLO;
                }

                break;
           case 'n':
                printf("option number of points: %s.\n",optarg);
                optList->nPoints = atoi(optarg);
                break;
           case 'i':
                printf("option increment of points: %s.\n",optarg);
                optList->inc = atof(optarg);
                break;
           case 'x':
                printf("option initial point: %s.\n",optarg);
                optList->x = atof(optarg);
                break;
           case '?':
                errflg++;
           }
      }

      if (errflg)
      {
           (void)fprintf(stderr,
                "\nusage: ex_dist_eval  [-?|-d <name> |-p <a1,a2,..,ak> |-f <n>]\n\n"
                "             -?              display options.                            \n"
                "             -d              distribution name (no(0,1))                 \n"
                "             -p              parameter list (comma delimited)            \n"
                "             -x              point to evaluate the function (0.0)        \n"
                "             -i              point to evaluate the function (1.e-2)      \n"
                "             -n              number of points with increments di (10)    \n"
                "             -f <n>          distribution function (cdf)                 \n"
                "             -m              sampling type (latin,anti,monte)            \n"
                "  Example:                                                               \n"
                "  ex_dist_eval -d no -p 30,5                                                 \n"
                "  ex_dist_eval -d lo -p 0.3                                                  \n"
                );
           errorcode = LSERR_ERROR_IN_INPUT;
      }
      for ( ; optind < argc; optind++)
           (void)printf("%s\n", argv[optind]);

//ErrorReturn:

       return errorcode;
 }

/*/////////////////////////////////////////////////////////////////////*
 *
 *      Main
 *
 */
int main(int argc, char **argv)
{
   APIERRORSETUP;

    /* Declare an instance of the LINDO environment object */
   pLSenv pEnv = NULL;

    /* Declare instances of the LINDO model object */
   pLSmodel pModel=NULL;

   pLSsample pSample = NULL;

   pLSrandGen pRG = NULL;


   int i=0, j=0;
   t_opts optList;

   double d1, d2, derr, x, *pX=NULL;

   char MY_LICENSE_KEY[1024];


   APIVERSION;

   /*
    *     Parse options.
    */
   memset(&optList,0,sizeof(t_opts));
   optList.nDistType = LSDIST_TYPE_NORMAL;
   strcpy(optList.szDistName,DIST_TYPE_NORMAL);
   optList.nargs = 2;
   optList.dargs[0] = 2;
   optList.dargs[1] = 3;

   optList.nFuncType = LS_CDF;
   optList.nPoints = 10;
   optList.x = -LS_INFINITY;
   optList.nSampType = LS_MONTECARLO;
   optList.inc = 1e-2;

   nErrorCode = parseopt(argc,argv,&optList);
   if (nErrorCode)
     return (1);


   /*
    *     Create a LINDO environment.
    */
   nErrorCode = LSloadLicenseString("../../../license/lndapi130.lic",MY_LICENSE_KEY);

   pEnv = LScreateEnv ( &nErrorCode, MY_LICENSE_KEY);
   if ( nErrorCode == LSERR_NO_VALID_LICENSE) {
      printf( "Invalid License Key!\n");
      exit( 1);
   }
   APIERRORCHECK;


   /*
    *     Create a pLSsample object to manage a given distribution
    */
   pSample = LSsampCreate(pEnv, optList.nDistType, &nErrorCode);
   APIERRORCHECK;


   pRG = LScreateRG(pEnv, LS_RANDGEN_FREE);

   nErrorCode = LSsampSetRG(pSample,pRG);
   APIERRORCHECK;

   LSsetRGSeed(pRG, 1031);


   /*
    *     Set arguments collected by parseopt()
    */
   for (i=0; i<optList.nargs; i++)
   {
     nErrorCode = LSsampSetDistrParam(pSample, i, optList.dargs[i]);
     APIERRORCHECK;
   }

   fprintf(stdout,"\nDistribution %s(",optList.szDistName);
   for (i=0; i<optList.nargs-1; i++)
     fprintf(stdout,"%g,",optList.dargs[i]);
   fprintf(stdout,"%g)\n",optList.dargs[i]);

   if (optList.x <= -LS_INFINITY)
   {
     fprintf(stdout,"\nGenerating %d random variables...\n",optList.nPoints);
     nErrorCode = LSsampGenerate(pSample, LS_LATINSQUARE, optList.nPoints);
     APIERRORCHECK;
     nErrorCode = LSsampGetPointsPtr(pSample,&i,&pX);
     APIERRORCHECK;
   }
   else
   {
     fprintf(stdout,"\nEvaluating %d points starting at %g...\n",optList.nPoints,optList.x);
   }



   /*
    *     Display x, F(x) and F^{-1}(u)
    */
   fprintf(stdout,"\n%3s %13s %13s %13s\n","idx","x","F(x)","F'(u)");
   derr = 0;
   for (i=0; i<optList.nPoints; i++)
   {
     if (optList.x > -LS_INFINITY)
     {
       x = optList.x + i*optList.inc;
     }
     else
     {
       x = pX[i];
     }

     nErrorCode = LSsampEvalDistr(pSample,optList.nFuncType,x,&d1);
     APIERRORCHECK;
     d2 = 0;
     if (optList.nFuncType == LS_CDF)
     {
       nErrorCode = LSsampEvalDistr(pSample,LS_CDFINV,d1,&d2);
       APIERRORCHECK;
       derr += fabs(d2 - x);
     }
     fprintf (stdout,"%3d %13.7f %13.7f %13.7f\n",i,x,d1,d2);
   }
   fprintf (stdout,"\nCDFINV() avr. error: ||x-F'(u)||=%g\n",derr/optList.nPoints);
   if (derr>1e-5)
     fprintf (stdout,"** significant error during CDFINV() **\n",derr);



Terminate:
   if (nErrorCode != LSERR_NO_ERROR)
   {
     char strbuf[255];
     LSgetErrorMessage(pEnv,nErrorCode,strbuf);
     fprintf (stdout,"\n Error %d: %s\n",nErrorCode,strbuf);
   }


   nErrorCode = LSsampDelete( &pSample);
   LSdisposeRG( &pRG);
   nErrorCode = LSdeleteModel( &pModel);
   nErrorCode = LSdeleteEnv( &pEnv);

   /*
    *     Terminate
    */


  /* Wait until user presses the Enter key */
  printf("Press <Enter> ...");
  getchar();

  return 0;
}
