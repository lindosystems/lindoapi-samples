/*
###################################################################
#                       LINDO-API
#                    Sample Programs
#
#                  Copyright (c) 2006
#
#         LINDO Systems, Inc.           312.988.7422
#         1415 North Dayton St.         info@lindo.com
#         Chicago, IL 60622             http://www.lindo.com
###################################################################

  File   : ex_iis.ox
  Purpose: Analyzing an infeasible LP using the LSfindIIS() function.
*/


#include <oxstd.h>

/* LINDO API header file is located under lindoapi\ox */
#import <packages/lindoapi/ox/oxlindo>

MyCallback( const pMod, const nLocation, const pMyData)
{

/* Display current iteration count and objective value */
   {
      decl nIter;
      decl dObj, dInf;
      LSgetCallbackInfo(pMod,nLocation,LS_IINFO_SIM_ITER,&nIter);
      LSgetCallbackInfo(pMod,nLocation,LS_DINFO_POBJ,&dObj);
      LSgetCallbackInfo(pMod,nLocation,LS_DINFO_PINFEAS,&dInf);
      /*
      println( "In MyCallback Location=", nLocation, ": Iters=", nIter, ", Obj=", dObj, " Pinf=", dInf);
      */
   }

   return( 0);
}

 /* main entry point */
main()
{
   decl nErrorCode;
   /* model data objects */
   decl n; /* number of variables */
   decl m; /* number of constraints */
   decl solstatus;/*solution status (see lindo.h for possible values)*/
   decl prep_level;
   decl mpsfile = "";

   /* IIS related data objects */
   decl nLevel,   /* level of analysis */
       nSuf_r,   /* number of sufficient rows     */
       nSuf_c,   /* number of sufficient columns  */
       nIIS_r,   /* number of rows in the IIS     */
       nIIS_c;   /* number of columns in the IIS  */
   decl aiRows = <>, /* index set of rows in the IIS     */
       aiCols = <>, /* index set of columns in the IIS  */
       anBnds = <>; /* bound type of columns in the IIS */
   decl j;
   decl bndtype, oufname, varname;

   /* declare an instance of the LINDO environment object */
   decl pEnv;
   /* declare an instance of the LINDO model object */
   decl pModel;
   /***************************************************************
    * Init: Command prompt calling sequence
    ***************************************************************/
   decl argv = arglist();
   decl argc = sizeof(argv);
   if (argc == 1)
   {
     print("\nUsage: oxl ex_iis filename\n\n");
     exit(0);
   }
   else if (argc == 2)
   {
     mpsfile = argv[1];
   }
  /*****************************************************************
   * Step 1: Create a LINDO environment. MY_LICENSE_KEY in licence.h
   * must be defined using the key shipped with your software.
   *****************************************************************/
   pEnv = OxLScreateEnv();

   /***************************************************************
    * Step 2: Create a model in the environment.
    ***************************************************************/
   pModel = LScreateModel ( pEnv, &nErrorCode);
   LSerrorCheck(pEnv, nErrorCode);

   /***************************************************************
    * Step 3: Read the model from an MPS file and
    ***************************************************************/
   nErrorCode = LSreadMPSFile(pModel,mpsfile,LS_UNFORMATTED_MPS);
   if (nErrorCode != LSERR_NO_ERROR)
   {
     print("\n\tBad MPS format... Trying LINDO format.");
     nErrorCode = LSreadLINDOFile(pModel,mpsfile);
     LSerrorCheck(pEnv, nErrorCode);
     print("\n\tLINDO format OK!\n\n");
   }
   else
   {
     print("\n\tMPS format OK!\n\n");
   }
   nErrorCode = LSgetInfo(pModel, LS_IINFO_NUM_VARS, &n);
   LSerrorCheck(pEnv, nErrorCode);

   nErrorCode = LSgetInfo(pModel, LS_IINFO_NUM_CONS, &m);
   LSerrorCheck(pEnv, nErrorCode);

   /***************************************************************
    * Step 4: Set Model parameters
    ***************************************************************/
   /* Turn off the LP preprocessor. This is required if the model
   is infeasible and the user wishes to debug it. */
   nErrorCode = LSgetModelIntParameter(pModel,LS_IPARAM_LP_PRELEVEL,
     &prep_level);
   LSerrorCheck(pEnv, nErrorCode);
   if (prep_level > 0)
     print("\tThe LP presolver is ON. Turning it OFF...\n\n");

   nErrorCode = LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRELEVEL,0);
   LSerrorCheck(pEnv, nErrorCode);

   nErrorCode = LSsetCallback( pModel, MyCallback, 0);
   LSerrorCheck(pEnv, nErrorCode);


   /***************************************************************
    * Step 5: Optimize the model
    ***************************************************************/
   nErrorCode = LSoptimize( pModel,LS_METHOD_FREE, &solstatus);
   LSerrorCheck(pEnv, nErrorCode);

    if (solstatus == LS_STATUS_INFEASIBLE)
    {
      print("\tThe model is infeasible. Debugging with LSfindIIS()..\n\n");
    }
    else if (solstatus == LS_STATUS_UNBOUNDED)
    {
      print("\tThe model is unbounded. Debug using LSfindIUS()...\n\n");
    }
    else if (solstatus == LS_STATUS_BASIC_OPTIMAL)
    {
      print("\tThe model is solved to optimality.\n");
    }

    nErrorCode = LSgetInfo(pModel,LS_IINFO_STATUS, &solstatus);
    LSerrorCheck(pEnv, nErrorCode);

   /***************************************************************
    * Step 6: Invoke LSfindIIS() if infeasible
    ***************************************************************/
    if (solstatus == LS_STATUS_INFEASIBLE)
    {
      oufname = "findiis.ltx";

      /*** Step 6.1: Find IIS ***/
      nErrorCode = LSfindIIS(pModel,LS_NECESSARY_ROWS +
                                    LS_NECESSARY_COLS +
                                    LS_SUFFICIENT_ROWS);
      LSerrorCheck(pEnv, nErrorCode);

	  nErrorCode = LSgetIIS(pModel,&nSuf_r,&nIIS_r,&aiRows,
                                   &nSuf_c,&nIIS_c,&aiCols,&anBnds);
      LSerrorCheck(pEnv, nErrorCode);

	  println("\t ***  LSfindIIS Summary ***\n");
      println("\t Number of Sufficient Rows = ",nSuf_r);
      println("\t Number of Sufficient Cols = ",nSuf_c);
      println("\t Number of Necessary  Rows = ",nIIS_r - nSuf_r);
      println("\t Number of Necessary  Cols = ",nIIS_c - nSuf_c);
      print("\n");

      /*** Step 6.2: Display row index sets ***/
      print("\n IIS Rows\n");
      for (j=0; j<nIIS_r; j++)
      {
        nErrorCode = LSgetConstraintNamei(pModel,aiRows[j],&varname);
        LSerrorCheck(pEnv, nErrorCode);

        if (j<nSuf_r)
          print("%2d", j, "] (", "%-8s", varname, ") is"
          " in the sufficient set.\n");
        else
          print("%2d", j, "] (", "%-8s", varname, ") is"
          " in the necessary set.\n");
      }

      /*** Step 6.3: Display column index sets ***/
      print("\n IIS Column Bounds\n");
      for (j=0; j<nIIS_c; j++)
      {
        if (anBnds[j] > 0)
          bndtype = "Lower";
        else
          bndtype = "Upper";

        nErrorCode = LSgetVariableNamej(pModel,aiCols[j],&varname);
        LSerrorCheck(pEnv, nErrorCode);
        if (j<nSuf_r)
          print("%2d", j, "] ", bndtype, " bound of (", "%-8s", varname, ") is"
          " in the sufficient set.\n");
        else
          print("%2d", j, "] ", bndtype, " bound of (", "%-8s", varname, ") is"
          " in the necessary set.\n");
      }

      LSwriteIIS(pModel,oufname);
      print("\n\n\t IIS is written to ", oufname, " !!\n");

    }

 /*****************************************************************
  * Step 7: Terminate
  *****************************************************************/
   nErrorCode = LSdeleteModel( &pModel);

   nErrorCode = LSdeleteEnv( &pEnv);
}
