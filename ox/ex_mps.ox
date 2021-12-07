/*
###################################################################
#                       LINDO-API
#                    Sample Programs
#                  Copyright (c) 2006
#
#         LINDO Systems, Inc.           312.988.7422
#         1415 North Dayton St.         info@lindo.com
#         Chicago, IL 60622             http://www.lindo.com
###################################################################

  File   : ex_mps.ox
  Purpose: Read a model from an MPS file and optimize
*/
#include <oxstd.h>

/* LINDO API header file is located under lindoapi\ox */
#import <packages/lindoapi/ox/oxlindo>

/****************************************************************
   Standard callback function to display local and intermediate
   solutions
 ****************************************************************/
print_log(const model, const iLoc, const cbData)
{
  decl iter=0;
  decl pfeas=0.0,pobj=0.0;
  decl bestbnd;
  decl status;

  if (iLoc == LSLOC_MIP)
  {
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_STATUS,&status);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_SIM_ITER,&iter);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_MIP_OBJ,&pobj);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_MIP_BESTBOUND,&bestbnd);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_PINFEAS,&pfeas);
    println("%2d", iLoc, "\tIt=", "%4d", iter, " \tInfeas= ", "%8.3e", pfeas,
		"\tObj=", "%11.5e", pobj, " \tBestBound=", "%11.5e", bestbnd, " \tStat=", status);
  }

  else if ( 0 && (iLoc == LSLOC_PRIMAL || iLoc ==LSLOC_DUAL || iLoc ==LSLOC_CONOPT))
  {
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_STATUS,&status);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_PINFEAS,&pfeas);
    println("%2d", iLoc, "\tIt=", "%4d", iter, " \tInfeas= ", "%8.3e", pfeas,
		"\tObj=", "%11.5e", pobj, " \tBestBound=", "%11.5e", bestbnd, " \tStat=", status);
  }
  return 0;
} /*print_log*/

/* main entry point */
main()
{
   decl nErrorCode;
   decl m, n; /* number of constraints and vars */
   decl nC=0, nB=0, nI=0; /* number of cont, bin. decl vars*/

   decl mpsfile, prifile = "";
   decl dObj;
   decl counter = 0, status;

/* declare an instance of the LINDO environment object */
   decl pEnv;
/* declare an instance of the LINDO model object */
   decl pModel;
  /****************************************************************
   * Init: Command prompt calling sequence
   ****************************************************************/
   decl argv = arglist();
   decl argc = sizeof(argv);
   if (argc == 1)
   {
     print("\nUsage: oxl ex_mps filename [prifilename]\n\n");
     exit(0);
   }
   else if (argc == 2)
   {
     mpsfile = argv[1];
   }
   else if (argc == 3)
   {
     mpsfile = argv[1];
     prifile = argv[2];
   }

  /****************************************************************
   * Step 1: Create a LINDO environment. MY_LICENSE_KEY in licence.h
   * must be defined using the key shipped with your software.
   ****************************************************************/
   pEnv = OxLScreateEnv();

  /****************************************************************
   * Step 2: Create a model in the environment.
   ****************************************************************/
   pModel = LScreateModel ( pEnv, &nErrorCode);
   LSerrorCheck(pEnv, nErrorCode);

  /****************************************************************
   * Step 3: Read the model from an MPS file and get the model size
   ****************************************************************/
   nErrorCode = LSreadMPSFile(pModel,mpsfile,LS_FORMATTED_MPS);
   if (nErrorCode != LSERR_NO_ERROR) {
     print("\n Bad  MPS  format... Trying LINDO format.\n");
     nErrorCode =LSreadLINDOFile(pModel,mpsfile);
     if (nErrorCode != LSERR_NO_ERROR){
       print(" Bad LINDO format... Trying  MPI  format.\n");
       nErrorCode = LSreadMPIFile(pModel,mpsfile);
       if (nErrorCode != LSERR_NO_ERROR){
         print(" Bad  MPI  format... Terminating...\n");
         LSerrorCheck(pEnv, nErrorCode);
       }else{
         print(" MPI format OK!\n\n");
       }
     }else
     {
       print(" LINDO format OK!. Exporting in MPS format...\n\n");
       {
         decl strbuf, ptr;
         strbuf = mpsfile;
		 ptr = strfindr(strbuf, '.');
		 if (ptr > 0)
		 	strbuf[ptr : ] = ".mps";
		 else
		 	strbuf ~= ".mps";
         nErrorCode = LSwriteMPSFile(pModel,strbuf,LS_FORMATTED_MPS);
       }
     }
   } else {
     print(" MPS format OK!. Exporting in LINDO format...\n\n");
     {
       decl strbuf, ptr;
       strbuf = mpsfile;
	   ptr = strfindr(strbuf, '.');
	   if (ptr > 0)
	   	  strbuf[ptr : ] = ".ltx";
	   else
		  strbuf ~= ".ltx";
       nErrorCode = LSwriteLINDOFile(pModel,strbuf);
     }
   }

   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_VARS,&n);
   LSerrorCheck(pEnv, nErrorCode);
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_CONS,&m);
   LSerrorCheck(pEnv, nErrorCode);
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_CONT,&nC);
   LSerrorCheck(pEnv, nErrorCode);
   /***************************************************************
    * Step 4: Read the priority file, if any exists.
    ***************************************************************/
   if (prifile != "") {
     nErrorCode = LSreadVarPriorities(pModel, prifile);
     LSerrorCheck(pEnv, nErrorCode);
   }

   /***************************************************************
    * Step 5: Optimize the model
    ***************************************************************/
   status = LS_STATUS_UNKNOWN;
   LSsetModelDouParameter(pModel,LS_DPARAM_CALLBACKFREQ,2.0);
   //LSsetModelIntParameter(pModel,LS_IPARAM_MIP_TOPOPT,1);
   //LSsetModelIntParameter(pModel,LS_IPARAM_MIP_PRELEVEL,0);
   //LSsetModelIntParameter(pModel,LS_IPARAM_MIP_ITRLIM,100);
   LSsetModelIntParameter(pModel,LS_IPARAM_NLP_ITRLMT,2);
   LSsetModelIntParameter(pModel,LS_IPARAM_LP_ITRLMT,2);

   if (n - nC > 0)
   {
     /* Install a callback function */
     LSsetCallback(pModel, print_log, 0);
     nErrorCode = LSsolveMIP( pModel, &status);
   }
   else
     nErrorCode = LSoptimize( pModel, LS_METHOD_FREE, &status);
   LSerrorCheck(pEnv, nErrorCode);

   /***************************************************************
    * Step 6: Access the final solution if optimal or feasible
    ***************************************************************/
   {
     decl  varname;
     decl  primal = <>, dual = <>;
     decl  j;

     if (n - nC > 0) {
       nErrorCode = LSgetInfo(pModel,LS_DINFO_MIP_OBJ,&dObj);
       LSerrorCheck(pEnv, nErrorCode);
       nErrorCode = LSgetMIPDualSolution( pModel,&dual);
       LSerrorCheck(pEnv, nErrorCode);
       nErrorCode = LSgetMIPPrimalSolution( pModel,&primal);
       LSerrorCheck(pEnv, nErrorCode);
     } else {
       nErrorCode = LSgetPrimalSolution( pModel, &primal) ;
       LSerrorCheck(pEnv, nErrorCode);
       nErrorCode = LSgetDualSolution( pModel, &dual) ;
       LSerrorCheck(pEnv, nErrorCode);
       nErrorCode = LSgetInfo(pModel,LS_DINFO_POBJ,&dObj);
       LSerrorCheck(pEnv, nErrorCode);
     }
     println ("\n Objective at optimal solution = ", dObj);
     /*
     print ("\n Primal Solution\n");
     for (j = 0; j<n; j++) {
       nErrorCode = LSgetVariableNamej(pModel,j,varname);
       println("\t", "%8s", varname, " = ", "%18.10e", primal[j]);
     }
     print ("\n Dual Solution\n");
     for (j = 0; j<m; j++) {
       nErrorCode = LSgetConstraintNamei(pModel,j,varname);
       println("\t", "%8s", varname, " = ", "%18.10e", duall[j]);
     }
     */
   }
   /***************************************************************
    * Step 7: Terminate
    ***************************************************************/
   nErrorCode = LSdeleteModel( &pModel);
   nErrorCode = LSdeleteEnv( &pEnv);
}
