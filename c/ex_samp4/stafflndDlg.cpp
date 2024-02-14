// stafflndDlg.cpp : implementation file
//

#include "stdafx.h"
#include "stafflnd.h"
#include "stafflndDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CAboutDlg dialog used for App About

class CAboutDlg : public CDialog
{
public:
	CAboutDlg();

// Dialog Data
	//{{AFX_DATA(CAboutDlg)
	enum { IDD = IDD_ABOUTBOX };
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CAboutDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	//{{AFX_MSG(CAboutDlg)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

CAboutDlg::CAboutDlg() : CDialog(CAboutDlg::IDD)
{
	//{{AFX_DATA_INIT(CAboutDlg)
	//}}AFX_DATA_INIT
}

void CAboutDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAboutDlg)
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CAboutDlg, CDialog)
	//{{AFX_MSG_MAP(CAboutDlg)
		// No message handlers
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CStafflndDlg dialog

CStafflndDlg::CStafflndDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CStafflndDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CStafflndDlg)
	m_nNeedsMon = 0;
	m_nNeedsTue = 0;
	m_nNeedsWed = 0;
	m_nNeedsThu = 0;
	m_nNeedsFri = 0;
	m_nNeedsSat = 0;
	m_nNeedsSun = 0;
	m_csOnDutyMon = _T("");
	m_csOnDutyTue = _T("");
	m_csOnDutyWed = _T("");
	m_csOnDutyThu = _T("");
	m_csOnDutySat = _T("");
	m_csOnDutySun = _T("");
	m_csOnDutyFri = _T("");
	m_csStartMon = _T("");
	m_csStartTue = _T("");
	m_csStartWed = _T("");
	m_csStartThu = _T("");
	m_csStartFri = _T("");
	m_csStartSat = _T("");
	m_csStartSun = _T("");
	m_csTotal = _T("");
	//}}AFX_DATA_INIT
	// Note that LoadIcon does not require a subsequent DestroyIcon in Win32
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
}

void CStafflndDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CStafflndDlg)
	DDX_Text(pDX, IDC_NEEDS_MON, m_nNeedsMon);
	DDV_MinMaxInt(pDX, m_nNeedsMon, 0, 1000000);
	DDX_Text(pDX, IDC_NEEDS_TUE, m_nNeedsTue);
	DDV_MinMaxInt(pDX, m_nNeedsTue, 0, 1000000);
	DDX_Text(pDX, IDC_NEEDS_WED, m_nNeedsWed);
	DDV_MinMaxInt(pDX, m_nNeedsWed, 0, 1000000);
	DDX_Text(pDX, IDC_NEEDS_THU, m_nNeedsThu);
	DDV_MinMaxInt(pDX, m_nNeedsThu, 0, 1000000);
	DDX_Text(pDX, IDC_NEEDS_FRI, m_nNeedsFri);
	DDX_Text(pDX, IDC_NEEDS_SAT, m_nNeedsSat);
	DDV_MinMaxInt(pDX, m_nNeedsSat, 0, 1000000);
	DDX_Text(pDX, IDC_NEEDS_SUN, m_nNeedsSun);
	DDV_MinMaxInt(pDX, m_nNeedsSun, 0, 1000000);
	DDX_Text(pDX, IDC_ON_MON, m_csOnDutyMon);
	DDX_Text(pDX, IDC_ON_TUE, m_csOnDutyTue);
	DDX_Text(pDX, IDC_ON_WED, m_csOnDutyWed);
	DDX_Text(pDX, IDC_ON_THU, m_csOnDutyThu);
 	DDX_Text(pDX, IDC_ON_FRI, m_csOnDutyFri);
	DDX_Text(pDX, IDC_ON_SAT, m_csOnDutySat);
	DDX_Text(pDX, IDC_ON_SUN, m_csOnDutySun);
	DDX_Text(pDX, IDC_START_MON, m_csStartMon);
	DDX_Text(pDX, IDC_START_TUE, m_csStartTue);
	DDX_Text(pDX, IDC_START_WED, m_csStartWed);
	DDX_Text(pDX, IDC_START_THU, m_csStartThu);
	DDX_Text(pDX, IDC_START_FRI, m_csStartFri);
	DDX_Text(pDX, IDC_START_SAT, m_csStartSat);
	DDX_Text(pDX, IDC_START_SUN, m_csStartSun);
	DDX_Text(pDX, IDC_TOTAL, m_csTotal);
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CStafflndDlg, CDialog)
	//{{AFX_MSG_MAP(CStafflndDlg)
	ON_WM_SYSCOMMAND()
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	ON_BN_CLICKED(IDOK, OnSolve)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CStafflndDlg message handlers

BOOL CStafflndDlg::OnInitDialog()
{
	CDialog::OnInitDialog();

	// Add "About..." menu item to system menu.

	// IDM_ABOUTBOX must be in the system command range.
	ASSERT((IDM_ABOUTBOX & 0xFFF0) == IDM_ABOUTBOX);
	ASSERT(IDM_ABOUTBOX < 0xF000);

	CMenu* pSysMenu = GetSystemMenu(FALSE);
	if (pSysMenu != NULL)
	{
		CString strAboutMenu;
		strAboutMenu.LoadString(IDS_ABOUTBOX);
		if (!strAboutMenu.IsEmpty())
		{
			pSysMenu->AppendMenu(MF_SEPARATOR);
			pSysMenu->AppendMenu(MF_STRING, IDM_ABOUTBOX, strAboutMenu);
		}
	}

	// Set the icon for this dialog.  The framework does this automatically
	//  when the application's main window is not a dialog
	SetIcon(m_hIcon, TRUE);			// Set big icon
	SetIcon(m_hIcon, FALSE);		// Set small icon

	// TODO: Add extra initialization here

	return TRUE;  // return TRUE  unless you set the focus to a control
}

void CStafflndDlg::OnSysCommand(UINT nID, LPARAM lParam)
{
	if ((nID & 0xFFF0) == IDM_ABOUTBOX)
	{
		CAboutDlg dlgAbout;
		dlgAbout.DoModal();
	}
	else
	{
		CDialog::OnSysCommand(nID, lParam);
	}
}

// If you add a minimize button to your dialog, you will need the code below
//  to draw the icon.  For MFC applications using the document/view model,
//  this is automatically done for you by the framework.

void CStafflndDlg::OnPaint()
{
	if (IsIconic())
	{
		CPaintDC dc(this); // device context for painting

		SendMessage(WM_ICONERASEBKGND, (WPARAM) dc.GetSafeHdc(), 0);

		// Center icon in client rectangle
		int cxIcon = GetSystemMetrics(SM_CXICON);
		int cyIcon = GetSystemMetrics(SM_CYICON);
		CRect rect;
		GetClientRect(&rect);
		int x = (rect.Width() - cxIcon + 1) / 2;
		int y = (rect.Height() - cyIcon + 1) / 2;

		// Draw the icon
		dc.DrawIcon(x, y, m_hIcon);
	}
	else
	{
		CDialog::OnPaint();
	}
}

// The system calls this to obtain the cursor to display while the user drags
//  the minimized window.
HCURSOR CStafflndDlg::OnQueryDragIcon()
{
	return (HCURSOR) m_hIcon;
}

#include "lindo.h"

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
         LSdeleteEnv( &pEnv); \
      } else {\
         printf( "Fatal Error\n"); \
      } \
      return; \
   } \

int CALLBACKTYPE MyCallback( pLSmodel pMod, void* pMyData,
 double dObj, double* dPrimal)
{
   CString csMessage;
   csMessage.Format(
    "New IP solution: Objective = %g", dObj);
   AfxMessageBox( csMessage);
   return( 0);
}

void CStafflndDlg::OnSolve()
{

   APIERRORSETUP;

   pLSenv pEnv = NULL;
   char MY_LICENSE_KEY[1024];

// >>> Step 1 <<< Create an environment
   nErrorCode = LSloadLicenseString("../../../license/lndapi150.lic",MY_LICENSE_KEY);
   APIERRORCHECK;

   pEnv = LScreateEnv( &nErrorCode, MY_LICENSE_KEY);
   if ( !pEnv)
   {
      AfxMessageBox("Unable to create environment!");
      return;
   }

// >>> Step 2 <<< Create a model in the environment
   pLSmodel pMod = NULL;
   pMod = LScreateModel( pEnv, &nErrorCode);
   APIERRORCHECK;

// >>> Step 3 <<< Construct the model

// Number of variables and constraints
   const int nVars = 7, nRows = 7;

// The direction of optimization
   int nDir = LS_MIN;

// The objective's constant term
   double dObjConst = 0.;

// The coefficients of the objective function
   double adC[ nVars] = {1.,1.,1.,1.,1.,1.,1.};

// Get right-hand sides of the constraints from
//  the Needs column of the dialog box
   UpdateData( true);
   double dNeeds[7];
   dNeeds[ 0] = m_nNeedsMon;
   dNeeds[ 1] = m_nNeedsTue;
   dNeeds[ 2] = m_nNeedsWed;
   dNeeds[ 3] = m_nNeedsThu;
   dNeeds[ 4] = m_nNeedsFri;
   dNeeds[ 5] = m_nNeedsSat;
   dNeeds[ 6] = m_nNeedsSun;

// The constraint types (all Greater-thans)
   char acConTypes[ nRows] = {'G','G','G','G','G','G','G'};

// The number of nonzeros in the constraint matrix
   const int nNZ = 35;

// The indices of the first nonzero in each column
   int anBegCol[ nVars + 1];
   for ( int i = 0; i <= nVars; i++)
   {
      anBegCol[ i] = 5 * i;
   }

// The length of each column. Since we aren't leaving
//  any blanks in our matrix, we can set this to NULL.
   int *pnLenCol = NULL;

// The nonzero coefficients and row indices
   double adA[ nNZ];
   int anRowX[ nNZ];

   int nX = 0;
   for ( int i = 0; i < 7; i++)
   {
      for ( int j = i; j < i + 5; j++)
      {
          adA[ nX] = 1.;
          anRowX[ nX] = j % 7;
          nX++;
      }
   }

// Simple upper and lower bounds on the variables.
//  By default, all variables have a lower bound of zero
//  and an upper bound of infinity.  Therefore pass NULL
//  pointers in order to use these default values.
   double *pdLower = NULL, *pdUpper = NULL;

// We have now assembled a full description of the model.
//  We pass this information to LSloadLPData with the
//  following call.
   nErrorCode = LSloadLPData( pMod, nRows, nVars, nDir,
    dObjConst, adC, dNeeds, acConTypes, nNZ, anBegCol,
    pnLenCol, adA, anRowX, pdLower, pdUpper);
   APIERRORCHECK;

// Mark all 7 variables as being general integer
   nErrorCode = LSloadMIPData( pMod, "IIIIIII");
   APIERRORCHECK;

   void* pMyData = NULL;
   nErrorCode = LSsetMIPCallback( pMod, (MIP_callback_t) MyCallback, pMyData);

// >>> Step 4 <<< Perform the optimization
   nErrorCode = LSsolveMIP( pMod, NULL);
   APIERRORCHECK;

// >>> Step 5 <<< Retrieve the solution
   double dObjVal, dStart[ 7], dSlacks[ 7];
   nErrorCode = LSgetInfo( pMod, LS_DINFO_MIP_OBJ, &dObjVal);
   nErrorCode = LSgetMIPPrimalSolution( pMod, dStart);
   APIERRORCHECK;
   nErrorCode = LSgetMIPSlacks( pMod, dSlacks);
   APIERRORCHECK;

// Display solution in dialog box
   m_csTotal.Format( "%d", (int) dObjVal);
   m_csStartMon.Format( "%d", (int) dStart[ 0]);
   m_csStartTue.Format( "%d", (int) dStart[ 1]);
   m_csStartWed.Format( "%d", (int) dStart[ 2]);
   m_csStartThu.Format( "%d", (int) dStart[ 3]);
   m_csStartFri.Format( "%d", (int) dStart[ 4]);
   m_csStartSat.Format( "%d", (int) dStart[ 5]);
   m_csStartSun.Format( "%d", (int) dStart[ 6]);
   m_csOnDutyMon.Format( "%d", (int) ( dNeeds[ 0] - dSlacks[ 0]));
   m_csOnDutyTue.Format( "%d", (int) ( dNeeds[ 1] - dSlacks[ 1]));
   m_csOnDutyWed.Format( "%d", (int) ( dNeeds[ 2] - dSlacks[ 2]));
   m_csOnDutyThu.Format( "%d", (int) ( dNeeds[ 3] - dSlacks[ 3]));
   m_csOnDutyFri.Format( "%d", (int) ( dNeeds[ 4] - dSlacks[ 4]));
   m_csOnDutySat.Format( "%d", (int) ( dNeeds[ 5] - dSlacks[ 5]));
   m_csOnDutySun.Format( "%d", (int) ( dNeeds[ 6] - dSlacks[ 6]));
   UpdateData( false);

// >>> Step 6 <<< Delete the LINDO environment
   LSdeleteEnv( &pEnv);

}
