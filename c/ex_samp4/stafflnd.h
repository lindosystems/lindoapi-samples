// stafflnd.h : main header file for the STAFFLND application
//

#if !defined(AFX_STAFFLND_H__F8463744_93B2_11D4_9FA7_004005A0998B__INCLUDED_)
#define AFX_STAFFLND_H__F8463744_93B2_11D4_9FA7_004005A0998B__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"		// main symbols

/////////////////////////////////////////////////////////////////////////////
// CStafflndApp:
// See stafflnd.cpp for the implementation of this class
//

class CStafflndApp : public CWinApp
{
public:
	CStafflndApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CStafflndApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CStafflndApp)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_STAFFLND_H__F8463744_93B2_11D4_9FA7_004005A0998B__INCLUDED_)
