// stafflndDlg.h : header file
//

#if !defined(AFX_STAFFLNDDLG_H__F8463746_93B2_11D4_9FA7_004005A0998B__INCLUDED_)
#define AFX_STAFFLNDDLG_H__F8463746_93B2_11D4_9FA7_004005A0998B__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/////////////////////////////////////////////////////////////////////////////
// CStafflndDlg dialog

class CStafflndDlg : public CDialog
{
// Construction
public:
	CStafflndDlg(CWnd* pParent = NULL);	// standard constructor

// Dialog Data
	//{{AFX_DATA(CStafflndDlg)
	enum { IDD = IDD_STAFFLND_DIALOG };
	int		m_nNeedsMon;
	int		m_nNeedsTue;
	int		m_nNeedsWed;
	int		m_nNeedsThu;
	int		m_nNeedsFri;
	int		m_nNeedsSat;
	int		m_nNeedsSun;
	CString	m_csOnDutyMon;
	CString	m_csOnDutyTue;
	CString	m_csOnDutyWed;
	CString	m_csOnDutyThu;
	CString	m_csOnDutyFri;
	CString	m_csOnDutySat;
	CString	m_csOnDutySun;
	CString	m_csStartMon;
	CString	m_csStartTue;
	CString	m_csStartWed;
	CString	m_csStartThu;
	CString	m_csStartFri;
	CString	m_csStartSat;
	CString	m_csStartSun;
	CString	m_csTotal;
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CStafflndDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	HICON m_hIcon;

	// Generated message map functions
	//{{AFX_MSG(CStafflndDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnSysCommand(UINT nID, LPARAM lParam);
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
	afx_msg void OnSolve();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_STAFFLNDDLG_H__F8463746_93B2_11D4_9FA7_004005A0998B__INCLUDED_)
