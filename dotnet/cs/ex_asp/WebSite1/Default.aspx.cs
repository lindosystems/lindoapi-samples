using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using ex_nlp3;

public partial class _Default : System.Web.UI.Page 
{
    protected void Page_Load(object sender, EventArgs e)
    {
        TextBox1.Text= String.Empty;
    }
    protected void Button1_Click(object sender, EventArgs e)
    {
        double obj = 0;
        int iter = 0;
        string res1, res2;
        TextBox1.Text = "Running...";
        nlp.start(ref obj, ref iter);
        res1 = obj.ToString();
        res2 = iter.ToString();
        TextBox1.Text = res1;
    }
}
