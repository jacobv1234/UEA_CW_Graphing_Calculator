using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;

namespace CSharpApp
{
    /// <summary>
    /// Interaction logic for Root_Popup.xaml
    /// </summary>
    public partial class Root_Popup : Window
    {

        public double Seed {  get; private set; }
        public Root_Popup()
        {
            InitializeComponent();
        }

        public void Root_Seed(object sender, EventArgs e)
        {
            Seed = double.Parse(seed.Text);
            this.Close();
        }
    }
}
