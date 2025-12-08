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
    /// Interaction logic for Tangent_Popup.xaml
    /// </summary>
    public partial class Tangent_Popup : Window
    {
        public double XVal {  get; private set; }
        public Tangent_Popup()
        {
            InitializeComponent();
        }

        private void Button_Click(object sender, RoutedEventArgs e)
        {
            if (double.TryParse(XValue.Text, out double v)) 
            {
                XVal = v;
                DialogResult = true;
                Close();
            }
            else
            {
                MessageBox.Show("Invalid Number");
            }
        }
    }
}
