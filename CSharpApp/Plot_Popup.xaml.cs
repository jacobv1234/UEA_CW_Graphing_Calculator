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
    /// Interaction logic for Plot_Popup.xaml
    /// </summary>
    public partial class Plot_Popup : Window
    {
        public double StartValue { get; private set; }
        public double StopValue { get; private set; }

        public double StepValue { get; private set; }

        public bool DerivativeChecked { get; private set; }
        
        public Plot_Popup()
        {
            InitializeComponent();
        }

        private void Button_Click(object sender, RoutedEventArgs e)
        {
            StartValue = double.Parse(Start.Text);
            StopValue = double.Parse(Stop.Text);
            StepValue = double.Parse(Step.Text);
            DerivativeChecked = (bool)Show_Derivative.IsChecked;
            
            this.DialogResult = true;
            this.Close();
        }

        
    }
}
