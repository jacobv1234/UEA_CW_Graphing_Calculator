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
    /// Interaction logic for Integral_Popup.xaml
    /// </summary>
    public partial class Integral_Popup : Window
    {

        public double start_value {  get; private set; }

        public double stop_value { get; private set; }

        public int num_trap {  get; private set; }

        public Integral_Popup()
        {
            InitializeComponent();
        }

        private void IntegralCalculate(object sender, RoutedEventArgs e)
        {
            start_value = double.Parse(Start_Value.Text);
            stop_value = double.Parse(Stop_Value.Text);
            num_trap = int.Parse(Trap_Num.Text);

            this.Close();
        }
    }
}
