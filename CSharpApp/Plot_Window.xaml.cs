using OxyPlot;
using OxyPlot.Series;
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
    /// Interaction logic for Plot_Window.xaml
    /// </summary>
    public partial class Plot_Window : Window
    {
        public Plot_Window(PlotModel m)
        {
            InitializeComponent();

            PlotView.Model = m;
        }
    }
}
