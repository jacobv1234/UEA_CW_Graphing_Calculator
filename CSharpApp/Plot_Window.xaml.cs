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
        string equation;
        public Plot_Window(PlotModel m, string line_equ)
        {
            InitializeComponent();

            PlotView.Model = m;

            this.equation = line_equ;
        }

        public void IntegralClick(object sender, RoutedEventArgs e)
        {
            Integral_Popup iwin = new Integral_Popup();
            iwin.ShowDialog();

            try
            {
                double start = iwin.start_value;
                double stop = iwin.stop_value;
                string line_equ = equation;
                int num_trap = iwin.num_trap;

                var points = FSInterpreter.integral(line_equ, start, stop, num_trap);

                double total_y = 0;

                var point = points.Select(innerlist => innerlist.First()).Select(pair => new DataPoint(pair.Item1, pair.Item2));

                foreach (var pair in point)
                {
                    total_y += pair.Y;
                }

                Output_Plot.Text = total_y.ToString();
            }
            catch (Exception ex)
            {
                Output_Plot.Text = ex.Message;
            }
        }

        public void Find_Root(object sender, RoutedEventArgs e)
        {
            Root_Popup rwin = new Root_Popup();
            rwin.ShowDialog();

            try
            {
                double seed = rwin.Seed;
                string line_equ = equation;

                var final_seed = FSInterpreter.get_Root(equation, seed);

                Output_Plot.Text = final_seed.ToString();
            }
            catch (Exception ex)
            {
                Output_Plot.Text = ex.Message;
            }


        }

    }
}
