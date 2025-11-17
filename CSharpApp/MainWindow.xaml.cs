using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;
using System.Linq.Expressions;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using OxyPlot;
using OxyPlot.Series;
using OxyPlot.Wpf;

namespace CSharpApp
{

    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
        }

        private void Help_Button_Click(object sender, RoutedEventArgs e)
        {
            HelpWindow hwin = new HelpWindow();
            hwin.Show();
        }

        private void SendText_Button_Click(object sender, RoutedEventArgs e)
        {
            string input_txt = Input_Text.Text;

            try 
            {
                double result = FSInterpreter.calculate(input_txt);
                Output_txt.Text = result.ToString();
            }
            catch (Exception ex) 
            { 
                Error_txt.Text = ex.Message;
            }
            
        }
        
        private void Graph_Button_Click(object sender, RoutedEventArgs e)
        {
            string line_equ = Input_Text.Text;

            Plot_Popup pwin = new Plot_Popup();
            pwin.ShowDialog();


            if (pwin.Start.Text == "" || pwin.Stop.Text == "" || pwin.Step.Text == "")
            {
                Error_txt.Text = "Please fill in all fields for plotting.";
                return;
            }
            try
            {
                double start = double.Parse(pwin.Start.Text);
                double stop = double.Parse(pwin.Stop.Text);
                double step = double.Parse(pwin.Step.Text);
                bool Show_Derivative = (bool)pwin.Show_Derivative.IsChecked;



                var plot_results = FSInterpreter.plot(line_equ, start, stop, step, Show_Derivative);
                
                

                var points = plot_results.Select(innerlist => innerlist.First()).Select(pair => new DataPoint(pair.Item1, pair.Item2));

                var model = new PlotModel { Title = line_equ };
                var line_series = new LineSeries
                {
                    Title = line_equ,
                    StrokeThickness = 2,
                    MarkerType = MarkerType.Circle,
                    MarkerSize = 2,
                };

                line_series.Points.AddRange(points);
                model.Series.Add(line_series);

                PlotView.Model = model;
            }
            catch (Exception ex)
            {
                Error_txt.Text = ex.Message;
            }
            


        }
    }
}