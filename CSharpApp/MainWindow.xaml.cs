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
            string input_txt = Input_Text.Text;

            try
            {
                string[] input_parts = input_txt.Split(';');

                string equation = input_parts[0];
                double.TryParse(input_parts[1], out double start);
                double.TryParse(input_parts[2], out double stop);
                double.TryParse(input_parts[3], out double step);

                var plot_results = FSInterpreter.plot(equation, start, stop, step);

                var points = plot_results.Select(innerlist => innerlist.First()).Select(pair => new DataPoint(pair.Item1, pair.Item2));

                var model = new PlotModel { Title = equation };
                var line_series = new LineSeries
                {
                    Title = equation,
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