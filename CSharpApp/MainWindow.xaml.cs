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
using OxyPlot.Axes;
using System.Windows.Media.Animation;
using OxyPlot.Legends;

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
                double start = pwin.StartValue;
                double stop = pwin.StopValue;
                double step = pwin.StepValue;
                bool Show_Derivative = pwin.DerivativeChecked;



                var plot_results = FSInterpreter.plot(line_equ, start, stop, step, false);

                var deriv_plot_results = FSInterpreter.plot(line_equ, start, stop, step, true);
                
                

                var points = plot_results.Select(innerlist => innerlist.First()).Select(pair => new DataPoint(pair.Item1, pair.Item2));

                var deriv_points = deriv_plot_results.Select(innerlist => innerlist.First()).Select(pair => new DataPoint(pair.Item1, pair.Item2));

                var model = new PlotModel { Title = line_equ };
                var line_series = new LineSeries
                {
                    Title = line_equ,
                    StrokeThickness = 2,
                    MarkerType = MarkerType.Circle,
                    MarkerSize = 2,
                    InterpolationAlgorithm = InterpolationAlgorithms.CatmullRomSpline,
                    Color = OxyColors.Blue

                };

                var deriv_line_series = new LineSeries
                {
                    Title = "Derivative of " + line_equ,
                    StrokeThickness = 2,
                    MarkerType = MarkerType.Circle,
                    MarkerSize = 2,
                    InterpolationAlgorithm = InterpolationAlgorithms.CatmullRomSpline,
                    Color = OxyColors.Red

                };

                line_series.Points.AddRange(points);
                
                deriv_line_series.Points.AddRange(deriv_points);

                

                model.Series.Add(line_series);
                model.Series.Add(deriv_line_series);


                var xAxis = new LinearAxis
                { 
                    Position = AxisPosition.Bottom,
                    MajorGridlineStyle = LineStyle.Solid, 
                    MinorGridlineStyle = LineStyle.Dot,
                    Title = "X-Axis"
                
                };

                model.Axes.Add(xAxis);

                var yAxis = new LinearAxis
                {
                    Position = AxisPosition.Left,
                    MajorGridlineStyle = LineStyle.Solid,
                    MinorGridlineStyle = LineStyle.Dot,
                    Title = "Y-Axis"
                };

                model.Axes.Add(yAxis);

                var legend = new Legend
                {
                    LegendPosition = LegendPosition.TopRight,
                    LegendPlacement = LegendPlacement.Outside,
                    LegendOrientation = LegendOrientation.Vertical,
                    LegendBorderThickness = 1
                };

                model.Legends.Add(legend);

                Plot_Window gwin = new Plot_Window(model, line_equ);
                gwin.Show();
            }
            catch (Exception ex)
            {
                Error_txt.Text = ex.Message;
            }
            


        }
    }
}