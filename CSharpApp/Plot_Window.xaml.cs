using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Reflection;
using OxyPlot;
using OxyPlot.Series;
using System;
using System.CodeDom.Compiler;
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
using System.Xml;

namespace CSharpApp
{
    /// <summary>
    /// Interaction logic for Plot_Window.xaml
    /// </summary>
    public partial class Plot_Window : Window
    {
        string equation;
        private PlotModel model;

        public Plot_Window(PlotModel m, string line_equ)
        {
            InitializeComponent();

            PlotView.Model = m;

            this.equation = line_equ;
            this.model = PlotView.Model;
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

                // generate a set of areas to add together
                var points = FSInterpreter.integral(line_equ, start, stop, num_trap);

                double trap_width = (stop - start) / num_trap;

                // generate the relevant xy points on the original line to visualise
                var trap_bounds = FSInterpreter.plot(line_equ, start, stop+0.00000001, trap_width, false);

                // total areas to calculate definite integral
                double total_y = 0;

                var point = points.Select(innerlist => innerlist.First()).Select(pair => new DataPoint(pair.Item1, pair.Item2));

                foreach (var pair in point)
                {
                    total_y += pair.Y;
                }

                // use trap_bounds to create shaded region
                var areaSeries = new AreaSeries
                {
                    Color = OxyColors.Transparent,
                    Fill = OxyColor.FromAColor(100, OxyColors.SkyBlue),
                    StrokeThickness = 0
                };

                foreach (var innerlist in trap_bounds)
                {
                    var pair = innerlist.First();

                    double x = pair.Item1;
                    double y = pair.Item2;

                    areaSeries.Points.Add(new DataPoint(x, y));
                    areaSeries.Points2.Add(new DataPoint(x, 0));
                }

                model.Series.Add(areaSeries);
                model.InvalidatePlot(true);
                // area between comments are all the visual integral stuff so can be deleted
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

        private void Tangent_Click(object sender, RoutedEventArgs e)
        {
            Tangent_Popup twin = new Tangent_Popup();
            twin.ShowDialog();

            try
            {
                double xVal = twin.XVal;
                string line_equ = equation;

                var results = FSInterpreter.tangent(line_equ, xVal);


                var list1 = results.Select(innerlist => innerlist.Item1).First();
                var list2 = results.Select(innerlist => innerlist.Item2).First();

                var p1 = list1.First();
                var p2 = list2.First();
                
                // Coordinates of the 2 points
                double x1 = p1.Item1, y1 = p1.Item2;
                double x2 = p2.Item1, y2 = p2.Item2;
                
                double slope = (y2-y1) / (x2-x1);
                double yIntercept = y1 - slope * x1;

                double Xmin = model.Axes.First(a => a.Position == OxyPlot.Axes.AxisPosition.Bottom).ActualMinimum;
                double Xmax = model.Axes.First(a => a.Position == OxyPlot.Axes.AxisPosition.Bottom).ActualMaximum;
                double Ymin = slope * Xmin + yIntercept;
                double Ymax = slope * Xmax + yIntercept;

                var tanSeries = new LineSeries
                {
                    Title = $"Tangent at x = {xVal}",
                    Color = OxyColors.Brown,
                    StrokeThickness = 2
                };

                tanSeries.Points.Add(new DataPoint(Xmin, Ymin));
                tanSeries.Points.Add(new DataPoint(Xmax, Ymax));

                model.Series.Add(tanSeries);
                model.InvalidatePlot(true);

            }
            catch (Exception ex) 
            { 
                Output_Plot.Text = ex.Message;
            }

        }
    }
}
