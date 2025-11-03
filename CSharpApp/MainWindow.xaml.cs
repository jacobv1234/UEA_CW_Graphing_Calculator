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
            Graph_Window gwin = new Graph_Window();
            gwin.Show();
        }
    }
}