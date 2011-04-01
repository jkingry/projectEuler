using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace P14
{
    internal static class Program
    {
        static Tuple<int,int> Solution(int start) 
        { 
            int longest = 0; 
            int terms = 0; 
            int i; 
            long j; 

            for (i = 1; i <= start; i++) 
            { 
                j = i; 
                int this_terms = 1; 

                while (j != 1) 
                { 
                    this_terms++; 

                    if (this_terms > terms) 
                    { 
                        terms = this_terms; 
                        longest = i; 
                    } 

                    if (j % 2 == 0) 
                    { 
                        j = j / 2; 
                    } 
                    else 
                    { 
                        j = 3 * j + 1; 
                    } 
                } 
            } 

            return Tuple.Create(longest, terms);
        }

        static void Main()
        {
            while(true)
            {
                var line = Console.ReadLine();
                var n = Int32.Parse(line);
                Console.WriteLine("{0} = {1}", n, Solution(n));
            }
        }
    }
}
