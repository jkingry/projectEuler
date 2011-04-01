using System;
using System.Collections.Generic;

namespace P14
{
    internal static class Program
    {
        static Dictionary<long, int> lengths = new Dictionary<long, int>();

        static long Colltaz(long n)
        {
            if (n % 2 == 0)
            {
                return n / 2;
            }
            else
            {
                return 3 * n + 1;
            }
        }

        static int CollatzLength(long n)
        {
            if (n <= 1) return 1;

            if (lengths.ContainsKey(n))
            {
                return lengths[n];
            }

            var length = 1 + CollatzLength(Colltaz(n));
            lengths.Add(n, length);

            return length;
        }

        static Tuple<long, int> MaxCollatzLength(long n)
        {
            if (n <= 1) return Tuple.Create(1L,1);

            var m = 1L;
            var mL = 1;
            for (var i = 0L; i <= n; ++i)
            {
                var L = CollatzLength(i);
                if (L >= mL)
                {
                    mL = L;
                    m = i;
                }
            }

            return Tuple.Create(m, mL);
        }

        static void Main()
        {
            Console.WriteLine(MaxCollatzLength(1000000).Item1);
        }
    }
}
