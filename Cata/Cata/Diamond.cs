using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Cata
{
    public class Diamond
    {
        public string Get(char letter)
        {
            var horizontal = Range.New('A', letter).Reverse()
                .Concat(Range.New('B', letter));

            return Range.New('A', letter)
                    .Concat(Range.New('A', (char)(letter - 1)).Reverse())
                    .Select(c => horizontal.Select(l => c == l ? l : ' ').Implode(""))
                    .Implode(Environment.NewLine);
        }
    }

    public static class Range
    {
        public static IEnumerable<char> New(char from, char to)
        {
            return Enumerable.Range(from, to - from).Select(c => (char)c);
        }
    }

    public static class JoinStringExtension
    {
        public static string Implode<T>(this IEnumerable<T> @this, string delimiter)
        {
            return string.Join(delimiter, @this);
        }
    }
}
