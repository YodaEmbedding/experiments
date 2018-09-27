using System;
using System.Collections.Generic;
using System.Linq;

namespace DesignPatterns
{
	internal static class LINQExample
	{
		public static void RunExample()
		{
			Console.WriteLine("LINQ Example:");

			ListEvens(0, 10);
			RemoveExample();
		}

		public static List<T> RemoveValuesContainedInHashSet<T>(
			List<T> list,
			HashSet<T> hashSet)
		{
			// There are three ways to do this.

			// RemoveAll
			var removed = list.ToList();
			removed.RemoveAll(x => hashSet.Contains(x));

			// Where
			var removed2 = list.ToList().Where(x => !hashSet.Contains(x)).ToList();

			// Query
			var removed3 = (
				from x in list
				where !hashSet.Contains(x)
				select x
			).ToList();

			return removed;
		}

		public static void RemoveExample()
		{
			var list = Enumerable.Range(0, 10).ToList();
			var fibonnaci = new HashSet<int>() { 1, 1, 2, 3, 5, 8 };

			var notFibonnaci = RemoveValuesContainedInHashSet(list, fibonnaci);

			Console.WriteLine($"Remove:  {String.Join(", ", fibonnaci)}");
			Console.WriteLine($"Removed: {String.Join(", ", notFibonnaci)}");
		}

		public static void ListEvens(int start, int count)
		{
			var numbers = Enumerable.Range(start, count).ToList();

			var evens =
				from x in numbers
				where (x % 2) == 0
				select x;

			Console.WriteLine($"Numbers: {String.Join(", ", numbers)}");
			Console.WriteLine($"Evens:   {String.Join(", ", evens)}");
		}
	}
}