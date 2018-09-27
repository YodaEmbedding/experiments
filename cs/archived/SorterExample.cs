using System;
using System.Collections.Generic;

namespace SorterExample
{
	public class SorterExample
	{
		public static void RunExample()
		{
			Console.WriteLine("Class Library:");
			var sorter = new Sorter<int>();
			sorter.List = new List<int>() { 5, 3, 1, 4, 9 };
			// sorter.Comparer = new MyComparer<int>();
			sorter.setComparer((a, b) => a - b);

			Console.WriteLine($"Unsorted: {sorter.ToString()}");
			sorter.Sort();
			Console.WriteLine($"Sorted:   {sorter.ToString()}");
		}
	}
}