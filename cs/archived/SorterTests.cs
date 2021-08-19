using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.Linq;

namespace SorterExample.Tests
{
	[TestClass()]
	public class SorterTests
	{
		[TestMethod()]
		public void SorterTest()
		{
			// Assert.Fail("default fail lel");
		}

		[TestMethod()]
		public void SortTest()
		{
			var sorter = new Sorter<int>();

			sorter.List = new List<int>() { 5, 3, 1, 4, 9 };
			sorter.Comparer = new MyComparer();
			sorter.Sort();

			var actualSort = new List<int> { 1, 3, 4, 5, 9 };
			if (Enumerable.SequenceEqual(sorter.List, actualSort))
			{
				// Pass!
			}
			else
			{
				Assert.Fail();
			}
		}
	}
}