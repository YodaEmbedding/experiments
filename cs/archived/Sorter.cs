using System;
using System.Collections.Generic;

// This is literally the worst code ever written
// Never do this.
namespace SorterExample
{
	public class MyComparer<T> : IComparer<T>
	{
		// x < y returns negative
		// x = y returns 0
		// x > y returns positive
		public int Compare(T x, T y)
		{
			// Works when subtraction is defined
			// return (dynamic)(x) - (dynamic)y;

			// Works when value based comparisons are defined on ==, >, <
			var _x = (dynamic)x;
			var _y = (dynamic)y;

			if (_x == _y)
				return 0;
			else if (_x < _y)
				return -1;
			else if (_x > _y)
				return 1;

			return int.MaxValue; // error
		}
	}

	// Takes in a delegate or IComparer and allows user to get an IComparer back
	// Should be reimplemented using recognized design patterns...
	// Actually, don't ever code like this. Ever.
	// It's probably better just to create different overloads/templates or something...
	public class SortingAlgorithm<T>
	{
		public delegate int ComparingDelegate(T a, T b);

		private ComparingDelegate comparer_delegate;
		private IComparer<T> comparer_icomparer;

		public SortingAlgorithm(IComparer<T> comparer)
		{
			comparer_delegate = null;
			comparer_icomparer = comparer;
		}

		public SortingAlgorithm(ComparingDelegate comparer)
		{
			comparer_delegate = comparer;
			comparer_icomparer = null;
		}

		// this is bad... very bad
		private class DelegateToIComparer<U> : IComparer<U> where U : T
		{
			private ComparingDelegate comparing_delegate;

			public DelegateToIComparer(ComparingDelegate comparer)
			{
				comparing_delegate = comparer;
			}

			public int Compare(U x, U y)
			{
				return comparing_delegate(x, y);
			}
		}

		public IComparer<T> getIComparer()
		{
			if (comparer_delegate != null)
			{
				return new DelegateToIComparer<T>(comparer_delegate);
			}

			return comparer_icomparer;
		}
	}

	public class Sorter<T>
	{
		// propfull is shortcut for get set
		private List<T> list;

		public List<T> List
		{
			get { return list; }
			set { list = value; }
		}

		private SortingAlgorithm<T> comparer;

		public void setComparer(IComparer<T> value)
		{
			comparer = new SortingAlgorithm<T>(value);
		}

		public void setComparer(SortingAlgorithm<T>.ComparingDelegate value)
		{
			comparer = new SortingAlgorithm<T>(value);
		}

		public Sorter()
		{
		}

		public List<T> Sort()
		{
			list.Sort(comparer.getIComparer());
			return list;
		}

		public override string ToString()
		{
			return String.Join(", ", list);
			// return list.Aggregate((a, b) => $"{a.ToString()}, {b.ToString()}");
		}
	}
}