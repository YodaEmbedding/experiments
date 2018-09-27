using System.Collections.Generic;

namespace DesignPatterns
{
	public class FixedSizeQueue<T> : Queue<T>
	{
		private int capacity;

		public FixedSizeQueue(int capacity)
		{
			this.capacity = capacity;
		}

		public new object Enqueue(T item)
		{
			base.Enqueue(item);

			if (Count > capacity)
				return Dequeue();

			return null;
		}
	}

	/*
	public class FixedSizeQueue<T> : IEnumerable
	{
		private Queue<T> queue;
		private int capacity;
		private int size;

		public FixedSizeQueue()
		{
			queue = new Queue<T>();
		}

		public FixedSizeQueue(int capacity)
		{
			this.capacity = capacity;
			queue = new Queue<T>(capacity);
		}

		public void Enqueue(T item)
		{
			if (size + 1 < capacity)
			{
				queue.Enqueue(item);
				++size;
			}
			else
			{
				queue.Dequeue();
				queue.Enqueue(item);
			}
		}

		public object Dequeue()
		{
			if (size > 0)
				--size;

			return queue.Dequeue();
		}

		IEnumerator IEnumerable.GetEnumerator()
		{
			return (IEnumerator)GetEnumerator();
		}

		private IEnumerator GetEnumerator()
		{
			return queue.GetEnumerator();
		}
	}
	*/
}