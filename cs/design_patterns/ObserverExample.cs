using System;
using System.Collections.Generic;

/* Observer Pattern
	Central observable/subject which observers subscribe to.
	Whenever observable changes state, all observers are notified.
*/

namespace DesignPatterns
{
	public static class ObserverExample
	{
		public static void RunExample()
		{
			Console.WriteLine("Observer Pattern:");
			var dataStream = new DataStream(2);
			var observers = new List<IObserver<DataStreamInfo>>() {
				new Plotter()
			};

			using (UnsubscriberList<DataStreamInfo> unsubscribers =
				dataStream.Subscribe(observers))
			{
				// Simulate receiving data
				for (int i = 0; i < 3; i++)
				{
					dataStream.ReceiveData(new DataStreamInfo() { data = i });
				}

				Console.WriteLine();
			}

			// Add new observer
			observers.Add(new Plotter());

			using (UnsubscriberList<DataStreamInfo> unsubscribers =
				dataStream.SubscribeAndGetHistory(observers))
			{
			}
		}

		public class DataStream : IObservable<DataStreamInfo>
		{
			private List<IObserver<DataStreamInfo>> observers = new List<IObserver<DataStreamInfo>>();
			private FixedSizeQueue<DataStreamInfo> history;

			public DataStream(int capacity = 256)
			{
				this.history = new FixedSizeQueue<DataStreamInfo>(capacity);
			}

			public IDisposable Subscribe(
				IObserver<DataStreamInfo> observer)
			{
				if (!observers.Contains(observer))
				{
					observers.Add(observer);
				}

				return new Unsubscriber<DataStreamInfo>(observers, observer);
			}

			public List<IDisposable> Subscribe(
				List<IObserver<DataStreamInfo>> observers)
			{
				var unsubscribers = new List<IDisposable>();

				foreach (var observer in observers)
				{
					unsubscribers.Add(Subscribe(observer));
				}

				return unsubscribers;
			}

			public IDisposable SubscribeAndGetHistory(
				IObserver<DataStreamInfo> observer)
			{
				if (!observers.Contains(observer))
				{
					observers.Add(observer);
					GetHistory(observer);
				}

				return new Unsubscriber<DataStreamInfo>(observers, observer);
			}

			public List<IDisposable> SubscribeAndGetHistory(
				List<IObserver<DataStreamInfo>> observers)
			{
				var unsubscribers = new List<IDisposable>();

				foreach (var observer in observers)
				{
					unsubscribers.Add(SubscribeAndGetHistory(observer));
				}

				return unsubscribers;
			}

			public void GetHistory(IObserver<DataStreamInfo> observer)
			{
				foreach (DataStreamInfo item in history)
				{
					observer.OnNext(item);
				}
			}

			public void ReceiveData(DataStreamInfo value)
			{
				history.Enqueue(value);

				BroadcastToObservers(value);
			}

			private void BroadcastToObservers(DataStreamInfo value)
			{
				foreach (var observer in observers)
				{
					observer.OnNext(value);
				}
			}
		}

		public class Plotter : IObserver<DataStreamInfo>
		{
			public void OnCompleted()
			{
				throw new NotImplementedException();
			}

			public void OnError(Exception error)
			{
				throw new NotImplementedException();
			}

			public void OnNext(DataStreamInfo value)
			{
				Console.WriteLine(value.ToString());
			}
		}
	}

	public struct DataStreamInfo
	{
		public int data;

		public override String ToString()
		{
			return data.ToString();
		}
	}

	public class Unsubscriber<T> : IDisposable
	{
		private IObserver<DataStreamInfo> observer;
		private List<IObserver<DataStreamInfo>> observers;

		public Unsubscriber(
			List<IObserver<DataStreamInfo>> observers,
			IObserver<DataStreamInfo> observer)
		{
			this.observers = observers;
			this.observer = observer;
		}

		public Unsubscriber(IDisposable unsubscriber)
		{
			this.observers = ((Unsubscriber<T>)unsubscriber).observers;
			this.observer = ((Unsubscriber<T>)unsubscriber).observer;
		}

		public void Dispose()
		{
			observers.Remove(observer);
		}
	}

	public class UnsubscriberList<T> : IDisposable
	{
		private List<Unsubscriber<T>> unsubscribers = new List<Unsubscriber<T>>();

		public void Dispose()
		{
			foreach (var observer in unsubscribers)
				observer.Dispose();
		}

		public UnsubscriberList(List<Unsubscriber<T>> unsubscribers)
		{
			this.unsubscribers = unsubscribers;
		}

		// Because C# does not support covariance, we must do something dumb like this
		public UnsubscriberList(List<IDisposable> unsubscribers)
		{
			// var ed = (IEnumerable<IDisposable>)(unsubscribers.ToArray());
			// var eu = (IEnumerable<Unsubscriber<T>>)(ed);
			// this.unsubscribers = new List<Unsubscriber<T>>(eu);

			this.unsubscribers = new List<Unsubscriber<T>>();

			foreach (var unsubscriber in unsubscribers)
			{
				this.unsubscribers.Add((Unsubscriber<T>)unsubscriber);
			}
		}

		public static implicit operator UnsubscriberList<T>(
				List<Unsubscriber<T>> unsubscribers)
		{
			return new UnsubscriberList<T>(unsubscribers);
		}

		public static implicit operator UnsubscriberList<T>(
			List<IDisposable> unsubscribers)
		{
			return new UnsubscriberList<T>(unsubscribers);
		}
	}
}