using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;

/*  Multithreading

	Two main ways:
	 1) Starting own threads with ThreadStart delegates
	 2) Using ThreadPool:
	    Directly: ThreadPool.QueueUserWorkItem
		Indirectly: aync methods like Stream.BeginRead or calling BeginInvoke on delegates

	Method 1 is best for long-running tasks.
	ThreadPool is used internally so, we shouldn't block it.
	Method 2 works best for short-running tasks which are created often

	*/

namespace DesignPatterns
{
	internal static class MultithreadingExample
	{
		public static void RunExample()
		{
			Console.WriteLine("Multithreading:");
			/*
			var mythread = new MyThread();
			var job = new ThreadStart(mythread.Upcounter);
			var thread = new Thread(job);

			thread.Start();

			while (thread.IsAlive)
			{
				while (thread.ThreadState == ThreadState.Running) ;
				Console.WriteLine("(Main!)");
				while (thread.ThreadState == ThreadState.WaitSleepJoin) ;
			}
			*/

			/*
			var mythreads = ListCreator<MyThread>.create(3,
				(i) => new MyThread() { ThreadName = $"UPC{i}" });

			var threads = ListCreator<Thread>.create(3,
				(i) => new Thread(
					new ThreadStart(
						mythreads[i].Upcounter
					)));
			*/

			var mythreads = ListCreator<MyThread>.create(3,
				(i) => new MyThread() { ThreadName = $"UPC{i}" });
			
			var jobs = mythreads.Select(mythread => new ThreadStart(mythread.Upcounter)).ToList();
			var threads = jobs.Select(job => new Thread(job)).ToList();

			threads.ForEach((thread) => thread.Start());
			threads.ForEach((thread) => thread.Join());
		}

		public class MyThread
		{
			public string ThreadName { get; set; }

			public void Upcounter()
			{
				for (int i = 0; i < 4; i++)
				{
					Console.WriteLine($"({ThreadName}) Count: {i} seconds");

					if (i < 3)
						Thread.Sleep(1000);
				}
			}
		}

		public static class ListCreator<T>
		{
			public static List<T> create(int numOfThreads, Func<int, T> creator)
			{
				var list = new List<T>();

				for (int i = 0; i < numOfThreads; i++)
				{
					list.Add(creator(i));
				}

				return list;
			}
		}
	}
}