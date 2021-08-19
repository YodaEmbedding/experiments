/* See http://stackoverflow.com/documentation/c%23/15/getting-started-with-c-sharp-language/3710/creating-a-new-program-using-net-core#t=201611160828303537902

	BUILD COMMANDS

	dotnet new - t console
	dotnet restore
	dotnet build
	dotnet run */


using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace ConsoleApplication {
	public class Program {
		public static IEnumerable < T > ChainOrig < T > (Dictionary < T, T > dict, T seed) {
			var curr = seed;
			var next = dict[curr];

			yield return curr;

			for (var i = 1; i < dict.Count; i++) {
				curr = next;
				next = dict[curr];
				yield return curr;
			}
		}

		public static IEnumerable <T> Chain <T, S> (
			IEnumerable<T> list, Func<T, S> sourceSelector, Func<T, S> targetSelector, T seed) {
			var dict = list.ToDictionary(sourceSelector, x => x);
			var curr = seed;

			do {
				yield return curr;
			} while(dict.TryGetValue(targetSelector(curr), out curr));
		}

		public static void Main(string[] args) {
			Console.WriteLine("Hello World!");

			var list = new List<Tuple<string, string>> {
				Tuple.Create("1", "2"),
				Tuple.Create("3", "5"),
				Tuple.Create("2", "3"),
				Tuple.Create("8", "13"),
				Tuple.Create("5", "8"),
			};

			foreach (var i in Chain<Tuple<string, string>, string>(list, x => x.Item1, x => x.Item2, list.First()))
				Console.Write(i);
		}
	}
}
