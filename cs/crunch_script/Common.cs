using System;
using System.Collections.Generic;

namespace CrunchScript
{
	public static class Common
	{
		public static TAccumulate AggregateUntil<TSource, TAccumulate>(
			this IEnumerable<TSource> source,
			TAccumulate seed,
			Func<TAccumulate, TSource, TAccumulate> func,
			Func<TAccumulate, bool> predicate)
		{
			if (source == null)
				throw new ArgumentNullException(nameof(source));

			if (func == null)
				throw new ArgumentNullException(nameof(func));

			if (predicate == null)
				throw new ArgumentNullException(nameof(func));

			var accumulate = seed;
			foreach (var item in source)
			{
				accumulate = func(accumulate, item);
				if (predicate(accumulate)) break;
			}
			return accumulate;
		}

		public static TAccumulate AggregateWhile<TSource, TAccumulate>(
			this IEnumerable<TSource> source,
			TAccumulate seed,
			Func<TAccumulate, TSource, TAccumulate> func,
			Func<TAccumulate, bool> predicate)
		{
			if (source == null)
				throw new ArgumentNullException(nameof(source));

			if (func == null)
				throw new ArgumentNullException(nameof(func));

			if (predicate == null)
				throw new ArgumentNullException(nameof(func));

			var accumulate = seed;
			foreach (var item in source)
			{
				var tmp = func(accumulate, item);
				if (!predicate(tmp)) break;
				accumulate = tmp;
			}
			return accumulate;
		}

		public static T ComposeUntil<T>(
			this IEnumerable<Func<T, T>> source,
			T seed,
			Func<T, bool> predicate)
		{
			return source.AggregateUntil(seed, (x, f) => f(x), predicate);
		}

		public static T ComposeWhile<T>(
			this IEnumerable<Func<T, T>> source,
			T seed,
			Func<T, bool> predicate)
		{
			return source.AggregateWhile(seed, (x, f) => f(x), predicate);
		}
	}
}