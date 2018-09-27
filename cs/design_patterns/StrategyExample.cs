using System;

namespace DesignPatterns
{
	public static class StrategyExample
	{
		public static void RunExample()
		{
			Console.WriteLine("Strategy Pattern:");
			var context = new StrategyContext();
			context.SetContextStrategy(new StrategyA());
			context.ContextInterface();
		}

		// public abstract class Strategy
		public interface Strategy
		{
			void AlgorithmInterface();
		}

		public class StrategyA : Strategy
		{
			public void AlgorithmInterface()
			{
				Console.WriteLine("Strategy A!");
			}
		}

		public class StrategyB : Strategy
		{
			public void AlgorithmInterface()
			{
				Console.WriteLine("Strategy B!");
			}
		}

		public class StrategyContext
		{
			private Strategy _strategy;

			public StrategyContext()
			{
			}

			public StrategyContext(Strategy strategy)
			{
				this._strategy = strategy;
			}

			public void SetContextStrategy(Strategy strategy)
			{
				_strategy = strategy;
			}

			public void ContextInterface()
			{
				_strategy.AlgorithmInterface();
			}
		}
	}
}