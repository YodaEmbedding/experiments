using System;
using System.Collections.Generic;

namespace CrunchScript.Parser
{
	// OptionalComponent -> * Factor
	// OptionalComponent -> / Factor
	public class OptionalComponent : ConcreteNode
	{
		public OptionalComponent(ConcreteNode parent) : base(parent)
		{
		}

		protected override IEnumerable<Token> Match(IEnumerable<Token> tokens)
		{
			return new List<Func<IEnumerable<Token>, IEnumerable<Token>>>
			{
				t =>
					new Terminal(this, TokenType.BinaryOperator, "*").AddIfMatch(t) ??
					new Terminal(this, TokenType.BinaryOperator, "/").AddIfMatch(t),
				t => new Factor(this).AddIfMatch(t)
			}.ComposeUntil(tokens, t => t == null) ?? tokens;
		}
	}
}