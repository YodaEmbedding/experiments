using System;
using System.Collections.Generic;

namespace CrunchScript.Parser
{
	// Expression -> Component OptionalExpression
	public class Expression : ConcreteNode
	{
		public Expression(ConcreteNode parent) : base(parent)
		{
		}

		protected override IEnumerable<Token> Match(IEnumerable<Token> tokens)
		{
			return new List<Func<IEnumerable<Token>, IEnumerable<Token>>>
			{
				t => new Component(this).AddIfMatch(t),
				t => new OptionalExpression(this).AddIfMatch(t)
			}.ComposeUntil(tokens, t => t == null);
		}
	}
}