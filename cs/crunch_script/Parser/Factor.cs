using System;
using System.Collections.Generic;

namespace CrunchScript.Parser
{
	// Factor -> Value
	// Factor -> (Expression)
	public class Factor : ConcreteNode
	{
		public Factor(ConcreteNode parent) : base(parent)
		{
		}

		protected override IEnumerable<Token> Match(IEnumerable<Token> tokens)
		{
			return
				new Terminal(this, TokenType.Value).AddIfMatch(tokens) ??
				new List<Func<IEnumerable<Token>, IEnumerable<Token>>>
				{
					t => new Terminal(this, TokenType.LParen).AddIfMatch(t),
					t => new Expression(this).AddIfMatch(t),
					t => new Terminal(this, TokenType.RParen).AddIfMatch(t),
				}.ComposeUntil(tokens, t => t == null);
		}
	}
}