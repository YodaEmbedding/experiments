using System.Collections.Generic;
using System.Linq;

namespace CrunchScript.Parser
{
	//!todo Terminal nodes have no children
	public class Terminal : ConcreteNode
	{
		public Terminal(ConcreteNode parent, TokenType expectedTokenType, string expectedTokenString)
			: base(parent)
		{
			ExpectedTokenType = expectedTokenType;
			ExpectedTokenString = expectedTokenString;
		}

		public Terminal(ConcreteNode parent, TokenType expectedTokenType)
			: this(parent, expectedTokenType, null)
		{
		}

		public Token Token { get; /*private*/ set; }

		private string ExpectedTokenString { get; }

		private TokenType ExpectedTokenType { get; }

		public override string ToPrettyString(string indent = "")
		{
			return indent + ToString();
		}

		public override string ToString()
		{
			return $"{base.ToString()} {Token}";
		}

		protected override IEnumerable<Token> Match(IEnumerable<Token> tokens)
		{
			Token = tokens.First();

			return IsMatch(Token) ? tokens.Skip(1) : null;
		}

		private bool IsMatch(Token token) =>
			ExpectedTokenType.Equals(token.Type) &&
			(ExpectedTokenString?.Equals(token.Value) ?? true);
	}
}