namespace CrunchScript
{
	public enum TokenType
	{
		Empty,
		Whitespace,
		BinaryOperator,
		Value,
		LParen,
		RParen
	}

	public class Token
	{
		public Token(TokenType type, string value)
		{
			Type = type;
			Value = value;
		}

		public TokenType Type { get; }
		public string Value { get; }

		public override string ToString()
		{
			return $"{Type} {Value}";
		}

		public override bool Equals(object other)
		{
			return Equals(other as Token);
		}

		protected bool Equals(Token other)
		{
			return other != null && Type == other.Type && string.Equals(Value, other.Value);
		}

		public override int GetHashCode()
		{
			unchecked
			{
				return ((int)Type * 397) ^ (Value?.GetHashCode() ?? 0);
			}
		}
	}
}