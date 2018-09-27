using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace CrunchScript
{
	public class Tokenizer
	{
		public Tokenizer(string text)
		{
			Text = text;
			Parse();
		}

		public string Text { get; }

		public List<Token> Tokens { get; } = new List<Token>();

		private void Parse()
		{
			var text = Text;

			while (text.Length > 0)
			{
				// !todo regex match classes, list, polymorphic
				// map? probably just make a separate internal class

				var whitespaceRegex = new Regex("^\\s+"); // Includes tabs, newlines?
				var whitespaceMatches = whitespaceRegex.Match(text);

				var binaryOperatorRegex = new Regex("^(\\+|\\-|\\*|/)");
				var binaryOperatorMatches = binaryOperatorRegex.Match(text);

				var valueRegex = new Regex("^((\\d+\\.?\\d*)|(\\.\\d+))");
				var valueMatches = valueRegex.Match(text);

				if (whitespaceMatches.Success)
				{
					Tokens.Add(new Token(TokenType.Whitespace, whitespaceMatches.Value));
					text = whitespaceRegex.Replace(text, "");
				}
				else if (binaryOperatorMatches.Success)
				{
					Tokens.Add(new Token(TokenType.BinaryOperator, binaryOperatorMatches.Value));
					text = binaryOperatorRegex.Replace(text, "");
				}
				else if (valueMatches.Success)
				{
					Tokens.Add(new Token(TokenType.Value, valueMatches.Value));
					text = valueRegex.Replace(text, "");
				}
				else
				{
					//throw new KeyNotFoundException();
					throw new Exception($"Cannot find next token in: {text}");
				}
			}

			Tokens.Add(new Token(TokenType.Empty, ""));
		}
	}
}