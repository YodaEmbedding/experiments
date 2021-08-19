using System;
using System.Collections.Generic;
using System.Linq;

namespace CrunchScript.Parser
{
	//!todo Construct parse tree using nodes
	//!todo Parse tree => Abstract syntax tree
	//!todo New class which handles Concrete syntax (parse) tree, and another for Abstract syntax tree
	//!todo Whitespace ignoring
	public class Parser
	{
		public Parser(List<Token> tokens)
		{
			Tokens = tokens;
			Parse();
		}

		public RootNode RootNode { get; } = new RootNode();

		public List<Token> Tokens { get; }

		public INode GetAbstractSyntaxTree()
		{
			// Take all leaves of parse tree?
			// Some structure should obviously be preserved

			throw new NotImplementedException();
		}

		// Expression -> Component OptionalExpression
		// Component -> Factor OptionalComponent
		// Factor -> Value
		// Factor -> (Expression)
		// OptionalExpression -> + Component
		// OptionalExpression -> - Component
		// OptionalComponent -> * Factor
		// OptionalComponent -> / Factor
		private void Parse()
		{
			if (new Expression(RootNode).AddIfMatch(Tokens).Single().Type != TokenType.Empty)
				throw new Exception("Could not parse expression");
		}
	}
}