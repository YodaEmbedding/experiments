using System;
using System.Collections.Generic;

namespace CrunchScript.Parser
{
	// Component -> Factor OptionalComponent
	public class Component : ConcreteNode
	{
		public Component(ConcreteNode parent) : base(parent)
		{
		}

		protected override IEnumerable<Token> Match(IEnumerable<Token> tokens)
		{
			return new List<Func<IEnumerable<Token>, IEnumerable<Token>>>
			{
				t => new Factor(this).AddIfMatch(t),
				t => new OptionalComponent(this).AddIfMatch(t)
			}.ComposeUntil(tokens, t => t == null);
		}
	}
}