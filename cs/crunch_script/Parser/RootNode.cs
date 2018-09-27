using System;
using System.Collections.Generic;

namespace CrunchScript.Parser
{
	//!todo RootNode has no parent
	public class RootNode : ConcreteNode
	{
		public RootNode() : base(null)
		{
		}

		protected override IEnumerable<Token> Match(IEnumerable<Token> tokens)
		{
			throw new NotImplementedException();
		}
	}
}