using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace CrunchScript.Parser
{
	public abstract class ConcreteNode : IEnumerable<ConcreteNode>
	{
		protected ConcreteNode(ConcreteNode parent)
		{
			Parent = parent;
		}

		public Stack<ConcreteNode> Children { get; } = new Stack<ConcreteNode>();

		public ConcreteNode Parent { get; }

		public void Add(ConcreteNode node)
		{
			Children.Push(node);
		}

		// Add self to parent if matches
		public virtual IEnumerable<Token> AddIfMatch(IEnumerable<Token> tokens)
		{
			var r = Match(tokens);

			if (r != null) Parent.Children.Push(this);

			return r;
		}

		public IEnumerator<ConcreteNode> GetEnumerator()
		{
			return Children.GetEnumerator();
		}

		IEnumerator IEnumerable.GetEnumerator()
		{
			return GetEnumerator();
		}

		public virtual string ToPrettyString(string indent = "")
		{
			var children = String.Join("\n", Children.Reverse().Select(node => node.ToPrettyString(indent + "\t")));
			return indent + ToString() + (children.Length > 0 ? "\n" + children : "");
		}

		public override string ToString()
		{
			return GetType().Name;
		}

		protected abstract IEnumerable<Token> Match(IEnumerable<Token> tokens);
	}
}