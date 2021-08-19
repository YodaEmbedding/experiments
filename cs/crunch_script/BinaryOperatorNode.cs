namespace CrunchScript
{
	public abstract class BinaryOperatorNode : IBinaryOperatorNode
	{
		protected BinaryOperatorNode(INode left, INode right)
		{
			Left = left;
			Right = right;
		}

		public INode Left { get; set; }

		public abstract Operator Operation { get; }

		public INode Right { get; set; }

		public abstract double Evaluate();
	}

	public class AdditionOperatorNode : BinaryOperatorNode
	{
		public override Operator Operation => Operator.Add;

		public override double Evaluate() => Left.Evaluate() + Right.Evaluate();

		public AdditionOperatorNode(INode left, INode right) : base(left, right)
		{
		}
	}

	public class DivisionOperatorNode : BinaryOperatorNode
	{
		public override Operator Operation => Operator.Div;

		public override double Evaluate() => Left.Evaluate() / Right.Evaluate();

		public DivisionOperatorNode(INode left, INode right) : base(left, right)
		{
		}
	}

	public class MultiplicationOperatorNode : BinaryOperatorNode
	{
		public override Operator Operation => Operator.Mul;

		public override double Evaluate() => Left.Evaluate() * Right.Evaluate();

		public MultiplicationOperatorNode(INode left, INode right) : base(left, right)
		{
		}
	}

	public class SubtractionOperatorNode : BinaryOperatorNode
	{
		public override Operator Operation => Operator.Sub;

		public override double Evaluate() => Left.Evaluate() - Right.Evaluate();

		public SubtractionOperatorNode(INode left, INode right) : base(left, right)
		{
		}
	}
}