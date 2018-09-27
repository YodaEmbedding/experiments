namespace CrunchScript
{
	public interface INode
	{
		double Evaluate();
	}

	public interface IOperatorNode : INode
	{
		//!todo remove?
		Operator Operation { get; }
	}

	public interface IBinaryOperatorNode : IOperatorNode
	{
		INode Left { get; set; }
		INode Right { get; set; }
	}

	public interface IValueNode : INode
	{
		double Value { get; set; }
	}
}