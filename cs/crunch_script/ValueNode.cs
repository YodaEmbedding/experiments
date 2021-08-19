namespace CrunchScript
{
	public class ValueNode : IValueNode
	{
		public ValueNode(double value)
		{
			Value = value;
		}

		public double Evaluate() => Value;

		public double Value { get; set; }
	}
}