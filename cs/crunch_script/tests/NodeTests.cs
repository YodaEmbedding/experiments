using CrunchScript;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;

namespace Tests
{
    [TestClass]
    public class NodeTests
    {
        [TestMethod]
        public void NodeTests_EvaluateManualNodeTree()
        {
            var addNode = new AdditionOperatorNode(new ValueNode(4), new ValueNode(20));
            var mulNode = new MultiplicationOperatorNode(new ValueNode(17.5), addNode);

            Assert.AreEqual(420, mulNode.Evaluate());
        }

        [TestMethod]
        public void NodeTests_EvaluateDivisionByZero()
        {
            throw new NotImplementedException();
        }
    }
}