using CrunchScript;
using log4net;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections.Generic;
using CrunchScript.Parser;

namespace Tests
{
	[TestClass]
	public class ParserTests
	{
		private static ILog log = LogManager.GetLogger(System.Reflection.MethodBase.GetCurrentMethod().DeclaringType);

		[TestMethod]
		public void ComposeUntil_Test()
		{
			var i = new List<Func<int, int>>
			{
				x => x + 1,
				x => x + 2,
				x => x + 3,
			}.ComposeUntil(4, x => x >= 7);

			Assert.AreEqual(7, i);
		}

		[TestMethod]
		public void ParserTests_Test()
		{
			var parser = new Parser(new Tokenizer("3+3").Tokens);
			var actualNode = parser.RootNode;

			log.Debug("\n" + actualNode.ToPrettyString());

			var expectedNode = new RootNode
			{
				new Expression(null)
				{
					new Component(null)
					{
						new Factor(null)
						{
							new Terminal(null, TokenType.Value) {Token =  new Token(TokenType.Value, "3")},
						},
						new OptionalComponent(null)
					},
					new OptionalExpression(null)
					{
						new Terminal(null, TokenType.BinaryOperator) {Token = new Token(TokenType.BinaryOperator, "+")},
						new Component(null)
						{
							new Factor(null)
							{
								new Terminal(null, TokenType.Value) {Token = new Token(TokenType.Value, "3")}
							},
							new OptionalComponent(null)
						}
					}
				}
			};

			Assert.AreEqual(expectedNode.ToPrettyString(), actualNode.ToPrettyString());
		}
	}
}