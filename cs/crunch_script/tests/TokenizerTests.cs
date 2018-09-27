using CrunchScript;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Tests
{
    [TestClass]
    public class TokenizerTests
    {
        [TestMethod]
        public void TokenizerTests_Invalid()
        {
            //!todo also need expression validator in parser?

            throw new NotImplementedException();
        }

        [TestMethod]
        public void TokenizerTests_BasicExpression()
        {
            var expectedTokens = new List<Token>
            {
                new Token(TokenType.Whitespace, " "),
                new Token(TokenType.Value, "4.20"),
                new Token(TokenType.Whitespace, "  "),
                new Token(TokenType.BinaryOperator, "+"),
                new Token(TokenType.Value, "3"),
                new Token(TokenType.BinaryOperator, "*"),
                new Token(TokenType.Value, "4."),
            };

            var actualTokens = new Tokenizer(" 4.20  +3*4.").Tokens;

            foreach (var pair in expectedTokens.Zip(actualTokens, Tuple.Create))
            {
                Assert.AreEqual(pair.Item1, pair.Item2);
            }
        }
    }
}