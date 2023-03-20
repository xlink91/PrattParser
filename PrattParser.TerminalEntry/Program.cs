﻿// See https://aka.ms/new-console-template for more information


using System.Linq.Expressions;
using System.Threading.Channels;
using PrattParser.Lexer;

//var expr = "1 + 2 * (1+3) / 5 + ++b * (1 + b -a_d3123s_df)";
List<(string, double)> exprList = new List<(string, double)>
{
    //("4 * 6 + ++3 + --2 + 5;", 34),
    //("4 * (6 + ++3) + --2 + 5;", 46),
    //("3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3;", 3.001953125),
    //("1.5 + 1 + 0.5 + 0.5 * 2 + 0.5;", 4.5),
    //("let a = 2 ^ 3; let b = (++a + 1)*2; b + 5;", 25),
    ("let a = func(b, c) { return b + c; }; a(1, 2);", 3)
};
foreach (var (expr, rs) in exprList)
{
    var lexer = new Lexer(expr);
    var parser = new Parser(lexer.GetTokens());
    var sts = parser.GetStatements();
    foreach (var st in sts)
    {
        var r = st.Synthetize();
        if (st.HasReturn)
        {
            Console.WriteLine(r);
        }
    }
}
