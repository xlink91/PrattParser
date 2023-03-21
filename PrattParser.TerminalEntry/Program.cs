// See https://aka.ms/new-console-template for more information


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
    //("let a = 2^3; let b = (++a + 1)*2; b + 5;", 25),
    //("let a = func(b, c) { return b + c; }; a(1, 2);", 3),
    //("let det = func(a, b, c){ let num = -b + (b^2 - 4*a*c)^0.5; return num / 2*a;};det(1,2,1);", -1),
    //("let det = func(a, b, c){ let num = -b + (b^2 - 4*a*c)^0.5; return num / 2*a;};let squareEq = func (x) { return x*x + 2*x + 1;}; let root = det(1,2,1); squareEq(root);", 0),
    ("let f = func(x) { return x / 10.0; }; f(1 + 4*10); let det = func(a, b, c){ let num = func (a, b, c) { return -b + (b^2 - 4*a*c)^0.5; }; let den = func(a) { return 2*a; }; return num(a, b, c)/den(a);};det(1,2,1);", -1)
};
foreach (var (expr, rs) in exprList)
{
    var lexer = new Lexer(expr);
    var parser = new Parser(lexer.GetTokens());
    var sts = parser.GetStatements();
    double? finalResult = null;
    foreach (var st in sts)
    {
        var r = st.Synthetize();
        if (st.HasReturn)
        {
            finalResult = Convert.ToDouble(r);
            if (st != sts.Last())
            {
                Console.WriteLine("Intermediate result: " + r);
            }
        }
    }
    Console.WriteLine($"Assert.Equal({rs}, {finalResult})");
}
