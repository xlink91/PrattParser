// See https://aka.ms/new-console-template for more information
using PrattParser.Lexer;

//var expr = "1 + 2 * (1+3) / 5 + ++b * (1 + b -a_d3123s_df)";
List<(string, double)> exprList = new List<(string, double)>
{
    ("4 * 6 + ++3 + --2 + 5;", 34),
    ("4 * (6 + ++3) + --2 + 5;", 46),
    ("3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3;", 3.001953125),
    ("1.5 + 1 + 0.5 + 0.5 * 2 + 0.5;", 4.5),
    ("let a = 2^3; let b = (++a + 1)*2; b + 5;", 25),
    ("let a = func(b, c) { return b + c; }; a(1, 2);", 3),
    ("let det = func(a, b, c){ let num = -b + (b^2 - 4*a*c)^0.5; return num / 2*a;};det(1,2,1);", -1),
    ("let det = func(a, b, c){ let num = -b + (b^2 - 4*a*c)^0.5; return num / 2*a;};let squareEq = func (x) { return x*x + 2*x + 1;}; let root = det(1,2,1); squareEq(root);", 0),
    ("let f = func(x) { return x / 10.0; }; f(1 + 4*10); let det = func(a, b, c){ let num = func (a, b, c) { return -b + (b^2 - 4*a*c)^0.5; }; let den = func(a) { return 2*a; }; return num(a, b, c)/den(a);};det(1,2,1);", -1),
    ("let a = func (x, y) {	let r = func (x, y) { return x*y; }; return (x+y) * r(x, y); }; a(1, 2);", 6),
    ("let a = func (x, y) {	let r = func (x, y) { return x*y; }; return (x+y) * r(x, y);};a(1, 2);let r = func(x) {	return x+1;};let one = 1;let two = 2;r(a(one, two+8));", 111),
    ("let a = func (x, y) {	let r = func (x, y) { return x*y;	};	return (x+y) * r(x, y);};a(1, 2);let t = func(x) {	return x+a(one, two)+1;}; let one = 1;let two = 2; t(2);", 9),
    ("let a = func (x, y) {	let r = func () { return x*y; }; return (x+y) * r();};a(1, 2); let r = func(x) { return a(x, x+1);};let one = 1;let two = 2; a(one, two+8); r(a(one, two+8));", 2698410),
    ("let fib = func (x) { if(x < 1) { return 0; } if(x is 1) { return 1; } if(x is 2) { return 1; } return fib(x - 1) + fib(x - 2);};fib(1);fib(2);fib(3);fib(8);", 21),
    ("let for_ = func (from, to) {    if(from is to) {        return to;    }    return from + for_(from+1, to);};for_(1, 10);", 55)
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
    if ((double)rs - (double)finalResult != 0)
    {
        throw new Exception($"Assertion fail.");
    }
}
