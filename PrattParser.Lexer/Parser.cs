using System.Collections;
using System.Data.Common;
using System.Formats.Asn1;
using System.Linq.Expressions;
using System.Net.Http.Headers;
using System.Security.AccessControl;
using System.Threading.Channels;

namespace PrattParser.Lexer;

public class Parser
{
    private List<Token> _tokenList;
    private int _index = 0;
    private IDictionary<TokenType, short> _preffixPrecedence; 
    private IDictionary<TokenType, short> _infixPrecedence; 
    public Parser(List<Token> tokenList)
    {
        _tokenList = tokenList;
        _preffixPrecedence = new Dictionary<TokenType, short>
        {
            { TokenType.LPARENTESIS, 103 },
            { TokenType.INT, 102 },
            { TokenType.DECIMAL, 102 },
            { TokenType.PREFIX_SUBSTRACTION, 101 },
            { TokenType.PREFFIX_SUM, 101 },
            { TokenType.SUBSTRACTION, 100 },
            { TokenType.SUM, 100},
        };
        _infixPrecedence = new Dictionary<TokenType, short>
        {
            { TokenType.EXP, 3 },
            { TokenType.PRODUCT, 2 },
            { TokenType.DIVISION, 2 },
            { TokenType.SUBSTRACTION, 1 },
            { TokenType.SUM, 1 },
            { TokenType.RPARENTESIS, 0 },
        };
    }

    public Statement[] GetStatements()
    {
        List<Dictionary<string, object>> scopedValues = new List<Dictionary<string, object>>();
        List<Dictionary<string, LetStatementFuncDeclaration>> functions = new List<Dictionary<string, LetStatementFuncDeclaration>>();
        List<Statement> sts = new List<Statement>();
        scopedValues.Add(new Dictionary<string, object>());
        functions.Add(new Dictionary<string, LetStatementFuncDeclaration>());
        while (!Eof())
        {
            var token = CurrentToken();
            Statement st = null;
            switch (token.Type)
            {
                case TokenType.LET:
                    st = ParseLet(scopedValues, functions);
                    break;
                default:
                    st = ParseExpressionStatement(scopedValues, functions);
                    break;
            }
            sts.Add(st);
            if (!IsStatementEnd())
            {
                throw new Exception($"Expected ; at the end of a statement found instead {CurrentToken()}.");
            }
            MoveNext();
        }
        return sts.ToArray();
    }

    LetStatement ParseLet(List<Dictionary<string, object>> scopedValues, 
                          List<Dictionary<string, LetStatementFuncDeclaration>> functions)
    {
        MoveNext(); //Let
        var ident = CurrentToken();
        if (ident.Type != TokenType.IDENT)
        {
            throw new Exception($"Invalid identifier {ident.Value} in left part of let statement.");
        }
        MoveNext();
        var equal = CurrentToken();
        if (equal.Type != TokenType.ASSIGN)
        {
            throw new Exception($"Invalid assign operator {equal.Value} in  let statement.");
        }
        MoveNext();
        if (IsFunction())
        {
            MoveNext(); // '('
            MoveNext();
            var args = ParseFuncDeclarionArgs(scopedValues, functions);
            if (CurrentToken().Type != TokenType.RPARENTESIS)
            {
                throw new Exception($"Expected ')' found {CurrentToken().Value}");
            }
            MoveNext(); // ')'
            MoveNext(); // '{'
            var statements = GetFunctionStatements(scopedValues, functions);
            MoveNext(); // '}'
            return new LetStatementFuncDeclaration(ident, args, statements, scopedValues, functions);
        } 
        var expr = ParseExpression(0, scopedValues, functions);
        return new LetStatementExpression(ident, expr, scopedValues, functions);
    }

    bool IsPreffixFunction()
    {
        var token = CurrentToken();
        var nextToken = PeekNextToken();
        return token.Type == TokenType.IDENT && nextToken.Type == TokenType.LPARENTESIS;
    }
    
    bool IsFunction()
    {
        var token = CurrentToken();
        var nextToken = PeekNextToken();
        return token.Type == TokenType.FUNC && nextToken.Type == TokenType.LPARENTESIS;
    }

    Statement[] GetFunctionStatements(List<Dictionary<string, object>> scopedValues,
                                      List<Dictionary<string, LetStatementFuncDeclaration>> functions)
    {
        scopedValues.Add(new Dictionary<string, object>());
        functions.Add(new Dictionary<string, LetStatementFuncDeclaration>());
        List<Statement> sts = new List<Statement>();
        while (CurrentToken().Type != TokenType.RBRACE)
        {
            var token = CurrentToken();
            Statement st = null;
            switch (token.Type)
            {
                case TokenType.LET:
                    st = ParseLet(scopedValues, functions);
                    break;
                case TokenType.RETURN:
                    st = ParseReturnStatement(scopedValues, functions);
                    break;
                default:
                    st = ParseExpressionStatement(scopedValues, functions);
                    break;
            }
            sts.Add(st);
            if (CurrentToken().Type != TokenType.SEMICOLON)
            {
                throw new Exception($"Expected ';' at the end of a statements found instead {CurrentToken()}.");
            } 
            MoveNext();
        }
        return sts.ToArray();
    }

    ReturnStatement ParseReturnStatement(List<Dictionary<string, object>> scopedValues,
                                         List<Dictionary<string, LetStatementFuncDeclaration>> functions)
    {
        var returnToken = CurrentToken();
        if (returnToken.Type != TokenType.RETURN)
        {
            throw new Exception($"Expected keyword 'return' found {CurrentToken().Value}");
        }
        MoveNext();
        var expr = ParseExpression(0, scopedValues, functions);
        return new ReturnStatement(expr, scopedValues, functions);
    }
    
    ArgStatement ParseFuncDeclarionArgs(List<Dictionary<string, object>> scopedValues,
                                        List<Dictionary<string, LetStatementFuncDeclaration>> functions)
    {
        List<Token> argsToken = new List<Token>();
        while (CurrentToken().Type == TokenType.IDENT)
        {
            argsToken.Add(CurrentToken());
            MoveNext();
            if (CurrentToken().Type == TokenType.RPARENTESIS)
            {
                break;
            }
            if (CurrentToken().Type != TokenType.COMMA)
            {
                throw new Exception($"Expected ',', found instead {CurrentToken().Value}");
            }
            MoveNext();
        }
        return new ArgStatement(argsToken, scopedValues, functions);
    }
    ExpressionStatement ParseExpressionStatement(List<Dictionary<string, object>> scopedValues,
                                                 List<Dictionary<string, LetStatementFuncDeclaration>> functions)
    {
        return new ExpressionStatement(ParseExpression(0, scopedValues, functions), scopedValues, functions);
    }
    
    Expression ParseExpression(int precedence, 
                               List<Dictionary<string, object>> scopedValues,
                               List<Dictionary<string, LetStatementFuncDeclaration>> functions)
    {
        if (IsExpressionEnd())
        {
            return null;
        }
        var l = BuildPreffixExpression(scopedValues, functions);
        while (!IsExpressionEnd() && precedence < GetPrecedence(_infixPrecedence))
        {
            var token = CurrentToken();
            MoveNext();
            l = BuildInfixExpression(l, token, GetPrecedence(token, _infixPrecedence), scopedValues, functions);
        }
        return l;
    }

    bool IsStatementEnd()
    {
        return CurrentToken().Type == TokenType.SEMICOLON;
    }

    bool IsExpressionEnd()
    {
        return Eof() || IsStatementEnd() || CurrentToken().Type == TokenType.COMMA;
    }
    bool Eof()
    {
        return CurrentToken().Type == TokenType.EOF;
    }

    Expression BuildInfixExpression(Expression left, Token token, int precedence,
                                    List<Dictionary<string, object>> scopedValues,
                                    List<Dictionary<string, LetStatementFuncDeclaration>> functions)
    {
        var r = ParseExpression(precedence, scopedValues, functions);
        return new BinaryExpression(token, left, r);
    }
    
    private bool IsPreffixExpression(Token token)
    {
        return new TokenType[]
        {
            TokenType.PREFFIX_SUM,
            TokenType.PREFIX_SUBSTRACTION,
            TokenType.INT,
            TokenType.SUBSTRACTION,
            TokenType.SUM,
            TokenType.LPARENTESIS,
            TokenType.DECIMAL,
            TokenType.IDENT
        }.Contains(token.Type);
    }
    
    private Expression BuildPreffixExpression(List<Dictionary<string, object>> scopedValues,
                                              List<Dictionary<string, LetStatementFuncDeclaration>> functions)
    {
        if (!IsPreffixExpression(CurrentToken()))
        {
            throw new ArgumentException(CurrentToken().Value.ToString());
        }
        var token = CurrentToken();
        if (token.Type == TokenType.INT)
        {
            return ParseInt();
        } else if (token.Type == TokenType.DECIMAL)
        {
            return ParseDecimal();
        } else if (IsPreffixFunction())
        {
            return ParseFunctionCall(scopedValues, functions);
        } 
        else if (token.Type == TokenType.IDENT)
        {
            return ParseVariable();
        }
        else if (token.Type == TokenType.LPARENTESIS)
        {
            MoveNext();
            var parResp = ParseExpression(0, scopedValues, functions);
            MoveNext();
            return new PreffixExpression(token, parResp);
        }
        MoveNext();
        var r = ParseExpression(GetPrecedence(token, _preffixPrecedence), scopedValues, functions);
        return new PreffixExpression(token, r);
    }

    private Expression ParseFunctionCall(List<Dictionary<string, object>> scopedValues, 
                                         List<Dictionary<string, LetStatementFuncDeclaration>> functions)
    {
        var funcIdent = CurrentToken();
        MoveNext();
        var lP = CurrentToken();
        if (lP.Type != TokenType.LPARENTESIS)
        {
            throw new Exception($"Expecting '(' found {CurrentToken()}.");
        }
        MoveNext();
        var argExpressionList = new List<Expression>();
        while (CurrentToken().Type != TokenType.RPARENTESIS)
        {
            argExpressionList.Add(ParseExpression(0, scopedValues, functions));
            var endToken = CurrentToken();
            if (endToken.Type != TokenType.COMMA && endToken.Type != TokenType.RPARENTESIS)
            {
                throw new Exception($"Expected ',' found {CurrentToken().Value}");
            }
            if (endToken.Type == TokenType.RPARENTESIS)
            {
                break;
            }
            MoveNext();
        }
        MoveNext();
        return new FunctionCallExpression(funcIdent, argExpressionList);
    }
    private int GetPrecedence(IDictionary<TokenType, short> precedence)
    {
        var token = CurrentToken();
        return precedence[token.Type];
    }
    private int GetPrecedence(Token token, IDictionary<TokenType, short> precedence)
    {
        return precedence[token.Type];
    }
    private Expression ParseInt()
    {
        var expr = new IntExpression(CurrentToken());
        MoveNext();
        return expr;
    }

    private Expression ParseVariable()
    {
        var v = new VariableExpression(CurrentToken());
        MoveNext();
        return v;
    }
    private Expression ParseDecimal()
    {
        var expr = new DecimalExpression(CurrentToken());
        MoveNext();
        return expr;
    }
    private bool IsBinaryOperation(Token token)
    {
        return new TokenType[] { TokenType.SUM, TokenType.PRODUCT, TokenType.SUBSTRACTION, TokenType.DIVISION }.Contains(CurrentToken().Type);
    }
    private Token CurrentToken()
    {
        return _tokenList[_index];
    }

    private Token PeekNextToken()
    {
        return _tokenList[_index + 1];
    }
    private void MoveNext()
    {
        ++_index;
    }
    private bool HasToken()
    {
        return _index < _tokenList.Count;
    }
}

public abstract class Statement
{
    public Token Token;
    public bool HasReturn;
    public List<Dictionary<string, object>> ScopedValues;
    public List<Dictionary<string, LetStatementFuncDeclaration>> Functions;

    public Statement(Token token, bool hasReturn, 
                     List<Dictionary<string, object>> scopedValues,
                     List<Dictionary<string, LetStatementFuncDeclaration>> functions)
    {
        Token = token;
        HasReturn = hasReturn;
        ScopedValues = scopedValues;
        Functions = functions;
    }

    public void SetScopeValues(List<Dictionary<string, object>> scopedValues)
    {
        ScopedValues = scopedValues;
    }

    public void SetScopeFunctions(List<Dictionary<string, LetStatementFuncDeclaration>> scopedFunctions)
    {
        Functions = scopedFunctions;
    }

    protected Wrapper<LetStatementFuncDeclaration> GetFunctionInGivenScope(string name)
    {
        if (Functions.Last().ContainsKey(name))
        {
            return new Wrapper<LetStatementFuncDeclaration>(true, Functions.Last()[name]);
        }
        return new Wrapper<LetStatementFuncDeclaration>(false, null);
    }

    protected Wrapper<LetStatementFuncDeclaration> GetFunctionByName(string name)
    {
        for (int c = Functions.Count - 1; c >= 0; --c)
        {
            var scopedFunctions = Functions[c];
            if (scopedFunctions.ContainsKey(name))
            {
                return new Wrapper<LetStatementFuncDeclaration>(true, scopedFunctions[name]);
            }
        }
        return new Wrapper<LetStatementFuncDeclaration>(false, null);
    }

    protected Wrapper<object> GetValue(string name)
    {
        for (int c = ScopedValues.Count - 1; c >= 0; --c)
        {
            var scopedValues = ScopedValues[c];
            if (scopedValues.ContainsKey(name))
            {
                return new Wrapper<object>(true, scopedValues[name]);
            }
        }

        return new Wrapper<object>(false, null);
    }

    public abstract object Synthetize();
}

public class ReturnStatement : Statement
{
    public Expression Expr;
    public ReturnStatement(Expression expr, 
                           List<Dictionary<string, object>> scopedValues,
                           List<Dictionary<string, LetStatementFuncDeclaration>> functions) :
        base(new Token(TokenType.RETURN, "return"), true, scopedValues, functions)
    {
        Expr = expr;
    }

    public override object Synthetize()
    {
        return Expr.Synthetize(ScopedValues, Functions);
    }
}

public abstract class LetStatement : Statement
{
    protected LetStatement(Token token, bool hasReturn,
                           List<Dictionary<string, object>> scopedValues,
                           List<Dictionary<string, LetStatementFuncDeclaration>> functions) 
        : base(token, hasReturn, scopedValues, functions)
    {
    }
}

public class LetStatementExpression : LetStatement
{
    public Token Identifier { get; set; }
    public Expression Expr { get; set; }
    public LetStatementExpression(Token identifier, Expression expr,
                                  List<Dictionary<string, object>> scopedValues,
                                  List<Dictionary<string, LetStatementFuncDeclaration>> functions) : 
        base(new Token(TokenType.LET, "let"), false, scopedValues, functions)
    {
        Identifier = identifier;
        Expr = expr;
    }

    public override object Synthetize()
    {
        var identifier = GetValue(Identifier.Value);
        if (identifier.Exists)
        {
            throw new Exception($"Variable {Identifier.Value} declare more than one in the same scope.");
        }
        ScopedValues.Last().Add(Identifier.Value, Expr.Synthetize(ScopedValues, Functions));
        return null;
    }
}

public class ArgStatement : Statement
{
    public List<Token> Args { get; set; }
    public ArgStatement(List<Token> args, 
                        List<Dictionary<string, object>> scopedValues,
                        List<Dictionary<string, LetStatementFuncDeclaration>> functions) 
        : base( args.FirstOrDefault(), false, scopedValues, functions)
    {
        Args = args;
    }
    public override object Synthetize()
    {
        return null;
    }
}

public class LetStatementFuncDeclaration : LetStatement
{
    public ArgStatement Args;
    public Statement[] Statements;
    public LetStatementFuncDeclaration(Token ident, ArgStatement args, Statement[] statements,
                                       List<Dictionary<string, object>> scopedValues, 
                                       List<Dictionary<string, LetStatementFuncDeclaration>> functions)
        : base(ident, false, scopedValues, functions)
    {
        Args = args;
        Statements = statements;
        ScopedValues = scopedValues;
        Functions = functions;
    }

    public override object Synthetize()
    {
        var funcName = Token.Value;
        var functionDecl = GetFunctionInGivenScope(funcName);
        if (functionDecl.Exists)
        {
            throw new Exception($"Function {funcName} already declared.");
        }
        Functions.Last().Add(funcName, this);
        return null;
    }
}

public class ExpressionStatement : Statement
{
    public ExpressionStatement(Expression expr, 
                               List<Dictionary<string, object>> scopedValues,
                               List<Dictionary<string, LetStatementFuncDeclaration>> functions) :
        base(new Token(TokenType.SUM, "+"), true, scopedValues, functions)
    {
        Expr = expr;
    }
    public Expression Expr { get; set; }
    public override object Synthetize()
    {
        return Expr.Synthetize(ScopedValues, Functions);
    }
}

public class Wrapper<TValue>
{
    public Wrapper(bool exists, TValue value)
    {
        Exists = exists;
        Value = value;
    }

    public bool Exists { get; set; }
    public TValue Value { get; set; }
}

public abstract class Expression
{
    public Token Token;
    public Expression(Token token)
    {
        Token = token;
    }
    public abstract object Synthetize(List<Dictionary<string, object>> scopedValues,
                                      List<Dictionary<string, LetStatementFuncDeclaration>> functions);

    protected Wrapper<object> GetValue(List<Dictionary<string, object>> scopedValues, string name)
    {
        for (int c = scopedValues.Count - 1; c >= 0; --c)
        {
            if (scopedValues[c].ContainsKey(name))
            {
                return new Wrapper<object>(true, scopedValues[c][name]);
            }
        }

        return new Wrapper<object>(false, null);
    }
    
    protected Wrapper<LetStatementFuncDeclaration> GetFunction(List<Dictionary<string, LetStatementFuncDeclaration>> functions, string name)
    {
        for (int c = functions.Count - 1; c >= 0; --c)
        {
            if (functions[c].ContainsKey(name))
            {
                return new Wrapper<LetStatementFuncDeclaration>(true, functions[c][name]);
            }
        }
        return new Wrapper<LetStatementFuncDeclaration>(false, null);
    }
}

public class VariableExpression : Expression
{
    public VariableExpression(Token token) : base(token)
    {
    }

    public override object Synthetize(List<Dictionary<string, object>> scopedValues,
                                      List<Dictionary<string, LetStatementFuncDeclaration>> functions)
    {
        var identifier = Token.Value;
        var value = GetValue(scopedValues, identifier);
        if (!value.Exists)
        {
            throw new Exception($"Undeclared variable {identifier}");
        }
        return Convert.ToDouble(value.Value);
    }
}    
    
public class IntExpression : Expression
{
    public IntExpression(Token token) : base(token)
    {
    }

    public override object Synthetize(List<Dictionary<string, object>> scopedValues,
                                      List<Dictionary<string, LetStatementFuncDeclaration>> functions)
    {
        return int.Parse(Token.Value);
    }
}

public class DecimalExpression : Expression
{
    public DecimalExpression(Token token) : base(token)
    {
    }

    public override object Synthetize(List<Dictionary<string, object>> scopedValues,
                                      List<Dictionary<string, LetStatementFuncDeclaration>> functions)
    {
        return double.Parse(Token.Value);
    }
}

public class FunctionCallExpression : Expression
{
    public List<Expression> ArgsExpr { get; set; }
    public FunctionCallExpression(Token token, List<Expression> argsExpr) 
        : base(token)
    {
        ArgsExpr = argsExpr;
    }

    public override object Synthetize(List<Dictionary<string, object>> scopedValues, 
                                      List<Dictionary<string, LetStatementFuncDeclaration>> functions)
    {
        var funcDeclWrapper = GetFunction(functions, Token.Value);
        if (!funcDeclWrapper.Exists)
        {
            throw new Exception($"Function {Token.Value} not declared.");
        }
        var funcDecl = funcDeclWrapper.Value;
        var args = funcDecl.Args;
        var currentEnv = scopedValues.Last();
        for(int i = 0; i < ArgsExpr.Count; ++i)
        {
            var argIdent = args.Args[i].Value;
            var value = ArgsExpr[i].Synthetize(scopedValues, functions);
            if (currentEnv.ContainsKey(argIdent))
            {
                currentEnv[argIdent] = value;
            }
            else
            {
                currentEnv.Add(argIdent, value);
            }
        } 
        
        var statements = funcDecl.Statements;
        functions.Add(new Dictionary<string, LetStatementFuncDeclaration>());
        scopedValues.Add(new Dictionary<string, object>());
        object rs = null;
        for (int i = 0; i < statements.Length; i++)
        {
            var st = statements[i];
            st.SetScopeFunctions(functions);
            st.SetScopeValues(scopedValues);
            rs = st.Synthetize();
        }
        functions.Remove(functions.Last());
        scopedValues.Remove(scopedValues.Last());
        return rs;
    }
}

public class BinaryExpression : Expression
{
    public Expression Left;
    public Expression Right;

    public BinaryExpression(Token token, Expression l, Expression r) : base(token)
    {
        Left = l;
        Right = r;
    }
    public BinaryExpression(Token token, Expression l) : base(token)
    {
        Left = l;
    }

    private double ToDouble(object e)
    {
        if (e is double)
        {
            return (double)e;
        }
        return Convert.ToDouble(e);
    }

    public override object Synthetize(List<Dictionary<string, object>> scopedValues,
                                      List<Dictionary<string, LetStatementFuncDeclaration>> functions)
    {
        double rs = 0;
        if (Token.Type == TokenType.SUM)
        {
            var l = ToDouble(Left.Synthetize(scopedValues, functions));
            var r = ToDouble(Right.Synthetize(scopedValues, functions));
            rs = l + r;
            return rs;
        } else
        if (Token.Type == TokenType.PRODUCT)
        {
            rs = ToDouble(Left.Synthetize(scopedValues, functions)) * ToDouble(Right.Synthetize(scopedValues, functions));
        } else
        if (Token.Type == TokenType.DIVISION)
        {
            rs = ToDouble(Left.Synthetize(scopedValues, functions)) / ToDouble(Right.Synthetize(scopedValues, functions));
        } else
        if (Token.Type == TokenType.SUBSTRACTION)
        {
            rs = ToDouble(Left.Synthetize(scopedValues, functions)) - ToDouble(Right.Synthetize(scopedValues, functions));
        } else 
        if (Token.Type == TokenType.EXP)
        {
            rs = Math.Pow(ToDouble(Left.Synthetize(scopedValues, functions)), ToDouble(Right.Synthetize(scopedValues, functions)));
        }
        return rs;
    }
}

public class PreffixExpression : Expression
{
    public Expression Right;
    public PreffixExpression(Token token, Expression right) : base(token)
    {
        Right = right;
    }
    public override object Synthetize(List<Dictionary<string, object>> scopedValues,
                                      List<Dictionary<string, LetStatementFuncDeclaration>> functions)
    {
        var r = int.Parse(Right.Synthetize(scopedValues, functions).ToString());
        if (Token.Type == TokenType.PREFFIX_SUM)
        {
            return r + 1;
        } else if (Token.Type == TokenType.PREFIX_SUBSTRACTION)
        {
            return r - 1;
        } else if (Token.Type == TokenType.SUBSTRACTION)
        {
            return r * -1;
        } else if (Token.Type == TokenType.LPARENTESIS)
        {
            return Right.Synthetize(scopedValues, functions);
        }
        throw new Exception("Unknown preffix expression.");
    }
}