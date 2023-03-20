using System.Collections;
using System.Data.Common;
using System.Linq.Expressions;
using System.Net.Http.Headers;
using System.Security.AccessControl;

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
        IDictionary<string, object> scopedValues = new Dictionary<string, object>();
        IDictionary<string, LetStatementFuncDeclaration> functions =
            new Dictionary<string, LetStatementFuncDeclaration>();
        List<Statement> sts = new List<Statement>();
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

    LetStatement ParseLet(IDictionary<string, object> scopedValues, IDictionary<string, LetStatementFuncDeclaration> functions)
    {
        var letToken = CurrentToken();
        MoveNext();
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
            var args = ParseFuncDeclarionArgs(scopedValues);
            if (CurrentToken().Type != TokenType.RPARENTESIS)
            {
                throw new Exception($"Expected ')' found {CurrentToken().Value}");
            }
            MoveNext(); // ')'
            MoveNext(); // '{'
            var statements = GetFunctionStatements(scopedValues);
            MoveNext(); // '}'
            return new LetStatementFuncDeclaration(ident, args, statements, scopedValues, functions);
        } 
        var expr = ParseExpression(0, scopedValues, functions);
        return new LetStatementExpression(ident, expr, scopedValues);
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

    Statement[] GetFunctionStatements(IDictionary<string, object> scopedValues)
    {
        IDictionary<string, object> localScopedValues = new Dictionary<string, object>();
        IDictionary<string, LetStatementFuncDeclaration> functions =
            new Dictionary<string, LetStatementFuncDeclaration>();
        List<Statement> sts = new List<Statement>();
        while (CurrentToken().Type != TokenType.RBRACE)
        {
            var token = CurrentToken();
            Statement st = null;
            switch (token.Type)
            {
                case TokenType.LET:
                    st = ParseLet(localScopedValues, functions);
                    break;
                case TokenType.RETURN:
                    st = ParseReturnStatement(localScopedValues, functions);
                    break;
                default:
                    st = ParseExpressionStatement(localScopedValues, functions);
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

    ReturnStatement ParseReturnStatement(IDictionary<string, object> scopedValues,
        IDictionary<string, LetStatementFuncDeclaration> functions)
    {
        var returnToken = CurrentToken();
        if (returnToken.Type != TokenType.RETURN)
        {
            throw new Exception($"Expected keyword 'return' found {CurrentToken().Value}");
        }
        MoveNext();
        var expr = ParseExpression(0, scopedValues, functions);
        return new ReturnStatement(expr, scopedValues);
    }
    
    ArgStatement ParseFuncDeclarionArgs(IDictionary<string, object> scopedValues)
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
        return new ArgStatement(argsToken, scopedValues);
    }
    ExpressionStatement ParseExpressionStatement(IDictionary<string, object> scopedValues,
        IDictionary<string, LetStatementFuncDeclaration> functions)
    {
        return new ExpressionStatement(ParseExpression(0, scopedValues, functions), scopedValues);
    }
    
    Expression ParseExpression(int precedence, IDictionary<string, object> scopedValues,
        IDictionary<string, LetStatementFuncDeclaration> functions)
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
        IDictionary<string, object> scopedValues, IDictionary<string, LetStatementFuncDeclaration> functions)
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
    
    private Expression BuildPreffixExpression(IDictionary<string, object> scopedValues,
        IDictionary<string, LetStatementFuncDeclaration> functions)
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
            return ParseVariable(scopedValues);
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

    private Expression ParseFunctionCall(IDictionary<string, object> scopedValues, 
        IDictionary<string, LetStatementFuncDeclaration> functions)
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
        return new FunctionCallExpression(funcIdent, argExpressionList, functions);
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

    private Expression ParseVariable(IDictionary<string, object> scopedValues)
    {
        var v = new VariableExpression(CurrentToken(), scopedValues);
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
    public bool HasReturn { get; set; }
    public IDictionary<string, object> ScopedValues { get; set; }

    public Statement(Token token, bool hasReturn, IDictionary<string, object> scopedValues)
    {
        Token = token;
        HasReturn = hasReturn;
        ScopedValues = scopedValues;
    }

    public abstract object Synthetize();
}

public class ReturnStatement : Statement
{
    public Expression Expr;

    public ReturnStatement(Expression expr, IDictionary<string, object> scopedValues) :
        base(new Token(TokenType.RETURN, "return"), true, scopedValues)
    {
        Expr = expr;
    }

    public override object Synthetize()
    {
        return Expr.Synthetize();
    }
}

public abstract class LetStatement : Statement
{
    protected LetStatement(Token token, bool hasReturn, IDictionary<string, object> scopedValues) 
        : base(token, hasReturn, scopedValues)
    {
    }
}

public class LetStatementExpression : LetStatement
{
    public Token Identifier { get; set; }
    public Expression Expr { get; set; }
    public LetStatementExpression(Token identifier, Expression expr, IDictionary<string, object> scopedValues) : 
        base(new Token(TokenType.LET, "let"), false, scopedValues)
    {
        Identifier = identifier;
        Expr = expr;
    }

    public override object Synthetize()
    {
        if (ScopedValues.ContainsKey(Identifier.Value))
        {
            throw new Exception($"Variable {Identifier.Value} declare more than one in the same scope.");
        }
        ScopedValues.Add(Identifier.Value, Expr.Synthetize());
        return null;
    }
}

public class ArgStatement : Statement
{
    public List<Token> Args { get; set; }
    public ArgStatement(List<Token> args, IDictionary<string, object> scopedValues) 
        : base(args.First(), false, scopedValues)
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
    public IDictionary<string, object> ScopedValues;
    public IDictionary<string, LetStatementFuncDeclaration> Functions;
    public LetStatementFuncDeclaration(Token ident, ArgStatement args, Statement[] statements,
                                       IDictionary<string, object> scopedValues, 
                                       IDictionary<string, LetStatementFuncDeclaration> functions)
        : base(ident, false, scopedValues)
    {
        Args = args;
        Statements = statements;
        ScopedValues = scopedValues;
        Functions = functions;
    }

    public override object Synthetize()
    {
        var funcName = Token.Value;
        if (Functions.ContainsKey(funcName))
        {
            throw new Exception($"Function {funcName} already declared.");
        }
        Functions.Add(funcName, this);
        return null;
    }
}

public class ExpressionStatement : Statement
{
    public ExpressionStatement(Expression expr, IDictionary<string, object> scopedValues) :
        base(new Token(TokenType.SUM, "+"), true, scopedValues)
    {
        Expr = expr;
    }
    public Expression Expr { get; set; }
    public override object Synthetize()
    {
        return Expr.Synthetize();
    }
}

public abstract class Expression
{
    public Token Token;

    public Expression(Token token)
    {
        Token = token;
    }
    public abstract object Synthetize();
}

public class VariableExpression : Expression
{
    private IDictionary<string, object> _scopedValues;
    public VariableExpression(Token token, IDictionary<string, object> scopedValues) : base(token)
    {
        _scopedValues = scopedValues;
    }

    public override object Synthetize()
    {
        var identifier = Token.Value;
        if (!_scopedValues.ContainsKey(identifier))
        {
            throw new Exception($"Undeclared variable {identifier}");
        }
        return Convert.ToDouble(_scopedValues[identifier]);
    }
}    
    
public class IntExpression : Expression
{
    public IntExpression(Token token) : base(token)
    {
    }

    public override object Synthetize()
    {
        return int.Parse(Token.Value);
    }
}

public class DecimalExpression : Expression
{
    public DecimalExpression(Token token) : base(token)
    {
    }

    public override object Synthetize()
    {
        return double.Parse(Token.Value);
    }
}

public class FunctionCallExpression : Expression
{
    public IDictionary<string, LetStatementFuncDeclaration> Functions { get; set; }
    public List<Expression> ArgsExpr { get; set; }
    public FunctionCallExpression(Token token, List<Expression> argsExpr,
                                  IDictionary<string, LetStatementFuncDeclaration> functions) 
        : base(token)
    {
        ArgsExpr = argsExpr;
        Functions = functions;
    }

    public override object Synthetize()
    {
        var funcIdent = Token.Value;
        if (!Functions.ContainsKey(funcIdent))
        {
            throw new Exception($"Function {funcIdent} not declared.");
        }
        var funcDecl = Functions[funcIdent];
        var args = funcDecl.Args;
        var scopedValues = funcDecl.ScopedValues;

        var clonedScope = scopedValues.
            ToDictionary(x => x.Key, y => y.Value);
        for(int i = 0; i < ArgsExpr.Count; ++i)
        {
            var argIdent = args.Args[i].Value;
            var value = ArgsExpr[i].Synthetize();
            if (clonedScope.ContainsKey(argIdent))
            {
                clonedScope[argIdent] = value;
            }
            else
            {
                clonedScope.Add(argIdent, value);
            }
        } 
        
        var statements = funcDecl.Statements;
        for (int i = 0; i < statements.Length - 1; i++)
        {
            var st = statements[i];
            st.ScopedValues = clonedScope;
            st.Synthetize();
        }
        return statements.Last().Synthetize();
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

    public override object Synthetize()
    {
        double rs = 0;
        if (Token.Type == TokenType.SUM)
        {
            var l = ToDouble(Left.Synthetize());
            var r = ToDouble(Right.Synthetize());
            rs = l + r;
            return rs;
        } else
        if (Token.Type == TokenType.PRODUCT)
        {
            rs = ToDouble(Left.Synthetize()) * ToDouble(Right.Synthetize());
        } else
        if (Token.Type == TokenType.DIVISION)
        {
            rs = ToDouble(Left.Synthetize()) / ToDouble(Right.Synthetize());
        } else
        if (Token.Type == TokenType.SUBSTRACTION)
        {
            rs = ToDouble(Left.Synthetize()) - ToDouble(Right.Synthetize());
        } else 
        if (Token.Type == TokenType.EXP)
        {
            rs = Math.Pow(ToDouble(Left.Synthetize()), ToDouble(Right.Synthetize()));
        }
        return rs;
    }

    private double ToDouble(object e)
    {
        if (e is double)
        {
            return (double)e;
        }
        return Convert.ToDouble(e);
    }
}

public class PreffixExpression : Expression
{
    public Expression Right;
    public PreffixExpression(Token token, Expression right) : base(token)
    {
        Right = right;
    }
    public override object Synthetize()
    {
        var r = int.Parse(Right.Synthetize().ToString());
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
            return Right.Synthetize();
        }
        throw new Exception("Unknown preffix expression.");
    }
}