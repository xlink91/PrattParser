using System.Collections;
using System.Reflection.Metadata.Ecma335;
using System.Text;

namespace PrattParser.Lexer;

public class Lexer
{
    private int _index;
    private string _code;

    private IDictionary<string, TokenType> _reservedWords = new Dictionary<string, TokenType>()
    {
        { "let", TokenType.LET },
        { "return", TokenType.RETURN },
        { "func", TokenType.FUNC }
    };

    public Lexer(string code)
    {
        _code = code;
    }
    
    public List<Token> GetTokens()
    {
        var tokenList = new List<Token>();
        while(HasNext())
        {
            char ch = Current();
            Token token = null;
            if (ch.IsWhiteSpace())
            {
                MoveNext();
                continue;
            }
            if (ch.IsSemicolon())
            {
                token = new Token(TokenType.SEMICOLON, ";");
                MoveNext();
            }else if (char.IsDigit(ch))
            {
                token = GetNumber();
            }else if (ch.IsOperator())
            {
                token = GetOperator();
            } else if (ch.IsLeftParenthesis())
            {
                token = new Token(TokenType.LPARENTESIS, "(");
                MoveNext();
            } else if (ch.IsRightParenthesis())
            {
                token = new Token(TokenType.RPARENTESIS, ")");
                MoveNext();
            } else if (ch.IsIdent())
            {
                token = GetIdentifier();
            } else if (ch.IsBraces())
            {
                token = GetBraces();
                MoveNext();
            } else if (ch.IsComa())
            {
                token = GetComa();
                MoveNext();
            }
            tokenList.Add(token);
        }
        tokenList.Add(new Token(TokenType.EOF, string.Empty));
        return tokenList;
    }
    private bool HasNext()
    {
        return _index < _code.Length;
    }
    private char PeekNextChar()
    {
        if (HasNext()) return _code[_index + 1];
        return (char)0;
    }
    private void MoveNext()
    {
        _index++;
    }
    private char Current()
    {
        return _code[_index];
    }
    private Token GetNumber()
    {
        var number = new StringBuilder();
        while (HasNext() && (char.IsDigit(Current()) || Current() == '.'))
        {
            number.Append(Current());
            MoveNext();
        }

        var numberAsString = number.ToString();
        var decimalPointCounter = numberAsString.Count(x => x == '.');
        if (decimalPointCounter > 1)
        {
            throw new Exception($"Wrong number {numberAsString}.");
        }
        return new Token(decimalPointCounter == 1 ? TokenType.DECIMAL : TokenType.INT, number.ToString());
    }
    private Token GetOperator()
    {
        Token token = null;
        if (Current() == '+' && PeekNextChar() == '+')
        {
            MoveNext();
            token = new Token(TokenType.PREFFIX_SUM, "++");
        } else if (Current() == '-' && PeekNextChar() == '-')
        {
            MoveNext();
            token = new Token(TokenType.PREFIX_SUBSTRACTION, "--");
        }
        else
        {
            switch (Current())
            {
               case '+': 
                   token = new Token(TokenType.SUM, "+");
                   break;
               case '-': 
                   token = new Token(TokenType.SUBSTRACTION, "-");
                   break;
               case '/': 
                   token = new Token(TokenType.DIVISION, "/");
                   break;
               case '*': 
                   token = new Token(TokenType.PRODUCT, "*");
                   break;
               case '^': 
                   token = new Token(TokenType.EXP, "^");
                   break;
               case '=': 
                   token = new Token(TokenType.ASSIGN, "=");
                   break;
            }
        }
        MoveNext();
        return token;
    }
    private Token GetIdentifier()
    {
        var builder = new StringBuilder();
        while (char.IsLetter(Current()) || char.IsDigit(Current()) || Current() == '_')
        {
            builder.Append(Current());
            MoveNext();
        }

        var identifier = builder.ToString();
        var identifierType = GetTokenTypeFromIdentifier(identifier);
        return new Token(identifierType, identifier);
    }

    private TokenType GetTokenTypeFromIdentifier(string identifier)
    {
        var exists = _reservedWords.TryGetValue(identifier, out TokenType type);
        return exists ? type : TokenType.IDENT;
    }

    private Token GetBraces()
    {
        var brace = Current();
        if (brace == '{')
        {
            return new Token(TokenType.LBRACE, "{");
        } else if (Current() == '}')
        {
            return new Token(TokenType.RBRACE, "}");
        }
        throw new Exception($"Expected token '{{' or '}}' found {Current()}.");
    }

    private Token GetComa()
    {
        if (Current() != ',')
        {
        throw new Exception($"Expected token ',' found {Current()}.");
        }
        return new Token(TokenType.COMMA, ",");
    }
}
    
internal static class CharacterRecognition 
{
    public static bool IsOperator(this char ch)
    {
        return new[] { '+', '-', '*', '/', '^', '=' }.Contains(ch);
    }
    public static bool IsBraces(this char ch)
    {
        return new[] { '{', '}' }.Contains(ch);
    }
    public static bool IsLeftParenthesis(this char ch)
    {
        return ch == '(';
    }
    public static bool IsRightParenthesis(this char ch)
    {
        return ch == ')';
    }
    public static bool IsWhiteSpace(this char ch)
    {
        return ch == ' ';
    }
    public static bool IsIdent(this char ch)
    {
        return char.IsLetter(ch);
    }
    public static bool IsSemicolon(this char ch)
    {
        return ch == ';';
    }
    public static bool IsComa(this char ch)
    {
        return ch == ',';
    }
}

public class Token
{
    public Token(TokenType type, string value)
    {
        Type = type;
        Value = value;
    }

    public TokenType Type { get; set; }
    public string Value { get; set; }

    override public string ToString()
    {
        return $"TokenType: {Type.ToString()}, Value: {Value}";
    }
}

public enum TokenType
{
    IDENT,                // variable
    INT,                  // integer
    DECIMAL,              // decimal
    EXP,                  // exponent
    PRODUCT,              // *
    SUM,                  // +
    SUBSTRACTION,         // -
    DIVISION,             // /    
    LPARENTESIS,          // (
    RPARENTESIS,          // )
    LBRACE,               // {
    RBRACE,               // }  
    PREFFIX_SUM,          // ++a
    PREFIX_SUBSTRACTION,  // --b
    
    SEMICOLON,            // ;
    COMMA,                 //
    
    ASSIGN,               // =
    
    LET,
    RETURN,
    FUNC,               
    EOF
}

