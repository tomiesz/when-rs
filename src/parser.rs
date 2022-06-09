use chrono::prelude::*;

#[derive(Debug, Clone, PartialEq)]
enum CalDate {
    Date(NaiveDate),
    Test(RPNExpr),
    Wildcard(Vec<Token>),
}

#[derive(Debug, Clone, PartialEq)]
struct RPNExpr {
    expr: Vec<Token>,
}

impl RPNExpr {
    fn from_infix(tokens: Vec<Token>) -> Self {
        let mut out = Vec::new();
        let mut op_stack = Vec::new();
        for t in tokens {
            match t {
                Token::Variable(v) => out.push(Token::Variable(v)),
                Token::Value(x) => out.push(Token::Value(x)),
                Token::Operator(OperatorType::CloseParenthesis) => loop {
                    match op_stack.pop() {
                        None => panic!("Missing opening parenthesis"),
                        Some(OperatorType::OpenParenthesis) => break,
                        Some(x) => out.push(Token::Operator(x)),
                    }
                },
                Token::Operator(OperatorType::OpenParenthesis) => {
                    op_stack.push(OperatorType::OpenParenthesis)
                }
                Token::Operator(op) => {
                    while !op_stack.is_empty() && op.prec() <= op_stack.last().unwrap().prec() {
                        if op == OperatorType::Not && op_stack.last().unwrap() == &OperatorType::Not
                        {
                            break;
                        } else {
                            out.push(Token::Operator(op_stack.pop().unwrap()));
                        }
                    }
                    op_stack.push(op);
                }
                Token::Month(m) => out.push(Token::Month(m)),
                Token::Wildcard => {
                    panic!("Wildcards should fall under simple evaluation")
                }
            }
        }

        if op_stack.contains(&OperatorType::OpenParenthesis) {
            panic!("Missing closing parenthesis.");
        } else if !op_stack.is_empty() {
            while let Some(op) = op_stack.pop() {
                out.push(Token::Operator(op));
            }
        }

        RPNExpr { expr: out }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CalItem<'a> {
    date: CalDate,
    descr: &'a str,
}

impl<'a> CalItem<'a> {
    fn from_line(txt: &'a str) -> Self {
        let mut temp_date: Option<CalDate> = None;
        let (date_str, descr) = txt.split_once(',').expect("Missing comma, no description");
        let tokens = CalItem::tokenize(date_str);
        if tokens.len() == 3 {
            match tokens[..] {
                [Token::Value(y), Token::Value(m), Token::Value(d)] => {
                    temp_date = Some(CalDate::Date(NaiveDate::from_ymd(
                        y.try_into().unwrap(),
                        m,
                        d,
                    )))
                }
                [Token::Value(y), Token::Month(m), Token::Value(d)] => {
                    temp_date = Some(CalDate::Date(NaiveDate::from_ymd(
                        y.try_into().unwrap(),
                        m,
                        d,
                    )))
                }
                _ => {
                    if tokens.contains(&Token::Wildcard)
                        && tokens.iter().any(|t| !t.is_operator() && !t.is_variable())
                    {
                        // if string contains 3 items, only wildcard and Value/Month then it must
                        // be a wildcard, otherwise it's a mixup in syntax
                        temp_date = Some(CalDate::Wildcard(tokens));
                    } else {
                        //malformed date
                        panic!("Malformed date string {}", date_str);
                    }
                }
            }
        } else if tokens.iter().any(|t| t.is_operator()) {
            //condition (not checked for correctness)
            temp_date = Some(CalDate::Test(RPNExpr::from_infix(tokens)));
        }
        CalItem {
            date: temp_date.unwrap(),
            descr: descr.trim(),
        }
    }
    fn tokenize(text: &str) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        let mut buf = String::new();
        for w in text.split_ascii_whitespace() {
            for c in w.chars() {
                if !buf.is_empty()
                    && CharType::from(c) != CharType::from(buf.chars().last().unwrap())
                {
                    tokens.push(Token::from(&buf.clone()));
                    buf.clear();
                }
                buf.push(c);
            }

            if !buf.is_empty() {
                tokens.push(Token::from(&buf.clone()));
                buf.clear();
            }
        }

        if !buf.is_empty() {
            tokens.push(Token::from(&buf));
        }

        tokens
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
enum Token {
    Month(u32),
    Operator(OperatorType),
    Wildcard,
    Value(u32),
    Variable(VariableType),
}

#[derive(Debug, Clone, PartialEq, Copy)]
enum CharType {
    Alphabetic,
    Numeric,
    Symbol,
    Misc,
}

impl CharType {
    fn from(c: char) -> Self {
        if c.is_ascii_alphabetic() {
            CharType::Alphabetic
        } else if c.is_ascii_digit() {
            CharType::Numeric
        } else if c.is_ascii_punctuation() {
            CharType::Symbol
        } else {
            CharType::Misc
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
enum OperatorType {
    Modulo,
    Substraction,
    Lesser,
    Greater,
    LesserOrEqual,
    GreaterOrEqual,
    Equal,
    NotEqual,
    And,
    Not, // this is right associative
    Or,
    OpenParenthesis,
    CloseParenthesis,
}

impl OperatorType {
    fn prec(&self) -> u8 {
        match self {
            OperatorType::Not => 6,
            OperatorType::Modulo => 5,
            OperatorType::Substraction => 4,
            OperatorType::Greater => 3,
            OperatorType::GreaterOrEqual => 3,
            OperatorType::Lesser => 3,
            OperatorType::LesserOrEqual => 3,
            OperatorType::Equal => 2,
            OperatorType::NotEqual => 2,
            OperatorType::And => 1,
            OperatorType::Or => 0,
            _ => 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
enum VariableType {
    DayOfWeek,
    Month,
    DayOfMonth,
    Year,
    ModifiedJulianDay,
    WeekInMonth,
    RWeekInMonth,
    NearWeekend,
    DaysUntilEaster,
    DayOfTheYear,
}

impl Token {
    fn get_value(&self) -> Option<u32> {
        match self {
            Token::Value(val) => Some(*val),
            _ => None,
        }
    }
    fn is_value(&self) -> bool {
        match self {
            Token::Value(_) => true,
            _ => false,
        }
    }
    fn is_operator(&self) -> bool {
        match self {
            Token::Operator(_) => true,
            _ => false,
        }
    }
    fn is_variable(&self) -> bool {
        match self {
            Token::Variable(_) => true,
            _ => false,
        }
    }
    fn is_wildcard(&self) -> bool {
        match self {
            Token::Wildcard => true,
            _ => false,
        }
    }
    fn from(tok: &str) -> Self {
        let tok_up = tok.to_uppercase();
        let months: [&str; 12] = [
            "JANUARY",
            "FEBRUARY",
            "MARCH",
            "APRIL",
            "MAY",
            "JUNE",
            "JULY",
            "AUGUST",
            "SEPTEMBER",
            "OCTOBER",
            "NOVEMBER",
            "DECEMBER",
        ];
        if let Ok(val) = tok_up.parse::<u32>() {
            Token::Value(val)
        } else {
            match tok_up.as_str() {
                "*" => Token::Wildcard,
                "W" => Token::Variable(VariableType::DayOfWeek),
                "M" => Token::Variable(VariableType::Month),
                "D" => Token::Variable(VariableType::DayOfMonth),
                "Y" => Token::Variable(VariableType::Year),
                "J" => Token::Variable(VariableType::ModifiedJulianDay),
                "A" => Token::Variable(VariableType::WeekInMonth),
                "B" => Token::Variable(VariableType::RWeekInMonth),
                "C" => Token::Variable(VariableType::NearWeekend),
                "E" => Token::Variable(VariableType::DaysUntilEaster),
                "Z" => Token::Variable(VariableType::DayOfTheYear),
                "%" => Token::Operator(OperatorType::Modulo),
                "-" => Token::Operator(OperatorType::Substraction),
                "<" => Token::Operator(OperatorType::Lesser),
                ">" => Token::Operator(OperatorType::Greater),
                "<=" => Token::Operator(OperatorType::LesserOrEqual),
                ">=" => Token::Operator(OperatorType::GreaterOrEqual),
                "=" => Token::Operator(OperatorType::Equal),
                "!=" => Token::Operator(OperatorType::NotEqual),
                "&" => Token::Operator(OperatorType::And),
                "!" => Token::Operator(OperatorType::Not),
                "|" => Token::Operator(OperatorType::Or),
                "(" => Token::Operator(OperatorType::OpenParenthesis),
                ")" => Token::Operator(OperatorType::CloseParenthesis),
                _ => {
                    let matching: Vec<&&str> = months
                        .iter()
                        .filter(|b| b.starts_with(tok_up.as_str()))
                        .collect();
                    if matching.len() == 1 {
                        let pos = months.iter().position(|t| t == matching[0]).unwrap() + 1;
                        Token::Month(pos.try_into().unwrap())
                    } else if matching.len() > 1 {
                        panic!("Cannot determine month: {}", tok);
                    } else {
                        panic!("Invalid token {}", tok);
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ParseError {
    SyntaxError,
    IllegalDay,
    IllegalYear,
    AmbiguousMonth,
    DateError,
}

#[derive(Debug, Clone, PartialEq, Copy)]
struct Timeframe {
    start: NaiveDate,
    end: NaiveDate,
}

#[cfg(test)]
pub mod test {
    use super::*;
    use crate::tokens;
    #[test]
    fn month_matching() {
        assert_eq!(Token::Month(4), Token::from("Ap"));
        assert_eq!(Token::Month(10), Token::from("octo"));
    }
    #[test]
    fn arbitrary_val_matching() {
        assert_eq!(Token::Value(10), Token::from("10"));
    }

    #[test]
    fn special_matching() {
        assert_eq!(Token::Wildcard, Token::from("*"));
        assert_eq!(
            Token::Operator(OperatorType::Substraction),
            Token::from("-")
        );
        assert_eq!(Token::Variable(VariableType::Month), Token::from("M"));
    }
    #[test]
    fn to_tokens() {
        assert_eq!(
            CalItem::tokenize("d<18 & d>20"),
            tokens!("D", "<", "18", "&", "D", ">", "20")
        );
        assert_eq!(CalItem::tokenize("ap * *"), tokens!("AP", "*", "*"));
        assert_eq!(
            CalItem::tokenize("d =18  d=18 d = 18  d=18"),
            tokens!("D", "=", "18", "D", "=", "18", "D", "=", "18", "D", "=", "18")
        );
    }
    #[test]
    #[should_panic(expected = "Invalid token D<18")]
    fn string_not_split() {
        Token::from("D<18");
    }
    #[test]
    fn basic_postfix() {
        let infix = [
            tokens!("D", ">", "18"),
            tokens!("!", "(", "d", ">", "18", ")", "&", "(", "d", "<", "20", ")"),
            tokens!("!", "(", "d", ">", "18", ")", "&", "!", "(", "d", "<", "20", ")"),
            tokens!(
                "(", "d", ">", "d", ")", "&", "!", "(", "d", "=", "d", ")", "|", "(", "d", "<",
                "d", ")"
            ),
            tokens!("d", ">", "d", "&", "d", "=", "d", "|", "d", "<", "d"),
        ];
        let postfix = [
            tokens!("D", "18", ">"),
            tokens!("d", "18", ">", "!", "d", "20", "<", "&"),
            tokens!("d", "18", ">", "!", "d", "20", "<", "!", "&"),
            tokens!("d", "d", ">", "d", "d", "=", "!", "&", "d", "d", "<", "|"),
            tokens!("d", "d", ">", "d", "d", "=", "&", "d", "d", "<", "|"),
        ];
        for (i, p) in infix.into_iter().zip(postfix) {
            assert_eq!(RPNExpr::from_infix(i), RPNExpr { expr: p })
        }
    }
    #[test]
    #[should_panic(expected = "Wildcards should fall under simple evaluation")]
    fn wildcard_panic() {
        RPNExpr::from_infix(tokens!("2022", "*", "1"));
    }
    #[test]
    #[should_panic(expected = "Missing opening parenthesis")]
    fn missing_open_parenth() {
        RPNExpr::from_infix(tokens!("d", "<", "18", "&", ")"));
    }
    #[test]
    #[should_panic(expected = "Missing closing parenthesis")]
    fn missing_close_parenth() {
        RPNExpr::from_infix(tokens!("d", "<", "18", "&", "("));
    }
    #[test]
    fn create_simple_date() {
        assert_eq!(
            CalItem::from_line("2022 Octo 05, simple date with month name"),
            CalItem {
                date: CalDate::Date(NaiveDate::from_ymd(2022, 10, 05)),
                descr: "simple date with month name"
            }
        );
        assert_eq!(
            CalItem::from_line("2022 10 05, simple date"),
            CalItem {
                date: CalDate::Date(NaiveDate::from_ymd(2022, 10, 05)),
                descr: "simple date"
            }
        );
    }
    #[test]
    fn create_expression() {
        assert_eq!(
            CalItem::from_line("y=2022 & m=10 & d=15, simple expr"),
            CalItem {
                date: CalDate::Test(RPNExpr {
                    expr: tokens!("y", "2022", "=", "m", "10", "=", "&", "d", "15", "=", "&")
                }),
                descr: "simple expr"
            }
        );

        assert_eq!(
            CalItem::from_line("y=2022 & m=10 & d>10 & d<15, conditional"),
            CalItem {
                date: CalDate::Test(RPNExpr {
                    expr: tokens!(
                        "y", "2022", "=", "m", "10", "=", "&", "d", "10", ">", "&", "d", "15", "<",
                        "&"
                    )
                }),
                descr: "conditional"
            }
        );
    }
    #[test]
    fn create_wildcard() {
        assert_eq!(
            CalItem::from_line("* 10 15, wildcard year"),
            CalItem {
                date: CalDate::Wildcard(tokens!("*", "10", "15")),
                descr: "wildcard year"
            }
        );

        assert_eq!(
            CalItem::from_line("2022 * 15, wildcard month"),
            CalItem {
                date: CalDate::Wildcard(tokens!("2022", "*", "15")),
                descr: "wildcard month"
            }
        );
        assert_eq!(
            CalItem::from_line("2022 10 *, wildcard day"),
            CalItem {
                date: CalDate::Wildcard(tokens!("2022", "10", "*")),
                descr: "wildcard day"
            }
        );
    }
    #[test]
    fn additional_wildcards() {
        //this is not in the original program
        assert_eq!(
            CalItem::from_line("2022 * *, wildcard every day of year"),
            CalItem {
                date: CalDate::Wildcard(tokens!("2022", "*", "*")),
                descr: "wildcard every day of year"
            }
        );
        assert_eq!(
            CalItem::from_line("* * *, wildcard every day"),
            CalItem {
                date: CalDate::Wildcard(tokens!("*", "*", "*")),
                descr: "wildcard every day"
            }
        );
        assert_eq!(
            CalItem::from_line("* * 10, wildcard every tenth day of month"),
            CalItem {
                date: CalDate::Wildcard(tokens!("*", "*", "10")),
                descr: "wildcard every tenth day of month"
            }
        );
    }
}
