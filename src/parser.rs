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
    fn from_infix(tokens: Vec<Token>) -> Result<Self, ParseError> {
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
                Token::Month(m) => out.push(Token::Value(m)),
                Token::Weekday(w) => out.push(Token::Value(w)),
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

        Ok(RPNExpr { expr: out })
    }
    fn eval(&self, date: NaiveDate) -> bool {
        let mut stack: Vec<StackItem> = Vec::new();
        for tok in &self.expr {
            dbg!(&stack);
            match tok {
                Token::Value(x) => stack.push(StackItem::Num((*x).try_into().unwrap())),
                Token::Variable(var) => stack.push(StackItem::Num(var.realise(date))), //convert variables to the proper things (another match?)
                Token::Month(_) => panic!("Month in infix expression"),
                Token::Weekday(_) => panic!("Weekday in infix expression"),
                Token::Wildcard => panic!("Wildcard in conditional"),
                Token::Operator(OperatorType::OpenParenthesis) => {
                    panic!("Opening parenthesis in infix expression")
                }
                Token::Operator(OperatorType::CloseParenthesis) => {
                    panic!("Closing parenthesis in infix expression")
                }
                Token::Operator(op) => op.exec(&mut stack),
            };
        }
        dbg!(&stack);
        if stack.len() == 1 {
            match stack[0] {
                StackItem::Bool(b) => return b,
                StackItem::Num(_) => panic!(),
                StackItem::Op(_) => panic!(),
            };
        } else {
            panic!("Conditional evaluation failed");
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CalItem<'a> {
    date: CalDate,
    descr: &'a str,
}

impl<'a> CalItem<'a> {
    fn from_line(txt: &'a str) -> Result<Self, ParseError> {
        let mut temp_date: Option<CalDate> = None;
        let (date_str, descr) = txt.split_once(',').expect("Missing comma, no description");
        let tokens = CalItem::tokenize(date_str)?;
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
            temp_date = Some(CalDate::Test(RPNExpr::from_infix(tokens)?));
        }
        Ok(CalItem {
            date: temp_date.unwrap(),
            descr: descr.trim(),
        })
    }
    fn tokenize(text: &str) -> Result<Vec<Token>, ParseError> {
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

        Ok(tokens)
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
enum Token {
    Month(u32),
    Weekday(u32),
    Operator(OperatorType),
    Wildcard,
    Value(u32),
    Variable(VariableType),
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
        let daysofweek: [&str; 7] = [
            "MONDAY",
            "TUESDAY",
            "wEDNESDAY",
            "THURSDAY",
            "FRIDAY",
            "SATURDAY",
            "SUNDAY",
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
                "-" => Token::Operator(OperatorType::Subtraction),
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
                    let matching_month: Vec<&&str> = months
                        .iter()
                        .filter(|b| b.starts_with(tok_up.as_str()))
                        .collect();
                    let matching_day: Vec<&&str> = daysofweek
                        .iter()
                        .filter(|w| w.starts_with(tok_up.as_str()))
                        .collect();
                    if matching_month.len() == 1 {
                        let pos = months.iter().position(|t| t == matching_month[0]).unwrap() + 1;
                        Token::Month(pos.try_into().unwrap())
                    } else if matching_month.len() > 1 {
                        panic!("Cannot determine month: {}", tok);
                    } else if matching_day.len() == 1 {
                        let pos = daysofweek
                            .iter()
                            .position(|t| t == matching_day[0])
                            .unwrap()
                            + 1;
                        Token::Weekday(pos.try_into().unwrap())
                    } else if matching_day.len() > 1 {
                        panic!("Cannot determine day: {}", tok);
                    } else {
                        panic!("Invalid token {}", tok);
                    }
                }
            }
        }
    }
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
    Subtraction,
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
            OperatorType::Subtraction => 4,
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
    fn arg_count(&self) -> u8 {
        match self {
            OperatorType::Modulo => 2,
            OperatorType::Subtraction => 2,
            OperatorType::Lesser => 2,
            OperatorType::Greater => 2,
            OperatorType::LesserOrEqual => 2,
            OperatorType::GreaterOrEqual => 2,
            OperatorType::Equal => 2,
            OperatorType::NotEqual => 2,
            OperatorType::And => 2,
            OperatorType::Not => 1,
            OperatorType::Or => 2,
            _ => 0,
        }
    }
    fn exec(&self, stack: &mut Vec<StackItem>) {
        if self.arg_count() == 2 {
            let items = (stack.pop().unwrap(), stack.pop().unwrap());
            if let (StackItem::Num(y), StackItem::Num(x)) = items {
                match self {
                    OperatorType::Modulo => stack.push(StackItem::Num(x % y)),
                    OperatorType::Subtraction => stack.push(StackItem::Num(x - y)),
                    OperatorType::Lesser => stack.push(StackItem::Bool(x < y)),
                    OperatorType::Greater => stack.push(StackItem::Bool(x > y)),
                    OperatorType::LesserOrEqual => stack.push(StackItem::Bool(x <= y)),
                    OperatorType::GreaterOrEqual => stack.push(StackItem::Bool(x >= y)),
                    OperatorType::Equal => stack.push(StackItem::Bool(x == y)),
                    OperatorType::NotEqual => stack.push(StackItem::Bool(x != y)),
                    _ => panic!(),
                }
            } else if let (StackItem::Bool(b), StackItem::Bool(a)) = items {
                match self {
                    OperatorType::Equal => stack.push(StackItem::Bool(a == b)),
                    OperatorType::NotEqual => stack.push(StackItem::Bool(a != b)),
                    OperatorType::And => stack.push(StackItem::Bool(a && b)),
                    OperatorType::Or => stack.push(StackItem::Bool(a || b)),
                    _ => panic!(),
                }
            }
        } else if self.arg_count() == 1 {
            if let Some(StackItem::Bool(a)) = stack.pop() {
                stack.push(StackItem::Bool(!a));
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }
}

#[derive(Clone, Debug, Copy, PartialEq)]
enum StackItem {
    Bool(bool),
    Num(i32),
    Op(OperatorType),
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

impl VariableType {
    fn realise(&self, date: NaiveDate) -> i32 {
        match self {
            VariableType::Month => date.month().try_into().unwrap(),
            VariableType::DayOfWeek => date.weekday().number_from_sunday().try_into().unwrap(),
            VariableType::DayOfMonth => date.day().try_into().unwrap(),
            VariableType::Year => date.year().try_into().unwrap(),
            VariableType::ModifiedJulianDay => {
                (date.num_days_from_ce() - 678575).try_into().unwrap()
            }
            VariableType::WeekInMonth => ((f64::from(date.day()) / 7f64).ceil()) as i32,
            VariableType::RWeekInMonth => {
                ((f64::from(
                    date.with_month(date.month() + 1)
                        .unwrap()
                        .with_day(1)
                        .unwrap()
                        .pred()
                        .day()
                        - date.day(),
                ) / 7f64)
                    .ceil()) as i32
            }
            VariableType::NearWeekend => match date.weekday() {
                Weekday::Mon => date
                    .checked_add_signed(chrono::Duration::days(5))
                    .unwrap()
                    .day()
                    .try_into()
                    .unwrap(),
                Weekday::Fri => date.succ().day().try_into().unwrap(),
                _ => -1,
            },
            VariableType::DaysUntilEaster => panic!(), //i really dont care to implement this now.
            VariableType::DayOfTheYear => date.ordinal().try_into().unwrap(),
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
    MalformedData,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct Timeframe {
    pub start: NaiveDate,
    pub end: NaiveDate,
}

impl Timeframe {
    fn is_in(&self, date: NaiveDate) -> bool {
        date >= self.start && date <= self.end
    }
}

#[cfg(test)]
pub mod test {
    use super::*;
    use crate::tokens;
    fn test_tf() -> Timeframe {
        Timeframe {
            start: NaiveDate::from_ymd(2022, 10, 1),
            end: NaiveDate::from_ymd(2022, 10, 31),
        }
    }
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
        assert_eq!(Token::Operator(OperatorType::Subtraction), Token::from("-"));
        assert_eq!(Token::Variable(VariableType::Month), Token::from("M"));
    }
    #[test]
    fn to_tokens() {
        assert_eq!(
            CalItem::tokenize("d<18 & d>20"),
            Ok(tokens!("D", "<", "18", "&", "D", ">", "20"))
        );
        assert_eq!(CalItem::tokenize("ap * *"), Ok(tokens!("AP", "*", "*")));
        assert_eq!(
            CalItem::tokenize("d =18  d=18 d = 18  d=18"),
            Ok(tokens!(
                "D", "=", "18", "D", "=", "18", "D", "=", "18", "D", "=", "18"
            ))
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
            assert_eq!(RPNExpr::from_infix(i), Ok(RPNExpr { expr: p }))
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
            Ok(CalItem {
                date: CalDate::Date(NaiveDate::from_ymd(2022, 10, 05)),
                descr: "simple date with month name"
            })
        );
        assert_eq!(
            CalItem::from_line("2022 10 05, simple date"),
            Ok(CalItem {
                date: CalDate::Date(NaiveDate::from_ymd(2022, 10, 05)),
                descr: "simple date"
            })
        );
    }
    #[test]
    fn create_expression() {
        assert_eq!(
            CalItem::from_line("y=2022 & m=10 & d=15, simple expr"),
            Ok(CalItem {
                date: CalDate::Test(RPNExpr {
                    expr: tokens!("y", "2022", "=", "m", "10", "=", "&", "d", "15", "=", "&")
                }),
                descr: "simple expr"
            })
        );

        assert_eq!(
            CalItem::from_line("y=2022 & m=10 & d>10 & d<15, conditional"),
            Ok(CalItem {
                date: CalDate::Test(RPNExpr {
                    expr: tokens!(
                        "y", "2022", "=", "m", "10", "=", "&", "d", "10", ">", "&", "d", "15", "<",
                        "&"
                    )
                }),
                descr: "conditional"
            })
        );
    }
    #[test]
    fn create_wildcard() {
        assert_eq!(
            CalItem::from_line("* 10 15, wildcard year"),
            Ok(CalItem {
                date: CalDate::Wildcard(tokens!("*", "10", "15")),
                descr: "wildcard year"
            })
        );

        assert_eq!(
            CalItem::from_line("2022 * 15, wildcard month"),
            Ok(CalItem {
                date: CalDate::Wildcard(tokens!("2022", "*", "15")),
                descr: "wildcard month"
            })
        );
        assert_eq!(
            CalItem::from_line("2022 10 *, wildcard day"),
            Ok(CalItem {
                date: CalDate::Wildcard(tokens!("2022", "10", "*")),
                descr: "wildcard day"
            })
        );
    }
    #[test]
    fn additional_wildcards() {
        //this is not in the original program
        assert_eq!(
            CalItem::from_line("2022 * *, wildcard every day of year"),
            Ok(CalItem {
                date: CalDate::Wildcard(tokens!("2022", "*", "*")),
                descr: "wildcard every day of year"
            })
        );
        assert_eq!(
            CalItem::from_line("* * *, wildcard every day"),
            Ok(CalItem {
                date: CalDate::Wildcard(tokens!("*", "*", "*")),
                descr: "wildcard every day"
            })
        );
        assert_eq!(
            CalItem::from_line("* * 10, wildcard every tenth day of month"),
            Ok(CalItem {
                date: CalDate::Wildcard(tokens!("*", "*", "10")),
                descr: "wildcard every tenth day of month"
            })
        );
    }
    #[test]
    fn check_dates_in_timeframe() {
        let test1 = vec![
            NaiveDate::from_ymd(2022, 10, 15),
            NaiveDate::from_ymd(2023, 10, 15),
        ];
        let tf = test_tf();
        assert_eq!(tf.is_in(test1[0]), true);
        assert_eq!(tf.is_in(test1[1]), false);
    }
    #[test]
    fn eval_rpn_against_date() {
        let date1 = NaiveDate::from_ymd(2022, 10, 15);
        let date2 = NaiveDate::from_ymd(2022, 10, 14);
        let rpn1 = RPNExpr {
            expr: tokens!("y", "2022", "=", "m", "10", "=", "&", "d", "15", "=", "&"),
        };
        let rpn2 = RPNExpr {
            expr: tokens!(
                "y", "2022", "=", "m", "10", "=", "&", "d", "15", "<", "&", "d", "10", ">", "&"
            ),
        };
        assert_eq!(rpn1.eval(date1), true);
        assert_eq!(rpn1.eval(date2), false);
        assert_eq!(rpn2.eval(date2), true);
        assert_eq!(rpn2.eval(date1), false);
    }
    #[test]
    fn execute_op() {
        let stack_cmp = vec![StackItem::Num(10), StackItem::Num(15)];
        let ops_cmp = [
            OperatorType::Lesser,
            OperatorType::Greater,
            OperatorType::LesserOrEqual,
            OperatorType::GreaterOrEqual,
            OperatorType::Equal,
            OperatorType::NotEqual,
        ];
        let results_cmp = [true, false, true, false, false, true];
        for (op, res) in ops_cmp.iter().zip(results_cmp) {
            dbg!(op);
            let mut temp = stack_cmp.clone();
            op.exec(&mut temp);
            assert!(temp.len() == 1);
            match temp[0] {
                StackItem::Bool(b) => assert_eq!(b, res),
                _ => panic!("Wrong item left in stack: {:#?}", temp),
            }
        }

        let stack_bool = vec![StackItem::Bool(true), StackItem::Bool(false)];
        let ops_bool = vec![OperatorType::And, OperatorType::Or];
        let results_bool = [false, true];
        for (op, res) in ops_bool.iter().zip(results_bool) {
            dbg!(op);
            let mut temp = stack_bool.clone();
            op.exec(&mut temp);
            assert!(temp.len() == 1);
            match temp[0] {
                StackItem::Bool(b) => assert_eq!(b, res),
                _ => panic!("Wrong item left in stack: {:#?}", temp),
            }
        }

        let stack_math = vec![StackItem::Num(12), StackItem::Num(5)];
        let ops_math = [OperatorType::Modulo, OperatorType::Subtraction];
        let results_math = [2, 7];
        for (op, res) in ops_math.iter().zip(results_math) {
            dbg!(op);
            let mut temp = stack_math.clone();
            op.exec(&mut temp);
            assert!(temp.len() == 1);
            match temp[0] {
                StackItem::Num(n) => assert_eq!(n, res),
                _ => panic!("Wrong item left in stack: {:#?}", temp),
            }
        }

        let mut stack_not = vec![StackItem::Bool(true)];
        let not = OperatorType::Not;
        not.exec(&mut stack_not);
        assert_eq!(stack_not[0], StackItem::Bool(false));
        not.exec(&mut stack_not);
        assert_eq!(stack_not[0], StackItem::Bool(true));
    }
    #[test]
    #[should_panic(expected = "Opening parenthesis in infix expression")]
    fn open_parenth_in_infix() {
        let expr = RPNExpr {
            expr: vec![Token::Operator(OperatorType::OpenParenthesis)],
        };
        expr.eval(NaiveDate::from_ymd(2022, 10, 15));
    }
    #[test]
    #[should_panic(expected = "Closing parenthesis in infix expression")]
    fn close_parenth_in_infix() {
        let expr = RPNExpr {
            expr: vec![Token::Operator(OperatorType::CloseParenthesis)],
        };
        expr.eval(NaiveDate::from_ymd(2022, 10, 15));
    }
    #[test]
    fn realise_variable() {
        let date = NaiveDate::from_ymd(2022, 06, 12);
        let vars = [
            VariableType::DayOfWeek,
            VariableType::Month,
            VariableType::DayOfMonth,
            VariableType::Year,
            VariableType::ModifiedJulianDay,
            VariableType::WeekInMonth,
            VariableType::RWeekInMonth,
            VariableType::NearWeekend,
            // VariableType::DaysUntilEaster, TODO
            VariableType::DayOfTheYear,
        ];
        let results = [1, 6, 12, 2022, 59743, 2, 3, -1, 163];
        for (var, res) in vars.iter().zip(results) {
            dbg!(var);
            assert_eq!(var.realise(date), res);
        }
    }
    #[test]
    fn check_easter() {
        let date = NaiveDate::from_ymd(2022, 06, 09);
        assert_eq!(VariableType::DaysUntilEaster.realise(date), 303);
    }
    #[test]
    fn test_timeframe() {
        let tf = Timeframe {
            start: NaiveDate::from_ymd(1996, 10, 1),
            end: NaiveDate::from_ymd(1996, 10, 30),
        };
        assert_eq!(tf.is_in(NaiveDate::from_ymd(1996, 10, 15)), true);
        assert_eq!(tf.is_in(NaiveDate::from_ymd(1996, 9, 15)), false);
    }
}
