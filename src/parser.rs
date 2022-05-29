use chrono::prelude::*;
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct CalItem<'a> {
    date: NaiveDate,
    descr: &'a str,
}

impl<'a> CalItem<'_> {
    fn from_line(line: &'a str, eval_time: Timeframe) -> Result<Vec<CalItem>, ParseError> {
        let mut events = Vec::new();
        let mut parts = line.splitn(2, ','); // handle no description
        let (date_text, descr) = (parts.next().unwrap(), parts.next().unwrap());
        //use parser and convert dates to events
        if let Ok(dates) = dbg!(CalItem::parse_date(date_text, eval_time)) {
            for date in dates {
                events.push(CalItem { date, descr });
            }
        }
        Ok(events)
    }
    fn parse_date(date: &str, eval_time: Timeframe) -> Result<Vec<NaiveDate>, ParseError> {
        let mut tokens: Vec<Result<Token, ParseError>> = Vec::new();
        let mut buf = String::new();
        for w in date.to_uppercase().split_ascii_whitespace() {
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
        let mut eval_date = eval_time.start;

        //simple error handling for now. Returns first error only.
        let errors: Vec<Result<Token, ParseError>> =
            tokens.iter().cloned().filter(|t| t.is_err()).collect();
        if let Some(Err(x)) = errors.get(0) {
            return Err(*x);
        }
        let mut tokens: Vec<Token> = tokens.into_iter().filter_map(|t| t.ok()).collect();

        dbg!(&tokens);
        //short circuit if date is simple
        let simple = match tokens[..] {
            [Token::Value(y), Token::Value(m), Token::Value(d)] => {
                Some(NaiveDate::from_ymd(y.try_into().unwrap(), m, d))
            }
            [Token::Value(y), Token::Month(m), Token::Value(d)] => {
                Some(NaiveDate::from_ymd(y.try_into().unwrap(), m, d))
            }

            _ => None,
        };
        dbg!(simple);
        let mut res = Vec::new();
        if let Some(d) = simple {
            if d >= eval_time.start && d <= eval_time.end {
                res.push(d)
            } else {
                return Err(ParseError::DateError);
            }
        } else {
            while eval_date <= eval_time.end {
                eval_date = eval_date.succ();
            }
        }
        Ok(res)
    }
}

#[derive(Debug, Clone, PartialEq)]
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
    Or,
    OpenParenthesis,
    CloseParenthesis,
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
    fn is_operator(&self) -> bool {
        match self {
            Token::Operator(_) => true,
            _ => false,
        }
    }
    fn is_wildcard(&self) -> bool {
        match self {
            Token::Wildcard => true,
            _ => false,
        }
    }
    fn from(tok: &str) -> Result<Self, ParseError> {
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
        if let Ok(val) = tok.parse::<u32>() {
            Ok(Token::Value(val))
        } else {
            match tok {
                "*" => Ok(Token::Wildcard),
                "W" => Ok(Token::Variable(VariableType::DayOfWeek)),
                "M" => Ok(Token::Variable(VariableType::Month)),
                "D" => Ok(Token::Variable(VariableType::DayOfMonth)),
                "Y" => Ok(Token::Variable(VariableType::Year)),
                "J" => Ok(Token::Variable(VariableType::ModifiedJulianDay)),
                "A" => Ok(Token::Variable(VariableType::WeekInMonth)),
                "B" => Ok(Token::Variable(VariableType::RWeekInMonth)),
                "C" => Ok(Token::Variable(VariableType::NearWeekend)),
                "E" => Ok(Token::Variable(VariableType::DaysUntilEaster)),
                "Z" => Ok(Token::Variable(VariableType::DayOfTheYear)),
                "%" => Ok(Token::Operator(OperatorType::Modulo)),
                "-" => Ok(Token::Operator(OperatorType::Substraction)),
                "<" => Ok(Token::Operator(OperatorType::Lesser)),
                ">" => Ok(Token::Operator(OperatorType::Greater)),
                "<=" => Ok(Token::Operator(OperatorType::LesserOrEqual)),
                ">=" => Ok(Token::Operator(OperatorType::GreaterOrEqual)),
                "=" => Ok(Token::Operator(OperatorType::Equal)),
                "!=" => Ok(Token::Operator(OperatorType::NotEqual)),
                "&" => Ok(Token::Operator(OperatorType::And)),
                "|" => Ok(Token::Operator(OperatorType::Or)),
                "(" => Ok(Token::Operator(OperatorType::OpenParenthesis)),
                ")" => Ok(Token::Operator(OperatorType::CloseParenthesis)),
                _ => {
                    let matching: Vec<&&str> =
                        months.iter().filter(|b| b.starts_with(tok)).collect();
                    if matching.len() == 1 {
                        let pos = months.iter().position(|t| t == matching[0]).unwrap() + 1;
                        Ok(Token::Month(pos.try_into().unwrap()))
                    } else if matching.len() > 1 {
                        Err(ParseError::AmbiguousMonth)
                    } else {
                        Err(ParseError::SyntaxError)
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
    use crate::{cal, timeframe};
    fn test_tf() -> Timeframe {
        Timeframe {
            start: NaiveDate::from_ymd(2022, 4, 1),
            end: NaiveDate::from_ymd(2022, 4, 30),
        }
    }
    #[test]
    fn month_matching() {
        assert_eq!(Ok(Token::Month(4)), Token::from("AP"));
        assert_eq!(Ok(Token::Month(5)), Token::from("MAY"));
    }
    #[test]
    fn arbitrary_val_matching() {
        assert_eq!(Ok(Token::Value(10)), Token::from("10"));
    }
    #[test]
    fn special_matching() {
        assert_eq!(Ok(Token::Wildcard), Token::from("*"));
        assert_eq!(
            Ok(Token::Operator(OperatorType::Substraction)),
            Token::from("-")
        );
        assert_eq!(Ok(Token::Variable(VariableType::Month)), Token::from("M"));
    }
    #[test]
    fn basic_dates() {
        let date = "2022 April 22";
        let date2 = "2022 4 22";
        let res = NaiveDate::from_ymd(2022, 4, 22);
        assert_eq!(CalItem::parse_date(date, test_tf()).expect(""), [res]);
        assert_eq!(CalItem::parse_date(date2, test_tf()).expect(""), [res]);
    }

    #[test]
    fn basic() {
        let txt = "2022 April 22, 1";
        let item = CalItem {
            date: NaiveDate::from_ymd(2022, 4, 22),
            descr: " 1",
        };
        assert_eq!(CalItem::from_line(txt, test_tf()).expect(""), [item]);
        let multi_comma = "2022 4 22, multiple,commas,,,";
        let commas_item = CalItem {
            date: NaiveDate::from_ymd(2022, 4, 22),
            descr: " multiple,commas,,,",
        };
        assert_eq!(
            CalItem::from_line(multi_comma, test_tf()).expect(""),
            [commas_item]
        );
    }
    //#[test]
    fn basic_repetition() {
        let txt2 = "* October 15, Repeating yearly"; // figure the api for this (repeating struct)
        let tf2 = timeframe!(1996, 1, 1, 2022, 1, 1);
        let items2: Vec<CalItem> = (0..27)
            .map(|y| cal!(1996 + y, 10, 15, "Repeating yearly"))
            .collect();
        let txt3 = "* * 15, Repeating monthly"; // not present in original when
        let txt4 = "* * *, Repeating daily"; // not present in original when, maybe add compat flag
        dbg!(&items2);
        dbg!(&tf2);
        assert_eq!(CalItem::from_line(txt2, tf2).expect(""), items2);
    }
    #[test]
    fn basic_conditional_like_syntax() {
        let item = CalItem {
            date: NaiveDate::from_ymd(2022, 4, 22),
            descr: &"1",
        };
        let txt1 = "y=2022 & m=April & d=22, 1";
        let txt2 = "y=2022 & m=4 & d=22, 1";
        let txt3 = "y =2022 & m= April & d = 22, 1";
        assert_eq!(CalItem::from_line(txt1, test_tf()).expect(""), [item]);
        assert_eq!(CalItem::from_line(txt2, test_tf()).expect(""), [item]);
        assert_eq!(CalItem::from_line(txt3, test_tf()).expect(""), [item]);
    }
}
