#[macro_export]
macro_rules! timeframe {
    ($y1:expr,$m1:expr,$d1:expr,$y2:expr,$m2:expr,$d2:expr) => {
        Timeframe {
            start: NaiveDate::from_ymd($y1, $m1, $d1),
            end: NaiveDate::from_ymd($y2, $m2, $d2),
        }
    };
}

#[macro_export]
macro_rules! tokens {
    ($($i:expr),*) => {
        vec![$(Token::from($i),)*]
    };
}
