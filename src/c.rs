use chrono::prelude::*;
use num_traits::FromPrimitive;

pub fn c(today: NaiveDate) {
    print!("{}", assemble_calendar(today));
}

fn assemble_calendar(today: NaiveDate) -> String {
    //fix the star bullshit idk
    let prev = make_cal(today.with_month(today.month() - 1).unwrap(), false, false);
    let cur = make_cal(today, false, true);
    let next = make_cal(today.with_month(today.month() + 1).unwrap(), false, false);
    let mut combined = String::new();
    for x in 0..=6 {
        combined.push_str(format!("{}  {}  {}\n", prev[x], cur[x], next[x]).as_str());
    }
    combined
}

fn make_cal(dt: NaiveDate, start_mon: bool, star: bool) -> Vec<String> {
    let start = dt.with_day(1).unwrap();
    let month_len = start
        .signed_duration_since(NaiveDate::from_ymd(dt.year(), dt.month() + 1, 1))
        .num_days()
        .abs();
    //header
    let mut cal = Vec::from([format!(
        "{:-^1$}",
        Month::from_u32(dt.month()).unwrap().name(),
        21
    )]);

    //weekday header
    let (header, pad_count) = if start_mon {
        (
            " M  T  W  T  F  S  S ",
            start.weekday().num_days_from_monday(),
        )
    } else {
        (
            " S  M  T  W  T  F  S ",
            start.weekday().num_days_from_sunday(),
        )
    };
    cal.push(header.to_string());

    //fill lines with days of the week
    let mut line = "   ".repeat(pad_count.try_into().unwrap());
    for day in 1..=month_len {
        if star && day == dt.day().into() {
            line.push_str(" * ");
        } else {
            line.push_str(format!("{: >2} ", day).as_str());
        }
        if line.chars().count() == 21 {
            cal.push(line.clone());
            line.clear();
        }
    }

    let missing = 21 - line.chars().count();
    if missing != 21 {
        line.push_str(&" ".repeat(missing));
        cal.push(line);
    }
    cal
}

#[cfg(test)]
mod tests {
    //some tests here need to be moved to integration tests, or use docker or smthn. Currently they
    //are based on my personal computer setup
    use super::*;
    use rand::prelude::*;
    use std::process::Command;
    fn base_args() -> [&'static str; 5] {
        [
            "--past=0",
            "--future=0",
            "--noneighboring_months",
            "--nostyled_output",
            "--calendar_today_style='none'",
        ]
    }
    fn pretty_cal(cal: Vec<String>) -> String {
        let mut out = String::new();
        for line in cal {
            out.push_str(line.as_str());
            out.push_str("\n");
        }
        out
    }
    fn rn() -> NaiveDate {
        Local::today().naive_local()
    }
    #[test]
    fn parity_today() {
        //for whatever reason cant force when to output just a simple calendar, it replaces today
        //with a star. Works in terminal but not here for some reason even when executed through
        //shell. Added start print for today to match this test.
        let base_cal = Command::new("when")
            .args(base_args())
            .arg("c")
            .output()
            .expect("Failed to execute");
        assert_eq!(
            String::from_utf8(base_cal.stdout).unwrap(),
            pretty_cal(make_cal(rn(), false, true))
        );
    }
    #[test]
    fn parity_monday_start_today() {
        let monday_cal = Command::new("when")
            .args(base_args())
            .arg("--monday_first")
            .arg("c")
            .output()
            .expect("Failed to execute");
        assert_eq!(
            String::from_utf8(monday_cal.stdout).unwrap(),
            pretty_cal(make_cal(rn(), true, true))
        );
    }
    #[test]
    fn parity_random_date() {
        let mut rng = thread_rng();
        let date = NaiveDate::from_ymd(
            rng.gen_range(1900..2050),
            rng.gen_range(1..12),
            rng.gen_range(1..28),
        );
        let random_cal = Command::new("when")
            .args(base_args())
            .arg(format!(
                "--now={} {} {}",
                date.year(),
                date.month(),
                date.day()
            ))
            .arg("c")
            .output()
            .expect("Failed to execute");
        //dbg!(&random_cal);
        assert_eq!(
            String::from_utf8(random_cal.stdout).unwrap(),
            pretty_cal(make_cal(date, false, true))
        );
    }
}
