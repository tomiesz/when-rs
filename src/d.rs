use chrono::NaiveDate;
use chrono::Timelike;

///Given a --now parameter this function will return the given date, and current time. This is
///for compatibility.
pub fn d(today: NaiveDate) {
    print!("{}", datetime(today));
}
fn datetime(today: NaiveDate) -> String {
    let now = chrono::Local::now();
    format!(
        "{}\n",
        today
            .and_hms(now.hour(), now.minute(), now.second())
            .format("%a %Y %b %e   %-l:%M")
    )
}

#[cfg(test)]
pub mod test {
    use super::*;
    use chrono::Local;
    use std::process::Command;
    #[test]
    fn basic_time() {
        let when_time = Command::new("when")
            .arg("d")
            .output()
            .expect("Failed to execute");
        assert_eq!(
            String::from_utf8(when_time.stdout).unwrap(),
            datetime(Local::today().naive_local())
        );
    }
}
