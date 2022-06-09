use chrono::prelude::*;

pub fn j() {
    print!("{}", j_format(chrono::Local::today().naive_local()));
}

fn j_format(date: NaiveDate) -> String {
    format!(
        "The date {} corresponds to modified julian day {}.",
        date.format("%Y %b %e"),
        date.num_days_from_ce() - 678575
    )
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_out() {
        let date1 = NaiveDate::from_ymd(2022, 06, 9);
        let desired1 = "The date 2022 Jun  9 corresponds to modified julian day 59740.";
        assert_eq!(desired1, j_format(date1));
        let date2 = NaiveDate::from_ymd(1996, 10, 15);
        let desired2 = "The date 1996 Oct 15 corresponds to modified julian day 50372.";
        assert_eq!(desired2, j_format(date2));
    }
}
