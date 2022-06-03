use chrono::Local;

pub fn d() {
    print!("{}", datetime());
}
fn datetime() -> String {
    format!("{}\n", Local::now().format("%a %Y %b %e   %-l:%M"))
}

#[cfg(test)]
pub mod test {
    use super::*;
    use std::process::Command;
    #[test]
    fn basic_time() {
        let when_time = Command::new("when")
            .arg("d")
            .output()
            .expect("Failed to execute");
        assert_eq!(String::from_utf8(when_time.stdout).unwrap(), datetime());
    }
}
