use rmonkey_language::repl;
use std::io;

fn main() {
    let stdin = io::stdin();
    let mut reader = stdin.lock();
    let mut writer = io::stdout();

    repl::start(&mut reader, &mut writer);
}
