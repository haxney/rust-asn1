extern crate num;

pub fn main() {
    let head = "<head>".to_string();
    let lines:Vec<String> = vec!["<line1>", "<line2>"].iter().map(|s| s.to_string()).collect();
    let tail = "<tail>".to_string();
    let mut merg: Vec<String> = vec![head];
    merg.push_all_move(lines);
    merg.push(tail);

    println!("{}", merg.concat());
}
