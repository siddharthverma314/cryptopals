extern crate cryptolib;
use cryptolib::convert::Buf;
use cryptolib::xor;
use std::fs;

fn main() {
    // load file
    let base64str = fs::read_to_string("data/6.txt").
        unwrap().
        replace('\n', "");

    let bytes = Buf::from_base64(base64str.as_str()).bytes().to_vec();
    let (key, out) = xor::decrypt_xor_repeated(&bytes);
    
    println!("Key: {}\nText: {}", key, out);
}
