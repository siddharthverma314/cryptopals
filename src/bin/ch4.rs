extern crate cryptolib;
use cryptolib::convert;
use cryptolib::xor;
use std::fs;

fn main() {
    let txt = fs::read_to_string("data/4.txt").unwrap();
    for hexstr in txt.split('\n') {
        let bytes: Vec<u8> = convert::hex_decode(hexstr).collect();
        let new_bytes = xor::decrypt_xor_byte(&bytes).1;

        if xor::eng_similarity(&new_bytes) < 3f64 {
            for i in new_bytes.iter() {
                print!("{}", *i as char);
            }
        }
    }
}
