extern crate cryptolib;
use cryptolib::convert;
use cryptolib::xor;
use std::fs;

fn main() {
    let txt = fs::read_to_string("data/4.txt").unwrap();
    for hexstr in txt.split("\n") {
        let bytes = convert::hex_decode(hexstr.to_string());
        let b = xor::decrypt_xor_byte(&bytes);
        let new_bytes = xor::xor_byte(&bytes, b);

        if xor::eng_similarity(&new_bytes) < -80f32 {
            for i in new_bytes.iter() {
                print!("{}", *i as char);
            }
        }
    }
}
