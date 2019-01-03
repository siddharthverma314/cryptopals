extern crate cryptolib;
use cryptolib::convert;
use cryptolib::xor;

fn main() {
    let hexstr = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736";
    let bytes: Vec<u8> = convert::hex_decode(hexstr).collect();
    let decoded = xor::decrypt_xor_byte(&bytes).1;
    println!("Similarity: {}", xor::eng_similarity(&decoded));
    for i in decoded.iter() {
        print!("{}", *i as char);
    }
    println!();
}