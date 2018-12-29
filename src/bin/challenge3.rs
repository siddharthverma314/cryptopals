extern crate cryptolib;
use cryptolib::convert;
use cryptolib::xor;

fn main() {
    let hexstr = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736";
    let bytes = convert::hex_decode(hexstr.to_string());
    let b = xor::decrypt_xor_byte(&bytes);
    let decoded = xor::xor_byte(&bytes, b);
    println!("Similarity: {}", xor::eng_similarity(&decoded));
    for i in decoded.iter() {
        print!("{}", *i as char);
    }
    println!();
}
