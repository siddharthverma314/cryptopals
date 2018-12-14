mod convert;

fn main() {
    let hexstr = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d";
    let base64str = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t";
    let out = convert::hex_to_bytes(hexstr);
    let out = convert::bytes_to_string(out.as_slice());
    println!("{}", out);
    if out == base64str {
        println!("Success!");
    } else {
        println!("Whoops");
    }
}
