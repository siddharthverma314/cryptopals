pub struct ConvertCfg<'a> {
    convert: &'a str,
    size: usize
}

pub const HEX_CONFIG: ConvertCfg<'static> = ConvertCfg{
    convert: "0123456789abcdef",
    size: 4
};

pub const BASE64_CONFIG: ConvertCfg<'static> = ConvertCfg{
    convert: "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/",
    size: 6
};

pub type Buf<'a> = Box<Iterator<Item=u8> + 'a>;

fn convert_string_base<'a>(s: &'a str, convert: &'a str) -> Buf<'a> {
    Box::new(s.chars().map(move |x| convert.find(x).unwrap() as u8))
}

fn convert_base_bits<'a>(base: Buf<'a>, size: usize) -> Buf<'a>{
    let out = base.flat_map(move |x| {
        let mut res = Vec::new();
        let mut x = x;
        for _ in 0..size {
            res.push(x & 1);
            x = x >> 1;
        }
        res.reverse();
        res.into_iter()
    });
    Box::new(out)
}

pub fn convert_bits<'a>(x: &'a str, cfg: &'a ConvertCfg) -> Buf<'a> {
    convert_base_bits(convert_string_base(x, cfg.convert), cfg.size)
}

// Chunking!!!

pub struct Chunker<'a> {
    buf: Buf<'a>,
    size: usize
}

impl<'a> Iterator for Chunker<'a> {
    type Item = u8;
    fn next(&mut self) -> Option<u8> {
        let mut acc = match self.buf.next() {
            Some(a) => a,
            None => {return None;}
        };

        for _ in 0..(self.size - 1) {
            acc <<= 1;
            acc += match self.buf.next() {
                Some(a) => a,
                None => 0
            };
        }
        Some(acc)
    }
}

fn convert_bits_base<'a>(bits: Buf<'a>, size: usize) -> Buf<'a> {
    Box::new(Chunker{buf: bits, size: size})
}

fn convert_base_string(bits: Buf, convert: &str) -> String{
    bits.map(|item| convert.chars().nth(item as usize).unwrap())
        .collect::<String>()
}

pub fn convert_string(bits: Buf, cfg: &ConvertCfg) -> String {
    convert_base_string(convert_bits_base(bits, cfg.size), cfg.convert)
}

pub fn hex_decode<'a>(hexstr: &'a str) -> Buf<'a> {
    let bits = convert_bits(hexstr, &HEX_CONFIG);
    convert_bits_base(bits, 8)
}

pub fn hex_encode(bytes: Buf) -> String {
    let bits = convert_base_bits(bytes, 8);
    convert_string(bits, &HEX_CONFIG)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn simple_hex_test() {
        let b: Vec<u8> = convert_bits("a", &HEX_CONFIG).collect();
        assert_eq!(b, vec![1, 0, 1, 0]);
        let b: Vec<u8> = convert_bits("ab1", &HEX_CONFIG).collect();
        assert_eq!(b, vec![1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1]);
    }

    #[test]
    fn simple_double_hex_test() {
        let bits = convert_bits("ab1", &HEX_CONFIG);
        let s = convert_string(bits, &HEX_CONFIG);
        assert_eq!(s, "ab1");
    }

    #[test]
    fn random_double_hex_test() {
        let b = convert_bits("ab1", &HEX_CONFIG);
        let b = convert_string(b, &HEX_CONFIG);
        assert_eq!(b, "ab1");
    }

    #[test]
    fn given_test() {
        let hexstr = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d";
        let base64str = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t";
        let b = convert_bits(hexstr, &HEX_CONFIG);
        let cbase64 = convert_string(b, &BASE64_CONFIG);
        assert_eq!(cbase64, base64str);
    }
}
