///////////////////
//    CHUNKER    //
///////////////////

struct Chunker<'a> {
    buf: Box<Iterator<Item=u8> + 'a>,
    base: usize,
}

impl<'a> Iterator for Chunker<'a> {
    type Item = u8;
    fn next(&mut self) -> Option<u8> {
        let mut acc = match self.buf.next() {
            Some(a) => a,
            None => {return None;}
        };

        for _ in 0..(self.base - 1) {
            acc <<= 1;
            acc += match self.buf.next() {
                Some(a) => a,
                None => 0
            };
        }
        Some(acc)
    }
}


//////////////////
//    BUFFER    //
//////////////////

pub const HEX_STR: &str = "0123456789abcdef";
pub const BASE64_STR: &str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

pub struct Buf<'a>{
    buf: Box<Iterator<Item=u8> + 'a>,
    base: usize,
}

impl<'a> Buf<'a>{
    pub fn from_string(s: &'a str, convert: &'a str, base: usize) -> Buf<'a> {
        let buf = Box::new(s.chars().map(move |x| convert.find(x).unwrap() as u8));
        Buf{ buf: buf, base: base }
    }

    pub fn from_hex(hexstr: &'a str) -> Buf<'a> {
        Buf::from_string(hexstr, HEX_STR, 4)
    }

    pub fn from_bytes(bytes: Vec<u8>) -> Buf<'a> {
        Buf{ buf: Box::new(bytes.into_iter()), base: 8 }
    }

    pub fn from_base64(base64str: &'a str) -> Buf<'a> {
        Buf::from_string(base64str, BASE64_STR, 6)
    }

    pub fn bits(self) -> Buf<'a> {
        let base = self.base;
        let out = self.buf.flat_map(move |x| {
            let mut res = Vec::new();
            let mut x = x;
            for _ in 0..base {
                res.push(x & 1);
                x = x >> 1;
            }
            res.reverse();
            res.into_iter()
        });
        Buf{ buf: Box::new(out), base: 1 }
    }

    pub fn base(self, base: usize) -> Buf<'a> {
        let mut buf = self;
        if buf.base != 1 {
            buf = buf.bits();
        }

        let buf = Box::new(Chunker{buf: buf.buf, base: base});
        Buf{ buf: buf, base: base }
    }

    pub fn bytes(self) -> Buf<'a> {
        self.base(8)
    }

    pub fn to_string(self, convert: &str) -> String {
        self.buf.map(|item| convert.chars().nth(item as usize).unwrap())
            .collect::<String>()
    }

    pub fn to_hex(self) -> String {
        self.base(4).to_string(HEX_STR)
    }

    pub fn to_base64(self) -> String {
        self.base(6).to_string(BASE64_STR)
    }

    pub fn to_vec(self) -> Vec<u8> {
        self.buf.collect()
    }

    pub fn iter(self) -> impl Iterator<Item=u8> + 'a {
        self.buf
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use rand::{thread_rng, Rng};
    use rand::prelude::*;

    #[test]
    fn simple_hex_test() {
        let b = Buf::from_hex("a").bits().to_vec();
        assert_eq!(b, vec![1, 0, 1, 0]);
        let b: Vec<u8> = Buf::from_hex("ab1").bits().to_vec();
        assert_eq!(b, vec![1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1]);
    }

    #[test]
    fn simple_reverse_hex_test() {
        assert_eq!(Buf::from_hex("ab1").to_hex(), "ab1");
    }

    #[test]
    fn random_reverse_hex_test() {
        const NUM_TESTS: usize = 1000;
        const LEN_STR: usize = 100;
        let mut rng = thread_rng();

        for _ in 0..NUM_TESTS {
            let hexstr = (0..LEN_STR).into_iter()
                .map(|_| HEX_STR.as_bytes().choose(&mut rng).cloned().unwrap())
                .map(|x| x as char)
                .collect::<String>();

            println!("Converting: {}", hexstr);
            assert_eq!(Buf::from_hex(hexstr.as_str()).bits().to_hex(), hexstr);
        }
    }

    #[test]
    fn random_reverse_base64_test() {
        const NUM_TESTS: usize = 1000;
        const LEN_STR: usize = 100;
        let mut rng = thread_rng();

        for _ in 0..NUM_TESTS {
            let base64str = (0..LEN_STR).into_iter()
                .map(|_| BASE64_STR.as_bytes().choose(&mut rng).cloned().unwrap())
                .map(|x| x as char)
                .collect::<String>();

            println!("Converting: {}", base64str);

            assert_eq!(Buf::from_base64(base64str.as_str()).to_base64(), base64str);
        }
    }

    #[test]
    fn random_double_test() {
        const NUM_TESTS: usize = 1000;
        const LEN_STR: usize = 100;
        let mut rng = thread_rng();

        for _ in 0..NUM_TESTS {
            let base64str = (0..LEN_STR).into_iter()
                .map(|_| BASE64_STR.as_bytes().choose(&mut rng).cloned().unwrap())
                .map(|x| x as char)
                .collect::<String>();

            println!("Converting: {}", base64str);

            let out = Buf::from_base64(base64str.as_str()).to_hex();
            let out = Buf::from_hex(out.as_str()).to_base64();

            assert_eq!(out, base64str);
        }
    }

    #[test]
    fn given_test() {
        let hexstr = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d";
        let base64str = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t";
        assert_eq!(Buf::from_hex(hexstr).to_base64(), base64str);
    }
}
