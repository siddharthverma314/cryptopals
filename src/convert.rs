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

pub const BYTES_CONFIG: ConvertCfg<'static> = ConvertCfg{
    convert: "",
    size: 8
};

pub fn convert_string_base(s: String, cfg: &ConvertCfg) -> Vec<u8>{
    s.chars()
        .map(|x| cfg.convert.find(x).unwrap() as u8)
        .collect::<Vec<u8>>()
}

pub fn convert_base_bits(base: Vec<u8>, cfg: &ConvertCfg) -> Vec<u8>{
    base.into_iter()
        .flat_map(|x| {
            let mut res = Vec::new();
            let mut x = x;
            for _ in 0..cfg.size {
                res.push(x & 1);
                x = x >> 1;
            }
            res.reverse();
            res.into_iter()
        })
        .collect()
}

pub fn convert_bits(x: String, cfg: &ConvertCfg) -> Vec<u8>{
    convert_base_bits(convert_string_base(x, cfg), cfg)
}

pub fn convert_bits_base(mut bits: Vec<u8>, cfg: &ConvertCfg) -> Vec<u8> {
    let extra = bits.len() % cfg.size;
    if extra != 0 {
        let padding = cfg.size - extra;
        for _ in 0..padding {
            bits.push(0)
        };
    }

    bits.chunks(cfg.size)
        .map(|item| item.iter().fold(0, |acc, elem| {(acc << 1) + elem}))
        .collect::<Vec<u8>>()
}

pub fn convert_base_string(bits: Vec<u8>, cfg: &ConvertCfg) -> String{
    bits.into_iter()
        .map(|item| cfg.convert.chars().nth(item as usize).unwrap())
        .collect::<String>()
}

pub fn convert_string(bits: Vec<u8>, cfg: &ConvertCfg) -> String {
    convert_base_string(convert_bits_base(bits, cfg), cfg)
}

pub fn hex_decode(hexstr: String) -> Vec<u8> {
    let bits = convert_bits(hexstr, &HEX_CONFIG);
    convert_bits_base(bits, &BYTES_CONFIG)
}

pub fn hex_encode(bytes: Vec<u8>) -> String {
    let bits = convert_base_bits(bytes, &BYTES_CONFIG);
    convert_string(bits, &HEX_CONFIG)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn simple_hex_test() {
        let b = convert_bits("a".to_string(), &HEX_CONFIG);
        assert_eq!(b, vec![1, 0, 1, 0]);
        let b = convert_bits("ab1".to_string(), &HEX_CONFIG);
        assert_eq!(b, vec![1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1]);
    }

    #[test]
    fn simple_double_hex_test() {
        let bits = convert_bits("ab1".to_string(), &HEX_CONFIG);
        let s = convert_string(bits, &HEX_CONFIG);
        assert_eq!(s, "ab1");
    }

    #[test]
    fn random_double_hex_test() {
        let b = convert_bits("ab1".to_string(), &HEX_CONFIG);
        let b = convert_string(b, &HEX_CONFIG);
        assert_eq!(b.to_string(), "ab1");
    }

    #[test]
    fn given_test() {
        let hexstr = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d";
        let base64str = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t";
        let b = convert_bits(hexstr.to_string(), &HEX_CONFIG);
        let cbase64 = convert_string(b, &BASE64_CONFIG);
        assert_eq!(cbase64, base64str);
    }
}
