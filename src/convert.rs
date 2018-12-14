fn hex_to_num(hexchar: char) -> u8 {
    if hexchar >= '0' && hexchar <= '9' {
        return hexchar as u8 - '0' as u8;
    } else if 'a' <= hexchar && hexchar <= 'f' {
        return hexchar as u8 - 'a' as u8 + 10;
    } else if 'A' <= hexchar && hexchar <= 'F' {
        return hexchar as u8 - 'A' as u8 + 10;
    } else{
        panic!("Invalid char, {}", hexchar);
    }
}

fn hex_to_byte(hex: &str) -> u8 {
    assert_eq!(hex.len(), 2);
    hex.chars()
        .map(hex_to_num)
        .fold(0, |x, y| (x << 4) + y)
}

fn byte_to_hex(byte: u8) -> String {
    let hexstr :Vec<char> = "0123456789abcdef".chars().collect();
    let c1 = (byte >> 4) as usize;
    let c2 = (byte & 15) as usize;
    let mut out = String::new();
    out.push(hexstr[c1]);
    out.push(hexstr[c2]);
    out
}

pub fn hex_to_bytes(hexstr: &str) -> Vec<u8> {
    hexstr.chars()
        .collect::<Vec<char>>()
        .chunks(2)
        .map(|a| a.iter().collect::<String>())
        .map(|a| hex_to_byte(a.as_str()))
        .collect::<Vec<u8>>()
}

pub fn bytes_to_hex(bytes: &[u8]) -> String {
    bytes.iter()
        .map(|x| byte_to_hex(*x))
        .collect::<String>()
}

// FIXME
pub fn bytes_to_string(bytes: &[u8]) -> String {
    let b64str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
        .chars()
        .collect::<Vec<char>>();
    bytes.iter()
        .map(|x| b64str[*x as usize])
        .collect::<String>()
}

#[cfg(test)]
mod tests {
    use convert;

    #[test]
    fn hex_to_num_test() {
        let s = "0123456789abcdef";
        for (i, c) in s.as_bytes().iter().enumerate() {
            assert_eq!(convert::hex_to_num(*c as char), i as u8);
        }
        let s = "0123456789ABCDEF";
        for (i, c) in s.as_bytes().iter().enumerate() {
            assert_eq!(convert::hex_to_num(*c as char), i as u8);
        }
    }

    #[test]
    #[should_panic]
    fn hex_to_num_panic_test() {
        convert::hex_to_num('g');
    }

    #[test]
    fn hex_to_byte_test() {
        let hexchars = "0123456789abcdef";
        for (i, c1) in hexchars.as_bytes().iter().enumerate() {
            for (j, c2) in hexchars.as_bytes().iter().enumerate() {
                let num = (i * 16 + j) as u8;
                let mut hexstr = String::new();
                hexstr.push(*c1 as char);
                hexstr.push(*c2 as char);
                println!("{}, {}", num, hexstr);
                assert_eq!(convert::hex_to_byte(hexstr.as_str()), num)
            }
        }
    }

    #[test]
    fn byte_to_hex_test() {
        let hexchars = "0123456789abcdef";
        for c1 in hexchars.as_bytes().iter() {
            for c2 in hexchars.as_bytes().iter() {
                let mut hexstr = String::new();
                hexstr.push(*c1 as char);
                hexstr.push(*c2 as char);
                let outstr = convert::hex_to_byte(hexstr.as_str());
                let outstr = convert::byte_to_hex(outstr);
                assert_eq!(outstr, hexstr);
            }
        }
    }

    #[test]
    fn test_final() {
        let hexstr = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d";
        let base64str = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t";
        let out = convert::hex_to_bytes(hexstr);
        let out = convert::bytes_to_string(out.as_slice());
        assert_eq!(out, base64str);
    }
}
