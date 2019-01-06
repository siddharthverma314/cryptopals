use std::collections::HashMap;
use std::f64;
use std::u32;
use rayon::prelude::*;
use nmin;

const ENG_FREQ:[(char, f64); 26] = [
    ('E', 0.120_195_498_702_709_22),
    ('T', 0.090_985_886_134_622_02),
    ('A', 0.081_238_377_865_421_85),
    ('O', 0.076_811_681_650_877_93),
    ('I', 0.073_054_200_973_105_22),
    ('N', 0.069_477_737_612_655_85),
    ('S', 0.062_807_523_737_952_74),
    ('R', 0.060_212_942_189_651_29),
    ('H', 0.059_214_604_257_746_72),
    ('D', 0.043_191_828_988_003_486),
    ('L', 0.039_785_412_198_373_04),
    ('U', 0.028_776_268_081_161_58),
    ('C', 0.027_114_199_985_738_028),
    ('M', 0.026_115_862_053_833_45),
    ('F', 0.023_038_567_659_336_38),
    ('Y', 0.021_135_143_140_815_018),
    ('W', 0.020_948_640_450_239_437),
    ('G', 0.020_257_483_420_459_344),
    ('P', 0.018_189_497_704_371_293),
    ('B', 0.014_892_788_379_785_303),
    ('V', 0.011_074_968_596_238_131),
    ('K', 0.006_895_114_178_044_245),
    ('X', 0.001_727_892_574_450_228_5),
    ('Q', 0.001_124_501_516_705_704_2),
    ('J', 0.001_031_250_171_417_914_2),
    ('Z', 0.000_702_127_776_284_537_3)
];

pub fn xor_fixed(buffer1: &[u8], buffer2: &[u8]) -> Vec<u8> {
    buffer1.par_iter().zip(buffer2).map(|(a, b)| a ^ b).collect()
}

pub fn xor_byte(buffer1: &[u8], byte: u8) -> Vec<u8> {
    buffer1.par_iter().map(|x| *x ^ byte).collect()
}

pub fn eng_similarity(buffer: &[u8]) -> f64 {
    let mut count: HashMap<u8, u32> = HashMap::new();

    // keep track of all A-Z
    for c in ('A' as u8)..('Z' as u8 + 1) {
        count.insert(c, 1);
    }

    // count all letters
    let mut total_count = 0;
    for b in buffer {
        let b = (*b as char).to_ascii_uppercase() as u8;
        if count.contains_key(&b) {
            *count.get_mut(&b).unwrap() += 1;
            total_count += 1;
        }
    }

    let total_count = total_count as f64;

    if total_count == 0f64 {
        return f64::INFINITY;
    }

    // compute similarity score
    let mut score: f64 = 0.0;
    for (c, pi) in &ENG_FREQ {
        let c = *c;
        let pi = *pi;
            
        let qi = *count.get(&(c as u8)).unwrap();
        let qi = (qi as f64) / total_count;


        let diff = qi * (qi / pi).ln();
        score += diff
    }

    let frac = total_count / (buffer.len() as f64);
    score / frac
}

pub fn decrypt_xor_byte(buffer1: &[u8]) -> (u8, Vec<u8>) {
    let ans = (0u8..255u8).into_par_iter()
        .map(|x| (x, xor_byte(buffer1, x)))
        .map(|(x, b)| (x, eng_similarity(&b), b))
        .min_by(|x, y| x.1.partial_cmp(&y.1).unwrap()).unwrap();

    (ans.0, ans.2)
}

pub fn xor_repeated(buffer1: &[u8], key: &[u8]) -> Vec<u8> {
    buffer1.iter()
        .zip(key.iter().cycle())
        .map(|(a, b)| *a ^ *b)
        .collect()
}

// Decrypt XOR repeated
fn hamming_distance(buf1: &[u8], buf2: &[u8]) -> u32 {
    buf1.par_iter()
        .zip(buf2.par_iter())
        .map(|(x, y)| *x ^ *y)
        .map(|mut x| {
            let mut count = 0u32;
            for _ in 0..8 {
                count += (x & 1) as u32;
                x >>= 1;
            }
            count
        })
        .sum()
}


static MAX_EDIT_LEN: usize = 40;
static MAX_NUM_KEYS: usize = 20;

fn find_keysize(buf: &[u8]) -> Vec<usize> {
    let mut nmin: nmin::NMin<(f32, usize)> = nmin::NMin::new(MAX_NUM_KEYS);

    for i in 2..MAX_EDIT_LEN.min(buf.len() / 2) {
        let hd = hamming_distance(&buf[0..i], &buf[i..2*i]);
        let hd = hd as f32 / i as f32;
        nmin.update((hd, i));
    }

    let out: Vec<usize> = nmin.get_items().par_iter().map(|x| x.1).collect();
    out
}

fn weave_blocks(buf: &[u8], num: usize) -> Vec<Vec<u8>> {
    assert!(num != 0);

    let mut out: Vec<Vec<u8>> = Vec::with_capacity(num);

    for _ in 0..num {
        out.push(Vec::new());
    }

    buf.iter()
        .zip((0..num).into_iter().cycle())
        .for_each(|(byte, index)| out.get_mut(index).unwrap().push(*byte));

    out
}

fn unweave_blocks(blocks: Vec<Vec<u8>>) -> Vec<u8> {
    let mut bytes: Vec<u8> = Vec::new();

    for j in 0..blocks.get(0).unwrap().len() {
        for i in 0..blocks.len() {
            bytes.push(*blocks.get(i).unwrap().get(j).unwrap_or(&0));
        }
    }

    bytes
}

pub fn decrypt_xor_repeated(buf: &[u8]) -> (String, String) {
    let keylens = find_keysize(buf);

    let result = keylens.into_par_iter()
        .map(|len| weave_blocks(buf, len))
        .map(|b| {
            let mut blocks: Vec<Vec<u8>> = Vec::new();
            let mut key = String::new();

            for block in b {
                let decrypt = decrypt_xor_byte(&block);
                blocks.push(decrypt.1);
                key.push(decrypt.0 as char);
            }

            let blocks = unweave_blocks(blocks);
            let mut out = String::new();
            let similarity = eng_similarity(&blocks);

            for block in blocks {
                out.push(block as char);
            }

            (similarity, key, out)
        })
        .min_by(|x, y| x.0.partial_cmp(&y.0).unwrap()).unwrap();

    (result.1, result.2)
}

#[cfg(test)]
mod test {
    use super::*;
    use rand;
    use convert::Buf;

    #[test]
    fn test_xor_fixed() {
        let buf1 = Buf::from_hex("1c0111001f010100061a024b53535009181c").bytes().to_vec();
        let buf2 = Buf::from_hex("686974207468652062756c6c277320657965").bytes().to_vec();
        let out = Buf::from_bytes(xor_fixed(&buf1, &buf2));
        assert_eq!(out.to_hex(), "746865206b696420646f6e277420706c6179");
    }

    #[test]
    fn test_xor_repeated() {
        let str1 = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal";
        let buffer1 = str1.bytes().collect::<Vec<u8>>();
        let buffer2 = "ICE".bytes().collect::<Vec<u8>>();
        let out = xor_repeated(&buffer1, &buffer2);
        let out = Buf::from_bytes(out).to_hex();
        assert_eq!(out, "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f");
    }

    #[test]
    fn test_hamming_distance() {
        let buf1: Vec<u8> = "this is a test".bytes().collect();
        let buf2: Vec<u8> = "wokka wokka!!!".bytes().collect();
        assert_eq!(hamming_distance(&buf1, &buf2), 37);
    }

    fn remove_zeros(mut buf: Vec<u8>) -> Vec<u8> {
        while buf.last().unwrap() == &0 {
            buf.pop();
        }
        buf
    }

    #[test]
    fn test_weave_1() {
        let buf: Vec<u8> = vec![1, 2, 3, 4, 5, 6];
        let weaved = weave_blocks(&buf, 2);
        assert_eq!(weaved, vec![vec![1, 3, 5], vec![2, 4, 6]]);
        assert_eq!(unweave_blocks(weaved), buf);
    }

    #[test]
    fn test_weave_2() {
        let buf: Vec<u8> = vec![1, 2, 3, 4, 5, 6];
        let weaved = weave_blocks(&buf, 3);
        assert_eq!(weaved, vec![vec![1, 4], vec![2, 5], vec![3, 6]]);
        assert_eq!(unweave_blocks(weaved), buf);
    }

    #[test]
    fn test_weave_random() {
        const BUF_LEN: usize = 1000;
        const NUM_ITER: usize = 100;

        for _ in 0..NUM_ITER {
            let mut buf: Vec<u8> = Vec::with_capacity(BUF_LEN);
            let blocks: u8 = rand::random();
            let blocks = blocks as usize + 1;

            for _ in 0..BUF_LEN {
                buf.push(rand::random());
            }

            let prev = buf.clone();
            
            let out = weave_blocks(&buf, blocks);
            let out = unweave_blocks(out);
            
            let out = remove_zeros(out);
            let prev = remove_zeros(prev);

            assert_eq!(out, prev);
        }
    }
}
