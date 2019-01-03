use std::collections::HashMap;
use std::f64;
use rayon::prelude::*;

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

pub fn xor_fixed(buffer1: &Vec<u8>, buffer2: &Vec<u8>) -> Vec<u8> {
    buffer1.par_iter().zip(buffer2).map(|(a, b)| a ^ b).collect()
}

pub fn xor_byte(buffer1: &Vec<u8>, byte: u8) -> Vec<u8> {
    buffer1.par_iter().map(|x| *x ^ byte).collect()
}

pub fn eng_similarity(buffer: &Vec<u8>) -> f64 {
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
        // println!("c: {}, \t pi: {:.5}, \t qi: {:.5}, \t diff: {:.5}", c, pi, qi, diff);
        score += diff
    }

    // println!("score: {:.10}", score);

    let frac = total_count / (buffer.len() as f64);
    score / frac
}

pub fn decrypt_xor_byte(buffer1: &Vec<u8>) -> (u8, Vec<u8>) {
    let ans = (0u8..255u8).into_par_iter()
        .map(|x| (x, xor_byte(buffer1, x)))
        .map(|(x, b)| (x, eng_similarity(&b), b))
        .reduce(|| (0, f64::INFINITY, vec!{0}),
                |(x, v, b), (nx, nv, nb)| {
                    if nv < v {
                        (nx, nv, nb)
                    } else {
                        (x, v, b)
                    }
                });
    (ans.0, ans.2)
}

pub fn xor_repeated(buffer1: &Vec<u8>, key: &Vec<u8>) -> Vec<u8> {
    buffer1.iter()
        .zip(key.iter().cycle())
        .map(|(a, b)| *a ^ *b)
        .collect()
}

// Decrypt XOR repeated
fn hamming_distance(buf1: &Vec<u8>, buf2: &Vec<u8>) -> u32 {
    buf1.iter()
        .zip(buf2.iter())
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

static MAX_EDIT_LEN: u32 = 40;
static NUM_KEYSIZE u32 = 4;

fn find_keysize(buf: &Vec<u8>, num_keys: u32) -> Vec<u32> {
    (2..MAX_EDIT_LEN.max(buf.len() as u32 / 2)).into_par_iter()
        .map(|x| (x, hamming_distance(buf1[0..x], buf1[x..2*x])))
        .collect::<Vec<u32>>()
        .sort()[0..NUM_KEYSIZE]
}

#[cfg(test)]
mod test {
    use super::*;
    use convert::*;

    #[test]
    fn test_xor_fixed() {
        let buffer1: Vec<u8> = hex_decode("1c0111001f010100061a024b53535009181c").collect();
        let buffer2: Vec<u8> = hex_decode("686974207468652062756c6c277320657965").collect();
        let out = xor_fixed(&buffer1, &buffer2);
        let out = hex_encode(Box::new(out.into_iter()));
        assert_eq!(out, "746865206b696420646f6e277420706c6179");
    }

    #[test]
    fn test_xor_repeated() {
        let str1 = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal";
        let buffer1 = str1.bytes().collect::<Vec<u8>>();
        let buffer2 = "ICE".bytes().collect::<Vec<u8>>();
        let out = xor_repeated(&buffer1, &buffer2);
        let out = hex_encode(Box::new(out.into_iter()));
        assert_eq!(out, "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f");
    }

    #[test]
    fn test_hamming_distance() {
        let buf1: Vec<u8> = "this is a test".bytes().collect();
        let buf2: Vec<u8> = "wokka wokka!!!".bytes().collect();
        assert_eq!(hamming_distance(&buf1, &buf2), 37);
    }
}
